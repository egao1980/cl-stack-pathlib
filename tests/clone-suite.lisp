;;; cl-stack-pathlib/clone-suite: the R4c clone laws.
;;;
;;; Every clone mechanism (fs-backend-darwin :clonefile, fs-backend-linux
;;; :ficlone, fs-backend-windows :block-clone) runs this suite with its
;;; mechanism; cl-stack-pathlib runs it with :COPY (the portable rung) and
;;; with fake mechanisms (tests/clone-test.lisp). The laws:
;;;
;;;   metadata      FS-CLONE-TREE / FS-CLONE-FILE give the source's tree:
;;;                 content, symlinks as links, permission bits, mtimes,
;;;                 directories (empty ones too), ignored paths left out.
;;;   independence  writing the clone never changes the source, and writing
;;;                 the source never changes the clone.
;;;   equivalence   the same edits give EQUAL FS-AREA-CHANGES in an area
;;;                 opened with the mechanism and in one opened with :COPY.
;;;   fallback      a clone to another volume is a :COPY with the reason
;;;                 :CROSS-VOLUME (only with :OTHER-VOLUME).
;;;
;;; Public API only, no test framework: the result is data, so a backend
;;; asserts (NULL failures) in its own runner, and a test can check that a
;;; mechanism sharing data with its source is caught.

(defpackage #:cl-stack-pathlib/clone-suite
  (:use #:cl #:cl-stack-pathlib)
  (:export #:run-clone-suite
           #:clone-suite-steps
           #:clone-suite-edits))

(in-package #:cl-stack-pathlib/clone-suite)

;;; --- environment ---------------------------------------------------------------

(defun %unix-modes-p ()
  "True when the local backend has permission bits (SBCL / ECL on unix)."
  #+(and unix (or sbcl ecl)) t
  #-(and unix (or sbcl ecl)) nil)

(defun %local-mtimes-p ()
  "True when the local backend can set mtimes (FS-SET-MTIME on SBCL unix)."
  #+(and unix sbcl) t
  #-(and unix sbcl) nil)

(defun %applicable-p (requirement backend)
  (ecase requirement
    (:always t)
    (:symlinks (or (eq backend :memory) (not (uiop:os-windows-p))))
    (:modes (and (eq backend :local) (%unix-modes-p)))
    (:mtimes (or (eq backend :memory) (%local-mtimes-p)))
    (:local (eq backend :local))
    (:child-process (eq backend :local))))

(defparameter +reasons+
  '(:no-mechanism :unsupported-filesystem :unsupported-volume :cross-volume :unavailable)
  "The locked reasons a whole call fell back to :COPY.")

(defparameter +file-reasons+
  (append +reasons+ '(:clone-failed))
  "The locked reasons of a per-file fallback.")

(defun %fresh-local-directory (parent prefix)
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "~A~36R" prefix (random (expt 36 8)))
                               (uiop:ensure-directory-pathname parent)))))
    (ensure-directories-exist dir)
    (truename dir)))

(defun %call-with-work (backend directory other-volume fn)
  "Call FN with an empty work directory PATH on BACKEND (and, local only,
an empty directory PATH under OTHER-VOLUME, or NIL), *FILESYSTEM* bound to
its filesystem. :LOCAL: a fresh subdirectory of DIRECTORY (default the temp
directory), by its truename; what the suite makes is deleted afterwards.
:MEMORY: /work on a new memory filesystem."
  (ecase backend
    (:memory
     (let ((fs (make-memory-filesystem)))
       (with-filesystem (fs)
         (mkdir "/work" :parents t)
         (funcall fn (ensure-path "/work" :directory t) nil))))
    (:local
     (let* ((parent (or (and directory (path-pathname (ensure-path directory :directory t)))
                        (uiop:temporary-directory)))
            (dir (%fresh-local-directory parent "cl-stack-pathlib-clone-"))
            (other (and other-volume
                        (%fresh-local-directory
                         (path-pathname (ensure-path other-volume :directory t))
                         "cl-stack-pathlib-clone-other-"))))
       (unwind-protect
            (with-filesystem ((make-local-filesystem))
              (funcall fn (ensure-path dir :directory t)
                       (and other (ensure-path other :directory t))))
         ;; Cleanup never masks the result (a failed run may leave odd entries).
         (dolist (d (list dir other))
           (when d
             (ignore-errors
              (uiop:delete-directory-tree d :validate t :if-does-not-exist :ignore)))))))))

;;; --- small helpers -------------------------------------------------------------

(defun %dir (root rel) (ensure-directory (join root rel)))

(defun %pn (path) (path-pathname path))

(defun %local-p (path) (typep (path-filesystem path) 'local-filesystem))

(defun %octets (string)
  (utf8-encode string))

(defun %put (root rel content)
  (let ((p (join root rel)))
    (mkdir (parent p) :parents t :exist-ok t)
    (if (stringp content) (write-text p content) (write-bytes p content))
    p))

(defun %rel (path root)
  (let* ((r (string-right-trim "/" (as-posix root)))
         (s (string-right-trim "/" (as-posix path))))
    (subseq s (1+ (length r)))))

(defun %message (condition)
  (handler-case (princ-to-string condition)
    (error () (format nil "~S" (type-of condition)))))

(defun %overwrite-in-place (path position octets)
  "Write OCTETS at POSITION in the existing file PATH without replacing it:
CL:OPEN :IF-EXISTS :OVERWRITE on the local backend (same inode), a
read-modify-write through the pathlib API elsewhere."
  (if (%local-p path)
      (with-open-file (s (%pn path) :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :overwrite :if-does-not-exist :error)
        (file-position s position)
        (write-sequence octets s))
      (let ((v (copy-seq (read-bytes path))))
        (replace v octets :start1 position)
        (write-bytes path v)))
  path)

(defun %append (path string)
  "Append STRING to PATH: CL:OPEN :IF-EXISTS :APPEND on the local backend."
  (if (%local-p path)
      (with-open-file (s (%pn path) :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :append :if-does-not-exist :error)
        (write-sequence (%octets string) s))
      (write-bytes path (%octets string) :append t))
  path)

(defun %mv (root from to &key directory)
  "Rename FROM to TO (relative to ROOT). Local: CL:RENAME-FILE on the native
pathnames (directory form for a DIRECTORY)."
  (if (%local-p root)
      (flet ((pn (rel)
               (let ((pn (%pn (join root rel))))
                 (if directory (uiop:ensure-directory-pathname pn) pn))))
        (rename-file (pn from) (pn to)))
      (rename-path (join root from) (join root to))))

(defun %child (root unix windows)
  (uiop:run-program (if (uiop:os-windows-p) windows unix)
                    :directory (%pn root)
                    :output :string :error-output :string))

(defun %signals (type thunk)
  "→ (values matched-p condition): did THUNK signal an error of TYPE?"
  (handler-case (progn (funcall thunk) (values nil nil))
    (error (c) (values (typep c type) c))))

;;; --- the base tree ---------------------------------------------------------------

(defparameter +large-size+ (* 4 1024 1024))
(defparameter +sparse-size+ (* 16 1024 1024))
(defparameter +old-mtime+ (+ 1000000000 2208988800)
  "2001-09-09T01:46:40Z as a universal time: base files get old, distinct mtimes.")

(defvar *large-octets* nil)

(defun %large-octets ()
  (or *large-octets*
      (setf *large-octets*
            (let ((v (make-array +large-size+ :element-type '(unsigned-byte 8)))
                  (x 12345))
              (declare (type (unsigned-byte 31) x))
              (dotimes (i +large-size+ v)
                (setf x (logand (+ (* x 1103515245) 12345) #x7fffffff))
                (setf (aref v i) (ldb (byte 8 16) x)))))))

(defun %base-files ()
  "(relative-path content mode) of the regular files of the base tree."
  `((".gitignore" ,(format nil "*.log~%build/~%") #o644)
    (".git/HEAD" ,(format nil "ref: refs/heads/main~%") #o644)
    ("README.md" ,(format nil "readme~%") #o644)
    ("a.txt" "aaa" #o644)
    ("b.txt" "bbb" #o644)
    ("run.sh" ,(format nil "#!/bin/sh~%echo hi~%") #o755)
    ("private.txt" "secret" #o600)
    ("src/c.lisp" "(c)" #o644)
    ("docs/guide.txt" "guide" #o644)
    ("docs/api/ref.txt" "ref" #o644)
    (,(format nil "dir with space/~C.txt" (code-char #xE9)) ,(format nil "~C~%" (code-char #xFC)) #o644)
    ("zero.bin" "" #o644)
    ("large.bin" :large #o644)
    ("x.log" "log" #o644)
    ("build/out.o" "o" #o644)
    ("build/sub/deep.o" "d" #o644)))

(defun %base-symlinks ()
  "(relative-path target): a file link and a directory link (never
followed). No link ever dangles (local listings drop a dangling relative
link: cl-stack-pathlib#41), and none points up with \"..\" (moved with its
directory by :DIRECTORY-RENAME, such a link was left behind by an area
discard in a trial run). FS-CLONE-TREE on \"..\" links is tested in
tests/clone-test.lisp."
  '(("link" "a.txt")
    ("lnk-dir" "src")))

(defun %make-sparse (path)
  "16 MiB, data only at both ends (a hole between, where the OS makes one)."
  (with-open-file (s (%pn path) :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede :if-does-not-exist :create)
    (write-sequence (%octets "head") s)
    (file-position s (- +sparse-size+ 4))
    (write-sequence (%octets "tail") s))
  path)

(defun %make-base (base backend)
  (let ((k 0))
    (dolist (f (%base-files))
      (destructuring-bind (rel content mode) f
        (let ((p (%put base rel (if (eq content :large) (%large-octets) content))))
          (chmod-if-possible p mode)
          (set-mtime p (+ +old-mtime+ (* 61 (incf k)))))))
    (mkdir (join base "empty") :parents t)
    (mkdir (join base "docs/empty-too") :parents t)
    (when (%applicable-p :symlinks backend)
      (dolist (l (%base-symlinks))
        (create-symlink (join base (first l)) (second l))))
    (when (eq backend :local)
      (let ((p (%make-sparse (join base "sparse.bin"))))
        (chmod-if-possible p #o644)
        (set-mtime p (+ +old-mtime+ (* 61 (incf k))))))
    base))

(defun chmod-if-possible (path mode)
  (fs-chmod (path-filesystem path) (%pn path) mode))

;;; --- observing trees -------------------------------------------------------------

(defun %scan (root &key ignore)
  "→ (values entries directories): ENTRIES ((rel kind mode mtime) ...) for
files and symlinks (MODE #o777 bits or NIL, MTIME of the file; NIL for a
symlink), DIRECTORIES the relative names of the real directories; both
sorted STRING<, symlinks never followed, IGNORE as WALK."
  (let ((entries '()) (dirs '()))
    (loop for (nil files subdirs) in (walk root :ignore ignore)
          do (dolist (p (append files subdirs))
               (let ((r (%rel p root)))
                 (cond ((symlink-p p) (push (list r :symlink nil nil) entries))
                       ((directory-p p) (push r dirs))
                       ((file-p p)
                        (let ((st (stat p)))
                          (push (list r :file
                                      (and (file-stat-mode st) (logand (file-stat-mode st) #o777))
                                      (file-stat-mtime st))
                                entries)))))))
    (values (sort entries #'string< :key #'first) (sort dirs #'string<))))

(defun %meta (root)
  "Everything the independence law compares: (snapshot-id entries directories)."
  (multiple-value-bind (entries dirs) (%scan root)
    (list (snapshot root :store (make-memory-snapshot-store)) entries dirs)))

(defun %details (changes)
  (mapcar (lambda (c) (list (fs-change-kind c) (fs-change-path c)
                            (fs-change-old-mode c) (fs-change-new-mode c)
                            (fs-change-old-hash c) (fs-change-new-hash c)))
          changes))

(defun %summary (changes)
  (mapcar (lambda (c) (list (fs-change-kind c) (fs-change-path c))) changes))

(defun %tree-changes (a a-ignore b b-ignore)
  "Summary of the snapshot changes from tree A to tree B."
  (let* ((store (make-memory-snapshot-store))
         (ia (snapshot a :ignore a-ignore :store store))
         (ib (snapshot b :ignore b-ignore :store store)))
    (%summary (snapshot-changes ia ib :store store))))

(defun %regular-file-count (root ignore)
  (let ((store (make-memory-snapshot-store)))
    (count :symlink (snapshot-entries (snapshot root :ignore ignore :store store) :store store)
           :key #'snapshot-entry-mode :test-not #'eq)))

;;; --- the suite state -----------------------------------------------------------

(defstruct (%ctx (:constructor %make-ctx (mechanism backend work base other)))
  mechanism backend work base other
  (failures '()))

(defun %fail (ctx step kind &rest plist)
  (push (list* :step step :kind kind plist) (%ctx-failures ctx))
  nil)

(defun %fs (ctx) (path-filesystem (%ctx-base ctx)))

(defun %req (ctx requirement) (%applicable-p requirement (%ctx-backend ctx)))

;;; --- checks --------------------------------------------------------------------

(defun %check-report (ctx step report expected-files)
  "The report of a tree or file call that had to use the suite's mechanism."
  (let ((m (%ctx-mechanism ctx)))
    (cond
      ((not (clone-report-p report))
       (%fail ctx step :report :check :type :actual report))
      (t
       (unless (eq (clone-report-mechanism report) m)
         (%fail ctx step :mechanism :check :report-mechanism
                                    :expected m :actual (clone-report-mechanism report)
                                    :reason (clone-report-reason report)))
       (when (clone-report-reason report)
         (%fail ctx step :mechanism :check :report-reason :actual (clone-report-reason report)))
       (let ((cloned (clone-report-cloned report))
             (copied (clone-report-copied report))
             (fallbacks (clone-report-fallbacks report)))
         (unless (eql expected-files (+ cloned copied))
           (%fail ctx step :report :check :file-count :expected expected-files
                                   :actual (list cloned copied)))
         (cond ((eq m :copy)
                (unless (and (eql cloned 0) (null fallbacks))
                  (%fail ctx step :report :check :copy-rung :cloned cloned :fallbacks fallbacks)))
               (t
                ;; A one-file call may fall back (a per-file reason); a tree
                ;; whose every file fell back did not use the mechanism.
                (when (and (> expected-files 1) (eql cloned 0))
                  (%fail ctx step :mechanism :check :nothing-cloned :copied copied
                                             :fallbacks fallbacks))
                (unless (eql copied (length fallbacks))
                  (%fail ctx step :report :check :fallback-count :copied copied
                                          :fallbacks fallbacks))))
         (unless (and (every (lambda (f) (and (consp f) (stringp (car f))
                                              (member (cdr f) +file-reasons+)))
                             fallbacks)
                      (equal (mapcar #'car fallbacks)
                             (sort (mapcar #'car fallbacks) #'string<)))
           (%fail ctx step :report :check :fallback-shape :fallbacks fallbacks)))))))

(defun %check-tree (ctx step source destination ignore)
  "DESTINATION holds SOURCE's tree (minus IGNORE): the metadata law."
  (let ((changes (%tree-changes source ignore destination nil)))
    (when changes
      (%fail ctx step :content :changes changes)))
  (multiple-value-bind (src-entries src-dirs) (%scan source :ignore ignore)
    (multiple-value-bind (dst-entries dst-dirs) (%scan destination)
      (unless (equal src-dirs dst-dirs)
        (%fail ctx step :metadata :check :directories :expected src-dirs :actual dst-dirs))
      (dolist (s src-entries)
        (let ((d (find (first s) dst-entries :key #'first :test #'string=)))
          (cond ((null d)
                 (%fail ctx step :metadata :check :missing :path (first s)))
                ((not (eq (second s) (second d)))
                 (%fail ctx step :metadata :check :kind :path (first s)
                                           :expected (second s) :actual (second d)))
                ((eq (second s) :file)
                 (when (and (%req ctx :modes) (not (eql (third s) (third d))))
                   (%fail ctx step :metadata :check :mode :path (first s)
                                             :expected (third s) :actual (third d)))
                 (when (and (%req ctx :mtimes) (not (eql (fourth s) (fourth d))))
                   (%fail ctx step :metadata :check :mtime :path (first s)
                                             :expected (fourth s) :actual (fourth d))))))))))

(defun %check-unchanged (ctx step what before after)
  "The independence law: BEFORE and AFTER are %META of the same tree."
  (unless (equal before after)
    (%fail ctx step :independence :tree what
           :entries (set-exclusive-or (second before) (second after) :test #'equal)
           :directories (set-exclusive-or (third before) (third after) :test #'equal)
           :same-id (equal (first before) (first after)))))

(defun %check-signals (ctx step check type thunk &key absent)
  "THUNK must signal TYPE, and ABSENT (a PATH), when given, must not exist after."
  (multiple-value-bind (ok c) (%signals type thunk)
    (unless ok
      (%fail ctx step :protocol :check check :expected type
                                :actual (and c (type-of c)) :message (and c (%message c)))))
  (when (and absent (or (exists-p absent) (symlink-p absent)))
    (%fail ctx step :protocol :check check :left-behind (as-posix absent))
    (ignore-errors (if (directory-p absent) (rmtree absent) (unlink absent)))))

;;; --- steps -------------------------------------------------------------------------

(defun %step-supported (ctx)
  (let ((fs (%fs ctx)) (m (%ctx-mechanism ctx))
        (base (%pn (%ctx-base ctx))) (work (%pn (%ctx-work ctx))))
    (let ((all (clone-mechanisms)))
      (unless (eq :copy (car (last all)))
        (%fail ctx :supported :protocol :check :copy-last :actual all)))
    (multiple-value-bind (name reason) (fs-clone-supported-p fs base work)
      (unless (or (and (keywordp name) (null reason))
                  (and (null name) (member reason +reasons+)))
        (%fail ctx :supported :protocol :check :supported-values :actual (list name reason)))
      (unless (eq m :copy)
        (let ((available (clone-mechanisms fs base work)))
          (unless (member m available)
            (%fail ctx :supported :mechanism :check :available
                                  :expected m :actual available :reason reason))
          (unless name
            (%fail ctx :supported :mechanism :check :supported-p :actual (list name reason))))))))

(defun %clone-and-check (ctx step ignore dst-name)
  (let* ((base (%ctx-base ctx))
         (dst (%dir (%ctx-work ctx) dst-name))
         (report (fs-clone-tree (%fs ctx) (%pn base) (%pn dst)
                                :ignore ignore :mechanism (%ctx-mechanism ctx))))
    (%check-report ctx step report (%regular-file-count base ignore))
    (%check-tree ctx step base dst ignore)
    dst))

(defun %step-tree (ctx) (%clone-and-check ctx :tree :auto "tree-auto"))
(defun %step-no-ignore (ctx) (%clone-and-check ctx :no-ignore nil "tree-all"))
(defun %step-matcher (ctx)
  (%clone-and-check ctx :matcher (parse-ignore (list "docs/" "zero.bin")) "tree-matcher"))

(defun %file-meta (path)
  (let ((st (stat path)))
    (list (content-hash path)
          (file-stat-size st)
          (and (file-stat-mode st) (logand (file-stat-mode st) #o777))
          (file-stat-mtime st))))

(defun %check-file (ctx step check source destination)
  (let ((s (%file-meta source)) (d (%file-meta destination)))
    (unless (and (equal (first s) (first d)) (eql (second s) (second d)))
      (%fail ctx step :content :check check :expected (subseq s 0 2) :actual (subseq d 0 2)))
    (when (and (%req ctx :modes) (not (eql (third s) (third d))))
      (%fail ctx step :metadata :check check :mode (list (third s) (third d))))
    (when (and (%req ctx :mtimes) (not (eql (fourth s) (fourth d))))
      (%fail ctx step :metadata :check check :mtime (list (fourth s) (fourth d))))))

(defun %step-file (ctx)
  (let* ((fs (%fs ctx)) (m (%ctx-mechanism ctx)) (base (%ctx-base ctx))
         (dir (%dir (%ctx-work ctx) "files")))
    (mkdir dir :parents t)
    (flet ((clone (from to &rest keys)
             (apply #'fs-clone-file fs (%pn from) (%pn to) :mechanism m keys)))
      ;; Content, mode, mtime; the report of one file.
      (dolist (name '("large.bin" "private.txt" "run.sh" "zero.bin"))
        (let ((r (clone (join base name) (join dir name))))
          (%check-report ctx :file r 1)
          (%check-file ctx :file name (join base name) (join dir name))))
      ;; Missing parents are made.
      (clone (join base "a.txt") (join dir "new/deeper/a.txt"))
      (%check-file ctx :file :parents (join base "a.txt") (join dir "new/deeper/a.txt"))
      ;; An existing destination: PATH-EXISTS-ERROR with OVERWRITE, or :REPLACE T.
      (let ((target (%put dir "existing.txt" "old content")))
        (%check-signals ctx :file :exists 'path-exists-error
                        (lambda () (clone (join base "a.txt") target)))
        (unless (equal "old content" (read-text target))
          (%fail ctx :file :protocol :check :exists-untouched))
        (let ((offered nil))
          (ignore-errors
           (handler-bind ((path-exists-error
                            (lambda (c)
                              (let ((r (find-restart 'overwrite c)))
                                (setf offered (and r t))
                                (when r (invoke-restart r))))))
             (clone (join base "a.txt") target)))
          (unless offered
            (%fail ctx :file :protocol :check :overwrite-restart))
          (%check-file ctx :file :overwrite (join base "a.txt") target))
        (%put dir "existing.txt" "old content again")
        (clone (join base "b.txt") target :replace t)
        (%check-file ctx :file :replace (join base "b.txt") target))
      ;; No temp file survives.
      (let ((names (sort (mapcar #'name (iterdir dir)) #'string<)))
        (unless (equal names (sort (list "existing.txt" "large.bin" "new" "private.txt"
                                         "run.sh" "zero.bin")
                                   #'string<))
          (%fail ctx :file :protocol :check :leftovers :actual names)))
      ;; A symlink source gives a symlink with the same target.
      (when (%req ctx :symlinks)
        (let ((dst (join dir "link")))
          (clone (join base "link") dst)
          (unless (and (symlink-p dst)
                       (equal (as-posix (read-symlink dst)) (as-posix (read-symlink (join base "link")))))
            (%fail ctx :file :metadata :check :symlink :path "link"))))
      ;; Refusals.
      (%check-signals ctx :file :directory-source 'path-error
                      (lambda () (clone (%dir base "src") (join dir "src-copy")))
                      :absent (join dir "src-copy"))
      (%check-signals ctx :file :missing-source 'path-not-found
                      (lambda () (clone (join base "no-such-file") (join dir "nothing")))
                      :absent (join dir "nothing"))
      (%check-signals ctx :file :directory-destination 'path-error
                      (lambda () (clone (join base "a.txt") (%dir dir "new"))))
      (%check-signals ctx :file :unknown-mechanism 'unsupported-operation
                      (lambda () (fs-clone-file fs (%pn (join base "a.txt")) (%pn (join dir "u.txt"))
                                                :mechanism :no-such-clone-mechanism))
                      :absent (join dir "u.txt")))))

(defun %step-sparse (ctx)
  (let* ((base (%ctx-base ctx))
         (dst (join (%dir (%ctx-work ctx) "sparse") "sparse.bin")))
    (let ((r (fs-clone-file (%fs ctx) (%pn (join base "sparse.bin")) (%pn dst)
                            :mechanism (%ctx-mechanism ctx))))
      (%check-report ctx :sparse r 1)
      (unless (eql +sparse-size+ (file-size dst))
        (%fail ctx :sparse :content :check :size :actual (file-size dst)))
      (%check-file ctx :sparse :sparse (join base "sparse.bin") dst))))

(defun %step-errors (ctx)
  (let* ((fs (%fs ctx)) (m (%ctx-mechanism ctx)) (base (%ctx-base ctx)) (work (%ctx-work ctx)))
    (flet ((clone (from to &optional (mechanism m))
             (fs-clone-tree fs (%pn from) (%pn to) :mechanism mechanism)))
      (%check-signals ctx :errors :missing-source 'path-not-found
                      (lambda () (clone (%dir work "no-such-dir") (%dir work "e1")))
                      :absent (%dir work "e1"))
      (let ((busy (%dir work "e2")))
        (%put busy "keep.txt" "keep")
        (%check-signals ctx :errors :non-empty-destination 'path-exists-error
                        (lambda () (clone base busy)))
        (unless (and (equal '("keep.txt") (mapcar #'name (iterdir busy)))
                     (equal "keep" (read-text (join busy "keep.txt"))))
          (%fail ctx :errors :protocol :check :non-empty-untouched)))
      (%check-signals ctx :errors :inside-source 'path-error
                      (lambda () (clone base (%dir base "inner")))
                      :absent (%dir base "inner"))
      (%check-signals ctx :errors :file-source 'path-error
                      (lambda () (clone (join base "a.txt") (%dir work "e3")))
                      :absent (%dir work "e3"))
      (%check-signals ctx :errors :unknown-mechanism 'unsupported-operation
                      (lambda () (clone base (%dir work "e4") :no-such-clone-mechanism))
                      :absent (%dir work "e4"))
      ;; An empty destination directory is used as it is.
      (let ((empty (%dir work "e5")))
        (mkdir empty :parents t)
        (clone base empty)
        (%check-tree ctx :errors base empty nil)))))

(defun %step-independence (ctx)
  (let* ((base (%ctx-base ctx))
         (before (%meta base))
         (dst (%dir (%ctx-work ctx) "indep")))
    (fs-clone-tree (%fs ctx) (%pn base) (%pn dst) :ignore :auto :mechanism (%ctx-mechanism ctx))
    (let ((clone-before (%meta dst)))
      ;; Every kind of write to the clone.
      (%overwrite-in-place (join dst "large.bin") 0 (%octets "LRG!"))
      (%append (join dst "a.txt") "+appended")
      (write-bytes (join dst "b.txt") (make-array 0 :element-type '(unsigned-byte 8)))
      (write-text (join dst "README.md") "replaced" :atomic t)
      (when (%req ctx :modes)
        (chmod-if-possible (join dst "private.txt") #o644))
      (when (%req ctx :mtimes)
        (set-mtime (join dst "src/c.lisp") (get-universal-time)))
      (unlink (join dst "docs/guide.txt"))
      (%mv dst "run.sh" "run2.sh")
      (%put dst "new.txt" "new")
      (when (%req ctx :local)
        (%overwrite-in-place (join dst "sparse.bin") (floor +sparse-size+ 2) (%octets "hole")))
      (when (equal clone-before (%meta dst))
        (%fail ctx :independence :corpus :check :clone-edits-did-nothing))
      (%check-unchanged ctx :independence :source before (%meta base)))))

(defun %step-independence-reverse (ctx)
  (let* ((base (%ctx-base ctx))
         (dst (%dir (%ctx-work ctx) "reverse")))
    (fs-clone-tree (%fs ctx) (%pn base) (%pn dst) :ignore :auto :mechanism (%ctx-mechanism ctx))
    (let ((before (%meta dst)))
      (%overwrite-in-place (join base "large.bin") 8 (%octets "SRC!"))
      (%append (join base "a.txt") "+source")
      (when (%req ctx :modes)
        (chmod-if-possible (join base "private.txt") #o640))
      (%check-unchanged ctx :independence-reverse :clone before (%meta dst)))))

;;; --- the equivalence edit script ---------------------------------------------------
;;;
;;; Applied in order to an area opened with the mechanism and to one opened
;;; with :COPY over the same base (:IGNORE :AUTO). Expectations are
;;; cumulative (changes since open) and check the :COPY area, so an edit that
;;; silently did nothing is a :CORPUS failure, not a pass.

(defun %edits ()
  "(name requirement edit-function expectation); EDIT-FUNCTION takes the area root."
  (list
   (list :none :always (lambda (r) (declare (ignore r))) '(:empty t))
   (list :create :always
         (lambda (r) (%put r "new.txt" "new") (%put r "src/new.lisp" "(new)"))
         '(:present ((:added "new.txt") (:added "src/new.lisp"))))
   ;; Same size, same second: only the hash tells.
   (list :modify-same-size :always
         (lambda (r) (%put r "a.txt" "zzz"))
         '(:present ((:modified "a.txt"))))
   ;; A write into a cloned file (shared blocks on a COW volume).
   (list :in-place :always
         (lambda (r) (%overwrite-in-place (join r "large.bin") 1000 (%octets "XXXX")))
         '(:present ((:modified "large.bin"))))
   (list :append :always
         (lambda (r) (%append (join r "b.txt") "+more"))
         '(:present ((:modified "b.txt"))))
   (list :truncate :always
         (lambda (r) (write-bytes (join r "README.md") (make-array 0 :element-type '(unsigned-byte 8))))
         '(:present ((:modified "README.md"))))
   (list :revert :always
         (lambda (r) (%put r "a.txt" "aaa"))
         '(:absent ("a.txt")))
   (list :delete :always
         (lambda (r) (unlink (join r "docs/guide.txt")))
         '(:present ((:removed "docs/guide.txt"))))
   (list :rename :always
         (lambda (r) (%mv r "src/c.lisp" "src/d.lisp"))
         '(:present ((:removed "src/c.lisp") (:added "src/d.lisp"))))
   (list :directory-create :always
         (lambda (r)
           (%put r "tree/x/1.txt" "1")
           (%put r "tree/x/y/2.txt" "2")
           (mkdir (join r "tree/empty") :parents t))
         '(:present ((:added "tree/x/1.txt") (:added "tree/x/y/2.txt"))))
   (list :directory-delete :always
         (lambda (r) (rmtree (join r "tree")))
         '(:absent ("tree/x/1.txt" "tree/x/y/2.txt")))
   (list :directory-rename :always
         (lambda (r) (%mv r "docs" "manual" :directory t))
         '(:present ((:removed "docs/api/ref.txt") (:added "manual/api/ref.txt"))
           :absent ("manual/guide.txt")))
   (list :ignored :always
         (lambda (r)
           (%put r "x.log" "changed")
           (%put r "y.log" "new")
           (%put r "build/new.o" "n"))
         '(:absent ("x.log" "y.log" "build/new.o")))
   (list :symlink-create :symlinks
         (lambda (r) (create-symlink (join r "link2") "zero.bin")) ; never removed: #41
         '(:present ((:added "link2"))))
   (list :symlink-retarget :symlinks
         (lambda (r)
           (unlink (join r "link"))
           (create-symlink (join r "link") "zero.bin"))
         '(:present ((:modified "link"))))
   (list :mode-change :modes
         (lambda (r) (chmod-if-possible (join r "run.sh") #o644))
         '(:present ((:mode-changed "run.sh"))))
   ;; Group / other bits are not part of an entry: no change.
   (list :private-mode :modes
         (lambda (r) (chmod-if-possible (join r "private.txt") #o640))
         '(:absent ("private.txt")))
   (list :sparse-write :local
         (lambda (r) (%overwrite-in-place (join r "sparse.bin") (* 8 1024 1024) (%octets "hole")))
         '(:present ((:modified "sparse.bin"))))
   (list :child-process :child-process
         (lambda (r)
           (%child r
                   "printf child > child.txt && printf zz > a.txt && rm README.md"
                   "echo child> child.txt && echo zz> a.txt && del README.md"))
         '(:present ((:added "child.txt") (:modified "a.txt") (:removed "README.md"))))))

(defun %corpus-failure (expectation changes)
  (let* ((summary (%summary changes))
         (missing (if (getf expectation :empty)
                      '()
                      (remove-if (lambda (kp) (member kp summary :test #'equal))
                                 (getf expectation :present))))
         (unexpected (if (getf expectation :empty)
                         summary
                         (remove-if-not (lambda (kp) (member (second kp) (getf expectation :absent)
                                                             :test #'string=))
                                        summary))))
    (when (or missing unexpected)
      (list :missing missing :unexpected unexpected))))

(defun clone-suite-edits (&key (backend :local))
  "Names of the equivalence edits RUN-CLONE-SUITE applies on BACKEND on this OS, in order."
  (loop for (name requirement) in (%edits)
        when (%applicable-p requirement backend)
          collect name))

(defun %step-equivalence (ctx)
  (let* ((m (%ctx-mechanism ctx))
         (base (%ctx-base ctx))
         (work (%ctx-work ctx))
         (base-before (snapshot base :ignore :auto :store (make-memory-snapshot-store)))
         (a nil) (b nil))
    (unwind-protect
         (progn
           (setf a (open-area base :ignore :auto :mechanism m :directory (%dir work "area-clone"))
                 b (open-area base :ignore :auto :mechanism :copy :directory (%dir work "area-copy")))
           (unless (and (eq (area-mechanism a) (if (eq m :copy) :copy :clone))
                        (eq (area-mechanism b) :copy)
                        (clone-report-p (area-clone-report a))
                        (eq m (clone-report-mechanism (area-clone-report a))))
             (%fail ctx :equivalence :mechanism :check :area
                                     :actual (list (area-mechanism a) (area-mechanism b)
                                                   (and (area-clone-report a)
                                                        (clone-report-mechanism (area-clone-report a))))))
           (flet ((summary (area)
                    (mapcar (lambda (e) (list (snapshot-entry-path e) (snapshot-entry-mode e)
                                              (snapshot-entry-hash e)))
                            (area-manifest area))))
             (unless (equal (summary a) (summary b))
               (%fail ctx :equivalence :equivalence :edit :open
                                       :clone (summary a) :copy (summary b))))
           (loop for (name requirement edit expectation) in (%edits)
                 when (%req ctx requirement)
                   do (handler-case
                          (progn
                            (funcall edit (area-root a))
                            (funcall edit (area-root b))
                            (let* ((ca (fs-area-changes a))
                                   (ra (fs-area-changes a :rescan t))
                                   (cb (fs-area-changes b))
                                   (corpus (%corpus-failure expectation cb)))
                              (when corpus
                                (apply #'%fail ctx :equivalence :corpus :edit name corpus))
                              (unless (and (equal (%details ca) (%details cb))
                                           (equal (%details ra) (%details cb)))
                                (%fail ctx :equivalence :equivalence :edit name
                                                        :clone (%details ca)
                                                        :clone-rescan (%details ra)
                                                        :copy (%details cb)))))
                        (error (c)
                          (%fail ctx :equivalence :error :edit name :error (%message c))
                          (loop-finish))))
           (close-area a)
           (close-area b)
           (unless (equal base-before (snapshot base :ignore :auto :store (make-memory-snapshot-store)))
             (%fail ctx :equivalence :independence :tree :base)))
      (dolist (area (list a b))
        (when (and area (area-open-p area))
          (ignore-errors (close-area area)))))))

(defun %step-cross-volume (ctx)
  (let* ((fs (%fs ctx)) (m (%ctx-mechanism ctx)) (base (%ctx-base ctx))
         (other (%ctx-other ctx)))
    (multiple-value-bind (name reason) (fs-clone-supported-p fs (%pn base) (%pn other))
      (unless (and (null name) (eq reason :cross-volume))
        (%fail ctx :cross-volume :fallback :check :supported-p :actual (list name reason))))
    (when (member m (clone-mechanisms fs (%pn base) (%pn other)))
      (%fail ctx :cross-volume :fallback :check :available :actual (clone-mechanisms fs (%pn base) (%pn other))))
    ;; :AUTO copies and says why.
    (let* ((dst (%dir other "xv-auto"))
           (r (fs-clone-tree fs (%pn base) (%pn dst) :ignore :auto)))
      (unless (and (clone-report-p r)
                   (eq :copy (clone-report-mechanism r))
                   (eq :cross-volume (clone-report-reason r))
                   (eql 0 (clone-report-cloned r)))
        (%fail ctx :cross-volume :fallback :check :auto-report
                                 :actual (and (clone-report-p r)
                                              (list (clone-report-mechanism r) (clone-report-reason r)
                                                    (clone-report-cloned r)))))
      (%check-tree ctx :cross-volume base dst :auto))
    ;; The mechanism itself, :CLONE and a :CLONE area refuse, leaving nothing.
    (%check-signals ctx :cross-volume :explicit 'unsupported-operation
                    (lambda () (fs-clone-tree fs (%pn base) (%pn (%dir other "xv-m")) :mechanism m))
                    :absent (%dir other "xv-m"))
    (%check-signals ctx :cross-volume :clone 'unsupported-operation
                    (lambda () (fs-clone-tree fs (%pn base) (%pn (%dir other "xv-c")) :mechanism :clone))
                    :absent (%dir other "xv-c"))
    (%check-signals ctx :cross-volume :clone-area 'unsupported-operation
                    (lambda () (open-area base :mechanism :clone :directory (%dir other "xv-area")))
                    :absent (%dir other "xv-area"))
    (let ((area (open-area base :ignore :auto :directory (%dir other "xv-area-auto"))))
      (unwind-protect
           (unless (and (eq :copy (area-mechanism area))
                        (eq :cross-volume (clone-report-reason (area-clone-report area))))
             (%fail ctx :cross-volume :fallback :check :auto-area
                                      :actual (list (area-mechanism area)
                                                    (clone-report-reason (area-clone-report area)))))
        (close-area area)))))

;;; --- the runner --------------------------------------------------------------------

(defun %steps ()
  "(name requirement function needs-other needs-clone-mechanism)"
  (list (list :supported :always #'%step-supported nil nil)
        (list :tree :always #'%step-tree nil nil)
        (list :no-ignore :always #'%step-no-ignore nil nil)
        (list :matcher :always #'%step-matcher nil nil)
        (list :file :always #'%step-file nil nil)
        (list :sparse :local #'%step-sparse nil nil)
        (list :errors :always #'%step-errors nil nil)
        (list :independence :always #'%step-independence nil nil)
        (list :equivalence :always #'%step-equivalence nil nil)
        (list :cross-volume :local #'%step-cross-volume t t)
        (list :independence-reverse :always #'%step-independence-reverse nil nil)))

(defun clone-suite-steps (&key (backend :local) (mechanism :copy) other-volume)
  "Names of the steps RUN-CLONE-SUITE runs for MECHANISM on BACKEND on this
OS, in order. :CROSS-VOLUME needs OTHER-VOLUME, the local backend and a
mechanism other than :COPY."
  (loop for (name requirement nil needs-other needs-clone) in (%steps)
        when (and (%applicable-p requirement backend)
                  (or (not needs-other) other-volume)
                  (or (not needs-clone) (not (eq mechanism :copy))))
          collect name))

(defun run-clone-suite (mechanism &key (backend :local) directory other-volume)
  "Run the clone laws for MECHANISM (:COPY, or a registered clone mechanism
name such as :CLONEFILE) on BACKEND (:LOCAL or :MEMORY). DIRECTORY (local
only): the directory to work in (a fresh subdirectory of it is used and
removed), by default the temp directory; it must be on a volume where
MECHANISM is available. OTHER-VOLUME (local only): a directory on another
volume, where MECHANISM cannot clone from DIRECTORY; it enables the
:CROSS-VOLUME step.
→ (values failures steps): STEPS are the step names run, FAILURES a list of
plists, NIL when every law holds. Each failure has :STEP and :KIND:
  :error         the step (or :setup) signalled; :ERROR is the message
  :mechanism     the mechanism was not used (not available, a report naming
                 another mechanism or a reason, nothing cloned, area kind)
  :report        the CLONE-REPORT counts or fallbacks are malformed
  :content       the destination's snapshot differs (:CHANGES)
  :metadata      a directory, kind, mode or mtime differs (:CHECK, :PATH)
  :protocol      a refusal or restart did not happen as locked (:CHECK)
  :independence  a write on one side changed the other (:TREE :SOURCE /
                 :CLONE / :BASE)
  :equivalence   FS-AREA-CHANGES differ between the mechanism and :COPY
                 (:EDIT, :CLONE, :CLONE-RESCAN, :COPY)
  :corpus        an edit did not do what the script expects (:EDIT)
  :fallback      the cross-volume fallback was not a reported :COPY"
  (let ((ran '()) (ctx nil))
    (%call-with-work
     backend directory other-volume
     (lambda (work other)
       (let ((base (%dir work "base")))
         (setf ctx (%make-ctx mechanism backend work base other))
         (if (handler-case (progn (%make-base base backend) t)
               (error (c) (%fail ctx :setup :error :error (%message c))))
             (loop for (name requirement fn needs-other needs-clone) in (%steps)
                   when (and (%applicable-p requirement backend)
                             (or (not needs-other) other)
                             (or (not needs-clone) (not (eq mechanism :copy))))
                     do (push name ran)
                        (handler-case (funcall fn ctx)
                          (error (c) (%fail ctx name :error :error (%message c)))))))))
    (values (reverse (%ctx-failures ctx)) (nreverse ran))))
