;;; cl-stack-pathlib/journal-suite: the R4b scripted edit corpus.
;;;
;;; The journal law: for every area and every edit, FS-AREA-CHANGES with a
;;; journal is EQUAL to FS-AREA-CHANGES with :RESCAN T (the full R4a rescan).
;;; RUN-JOURNAL-SUITE opens one area with :JOURNAL MECHANISM, applies the
;;; corpus steps in order and, after each step, compares the journal result
;;; with the rescan. Every journal backend (fs-backend-darwin, -linux,
;;; -windows) runs this same suite with its mechanism; cl-stack-pathlib runs
;;; it with :POLL and with fake mechanisms (tests/journal-test.lisp).
;;;
;;; Public API only (no internals), no test framework: the result is data, so
;;; a backend asserts (NULL failures) in its own runner and a suite test can
;;; check that a lying mechanism is caught.

(defpackage #:cl-stack-pathlib/journal-suite
  (:use #:cl #:cl-stack-pathlib)
  (:export #:run-journal-suite
           #:journal-suite-steps))

(in-package #:cl-stack-pathlib/journal-suite)

;;; --- environment ---------------------------------------------------------------

(defun %unix-modes-p ()
  "True when the local backend has permission bits (SBCL / ECL on unix)."
  #+(and unix (or sbcl ecl)) t
  #-(and unix (or sbcl ecl)) nil)

(defun %local-symlinks-p ()
  (not (uiop:os-windows-p)))

(defun %applicable-p (requirement backend)
  (ecase requirement
    (:always t)
    (:symlinks (or (eq backend :memory) (%local-symlinks-p)))
    (:modes (and (eq backend :local) (%unix-modes-p)))
    (:child-process (eq backend :local))))

(defun %call-with-work (backend directory fn)
  "Call FN with an empty work directory PATH on BACKEND, *FILESYSTEM* bound
to its filesystem. :LOCAL: DIRECTORY (a local directory designator, made
when missing) or a fresh temp directory, by its truename; what the suite
makes in it is deleted afterwards. :MEMORY: /work on a new memory filesystem."
  (ecase backend
    (:memory
     (let ((fs (make-memory-filesystem)))
       (with-filesystem (fs)
         (mkdir "/work" :parents t)
         (funcall fn (ensure-path "/work" :directory t)))))
    (:local
     (let* ((parent (uiop:ensure-directory-pathname
                     (or (and directory (path-pathname (ensure-path directory :directory t)))
                         (uiop:temporary-directory))))
            (dir (uiop:ensure-directory-pathname
                  (merge-pathnames (format nil "cl-stack-pathlib-journal-~36R" (random (expt 36 8)))
                                   parent))))
       (ensure-directories-exist dir)
       (unwind-protect
            (with-filesystem ((make-local-filesystem))
              (funcall fn (ensure-path (truename dir) :directory t)))
         ;; Cleanup never masks the result (a failed run may leave odd entries).
         (ignore-errors
          (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))))

;;; --- edits ---------------------------------------------------------------------

(defun %put (root rel text)
  (let ((p (join root rel)))
    (mkdir (parent p) :parents t :exist-ok t)
    (write-text p text)))

(defun %del (root rel)
  (unlink (join root rel)))

(defun %local-p (root)
  (typep (path-filesystem root) 'local-filesystem))

(defun %mv (root from to &key directory)
  "Rename FROM to TO (relative to ROOT). Local: CL:RENAME-FILE on the native
pathnames (directory form for a DIRECTORY), not the pathlib API."
  (if (%local-p root)
      (flet ((pn (rel)
               (let ((pn (path-pathname (join root rel))))
                 (if directory (uiop:ensure-directory-pathname pn) pn))))
        (rename-file (pn from) (pn to)))
      (rename-path (join root from) (join root to))))

(defun %child (root unix windows)
  "Run a shell command line in the directory ROOT in a child process: UNIX
through /bin/sh, WINDOWS through cmd.exe (UIOP:RUN-PROGRAM with a string)."
  (uiop:run-program (if (uiop:os-windows-p) windows unix)
                    :directory (path-pathname root)
                    :output :string :error-output :string))

;;; --- the corpus ------------------------------------------------------------------
;;;
;;; The base tree (opened with :IGNORE :AUTO): .gitignore ignores *.log and
;;; build/. Steps run in order on one area, so expectations are cumulative
;;; (changes since open). Each step: (name requirement edit expectation);
;;; the expectation checks the rescan itself, so a corpus edit that silently
;;; did nothing is reported as a :CORPUS failure, not taken as a pass.

(defun %base-tree ()
  `((".gitignore" ,(format nil "*.log~%build/~%"))
    ("README.md" "readme")
    ("a.txt" "aaa")
    ("b.txt" "bbb")
    ("run.sh" "echo")
    ("src/c.lisp" "(c)")
    ("docs/guide.txt" "guide")
    ("docs/api/ref.txt" "ref")
    "empty/"
    ("x.log" "log")
    ("build/out.o" "o")))

(defparameter +burst-size+ 2000)

(defun %burst-name (k) (format nil "burst/f~4,'0D.txt" k))

(defun %corpus ()
  "The steps: (name requirement edit-function expectation). EDIT-FUNCTION
takes the area root PATH. EXPECTATION: (:empty t) or (:present ((kind path)
...) :absent (path ...)), checked against the rescan."
  (list
   (list :no-edits :always
         (lambda (r) (declare (ignore r)))
         '(:empty t))
   (list :create :always
         (lambda (r) (%put r "new.txt" "new") (%put r "src/new.lisp" "(new)"))
         '(:present ((:added "new.txt") (:added "src/new.lisp"))))
   ;; Same size, same second as the copy: only the hash tells. The only
   ;; path this step touches is a.txt, so a mechanism that drops it is caught.
   (list :modify-same-size :always
         (lambda (r) (%put r "a.txt" "zzz"))
         '(:present ((:modified "a.txt"))))
   (list :modify-size :always
         (lambda (r) (%put r "b.txt" "bbbb, longer"))
         '(:present ((:modified "b.txt"))))
   (list :revert :always
         (lambda (r) (%put r "a.txt" "aaa"))
         '(:absent ("a.txt")))
   (list :delete :always
         (lambda (r) (%del r "b.txt"))
         '(:present ((:removed "b.txt"))))
   (list :rename-in-directory :always
         (lambda (r) (%mv r "src/c.lisp" "src/d.lisp"))
         '(:present ((:removed "src/c.lisp") (:added "src/d.lisp"))))
   (list :rename-across-directories :always
         (lambda (r) (%mv r "new.txt" "docs/new.txt"))
         '(:present ((:added "docs/new.txt")) :absent ("new.txt")))
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
         '(:present ((:removed "docs/guide.txt") (:removed "docs/api/ref.txt")
                     (:added "manual/guide.txt") (:added "manual/api/ref.txt")
                     (:added "manual/new.txt"))
           :absent ("docs/new.txt")))
   (list :ignored :always
         (lambda (r)
           (%put r "x.log" "changed")
           (%put r "y.log" "new")
           (%put r "build/new.o" "n")
           (%put r "build/sub/z.o" "z"))
         '(:absent ("x.log" "y.log" "build/new.o" "build/sub/z.o")))
   (list :symlink-create :symlinks
         (lambda (r)
           (create-symlink (join r "link") "a.txt")
           (create-symlink (join r "lnk-dir") "src"))
         '(:present ((:added "link") (:added "lnk-dir")) :absent ("lnk-dir/d.lisp")))
   (list :symlink-retarget :symlinks
         (lambda (r)
           (unlink (join r "link"))
           (create-symlink (join r "link") "src/d.lisp")) ; not dangling: #41
         '(:present ((:added "link"))))
   (list :mode-change :modes
         (lambda (r) (fs-chmod (path-filesystem r) (path-pathname (join r "run.sh")) #o755))
         '(:present ((:mode-changed "run.sh"))))
   ;; On a case-insensitive volume (macOS, Windows) "README.md" still opens
   ;; after the rename: only the directory listing has the new name.
   (list :case-only-rename :always
         (lambda (r) (%mv r "README.md" "readme.md"))
         '(:present ((:removed "README.md") (:added "readme.md"))))
   ;; An overflow candidate for every OS queue.
   (list :burst :always
         (lambda (r)
           (dotimes (k +burst-size+)
             (%put r (%burst-name k) "n")))
         `(:present ((:added ,(%burst-name 0)) (:added ,(%burst-name (1- +burst-size+))))))
   (list :burst-delete :always
         (lambda (r) (rmtree (join r "burst")))
         `(:absent (,(%burst-name 0) ,(%burst-name (1- +burst-size+)))))
   ;; Edits by another process: no Lisp in the loop.
   (list :child-process :child-process
         (lambda (r)
           (%child r
                   "printf child > child.txt && printf zz > a.txt && rm run.sh && mkdir kid && printf k > kid/k.txt && mv kid/k.txt kid/k2.txt"
                   "echo child> child.txt && echo zz> a.txt && del run.sh && mkdir kid && echo k> kid\\k.txt && move kid\\k.txt kid\\k2.txt > nul"))
         '(:present ((:added "child.txt") (:modified "a.txt") (:removed "run.sh")
                     (:added "kid/k2.txt"))
           :absent ("kid/k.txt")))))

(defun journal-suite-steps (&key (backend :local))
  "Names of the corpus steps RUN-JOURNAL-SUITE runs on BACKEND (:LOCAL or
:MEMORY) on this OS, in order."
  (loop for (name requirement) in (%corpus)
        when (%applicable-p requirement backend)
          collect name))

;;; --- checks ----------------------------------------------------------------------

(defun %details (changes)
  (mapcar (lambda (c) (list (fs-change-kind c) (fs-change-path c)
                            (fs-change-old-mode c) (fs-change-new-mode c)
                            (fs-change-old-hash c) (fs-change-new-hash c)))
          changes))

(defun %summary (changes)
  (mapcar (lambda (c) (list (fs-change-kind c) (fs-change-path c))) changes))

(defun %corpus-failure (name expectation rescan)
  "A :CORPUS failure plist when RESCAN (FS-CHANGE list) does not meet
EXPECTATION, else NIL."
  (let* ((summary (%summary rescan))
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
      (list :step name :kind :corpus :missing missing :unexpected unexpected))))

(defun %message (condition)
  (handler-case (princ-to-string condition)
    (error () (format nil "~S" (type-of condition)))))

;;; --- the runner --------------------------------------------------------------------

(defun run-journal-suite (mechanism &key (backend :local) (settle *journal-settle*) directory)
  "Run the corpus on one area opened with :JOURNAL MECHANISM (a mechanism
keyword such as :POLL or :INOTIFY, or T / :AUTO) on BACKEND (:LOCAL or
:MEMORY), with *JOURNAL-SETTLE* bound to SETTLE. DIRECTORY (local only): the
directory to work in (a fresh subdirectory of it is used and removed), by
default the temp directory.
→ (values failures steps): STEPS are the step names run, FAILURES a list of
plists, NIL when the mechanism keeps the law:
  (:step :open :kind :error :error message)       the area could not be opened
  (:step :open :kind :mechanism :expected m :actual a)  the area has another journal
  (:step name :kind :law :journal changes :rescan changes)   the law broke
  (:step name :kind :corpus :missing ... :unexpected ...)    the edit did not happen
  (:step name :kind :error :error message)        a step signalled (the run stops)
  (:step :close :kind :journal-open)              CLOSE-AREA left the journal open
  (:step :close :kind :error :error message)      CLOSE-AREA signalled
Changes in :LAW failures are ((kind path old-mode new-mode old-hash new-hash) ...)."
  (let ((failures '())
        (ran '()))
    (flet ((fail (&rest plist) (push plist failures)))
      (%call-with-work
       backend directory
       (lambda (work)
         (let ((*journal-settle* settle)
               (base (ensure-directory (join work "base")))
               (area nil))
           (dolist (e (%base-tree))
             (destructuring-bind (rel &optional (content "")) (if (consp e) e (list e))
               (if (char= #\/ (char rel (1- (length rel))))
                   (mkdir (join base rel) :parents t :exist-ok t)
                   (%put base rel content))))
           (handler-case
               (setf area (open-area base :ignore :auto :journal mechanism
                                          :directory (ensure-directory (join work "area"))))
             (error (c) (fail :step :open :kind :error :error (%message c))))
           (when area
             (unwind-protect
                  (let* ((journal (area-journal area))
                         (actual (and journal (journal-mechanism journal))))
                    (if (and (not (member mechanism '(t :auto))) (not (eq actual mechanism)))
                        (fail :step :open :kind :mechanism :expected mechanism :actual actual)
                        (let ((root (area-root area)))
                          (loop for (name requirement edit expectation) in (%corpus)
                                when (%applicable-p requirement backend)
                                  do (push name ran)
                                     (handler-case
                                         (progn
                                           (funcall edit root)
                                           (let* ((with-journal (fs-area-changes area))
                                                  (rescan (fs-area-changes area :rescan t))
                                                  (corpus (%corpus-failure name expectation rescan)))
                                             (when corpus (apply #'fail corpus))
                                             (unless (equal (%details with-journal) (%details rescan))
                                               (fail :step name :kind :law
                                                     :journal (%details with-journal)
                                                     :rescan (%details rescan)))))
                                       (error (c)
                                         (fail :step name :kind :error :error (%message c))
                                         (loop-finish))))
                          (let ((j (area-journal area)))
                            (handler-case
                                (progn
                                  (close-area area)
                                  (when (and j (journal-open-p j))
                                    (fail :step :close :kind :journal-open)))
                              (error (c) (fail :step :close :kind :error :error (%message c))))))))
               (when (area-open-p area)
                 (ignore-errors (close-area area)))))))))
    (values (nreverse failures) (nreverse ran))))
