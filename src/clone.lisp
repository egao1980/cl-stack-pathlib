(in-package #:cl-stack-pathlib)

;;; R4c-0: copy-on-write clones, the protocol and the portable copy rung.
;;;
;;; FS-CLONE-TREE / FS-CLONE-FILE make an independent copy of a tree or a
;;; file. A clone mechanism (APFS clonefile(2), Linux FICLONE, ReFS block
;;; cloning) shares data blocks with the source until either side is
;;; written; the fs-backend-* repos register theirs in *CLONE-MECHANISMS*.
;;; With none registered, or none available for the two directories, the
;;; result is the portable :COPY rung: the same tree, the same metadata (per
;;; the locked list in the brief), only slower and bigger. Areas
;;; (area.lisp) are built with FS-CLONE-TREE, so :CLONE and :COPY areas give
;;; the same FS-AREA-CHANGES for the same edits (the R4c law, checked by
;;; cl-stack-pathlib/clone-suite).
;;;
;;; Every call is a read-only prefix (paths checked, ignore resolved, the
;;; mechanism probed and chosen; every refusal is signalled here) followed
;;; by an acting suffix in UNWIND-PROTECT that restores DESTINATION when
;;; anything escapes.
;;; --- conditions and restarts -------------------------------------------------------

(define-condition clone-error (path-error)
  ((mechanism :initarg :mechanism :reader clone-error-mechanism :initform nil
              :documentation "Keyword of the clone mechanism that failed.")
   (operation :initarg :operation :reader clone-error-operation :initform nil
              :documentation "String naming the failing call, e.g. \"clonefile\".")
   (code :initarg :code :reader clone-error-code :initform nil
         :documentation "The OS error code (errno, GetLastError), or NIL."))
  (:documentation "A clone mechanism could not clone the file at PATH for a
reason that a plain copy may still get past (EACCES on the clone call, an
I/O error, ...). FS-CLONE-TREE and FS-CLONE-FILE offer the restart USE-COPY
(copy this file instead) around every mechanism call. Unhandled, it leaves
the destination as it was before the call.")
  (:report (lambda (c s)
             (format s "~@[~A: ~]clone of ~S failed~@[ in ~A~]~@[ (code ~A)~]"
                     (clone-error-mechanism c) (path-error-path c)
                     (clone-error-operation c) (clone-error-code c)))))

(defun invoke-use-copy (&optional condition)
  "Invoke the USE-COPY restart (copy the file instead of cloning it), if any."
  (let ((r (find-restart 'use-copy condition)))
    (when r (invoke-restart r))))

;;; --- the report ------------------------------------------------------------------

(defclass clone-report ()
  ((source :initarg :source :reader clone-report-source
           :documentation "PATH of the source tree or file.")
   (destination :initarg :destination :reader clone-report-destination
                :documentation "PATH of the destination tree or file.")
   (mechanism :initarg :mechanism :reader clone-report-mechanism
              :documentation "The mechanism that made the tree: :COPY or a registered name.")
   (reason :initarg :reason :reader clone-report-reason :initform nil
           :documentation "NIL, or why :COPY was used for the whole call when a clone was
asked for (:AUTO): :NO-MECHANISM, :UNSUPPORTED-FILESYSTEM, :UNSUPPORTED-VOLUME,
:CROSS-VOLUME or :UNAVAILABLE.")
   (cloned :initarg :cloned :reader clone-report-cloned :initform 0
           :documentation "Number of regular files the mechanism cloned.")
   (copied :initarg :copied :reader clone-report-copied :initform 0
           :documentation "Number of regular files copied (the :COPY rung, or per-file fallbacks).")
   (fallbacks :initarg :fallbacks :reader clone-report-fallbacks :initform nil
              :documentation "((relative-path . reason) ...) sorted STRING<: files a
clone mechanism handed back to the copy rung."))
  (:documentation "What FS-CLONE-TREE or FS-CLONE-FILE did."))

(defun clone-report-p (x) (typep x 'clone-report))

(defmethod print-object ((r clone-report) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~S~@[ (~S)~] cloned=~D copied=~D"
            (ignore-errors (clone-report-mechanism r))
            (ignore-errors (clone-report-reason r))
            (ignore-errors (clone-report-cloned r))
            (ignore-errors (clone-report-copied r)))))

;;; --- protocol ------------------------------------------------------------------

(defgeneric fs-clone-tree (fs source destination &key ignore mechanism)
  (:documentation "Make DESTINATION an independent copy of the directory tree
SOURCE on FS and return a CLONE-REPORT. DESTINATION must be absent (it is
created, with its parents) or an empty directory, and not inside SOURCE.
IGNORE: NIL (default), an IGNORE-MATCHER relative to SOURCE, or :AUTO
(LOAD-IGNORE-FILES + .git pruned), as FS-SNAPSHOT. Regular files keep their
content, permission bits (#o777, where the backend has modes) and mtime (to
the second, where the backend can set it: FS-SET-MTIME); symlinks are made
as symlinks with the same target string and never followed; directories are
created (empty ones too) with default modes; other kinds (FIFOs, sockets,
devices) and ignored paths are left out; ignored directories are not
descended.
MECHANISM: :AUTO (default) the best-ranked clone mechanism available for
SOURCE and DESTINATION, else :COPY with the REASON in the report; :COPY the
portable rung; :CLONE the best-ranked available clone mechanism, else
UNSUPPORTED-OPERATION; a registered name: that mechanism, or
UNSUPPORTED-OPERATION when it is unavailable for these directories. Unknown
names signal UNSUPPORTED-OPERATION. Every refusal happens before anything is
created. A missing SOURCE signals PATH-NOT-FOUND; a SOURCE that is not a
directory, or a DESTINATION inside SOURCE, PATH-ERROR; a non-empty
DESTINATION PATH-EXISTS-ERROR. Any condition that escapes leaves DESTINATION
as it was before the call."))

(defgeneric fs-clone-file (fs source destination &key mechanism replace)
  (:documentation "Make DESTINATION an independent copy of the file SOURCE on
FS (content, permission bits, mtime, as FS-CLONE-TREE) and return a
CLONE-REPORT. A symlink SOURCE gives a symlink with the same target. Missing
parents of DESTINATION are created. An existing DESTINATION signals
PATH-EXISTS-ERROR with the restart OVERWRITE unless REPLACE is true; a
replaced file is made under a temp name in its directory and renamed over
it, so readers see the old or the new file. MECHANISM as FS-CLONE-TREE. A
missing SOURCE signals PATH-NOT-FOUND; a directory SOURCE or DESTINATION,
PATH-ERROR."))

(defgeneric fs-clone-supported-p (fs directory &optional destination)
  (:documentation "→ (values mechanism reason). MECHANISM: the name of the
best-ranked clone mechanism available for cloning from DIRECTORY to
DESTINATION (default DIRECTORY itself, i.e. within its volume), or NIL; then
REASON says why: :NO-MECHANISM (none registered), or the reason the
best-ranked registered mechanism gave (:UNSUPPORTED-FILESYSTEM,
:UNSUPPORTED-VOLUME, :CROSS-VOLUME, :UNAVAILABLE). A DESTINATION that does
not exist yet is judged by its nearest existing ancestor directory."))

(defgeneric fs-set-mtime (fs pathname universal-time)
  (:documentation "Set the modification time of PATHNAME (symlinks followed)
to UNIVERSAL-TIME (whole seconds). Returns T when it was set, NIL on a
backend or platform that cannot set it (zip; local off SBCL unix), where it
is a no-op. A missing PATHNAME signals PATH-NOT-FOUND."))

;;; --- mechanism registry ------------------------------------------------------------

(defvar *clone-mechanisms* '()
  "The registered clone mechanisms, in registration order. Treat it as
opaque: change it with REGISTER-CLONE-MECHANISM and UNREGISTER-CLONE-MECHANISM,
which never modify the old list, so a LET binding of this variable scopes
registrations.")

(defstruct (%clone-mechanism (:constructor %make-clone-mechanism (name clone-file rank probe tree))
                             (:copier nil) (:predicate nil))
  (name nil :read-only t)          ; keyword
  (clone-file nil :read-only t)    ; (fs source destination) -> true | (values nil reason)
  (rank 0 :read-only t)            ; integer, highest first
  (probe nil :read-only t)         ; AVAILABLE-P: NIL (always) or (fs source-dir destination-dir)
  (tree nil :read-only t))         ; CLONE-TREE hook: NIL or (fs source destination matcher)

(defun %find-clone-mechanism (name)
  (find name *clone-mechanisms* :key #'%clone-mechanism-name))

(defun register-clone-mechanism (name clone-file &key (rank 0) available-p clone-tree)
  "Register (or replace, keeping its place) the clone mechanism NAME, a
keyword other than the reserved :COPY, :AUTO and :CLONE (those signal an
ERROR). Returns NAME.
CLONE-FILE is called as (FUNCALL CLONE-FILE fs source destination): SOURCE
the absolute pathname of a regular file, DESTINATION an absolute pathname
that does not exist and whose directory does. It returns true when
DESTINATION now holds SOURCE's content, or (VALUES NIL reason) when it made
nothing and the file should be copied instead (REASON NIL counts as
:CLONE-FAILED). It may signal CLONE-ERROR (restart USE-COPY).
RANK (an integer) orders mechanisms, highest first; equal ranks keep
registration order. AVAILABLE-P, NIL (always) or a function of (fs
source-directory destination-directory) returning (VALUES available-p
reason), tells whether the mechanism can clone between those directories
(the filesystem class, the volume's capability, the same volume).
CLONE-TREE, NIL or a function of (fs source destination matcher) with
DESTINATION an existing empty directory, may clone a whole tree at once: it
returns the number of regular files it cloned, or NIL to decline (then each
file goes through CLONE-FILE), and must make exactly the tree the per-file
walk would."
  (check-type name keyword)
  (when (member name '(:copy :auto :clone))
    (error "~S is a reserved clone mechanism name (:COPY, :AUTO and :CLONE are)" name))
  (check-type clone-file (and (or function symbol) (not null)))
  (check-type rank integer)
  (check-type available-p (or null function symbol))
  (check-type clone-tree (or null function symbol))
  (let ((new (%make-clone-mechanism name clone-file rank available-p clone-tree)))
    ;; SUBSTITUTE and APPEND both return a fresh list (the old one is never
    ;; modified, so an outer LET binding keeps its value).
    (setf *clone-mechanisms*
          (if (%find-clone-mechanism name)
              (substitute new name *clone-mechanisms* :key #'%clone-mechanism-name)
              (append *clone-mechanisms* (list new)))))
  name)

(defun unregister-clone-mechanism (name)
  "Remove the clone mechanism NAME. → T when it was registered, else NIL."
  (when (%find-clone-mechanism name)
    (setf *clone-mechanisms* (remove name *clone-mechanisms* :key #'%clone-mechanism-name))
    t))

(defun %ranked-clone-mechanisms ()
  "The registered mechanisms, best first. COPY-LIST: STABLE-SORT never sees
*CLONE-MECHANISMS* itself."
  (stable-sort (copy-list *clone-mechanisms*) #'> :key #'%clone-mechanism-rank))

;;; --- probes and the choice of mechanism (reads only) ---------------------------------

(defun %nearest-existing-directory (fs dir)
  "DIR (a directory pathname) when it is an existing directory, else its
nearest existing ancestor (the root at the latest)."
  (loop for d = (uiop:ensure-directory-pathname dir) then up
        for up = (uiop:ensure-directory-pathname (fs-parent fs d))
        until (or (fs-directory-p fs d) (%pathname-root-p d) (equal up d))
        finally (return d)))

(defun %clone-probe-dirs (fs source destination)
  "→ (values source-dir destination-dir): SOURCE as an absolute directory,
and DESTINATION (default SOURCE) or its nearest existing ancestor."
  (let ((s (%dir-root fs source)))
    (values s (%nearest-existing-directory fs (if destination (%dir-root fs destination) s)))))

(defun %clone-probe (m fs source-dir destination-dir)
  "→ (values available-p reason) of the mechanism M; a NIL reason is
:UNAVAILABLE. Never wrapped: a signalling probe propagates."
  (let ((probe (%clone-mechanism-probe m)))
    (if (null probe)
        (values t nil)
        (multiple-value-bind (ok reason) (funcall probe fs source-dir destination-dir)
          (if ok
              (values t nil)
              (values nil (or reason :unavailable)))))))

(defun %best-clone-mechanism (fs source-dir destination-dir)
  "→ (values mechanism nil), the best-ranked available one, or (values NIL
reason): the best-ranked registered mechanism's reason, :NO-MECHANISM when
none is registered."
  (let ((first-reason nil))
    (dolist (m (%ranked-clone-mechanisms) (values nil (or first-reason :no-mechanism)))
      (multiple-value-bind (ok reason) (%clone-probe m fs source-dir destination-dir)
        (when ok
          (return (values m nil)))
        (unless first-reason
          (setf first-reason reason))))))

(defun %resolve-clone-mechanism (fs source-dir destination-dir mechanism)
  "→ (values M reason): M a %CLONE-MECHANISM or :COPY, REASON non-NIL only
when :AUTO fell back to :COPY. Signals UNSUPPORTED-OPERATION for an unknown
name, or a strict choice (:CLONE, a name) that is unavailable. Creates nothing."
  (check-type mechanism keyword)
  (flet ((refuse (why)
           (%unsupported fs (format nil "clone mechanism ~S (~(~A~))" mechanism why) source-dir)))
    (case mechanism
      (:copy (values :copy nil))
      ((:auto :clone)
       (multiple-value-bind (m reason) (%best-clone-mechanism fs source-dir destination-dir)
         (cond (m (values m nil))
               ((eq mechanism :auto) (values :copy reason))
               (t (refuse reason)))))
      (t
       (let ((m (or (%find-clone-mechanism mechanism) (refuse :unknown))))
         (multiple-value-bind (ok reason) (%clone-probe m fs source-dir destination-dir)
           (if ok (values m nil) (refuse reason))))))))

(defun clone-mechanisms (&optional fs source destination)
  "Names of the registered clone mechanisms, best first (rank, then
registration order), followed by :COPY. With FS, SOURCE and DESTINATION
(directories; DESTINATION judged by its nearest existing ancestor), only the
mechanisms available between them; :COPY is always last."
  (append (mapcar #'%clone-mechanism-name
                  (if (and fs source)
                      (multiple-value-bind (s d) (%clone-probe-dirs fs source destination)
                        (remove-if-not (lambda (m) (values (%clone-probe m fs s d)))
                                       (%ranked-clone-mechanisms)))
                      (%ranked-clone-mechanisms)))
          (list :copy)))

(defmethod fs-clone-supported-p ((fs filesystem) directory &optional destination)
  (multiple-value-bind (s d) (%clone-probe-dirs fs directory destination)
    (multiple-value-bind (m reason) (%best-clone-mechanism fs s d)
      (if m
          (values (%clone-mechanism-name m) nil)
          (values nil reason)))))

;;; --- tree helpers (also used by area.lisp) -----------------------------------------

(defun %posix-under-p (dir candidate)
  "True when the directory CANDIDATE is DIR or below it (posix namestrings)."
  (let ((d (string-right-trim "/" (%pathname-as-posix dir)))
        (c (string-right-trim "/" (%pathname-as-posix candidate))))
    (or (string= d c)
        (uiop:string-prefix-p (concatenate 'string d "/") c))))

(defun %delete-tree (fs dir &key keep-root)
  "Delete the tree under the directory DIR (and DIR unless KEEP-ROOT).
Symlinks are removed, never followed."
  (labels ((clear (d)
             (dolist (name (%child-names fs d))
               (let ((pn (fs-join fs d name)))
                 (cond ((fs-symlink-p fs pn) (fs-unlink fs pn))
                       ((fs-directory-p fs pn)
                        (let ((sub (uiop:ensure-directory-pathname pn)))
                          (clear sub)
                          (fs-rmdir fs sub)))
                       (t (fs-unlink fs pn :missing-ok t)))))))
    (when (fs-directory-p fs dir)
      (clear dir)
      (unless keep-root
        (fs-rmdir fs dir)))))

(defun %file-form (fs dir)
  "The directory pathname DIR in file form (lstat sees a link itself)."
  (fs-join fs (fs-parent fs dir) (%entry-name dir)))

(defun %topmost-missing (fs dir)
  "The highest missing directory between the missing directory DIR and its
nearest existing ancestor: what removing restores \"absent\"."
  (loop with top = dir
        for up = (uiop:ensure-directory-pathname (fs-parent fs top))
        until (or (fs-directory-p fs up) (%pathname-root-p top) (equal up top))
        do (setf top up)
        finally (return top)))

(defun %remove-partial (fs pathname)
  "Remove whatever PATHNAME names now (a file, a symlink, a tree), if anything."
  (cond ((fs-symlink-p fs pathname) (fs-unlink fs pathname))
        ((fs-directory-p fs pathname) (%delete-tree fs (uiop:ensure-directory-pathname pathname)))
        (t (fs-unlink fs pathname :missing-ok t))))

(defmacro %without-masking (&body body)
  "Run the cleanup BODY of a failed call; a PATH-ERROR or FILE-ERROR it
signals is dropped, so the condition already escaping is what the caller
sees."
  `(handler-case (progn ,@body)
     ((or path-error file-error) () nil)))

;;; --- the per-file primitives ------------------------------------------------------

(defgeneric %copy-regular-file (fs source destination)
  (:documentation "Internal: write the content of the regular file SOURCE to
the absent DESTINATION, whose directory exists (the :COPY rung). Content only;
metadata is %APPLY-FILE-METADATA's.")
  (:method ((fs filesystem) source destination)
    (fs-write-bytes fs destination (fs-read-bytes fs source))
    t))

(defconstant +clone-copy-chunk+ (* 1024 1024)
  "Octets per read and write of the local :COPY rung.")

(defmethod %copy-regular-file ((fs local-filesystem) source destination)
  ;; Streamed in chunks of at most 1 MiB: a large file is never whole in memory.
  (let ((buf (make-array +clone-copy-chunk+ :element-type '(unsigned-byte 8)))
        (in-pn (fs-parse fs source)))
    (with-open-file (in in-pn :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (unless in
        (%local-not-found fs in-pn))
      (with-open-file (out (fs-parse fs destination) :direction :output
                                                     :element-type '(unsigned-byte 8)
                                                     :if-exists :error
                                                     :if-does-not-exist :create)
        (loop for n = (read-sequence buf in)
              while (plusp n)
              do (write-sequence buf out :end n))))
    t))

(defun %apply-file-metadata (fs source destination)
  "Give the regular file DESTINATION the #o777 bits (where the backend has
modes) and then the mtime of SOURCE. The mtime comes last: chmod does not
touch it, writes do. FS-CHMOD and FS-SET-MTIME are no-ops where unsupported."
  (let* ((st (fs-stat fs source))
         (mode (file-stat-mode st))
         (mtime (file-stat-mtime st)))
    (when mode
      (fs-chmod fs destination (logand mode #o777)))
    (when mtime
      (fs-set-mtime fs destination mtime))))

(defun %clone-one-file (fs m source destination)
  "Make the regular file DESTINATION from SOURCE with M (a %CLONE-MECHANISM
or :COPY). → (values :CLONED nil) or (values :COPIED fallback-reason), the
reason NIL on the :COPY rung. USE-COPY is established around every CLONE-FILE
call; any other condition propagates."
  (if (eq m :copy)
      (progn (%copy-regular-file fs source destination)
             (values :copied nil))
      (multiple-value-bind (ok reason)
          (restart-case (funcall (%clone-mechanism-clone-file m) fs source destination)
            (use-copy ()
              :report (lambda (s) (format s "Copy ~A instead of cloning it" source))
              (values nil :clone-failed)))
        (if ok
            (values :cloned nil)
            ;; RESTART-CASE has unwound: the copy runs outside the handler.
            (progn (%remove-partial fs destination)
                   (%copy-regular-file fs source destination)
                   (values :copied (or reason :clone-failed)))))))

(defun %clone-report (fs source destination m reason cloned copied fallbacks)
  (make-instance 'clone-report
                 :source (%wrap fs source) :destination (%wrap fs destination)
                 :mechanism (if (eq m :copy) :copy (%clone-mechanism-name m))
                 :reason reason :cloned cloned :copied copied
                 :fallbacks (sort (copy-list fallbacks) #'string< :key #'car)))

;;; --- fs-clone-tree ---------------------------------------------------------------

(defun %clone-tree-into (fs source destination matcher m reason)
  "Make the tree of SOURCE (minus MATCHER) in the existing empty DESTINATION
with M: the tree hook first, else the walk. → the CLONE-REPORT."
  (let* ((hook (and (not (eq m :copy)) (%clone-mechanism-tree m)))
         (count (and hook (funcall hook fs source destination matcher))))
    (check-type count (or null (integer 0)))
    (if count
        ;; A hook makes the exact tree including metadata: nothing is
        ;; re-applied after it (a lossy hook must show).
        (%clone-report fs source destination m reason count 0 '())
        (let ((cloned 0) (copied 0) (fallbacks '()))
          ;; Sorted: a directory comes before everything below it.
          (dolist (node (%scan-tree fs source matcher))
            (let ((from (%node-pathname node))
                  (to (fs-join fs destination (%node-path node))))
              (ecase (%node-kind node)
                (:directory
                 (fs-mkdir fs (uiop:ensure-directory-pathname to) :parents t :exist-ok t))
                (:symlink
                 (fs-create-symlink fs to (%pathname-as-posix (fs-read-symlink fs from))))
                (:file
                 (multiple-value-bind (how why) (%clone-one-file fs m from to)
                   (if (eq how :cloned) (incf cloned) (incf copied))
                   (when why
                     (push (cons (%node-path node) why) fallbacks)))
                 (%apply-file-metadata fs from to)))))
          (%clone-report fs source destination m reason cloned copied fallbacks)))))

(defmethod fs-clone-tree ((fs filesystem) source destination &key ignore (mechanism :auto))
  (check-type mechanism keyword)
  (let ((src (%dir-root fs source))
        (dst (%dir-root fs destination)))
    ;; Paths (reads only). SOURCE first: a missing source is PATH-NOT-FOUND
    ;; whatever the mechanism.
    (unless (fs-directory-p fs src)
      (if (or (fs-exists-p fs (%file-form fs src)) (fs-symlink-p fs (%file-form fs src)))
          (error 'path-error :filesystem fs :path src :message "the clone source is not a directory")
          (error 'path-not-found :filesystem fs :path src :message "the clone source does not exist")))
    (when (%posix-under-p src dst)
      (error 'path-error :filesystem fs :path dst :message "the clone destination is inside the source"))
    (let ((existed (fs-directory-p fs dst)))
      (cond (existed
             (when (fs-iterdir fs dst)
               (error 'path-exists-error :filesystem fs :path dst
                                         :message "the clone destination is not empty")))
            ((or (fs-exists-p fs (%file-form fs dst)) (fs-symlink-p fs (%file-form fs dst)))
             (error 'path-exists-error :filesystem fs :path dst
                                       :message "the clone destination is not a directory")))
      ;; Ignore and mechanism (reads and probes only): every refusal is here.
      (let ((matcher (%resolve-ignore fs src ignore))
            (top (and (not existed) (%topmost-missing fs dst))))
        (multiple-value-bind (m reason)
            (%resolve-clone-mechanism fs src (%nearest-existing-directory fs dst) mechanism)
          (let ((done nil))
            (unwind-protect
                 (progn
                   (unless existed
                     (fs-mkdir fs dst :parents t :exist-ok nil))
                   (prog1 (%clone-tree-into fs src dst matcher m reason)
                     (setf done t)))
              ;; DESTINATION as it was: removed (with the parents this call
              ;; made) when it was absent, emptied when it was empty.
              (unless done
                (%without-masking
                  (if existed
                      (%delete-tree fs dst :keep-root t)
                      (%delete-tree fs top)))))))))))

;;; --- fs-clone-file ---------------------------------------------------------------

(defvar *clone-random-state* nil
  "Random state for temp names of replaced files, seeded on first use.")

(defun %clone-temp-name (fs destination)
  "A free name in DESTINATION's directory for the replacement."
  (let ((state (or *clone-random-state* (setf *clone-random-state* (make-random-state t)))))
    (loop repeat 100
          for tmp = (fs-join fs (fs-parent fs destination)
                             (format nil ".~A.clone-~(~36R~).tmp"
                                     (fs-name fs destination) (random (expt 36 8) state)))
          unless (or (fs-exists-p fs tmp) (fs-symlink-p fs tmp))
            do (return tmp)
          finally (error 'path-error :filesystem fs :path destination
                                     :message "no free temp name for the replacement"))))

(defmethod fs-clone-file ((fs filesystem) source destination &key (mechanism :auto) replace)
  (check-type mechanism keyword)
  (let* ((src (fs-absolute fs source))
         (dst (fs-absolute fs destination))
         (link (and (not (uiop:directory-pathname-p src)) (fs-symlink-p fs src))))
    ;; Kinds (reads only).
    (unless link
      (cond ((or (uiop:directory-pathname-p src) (fs-directory-p fs src))
             (if (fs-exists-p fs src)
                 (error 'path-error :filesystem fs :path src :message "the clone source is a directory")
                 (error 'path-not-found :filesystem fs :path src
                                        :message "the clone source does not exist")))
            ((not (fs-file-p fs src))
             (error 'path-not-found :filesystem fs :path src :message "the clone source does not exist"))))
    (when (or (uiop:directory-pathname-p dst)
              (and (fs-directory-p fs dst) (not (fs-symlink-p fs dst))))
      (error 'path-error :filesystem fs :path dst :message "the clone destination is a directory"))
    ;; Mechanism (probes only), before any parent is made.
    (multiple-value-bind (m reason)
        (%resolve-clone-mechanism fs (%dir-root fs (fs-parent fs src))
                                  (%nearest-existing-directory fs (fs-parent fs dst)) mechanism)
      (when (and (not replace) (or (fs-exists-p fs dst) (fs-symlink-p fs dst)))
        (restart-case (error 'path-exists-error :filesystem fs :path dst
                                                :message "the clone destination exists")
          (overwrite ()
            :report (lambda (s) (format s "Replace ~A with the clone" dst))
            (setf replace t))))
      (fs-mkdir fs (fs-parent fs dst) :parents t :exist-ok t)
      (let* ((replacing (and replace (or (fs-exists-p fs dst) (fs-symlink-p fs dst))))
             (target (if replacing (%clone-temp-name fs dst) dst))
             (done nil))
        (unwind-protect
             (multiple-value-bind (how why)
                 (if link
                     (progn (fs-create-symlink fs target (%pathname-as-posix (fs-read-symlink fs src)))
                            (values nil nil))
                     (multiple-value-prog1 (%clone-one-file fs m src target)
                       (%apply-file-metadata fs src target)))
               ;; The old file stays until this rename: readers see old or new.
               (when replacing
                 (fs-rename fs target dst :replace t))
               (setf done t)
               (%clone-report fs src dst m reason
                              (if (eq how :cloned) 1 0) (if (eq how :copied) 1 0)
                              (and why (list (cons (fs-name fs src) why)))))
          (unless done
            (%without-masking (%remove-partial fs target))))))))

;;; --- fs-set-mtime ----------------------------------------------------------------

(defmethod fs-set-mtime ((fs filesystem) pathname universal-time)
  ;; Zip and any backend without settable mtimes: a no-op after the check.
  (check-type universal-time integer)
  (unless (fs-exists-p fs pathname)
    (error 'path-not-found :filesystem fs :path pathname :message "path does not exist"))
  nil)

(defmethod fs-set-mtime ((fs memory-filesystem) pathname universal-time)
  (check-type universal-time integer)
  (let* ((root (memory-fs-root fs))
         (key (%mem-real-key fs (fs-absolute fs pathname))) ; links followed, as stat(2)
         (ent (gethash key root)))
    (cond ((and (consp ent) (eq (car ent) :file))
           ;; A new entry, never a mutated one: the vector may be shared.
           (setf (gethash key root) (list :file (second ent) universal-time))
           t)
          (ent nil)                     ; a directory has no timestamp here
          (t (error 'path-not-found :filesystem fs :path pathname :message "path does not exist")))))

(defconstant +unix-epoch-universal-time+ (encode-universal-time 0 0 0 1 1 1970 0))

(defmethod fs-set-mtime ((fs local-filesystem) pathname universal-time)
  (check-type universal-time integer)
  (let ((pn (fs-parse fs pathname)))
    (unless (fs-exists-p fs pn)         ; follows links: a dangling one is missing
      (error 'path-not-found :filesystem fs :path pn :message "path does not exist"))
    #+(and sbcl unix)
    (let ((unix (- universal-time +unix-epoch-universal-time+)))
      (handler-case (sb-posix:utimes (%native pn) unix unix) ; atime = mtime
        (sb-posix:syscall-error (e)
          (error 'path-error :filesystem fs :path pn
                             :message (format nil "utimes(2) failed: ~A" e))))
      t)
    #-(and sbcl unix)
    nil))

;;; --- facade --------------------------------------------------------------------

(defun clone-tree (source destination &key ignore (mechanism :auto))
  "FS-CLONE-TREE from the directory SOURCE to DESTINATION, a designator on
the same filesystem."
  (let ((p (ensure-path source :directory t)))
    (fs-clone-tree (path-filesystem p) (path-pathname p)
                   (%pn (ensure-path destination :filesystem (path-filesystem p) :directory t))
                   :ignore ignore :mechanism mechanism)))

(defun clone-file (source destination &key (mechanism :auto) replace)
  "FS-CLONE-FILE from the file SOURCE to DESTINATION, a designator on the
same filesystem."
  (let ((p (ensure-path source)))
    (fs-clone-file (path-filesystem p) (path-pathname p)
                   (%pn (ensure-path destination :filesystem (path-filesystem p)))
                   :mechanism mechanism :replace replace)))

(defun clone-supported-p (directory &optional destination)
  "FS-CLONE-SUPPORTED-P for the directory designator DIRECTORY (and
DESTINATION, on the same filesystem). → (values mechanism reason)."
  (let ((p (ensure-path directory :directory t)))
    (if destination
        (fs-clone-supported-p (path-filesystem p) (path-pathname p)
                              (%pn (ensure-path destination :filesystem (path-filesystem p)
                                                            :directory t)))
        (fs-clone-supported-p (path-filesystem p) (path-pathname p)))))

(defun set-mtime (designator universal-time)
  "FS-SET-MTIME on DESIGNATOR. → T when set, NIL where the backend cannot."
  (let ((p (ensure-path designator)))
    (fs-set-mtime (path-filesystem p) (path-pathname p) universal-time)))
