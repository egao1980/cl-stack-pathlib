(in-package #:cl-stack-pathlib)

;;; R4a: sandboxed areas by folder copy, with change tracking.
;;;
;;; An area is a writable copy of a base directory on the same filesystem. At
;;; open it records a manifest of the copy (path, mode, size, mtime, hash);
;;; FS-AREA-CHANGES rescans the copy, so edits made by anything (pathlib,
;;; CL:OPEN, a shell, another Lisp image) are found. FS-CLOSE-AREA discards,
;;; keeps, or commits the changes back to the base. Only the :COPY mechanism
;;; is portable; clones, overlays and virtual mounts belong to the
;;; fs-backend-* repos (R4b+) and specialize the same generics.
;;;
;;; The scan, diff and change application are the snapshot internals
;;; (src/snapshot.lisp), so area changes are snapshot changes by construction.

;;; --- conditions ----------------------------------------------------------------

(define-condition area-error (error)
  ((area :initarg :area :reader area-error-area :initform nil)
   (message :initarg :message :reader %area-error-message :initform nil))
  (:documentation "Error about an area, e.g. an operation on a closed area.")
  (:report (lambda (c s)
             (format s "~@[~A ~]area=~S" (%area-error-message c) (area-error-area c)))))

(define-condition area-conflict (area-error)
  ((path :initarg :path :reader area-conflict-path
         :documentation "Relative POSIX path string of the conflicting entry.")
   (open-entry :initarg :open-entry :reader area-conflict-open-entry :initform nil
               :documentation "SNAPSHOT-ENTRY of the base at open time, or NIL (absent).")
   (base-entry :initarg :base-entry :reader area-conflict-base-entry :initform nil
               :documentation "SNAPSHOT-ENTRY of the base now, or NIL (absent).")
   (area-entry :initarg :area-entry :reader area-conflict-area-entry :initform nil
               :documentation "SNAPSHOT-ENTRY of the area now, or NIL (removed)."))
  (:documentation "FS-CLOSE-AREA :COMMIT found PATH changed both in the area and
in the base since the area was opened (to different results). Signalled once
per conflicting path, before anything is written, with restarts OVERWRITE
(the area's version wins), SKIP-PATH (the base keeps its version) and
ABORT-COMMIT (write nothing; the area stays open).")
  (:report (lambda (c s)
             (format s "~S changed in the base and in the area ~S"
                     (area-conflict-path c) (area-error-area c)))))

(defun invoke-skip-path (&optional condition)
  (let ((r (find-restart 'skip-path condition)))
    (when r (invoke-restart r))))

(defun invoke-abort-commit (&optional condition)
  (let ((r (find-restart 'abort-commit condition)))
    (when r (invoke-restart r))))

;;; --- the area value ------------------------------------------------------------

(defclass area ()
  ((filesystem :initarg :filesystem :reader area-filesystem)
   (base :initarg :base :reader %area-base :documentation "Directory pathname on FILESYSTEM.")
   (root :initarg :root :reader %area-root :documentation "Directory pathname of the copy.")
   (mechanism :initarg :mechanism :reader area-mechanism :initform :copy)
   (ignore :initarg :ignore :reader area-ignore :initform nil
           :documentation "The resolved IGNORE-MATCHER (fixed at open), or NIL.")
   (manifest :initarg :manifest :accessor %area-manifest :initform nil
             :documentation "Per-path record at open: entry + size + mtime of the copy.")
   (opened-at :initarg :opened-at :reader %area-opened-at :initform nil)
   (state :initarg :state :accessor %area-state :initform :open
          :documentation ":OPEN or :CLOSED."))
  (:documentation "A writable copy of a base directory. Made by FS-OPEN-AREA."))

(defun area-p (x) (typep x 'area))

(defun area-base (area)
  "PATH of the base directory."
  (%wrap (area-filesystem area) (%area-base area)))

(defun area-root (area)
  "PATH of the area's directory (the copy)."
  (%wrap (area-filesystem area) (%area-root area)))

(defstruct (%area-record (:constructor %make-area-record (entry size mtime))
                         (:copier nil) (:predicate nil))
  entry   ; SNAPSHOT-ENTRY of the base as copied
  size    ; size of the copy at open (NIL for symlinks)
  mtime)  ; mtime of the copy at open (NIL: always hash)

(defun area-manifest (area)
  "SNAPSHOT-ENTRY list (sorted by path) of the base as copied at open."
  (mapcar #'%area-record-entry (%area-manifest area)))

(defun area-open-p (area)
  (eq (%area-state area) :open))

(defmethod print-object ((a area) stream)
  (print-unreadable-object (a stream :type t)
    (format stream "~S ~A ~S" (area-mechanism a)
            (ignore-errors (%pathname-as-posix (%area-root a)))
            (%area-state a))))

;;; --- protocol ------------------------------------------------------------------

(defgeneric fs-open-area (fs base &key mechanism ignore directory)
  (:documentation "Open an area over the directory BASE on FS. MECHANISM
:COPY (the default and the only portable one; anything else signals
UNSUPPORTED-OPERATION here) copies the files, symlinks (as symlinks) and
directories of BASE minus IGNORE (as FS-SNAPSHOT; :AUTO is resolved against
BASE once, at open) into DIRECTORY, and records the manifest. DIRECTORY NIL
makes a fresh temp directory on FS; a given DIRECTORY must be absent or empty
(else PATH-EXISTS-ERROR). A missing BASE signals PATH-NOT-FOUND. Returns the
AREA."))

(defgeneric fs-area-changes (area)
  (:documentation "Rescan the area: the FS-CHANGE list (sorted by path) from
the manifest recorded at open to the area's tree now, ignored paths left out.
A file whose size or mtime differs from the record, or whose recorded mtime
is not older than the open time minus 2 s (mtime granularity), is hashed; a
same-size edit within the same second is found. An entry edited back to its
original content is not a change. A closed area signals AREA-ERROR."))

(defgeneric fs-close-area (area &key action)
  (:documentation "Close AREA. ACTION :DISCARD (default) deletes the copy;
:KEEP leaves it on disk; :COMMIT applies FS-AREA-CHANGES to the base and then
deletes the copy. A commit first checks every changed path against the base:
when the base entry differs both from the manifest and from the area entry,
AREA-CONFLICT is signalled (restarts OVERWRITE, SKIP-PATH, ABORT-COMMIT);
nothing is written until every conflict is resolved. Base paths the area did
not change are never touched, nor are ignored ones. Files are written with
FS-WRITE-ATOMIC, the execute bit follows the entry mode, and directories left
empty by removals are removed (never the base itself).
Returns (values changes status): status :DISCARDED, :KEPT, :COMMITTED (CHANGES
= the changes written to the base) or :ABORTED (ABORT-COMMIT; CHANGES NIL, the
area stays open). A closed area signals AREA-ERROR."))

;;; --- internals -----------------------------------------------------------------

(defun %check-area-open (area)
  (unless (area-open-p area)
    (error 'area-error :area area :message "the area is closed")))

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

(defun %check-area-directory (fs base dir)
  "DIR must be absent or an empty directory, and not inside BASE."
  (when (%posix-under-p base dir)
    (error 'path-error :filesystem fs :path dir
                       :message "the area directory is inside the base"))
  (cond ((fs-directory-p fs dir)
         (when (fs-iterdir fs dir)
           (error 'path-exists-error :filesystem fs :path dir
                                     :message "the area directory is not empty")))
        ((fs-exists-p fs dir)
         (error 'path-exists-error :filesystem fs :path dir
                                   :message "the area directory is not a directory"))))

(defun %fresh-area-directory (fs base)
  "Create and return a new empty temp directory on FS, outside BASE."
  (let ((parent nil))
    (loop repeat 100
          do (let ((candidate (uiop:ensure-directory-pathname
                               (apply #'fs-make-temp fs
                                      :prefix (format nil "area-~36R-" (random (expt 36 6)))
                                      (when parent (list :directory parent))))))
               (cond ((%posix-under-p base candidate)
                      (setf parent (fs-parent fs base)))
                     ((not (or (fs-exists-p fs candidate)
                               (fs-symlink-p fs (fs-join fs (fs-parent fs candidate)
                                                         (%entry-name candidate)))))
                      (fs-mkdir fs candidate :parents t :exist-ok nil)
                      ;; Canonical form: the temp directory may be spelled
                      ;; through a link or a short name (/var -> /private/var
                      ;; on macOS, RUNNER~1 on Windows) while listings under
                      ;; the root come back canonical.
                      (return-from %fresh-area-directory
                        (uiop:ensure-directory-pathname
                         (fs-resolve fs candidate :strict t)))))))
    (error 'path-error :filesystem fs :path base
                       :message "could not make a fresh area directory")))

(defun %copy-into-area (fs base root matcher)
  "Copy the tree under BASE (minus MATCHER) into ROOT; return the sorted
%AREA-RECORD list: the base entry as copied plus the copy's size and mtime."
  (let ((records '()))
    (dolist (node (%scan-tree fs base matcher))
      (let ((src (%node-pathname node))
            (rel (%node-path node)))
        (ecase (%node-kind node)
          (:directory
           (fs-mkdir fs (fs-join fs root rel) :parents t :exist-ok t))
          (:file
           (let* ((bytes (fs-read-bytes fs src))
                  (mode (%file-mode fs src))
                  (dst (fs-join fs root rel)))
             (fs-write-atomic fs dst bytes)
             (%fs-set-executable fs dst (eq mode :executable))
             (let ((st (fs-stat fs dst :follow-symlinks nil)))
               (push (%make-area-record (make-snapshot-entry rel mode (octets-hash bytes))
                                        (file-stat-size st) (file-stat-mtime st))
                     records))))
          (:symlink
           (let ((o (%symlink-octets fs src))
                 (dst (fs-join fs root rel)))
             (fs-mkdir fs (fs-parent fs dst) :parents t :exist-ok t)
             (fs-create-symlink fs dst (utf8-decode o))
             (push (%make-area-record (make-snapshot-entry rel :symlink (octets-hash o)) nil nil)
                   records))))))
    (nreverse records)))

(defun %record-fresh-p (fs node record cutoff)
  "Racy-git rule: the recorded hash of a regular file may be reused only when
its size and mtime match the record and the recorded mtime is older than
CUTOFF (open time - 2 s)."
  (let ((mtime (%area-record-mtime record)))
    (and mtime
         (eq (%node-kind node) :file)
         (member (snapshot-entry-mode (%area-record-entry record)) '(:file :executable))
         (< mtime cutoff)
         (let ((st (fs-stat fs (%node-pathname node) :follow-symlinks nil)))
           (and (eql (file-stat-size st) (%area-record-size record))
                (eql (file-stat-mtime st) mtime))))))

(defun %area-current-entries (area)
  "Sorted SNAPSHOT-ENTRY list of the area's tree now."
  (let ((fs (area-filesystem area))
        (cutoff (- (%area-opened-at area) 2))
        (records (make-hash-table :test #'equal)))
    (dolist (r (%area-manifest area))
      (setf (gethash (snapshot-entry-path (%area-record-entry r)) records) r))
    (loop for node in (%scan-tree fs (%area-root area) (area-ignore area))
          unless (eq (%node-kind node) :directory)
            collect (let ((r (gethash (%node-path node) records)))
                      (if (and r (%record-fresh-p fs node r cutoff))
                          ;; The mode is always read: chmod changes neither size nor content.
                          (make-snapshot-entry (%node-path node)
                                               (%file-mode fs (%node-pathname node))
                                               (snapshot-entry-hash (%area-record-entry r)))
                          (%node-entry fs node))))))

(defun %stage-area-content (area change)
  "Content octets the write of CHANGE needs, read from the area copy now and
checked against CHANGE's new hash."
  (when (member (fs-change-kind change) '(:added :modified))
    (let* ((fs (area-filesystem area))
           (pn (fs-join fs (%area-root area) (fs-change-path change)))
           (o (if (eq (fs-change-new-mode change) :symlink)
                  (%symlink-octets fs pn)
                  (fs-read-bytes fs pn))))
      (unless (string= (octets-hash o) (fs-change-new-hash change))
        (error 'area-error :area area
                           :message (format nil "~S changed in the area during the commit"
                                            (fs-change-path change))))
      o)))

(defun %commit-area (area)
  "FS-CLOSE-AREA :COMMIT. Decide every changed path (signalling AREA-CONFLICT
where the base changed too) and stage the area's content, then write."
  (let* ((fs (area-filesystem area))
         (base (%area-base area))
         (changes (fs-area-changes area))
         (open (make-hash-table :test #'equal))
         (plan '()))                     ; (area-change base-change . octets)
    (dolist (e (area-manifest area))
      (setf (gethash (snapshot-entry-path e) open) e))
    (dolist (c changes)
      (let* ((rel (fs-change-path c))
             (open-e (gethash rel open))
             (area-e (%change-new-entry c))
             (base-e (%entry-at fs base rel)))
        (flet ((accept ()
                 ;; Applied from the base as it is now, so OVERWRITE works
                 ;; whatever the base holds.
                 (let ((w (%entry-change base-e area-e)))
                   (push (list* c w (and w (%stage-area-content area w))) plan))))
          (cond ((%entry= base-e area-e))       ; already the same on both sides
                ((%entry= base-e open-e) (accept))
                (t
                 (restart-case
                     (error 'area-conflict :area area :path rel
                                           :open-entry open-e :base-entry base-e
                                           :area-entry area-e)
                   (overwrite ()
                     :report "Write the area's version of this path to the base."
                     (accept))
                   (skip-path ()
                     :report "Keep the base's version of this path."
                     nil)
                   (abort-commit ()
                     :report "Write nothing to the base; keep the area open."
                     (return-from %commit-area (values nil :aborted)))))))))
    (setf plan (nreverse plan))
    ;; Every conflict is resolved: write.
    (let ((staged (make-hash-table :test #'equal))
          (writes (remove nil (mapcar #'second plan))))
      (dolist (p plan)
        (when (cddr p)
          (setf (gethash (fs-change-path (first p)) staged) (cddr p))))
      (%apply-changes fs base writes
                      (lambda (entry) (gethash (snapshot-entry-path entry) staged))))
    (%delete-tree fs (%area-root area))
    (setf (%area-state area) :closed)
    (values (mapcar #'first plan) :committed)))

;;; --- default methods -----------------------------------------------------------

(defmethod fs-open-area ((fs filesystem) base &key (mechanism :copy) ignore directory)
  (unless (eq mechanism :copy)
    (%unsupported fs (format nil "area mechanism ~S" mechanism) base))
  (let ((base (%dir-root fs base)))
    (unless (fs-directory-p fs base)
      (error 'path-not-found :filesystem fs :path base :message "the area base does not exist"))
    (let* ((given (and directory (%dir-root fs directory)))
           (existed (and given (fs-directory-p fs given))))
      (when given
        (%check-area-directory fs base given))
      (let* ((matcher (%resolve-ignore fs base ignore)) ; :AUTO resolved once, here
             (opened-at (get-universal-time))        ; before the first copy write
             (root (if given
                       (fs-mkdir fs given :parents t :exist-ok t)
                       (%fresh-area-directory fs base)))
             (done nil))
        (unwind-protect
             (let ((records (%copy-into-area fs base root matcher)))
               (prog1 (make-instance 'area :filesystem fs :base base :root root
                                           :mechanism :copy :ignore matcher
                                           :manifest records :opened-at opened-at)
                 (setf done t)))
          (unless done
            (%delete-tree fs root :keep-root existed)))))))

(defmethod fs-area-changes ((area area))
  (%check-area-open area)
  (%diff-entries (area-manifest area) (%area-current-entries area)))

(defmethod fs-close-area ((area area) &key (action :discard))
  (%check-area-open area)
  (ecase action
    (:discard
     (%delete-tree (area-filesystem area) (%area-root area))
     (setf (%area-state area) :closed)
     (values nil :discarded))
    (:keep
     (setf (%area-state area) :closed)
     (values nil :kept))
    (:commit
     (%commit-area area))))

;;; --- facade --------------------------------------------------------------------

(defun open-area (base &key (mechanism :copy) ignore directory)
  "FS-OPEN-AREA over the directory BASE. DIRECTORY, when given, is a
designator on the same filesystem."
  (let ((p (ensure-path base :directory t)))
    (fs-open-area (path-filesystem p) (path-pathname p)
                  :mechanism mechanism :ignore ignore
                  :directory (when directory (%pn (ensure-path directory :directory t))))))

(defun area-changes (area)
  (fs-area-changes area))

(defun close-area (area &key (action :discard))
  (fs-close-area area :action action))

(defmacro with-area ((var base &rest keys) &body body)
  "Bind VAR to (OPEN-AREA BASE . KEYS); on exit, an area still open is
closed with :DISCARD."
  `(let ((,var (open-area ,base ,@keys)))
     (unwind-protect (progn ,@body)
       (when (area-open-p ,var)
         (close-area ,var :action :discard)))))
