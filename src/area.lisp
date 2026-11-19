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
;;; Contract skeleton: operations call %R4A-PENDING (src/snapshot.lisp).

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

(defun area-manifest (area)
  "SNAPSHOT-ENTRY list (sorted by path) of the base as copied at open."
  (declare (ignore area))
  (%r4a-pending 'area-manifest))

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

(defmethod fs-open-area ((fs filesystem) base &key (mechanism :copy) ignore directory)
  (declare (ignore base mechanism ignore directory))
  (%r4a-pending 'fs-open-area))

(defmethod fs-area-changes ((area area))
  (%r4a-pending 'fs-area-changes))

(defmethod fs-close-area ((area area) &key (action :discard))
  (declare (ignore action))
  (%r4a-pending 'fs-close-area))

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
