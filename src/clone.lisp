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
;;; Contract stubs: the functions and methods marked below call
;;; %R4C0-PENDING; the implementation replaces every call and then deletes
;;; %R4C0-PENDING.

(defun %r4c0-pending (what)
  (error "cl-stack-pathlib R4c-0: ~A is not implemented yet" what))

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
  (declare (ignore name clone-file rank available-p clone-tree))
  (%r4c0-pending 'register-clone-mechanism))

(defun unregister-clone-mechanism (name)
  "Remove the clone mechanism NAME. → T when it was registered, else NIL."
  (declare (ignore name))
  (%r4c0-pending 'unregister-clone-mechanism))

(defun clone-mechanisms (&optional fs source destination)
  "Names of the registered clone mechanisms, best first (rank, then
registration order), followed by :COPY. With FS, SOURCE and DESTINATION
(directories; DESTINATION judged by its nearest existing ancestor), only the
mechanisms available between them; :COPY is always last."
  (declare (ignore fs source destination))
  (%r4c0-pending 'clone-mechanisms))

;;; --- default methods (stubs) --------------------------------------------------------

(defmethod fs-clone-tree ((fs filesystem) source destination &key ignore (mechanism :auto))
  (declare (ignore source destination ignore mechanism))
  (%r4c0-pending 'fs-clone-tree))

(defmethod fs-clone-file ((fs filesystem) source destination &key (mechanism :auto) replace)
  (declare (ignore source destination mechanism replace))
  (%r4c0-pending 'fs-clone-file))

(defmethod fs-clone-supported-p ((fs filesystem) directory &optional destination)
  (declare (ignore directory destination))
  (%r4c0-pending 'fs-clone-supported-p))

(defmethod fs-set-mtime ((fs filesystem) pathname universal-time)
  (declare (ignore pathname universal-time))
  (%r4c0-pending 'fs-set-mtime))

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
