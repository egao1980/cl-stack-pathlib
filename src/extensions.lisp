(in-package #:cl-stack-pathlib)

;;; H2a filesystem extensions: value classes, portable default methods on
;;; FILESYSTEM, and the facade. The defaults must work for every backend that
;;; implements the base protocol (memory and zip get them as-is); local
;;; specializes where streaming or the OS matters.
;;;
;;; Contract stubs below call %H2A-PENDING; the implementation replaces them.

;;; --- values ----------------------------------------------------------------

(defclass file-stat ()
  ((path :initarg :path :reader file-stat-path
         :documentation "Pathname within the filesystem, as passed to FS-STAT.")
   (kind :initarg :kind :reader file-stat-kind
         :documentation ":FILE, :DIRECTORY, :SYMLINK (only with :FOLLOW-SYMLINKS NIL) or :OTHER.")
   (size :initarg :size :reader file-stat-size
         :documentation "Octets; 0 for directories.")
   (mtime :initarg :mtime :reader file-stat-mtime :initform nil
          :documentation "Universal time, or NIL when the backend does not track it.")
   (mode :initarg :mode :reader file-stat-mode :initform nil
         :documentation "Permission bits (LOGAND st_mode #o7777), or NIL when unknown.")
   (filesystem :initarg :filesystem :reader %file-stat-filesystem)
   (hash :initform nil :accessor %file-stat-hash-cache))
  (:documentation "Result of FS-STAT. FILE-STAT-HASH is computed on first call."))

(defun file-stat-hash (stat)
  "Content digest (OCTETS-HASH format) of a :FILE stat, computed through
FS-CONTENT-HASH on the first call (not by FS-STAT) and cached; NIL for other
kinds."
  (declare (ignore stat))
  (%h2a-pending 'file-stat-hash))

(defclass grep-match ()
  ((path :initarg :path :reader grep-match-path
         :documentation "Pathname from FS-GREP; PATH from the facade GREP.")
   (line :initarg :line :reader grep-match-line :documentation "1-based line number.")
   (column :initarg :column :reader grep-match-column
           :documentation "0-based character index of the first match on the line.")
   (text :initarg :text :reader grep-match-text :documentation "The line, without terminator."))
  (:documentation "One matching line. Several matches on one line give one GREP-MATCH."))

(defmethod print-object ((m grep-match) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A:~D:~D"
            (let ((p (grep-match-path m)))
              (if (path-p p) (as-posix p) (ignore-errors (uiop:unix-namestring p))))
            (grep-match-line m) (grep-match-column m))))

;;; --- portable defaults (stubs) --------------------------------------------

(defmethod fs-read-range ((fs filesystem) pathname start end)
  (declare (ignore pathname start end))
  (%h2a-pending 'fs-read-range))

(defmethod fs-read-lines ((fs filesystem) pathname &key (start 1) end)
  (declare (ignore pathname start end))
  (%h2a-pending 'fs-read-lines))

(defmethod fs-open ((fs filesystem) pathname &key (direction :input) (element-type '(unsigned-byte 8)))
  (declare (ignore pathname direction element-type))
  (%h2a-pending 'fs-open))

(defmethod fs-content-hash ((fs filesystem) pathname)
  (declare (ignore pathname))
  (%h2a-pending 'fs-content-hash))

(defmethod fs-write-atomic ((fs filesystem) pathname bytes &key expected-hash)
  (declare (ignore pathname bytes expected-hash))
  (%h2a-pending 'fs-write-atomic))

(defmethod fs-stat ((fs filesystem) pathname &key (follow-symlinks t))
  (declare (ignore pathname follow-symlinks))
  (%h2a-pending 'fs-stat))

(defmethod fs-grep ((fs filesystem) root pattern
                    &key (ignore :auto) include regex case-insensitive max-results)
  (declare (ignore root pattern ignore include regex case-insensitive max-results))
  (%h2a-pending 'fs-grep))

(defmethod fs-grep-matcher ((fs filesystem) pattern &key case-insensitive)
  (declare (ignore pattern case-insensitive))
  (%h2a-pending 'fs-grep-matcher))

;;; --- facade ----------------------------------------------------------------

(defun read-range (designator start &optional end)
  "Octets [START, END) of DESIGNATOR (see FS-READ-RANGE)."
  (let ((p (ensure-path designator)))
    (fs-read-range (path-filesystem p) (path-pathname p) start end)))

(defun read-lines (designator &key (start 1) end)
  "Lines START..END of DESIGNATOR, 1-based inclusive (see FS-READ-LINES)."
  (let ((p (ensure-path designator)))
    (fs-read-lines (path-filesystem p) (path-pathname p) :start start :end end)))

(defun open-path (designator &key (direction :input) (element-type '(unsigned-byte 8)))
  (let ((p (ensure-path designator)))
    (fs-open (path-filesystem p) (path-pathname p)
             :direction direction :element-type element-type)))

(defmacro with-open-path ((var designator &rest keys) &body body)
  "Bind VAR to (OPEN-PATH DESIGNATOR . KEYS); close it on unwind."
  `(let ((,var (open-path ,designator ,@keys)))
     (unwind-protect (progn ,@body)
       (close ,var))))

(defun content-hash (designator)
  (let ((p (ensure-path designator)))
    (fs-content-hash (path-filesystem p) (path-pathname p))))

(defun stat (designator &key (follow-symlinks t))
  (let ((p (ensure-path designator)))
    (fs-stat (path-filesystem p) (path-pathname p) :follow-symlinks follow-symlinks)))

(defun grep (root pattern &rest keys &key ignore include regex case-insensitive max-results)
  "FS-GREP over ROOT; GREP-MATCH-PATH values are PATH objects."
  (declare (ignore ignore include regex case-insensitive max-results))
  (let* ((p (ensure-path root))
         (fs (path-filesystem p)))
    (multiple-value-bind (matches truncated-p)
        (apply #'fs-grep fs (path-pathname p) pattern keys)
      (values (mapcar (lambda (m)
                        (make-instance 'grep-match
                                       :path (%wrap fs (grep-match-path m))
                                       :line (grep-match-line m)
                                       :column (grep-match-column m)
                                       :text (grep-match-text m)))
                      matches)
              truncated-p))))
