(in-package #:cl-stack-pathlib)

;;; Filesystem protocol — CLOS SPI (java.nio.file.FileSystem-shaped).
;;; Backends specialize these generics. Facade functions bind *filesystem*
;;; / path-filesystem and call through here.

(defclass filesystem ()
  ((name :initarg :name :reader filesystem-name :initform "filesystem")
   (separator :initarg :separator :reader filesystem-separator :initform "/")))

(defun filesystem-p (x) (typep x 'filesystem))

(defvar *filesystem* nil
  "Default filesystem for string/pathname designators.
   Bound to a LOCAL-FILESYSTEM at load of the local backend.")

(defmacro with-filesystem ((fs) &body body)
  `(let ((*filesystem* ,fs)) ,@body))

(defgeneric fs-parse (fs designator &key directory)
  (:documentation "Parse DESIGNATOR into a pathname identity for FS."))

(defgeneric fs-join (fs base relative)
  (:documentation "Join BASE with a single RELATIVE component/path."))

(defgeneric fs-parent (fs pathname))
(defgeneric fs-name (fs pathname))
(defgeneric fs-absolute (fs pathname &key defaults))
(defgeneric fs-resolve (fs pathname &key strict))
(defgeneric fs-normpath (fs pathname))
(defgeneric fs-expanduser (fs pathname))

(defgeneric fs-exists-p (fs pathname))
(defgeneric fs-file-p (fs pathname))
(defgeneric fs-directory-p (fs pathname))
(defgeneric fs-symlink-p (fs pathname))
(defgeneric fs-readable-p (fs pathname))
(defgeneric fs-writable-p (fs pathname))
(defgeneric fs-executable-p (fs pathname))
(defgeneric fs-file-size (fs pathname))
(defgeneric fs-last-modified (fs pathname))

(defgeneric fs-iterdir (fs pathname))
(defgeneric fs-glob (fs pathname pattern &key recursive))
(defgeneric fs-walk (fs pathname &key top-down follow-symlinks))

(defgeneric fs-mkdir (fs pathname &key parents exist-ok))
(defgeneric fs-rmdir (fs pathname))
(defgeneric fs-unlink (fs pathname &key missing-ok))
(defgeneric fs-touch (fs pathname &key exist-ok))
(defgeneric fs-rename (fs source target &key replace))
(defgeneric fs-copy (fs source target &key replace))
(defgeneric fs-create-symlink (fs link target))
(defgeneric fs-read-symlink (fs pathname))
(defgeneric fs-read-bytes (fs pathname))
(defgeneric fs-write-bytes (fs pathname bytes &key append if-exists if-does-not-exist))
(defgeneric fs-make-temp (fs &key directory prefix suffix))
(defgeneric fs-same-p (fs a b))
(defgeneric fs-as-uri (fs pathname))

(defmethod fs-symlink-p ((fs filesystem) pathname)
  (declare (ignore pathname))
  nil)

(defmethod fs-create-symlink ((fs filesystem) link target)
  (declare (ignore target))
  (%unsupported fs 'fs-create-symlink link))

(defmethod fs-read-symlink ((fs filesystem) pathname)
  (%unsupported fs 'fs-read-symlink pathname))

(defmethod fs-make-temp ((fs filesystem) &key directory prefix suffix)
  (declare (ignore directory prefix suffix))
  (%unsupported fs 'fs-make-temp nil))
