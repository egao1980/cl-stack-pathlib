(in-package #:cl-stack-pathlib)

;;; R4a: content-addressed snapshots of a directory tree.
;;;
;;; A snapshot is a flat manifest of (relative path, mode, content hash)
;;; entries in a locked canonical encoding (SNAPSHOT-MANIFEST-OCTETS); the
;;; snapshot id is OCTETS-HASH of that manifest, so the same tree has the same
;;; id on every run, machine and backend. Blobs (file contents, symlink
;;; targets) and manifests are kept in a SNAPSHOT-STORE under their digests:
;;; a manifest is itself a blob whose id is the snapshot id.
;;;
;;; Contract skeleton: every operation below calls %R4A-PENDING. The
;;; implementation replaces each call and then deletes %R4A-PENDING.

(defun %r4a-pending (what)
  (error "cl-stack-pathlib R4a: ~A is not implemented yet" what))

;;; --- conditions --------------------------------------------------------------

(define-condition snapshot-error (error)
  ((store :initarg :store :reader snapshot-error-store :initform nil)
   (id :initarg :id :reader snapshot-error-id :initform nil
       :documentation "The snapshot or blob digest involved, or NIL.")
   (message :initarg :message :reader %snapshot-error-message :initform nil))
  (:documentation "Error about a snapshot store or a snapshot manifest (a
malformed manifest is a SNAPSHOT-ERROR).")
  (:report (lambda (c s)
             (format s "~@[~A ~]snapshot=~S store=~S"
                     (%snapshot-error-message c)
                     (snapshot-error-id c)
                     (snapshot-error-store c)))))

(define-condition snapshot-not-found (snapshot-error) ()
  (:documentation "The manifest or a blob named by ID is not in STORE.")
  (:report (lambda (c s)
             (format s "snapshot object ~S not found in ~S"
                     (snapshot-error-id c) (snapshot-error-store c)))))

;;; --- value classes -------------------------------------------------------------

(defclass snapshot-entry ()
  ((path :initarg :path :reader snapshot-entry-path
         :documentation "Relative POSIX path string (\"/\" separators, no leading
or trailing \"/\").")
   (mode :initarg :mode :reader snapshot-entry-mode
         :documentation ":FILE, :EXECUTABLE or :SYMLINK.")
   (hash :initarg :hash :reader snapshot-entry-hash
         :documentation "OCTETS-HASH digest of the content (files) or of the
UTF-8 target string (symlinks)."))
  (:documentation "One entry of a snapshot manifest. Directories are not entries."))

(defun make-snapshot-entry (path mode hash)
  (check-type path string)
  (check-type mode (member :file :executable :symlink))
  (check-type hash string)
  (make-instance 'snapshot-entry :path path :mode mode :hash hash))

(defmethod print-object ((e snapshot-entry) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "~S ~S ~A" (snapshot-entry-path e) (snapshot-entry-mode e)
            (snapshot-entry-hash e))))

(defclass fs-change ()
  ((kind :initarg :kind :reader fs-change-kind
         :documentation ":ADDED, :REMOVED, :MODIFIED (content or entry kind
changed) or :MODE-CHANGED (only :FILE <-> :EXECUTABLE changed).")
   (path :initarg :path :reader fs-change-path
         :documentation "Relative POSIX path string.")
   (old-mode :initarg :old-mode :reader fs-change-old-mode :initform nil)
   (new-mode :initarg :new-mode :reader fs-change-new-mode :initform nil)
   (old-hash :initarg :old-hash :reader fs-change-old-hash :initform nil)
   (new-hash :initarg :new-hash :reader fs-change-new-hash :initform nil))
  (:documentation "One changed path between two trees. OLD-* are NIL for
:ADDED, NEW-* are NIL for :REMOVED. Plain pathlib values; diff-protocol
adapters convert them."))

(defmethod print-object ((c fs-change) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~S ~S" (fs-change-kind c) (fs-change-path c))))

;;; --- manifest encoding (locked) ------------------------------------------------

(defun snapshot-manifest-octets (entries)
  "The canonical manifest of ENTRIES (SNAPSHOT-ENTRY objects, any order) as
UTF-8 octets:

  \"cl-stack-pathlib tree v1\" LF
  then per entry, sorted by path (STRING<, = UTF-8 octet order):
  <mode> SP <hash> SP <n> SP <path> LF

<mode> is 100644 (:FILE), 100755 (:EXECUTABLE) or 120000 (:SYMLINK); <n> is
the decimal UTF-8 octet length of <path>. Duplicate paths signal an ERROR.
OCTETS-HASH of the result is the snapshot id."
  (declare (ignore entries))
  (%r4a-pending 'snapshot-manifest-octets))

(defun parse-snapshot-manifest (octets)
  "The SNAPSHOT-ENTRY list (sorted by path) encoded in OCTETS. A malformed
manifest signals SNAPSHOT-ERROR."
  (declare (ignore octets))
  (%r4a-pending 'parse-snapshot-manifest))

;;; --- stores --------------------------------------------------------------------

(defclass snapshot-store () ()
  (:documentation "Content-addressed blob store for snapshots. Specialize
SNAPSHOT-STORE-PUT, SNAPSHOT-STORE-GET and SNAPSHOT-STORE-CONTAINS-P."))

(defun snapshot-store-p (x) (typep x 'snapshot-store))

(defclass memory-snapshot-store (snapshot-store)
  ((blobs :initform (make-hash-table :test #'equal) :reader %memory-snapshot-store-blobs))
  (:documentation "Snapshot store in a hash table (digest -> octets)."))

(defun make-memory-snapshot-store ()
  (make-instance 'memory-snapshot-store))

(defclass directory-snapshot-store (snapshot-store)
  ((root :initarg :root :reader %directory-snapshot-store-root
         :documentation "PATH of the store directory (on any backend)."))
  (:documentation "Snapshot store in a directory: the blob with digest
sha256:<64 hex> is the file blobs/sha256/<first 2 hex>/<other 62 hex> under
the root, holding the raw octets. Writes are atomic; a blob already present is
not rewritten. Reads do not re-verify the digest."))

(defun make-directory-snapshot-store (designator)
  "A DIRECTORY-SNAPSHOT-STORE rooted at the directory DESIGNATOR (a PATH or a
designator on *FILESYSTEM*). The directory is created on the first write; an
existing store directory is reused, so snapshots survive the Lisp process."
  (make-instance 'directory-snapshot-store
                 :root (ensure-path designator :directory t)))

(defun directory-snapshot-store-root (store)
  (check-type store directory-snapshot-store)
  (%directory-snapshot-store-root store))

(defvar *snapshot-store* (make-memory-snapshot-store)
  "Store used when :STORE is not given: a process-wide memory store.")

(defgeneric snapshot-store-put (store octets)
  (:documentation "Store OCTETS; return their OCTETS-HASH digest. Idempotent."))

(defgeneric snapshot-store-get (store id)
  (:documentation "Fresh octet vector of the blob ID; SNAPSHOT-NOT-FOUND when
absent."))

(defgeneric snapshot-store-contains-p (store id)
  (:documentation "True when the blob ID is in STORE."))

(defmethod snapshot-store-put ((store memory-snapshot-store) octets)
  (declare (ignore octets))
  (%r4a-pending 'snapshot-store-put))

(defmethod snapshot-store-get ((store memory-snapshot-store) id)
  (declare (ignore id))
  (%r4a-pending 'snapshot-store-get))

(defmethod snapshot-store-contains-p ((store memory-snapshot-store) id)
  (declare (ignore id))
  (%r4a-pending 'snapshot-store-contains-p))

(defmethod snapshot-store-put ((store directory-snapshot-store) octets)
  (declare (ignore octets))
  (%r4a-pending 'snapshot-store-put))

(defmethod snapshot-store-get ((store directory-snapshot-store) id)
  (declare (ignore id))
  (%r4a-pending 'snapshot-store-get))

(defmethod snapshot-store-contains-p ((store directory-snapshot-store) id)
  (declare (ignore id))
  (%r4a-pending 'snapshot-store-contains-p))

;;; --- protocol ------------------------------------------------------------------

(defgeneric fs-snapshot (fs root &key ignore store)
  (:documentation "Snapshot the tree under the directory ROOT into STORE
(default *SNAPSHOT-STORE*) and return the snapshot id (a digest string).
Entries: every regular file (mode :EXECUTABLE when the owner execute bit is
set on a backend with modes, else :FILE) and every symlink (not followed; hash
of the posix namestring FS-READ-SYMLINK returns) under ROOT, minus IGNORE (NIL,
an IGNORE-MATCHER relative to ROOT, or :AUTO as for FS-WALK). Directories are
not entries. A missing ROOT signals PATH-NOT-FOUND. ROOT is not modified."))

(defgeneric fs-restore (fs root snapshot &key store ignore)
  (:documentation "Make the tree under ROOT equal SNAPSHOT (an id in STORE,
default *SNAPSHOT-STORE*): write files whose content or mode differ, create
symlinks, delete files and symlinks not in the snapshot, then remove
directories left empty (never ROOT). Paths ignored by IGNORE (as FS-SNAPSHOT)
are neither written nor deleted. ROOT is created when missing. An unknown
SNAPSHOT signals SNAPSHOT-NOT-FOUND before anything is touched. Returns the
FS-CHANGE list from the tree before the call to SNAPSHOT (NIL when nothing
changed)."))

(defgeneric fs-snapshot-changes (store a b)
  (:documentation "FS-CHANGE list (sorted by path, STRING<) from snapshot A to
snapshot B, both ids in STORE; NIL stands for the empty tree. Entries whose
mode and hash are equal are unchanged; only the mode (:FILE <-> :EXECUTABLE)
differs → :MODE-CHANGED; otherwise → :MODIFIED."))

(defmethod fs-snapshot ((fs filesystem) root &key ignore store)
  (declare (ignore root ignore store))
  (%r4a-pending 'fs-snapshot))

(defmethod fs-restore ((fs filesystem) root snapshot &key store ignore)
  (declare (ignore root snapshot store ignore))
  (%r4a-pending 'fs-restore))

(defmethod fs-snapshot-changes ((store snapshot-store) a b)
  (declare (ignore a b))
  (%r4a-pending 'fs-snapshot-changes))

;;; --- facade --------------------------------------------------------------------

(defun snapshot (designator &key ignore (store *snapshot-store*))
  "FS-SNAPSHOT of the directory DESIGNATOR."
  (let ((p (ensure-path designator :directory t)))
    (fs-snapshot (path-filesystem p) (path-pathname p) :ignore ignore :store store)))

(defun restore-snapshot (designator snapshot &key (store *snapshot-store*) ignore)
  "FS-RESTORE SNAPSHOT into the directory DESIGNATOR."
  (let ((p (ensure-path designator :directory t)))
    (fs-restore (path-filesystem p) (path-pathname p) snapshot :store store :ignore ignore)))

(defun snapshot-changes (a b &key (store *snapshot-store*))
  "FS-SNAPSHOT-CHANGES from A to B in STORE."
  (fs-snapshot-changes store a b))

(defun snapshot-entries (snapshot &key (store *snapshot-store*))
  "The SNAPSHOT-ENTRY list of SNAPSHOT (sorted by path)."
  (parse-snapshot-manifest (snapshot-store-get store snapshot)))
