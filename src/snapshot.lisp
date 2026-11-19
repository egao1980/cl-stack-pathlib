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
;;; Everything is portable code over the FILESYSTEM base protocol plus
;;; FS-STAT / FS-CONTENT-HASH / FS-WRITE-ATOMIC and the ignore matcher. The
;;; internal building blocks (tree scan, entry diff, change application, the
;;; execute-bit hook) are shared with the areas in area.lisp.

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

(defparameter +snapshot-manifest-header+ "cl-stack-pathlib tree v1")

(defun %snapshot-mode-code (mode)
  (ecase mode
    (:file "100644")
    (:executable "100755")
    (:symlink "120000")))

(defun %snapshot-digest-p (s)
  "True when S is \"sha256:\" followed by 64 lowercase hex digits."
  (and (stringp s)
       (= (length s) 71)
       (string= "sha256:" s :end2 7)
       (loop for k from 7 below 71
             always (let ((c (char s k)))
                      (or (char<= #\0 c #\9) (char<= #\a c #\f))))))

(defun %snapshot-bad-path-p (path)
  (or (zerop (length path))
      (char= (char path 0) #\/)
      (char= (char path (1- (length path))) #\/)))

(defun snapshot-manifest-octets (entries)
  "The canonical manifest of ENTRIES (SNAPSHOT-ENTRY objects, any order) as
UTF-8 octets:

  \"cl-stack-pathlib tree v1\" LF
  then per entry, sorted by path (STRING<, = UTF-8 octet order):
  <mode> SP <hash> SP <n> SP <path> LF

<mode> is 100644 (:FILE), 100755 (:EXECUTABLE) or 120000 (:SYMLINK); <n> is
the decimal UTF-8 octet length of <path>. Duplicate paths signal an ERROR.
OCTETS-HASH of the result is the snapshot id."
  (let ((sorted (sort (copy-list entries) #'string< :key #'snapshot-entry-path))
        (out (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for (a b) on sorted
          while b
          when (string= (snapshot-entry-path a) (snapshot-entry-path b))
            do (error "Duplicate path ~S in a snapshot manifest." (snapshot-entry-path a)))
    (flet ((put (octets) (loop for o across octets do (vector-push-extend o out))))
      (put (utf8-string-to-octets +snapshot-manifest-header+))
      (vector-push-extend 10 out)
      (dolist (e sorted)
        (let ((path (utf8-string-to-octets (snapshot-entry-path e)))
              (hash (snapshot-entry-hash e)))
          (unless (%snapshot-digest-p hash)
            (error "Snapshot entry ~S has a malformed hash ~S." (snapshot-entry-path e) hash))
          (when (%snapshot-bad-path-p (snapshot-entry-path e))
            (error "Snapshot entry path ~S is not a relative POSIX path." (snapshot-entry-path e)))
          (put (utf8-string-to-octets
                (format nil "~A ~A ~D " (%snapshot-mode-code (snapshot-entry-mode e)) hash
                        (length path))))
          (put path)
          (vector-push-extend 10 out))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %parse-manifest (octets store id)
  "PARSE-SNAPSHOT-MANIFEST; a SNAPSHOT-ERROR names STORE and ID."
  (let* ((v (%coerce-octets octets))
         (len (length v))
         (i 0)
         (prev nil)
         (out '()))
    (labels ((bad (fmt &rest args)
               (error 'snapshot-error :store store :id id
                                      :message (format nil "malformed snapshot manifest: ~?" fmt args)))
             (ascii (start end)
               (when (> end len) (bad "truncated at octet ~D" start))
               (let ((s (make-string (- end start))))
                 (loop for k from start below end
                       for b = (aref v k)
                       do (if (< 31 b 127)
                              (setf (char s (- k start)) (code-char b))
                              (bad "unexpected octet ~D at ~D" b k)))
                 s))
             (expect (octet)
               (unless (and (< i len) (= (aref v i) octet))
                 (bad "expected octet ~D at ~D" octet i))
               (incf i)))
      (let ((hl (length +snapshot-manifest-header+)))
        (unless (string= (ascii 0 hl) +snapshot-manifest-header+) (bad "bad header"))
        (setf i hl)
        (expect 10))
      (loop while (< i len)
            do (let* ((code (ascii i (+ i 6)))
                      (mode (cond ((string= code "100644") :file)
                                  ((string= code "100755") :executable)
                                  ((string= code "120000") :symlink)
                                  (t (bad "bad mode ~S at ~D" code i)))))
                 (incf i 6)
                 (expect 32)
                 (let ((hash (ascii i (+ i 71))))
                   (unless (%snapshot-digest-p hash) (bad "bad hash ~S at ~D" hash i))
                   (incf i 71)
                   (expect 32)
                   (let* ((j (or (position 32 v :start i) (bad "missing path length at ~D" i)))
                          (digits (ascii i j)))
                     (unless (and (<= 1 (length digits) 10)
                                  (every #'digit-char-p digits)
                                  (or (= 1 (length digits)) (char/= #\0 (char digits 0))))
                       (bad "bad path length ~S at ~D" digits i))
                     (setf i (1+ j))
                     (let ((n (parse-integer digits)))
                       (unless (and (plusp n) (<= (+ i n 1) len))
                         (bad "path of ~D octets at ~D overruns the manifest" n i))
                       (let ((path (or (%utf8-decode-strict (subseq v i (+ i n)))
                                       (bad "invalid UTF-8 path at ~D" i))))
                         (incf i n)
                         (expect 10)
                         (when (%snapshot-bad-path-p path) (bad "bad path ~S" path))
                         (when (and prev (not (string< prev path)))
                           (bad "unsorted or duplicate path ~S" path))
                         (setf prev path)
                         (push (make-snapshot-entry path mode hash) out))))))))
    (nreverse out)))

(defun parse-snapshot-manifest (octets)
  "The SNAPSHOT-ENTRY list (sorted by path) encoded in OCTETS. A malformed
manifest signals SNAPSHOT-ERROR."
  (%parse-manifest octets nil nil))

(defun %manifest-entries (store id)
  "Entries of the snapshot ID in STORE; NIL stands for the empty tree. An
absent manifest signals SNAPSHOT-NOT-FOUND."
  (when id
    (%parse-manifest (snapshot-store-get store id) store id)))

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
  (let* ((v (%coerce-octets octets))
         (id (octets-hash v))
         (blobs (%memory-snapshot-store-blobs store)))
    (unless (nth-value 1 (gethash id blobs))
      (setf (gethash id blobs) (copy-seq v)))
    id))

(defmethod snapshot-store-get ((store memory-snapshot-store) id)
  (multiple-value-bind (v found) (gethash id (%memory-snapshot-store-blobs store))
    (if found
        (copy-seq v)
        (error 'snapshot-not-found :store store :id id))))

(defmethod snapshot-store-contains-p ((store memory-snapshot-store) id)
  (nth-value 1 (gethash id (%memory-snapshot-store-blobs store))))

(defun %blob-path (store id)
  "PATH of the blob ID in the directory STORE, or NIL when ID is not a
digest (such an id is never in the store)."
  (when (%snapshot-digest-p id)
    (join (directory-snapshot-store-root store) "blobs" "sha256"
          (subseq id 7 9) (subseq id 9))))

(defun %blob-file-p (p)
  (and p (fs-file-p (path-filesystem p) (path-pathname p))))

(defmethod snapshot-store-put ((store directory-snapshot-store) octets)
  (let* ((v (%coerce-octets octets))
         (id (octets-hash v))
         (p (%blob-path store id)))
    (unless (%blob-file-p p)
      ;; Atomic: a reader sees no blob or the whole blob. Creates blobs/sha256/xx/.
      (fs-write-atomic (path-filesystem p) (path-pathname p) v))
    id))

(defmethod snapshot-store-get ((store directory-snapshot-store) id)
  (let ((p (%blob-path store id)))
    (unless (%blob-file-p p)
      (error 'snapshot-not-found :store store :id id))
    (fs-read-bytes (path-filesystem p) (path-pathname p))))

(defmethod snapshot-store-contains-p ((store directory-snapshot-store) id)
  (%blob-file-p (%blob-path store id)))

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

;;; --- internals shared with areas -----------------------------------------------

(defgeneric %fs-set-executable (fs pathname executable)
  (:documentation "Internal: make the owner execute bit of the regular file
PATHNAME follow EXECUTABLE. A no-op on backends without modes.")
  (:method ((fs filesystem) pathname executable)
    (declare (ignore pathname executable))
    nil))

(defmethod %fs-set-executable ((fs local-filesystem) pathname executable)
  (let* ((pn (fs-parse fs pathname))
         (raw (%local-raw-stat pn nil)))
    (when (and raw (= (logand raw #o170000) #o100000))
      (let* ((perm (logand raw #o7777))
             (new (if executable
                      ;; +x for the owner, and where readable for group / other (as git).
                      (logior perm #o100 (ash (logand perm #o044) -2))
                      (logandc2 perm #o111))))
        (unless (= new perm)
          (%local-chmod pn new))))))

(defun %dir-root (fs designator)
  "Absolute directory pathname of DESIGNATOR on FS."
  (uiop:ensure-directory-pathname (fs-absolute fs designator)))

(defstruct (%node (:constructor %make-node (path kind pathname))
                  (:copier nil) (:predicate nil))
  (path "" :type string)  ; relative POSIX path
  kind                    ; :FILE, :SYMLINK or :DIRECTORY
  pathname)               ; file-form pathname (directory form for :DIRECTORY)

(defun %child-names (fs dir)
  (remove-duplicates (mapcar #'%entry-name (fs-iterdir fs dir)) :test #'string=))

(defun %scan-tree (fs root matcher)
  "%NODEs under the directory ROOT, sorted by path (STRING<). Ignored entries
and everything below ignored directories are left out; symlinks are nodes and
are never followed. Other kinds (fifos, sockets) are skipped."
  (let ((out '()))
    (labels ((ignored-p (rel directory)
               (and matcher (ignore-path-p matcher rel :directory directory)))
             (visit (dir prefix)
               (dolist (name (%child-names fs dir))
                 (let ((rel (concatenate 'string prefix name))
                       ;; Rebuilt in file form: lstat("link/") would follow the link.
                       (pn (fs-join fs dir name)))
                   (cond ((fs-symlink-p fs pn)
                          (unless (ignored-p rel nil)
                            (push (%make-node rel :symlink pn) out)))
                         ((fs-directory-p fs pn)
                          (unless (ignored-p rel t)
                            (let ((d (uiop:ensure-directory-pathname pn)))
                              (push (%make-node rel :directory d) out)
                              (visit d (concatenate 'string rel "/")))))
                         ((fs-file-p fs pn)
                          (unless (ignored-p rel nil)
                            (push (%make-node rel :file pn) out))))))))
      (visit root ""))
    (sort out #'string< :key #'%node-path)))

(defun %owner-executable-p (fs pathname)
  (let ((mode (file-stat-mode (fs-stat fs pathname :follow-symlinks nil))))
    (and mode (logtest mode #o100))))

(defun %file-mode (fs pathname)
  (if (%owner-executable-p fs pathname) :executable :file))

(defun %symlink-octets (fs pathname)
  "The symlink's content: UTF-8 of the posix namestring of its target."
  (%coerce-octets (utf8-string-to-octets (%pathname-as-posix (fs-read-symlink fs pathname)))))

(defun %node-entry (fs node &key store)
  "SNAPSHOT-ENTRY of a :FILE or :SYMLINK node. With STORE the content is put
there (read once); without, it is only hashed."
  (let ((pn (%node-pathname node))
        (rel (%node-path node)))
    (ecase (%node-kind node)
      (:symlink
       (let ((o (%symlink-octets fs pn)))
         (make-snapshot-entry rel :symlink (if store (snapshot-store-put store o) (octets-hash o)))))
      (:file
       (make-snapshot-entry rel (%file-mode fs pn)
                            (if store
                                (snapshot-store-put store (fs-read-bytes fs pn))
                                (fs-content-hash fs pn)))))))

(defun %tree-entries (fs root matcher &key store)
  "Sorted SNAPSHOT-ENTRY list of the tree under ROOT."
  (loop for node in (%scan-tree fs root matcher)
        unless (eq (%node-kind node) :directory)
          collect (%node-entry fs node :store store)))

(defun %entry-at (fs dir rel)
  "The SNAPSHOT-ENTRY of REL under DIR now, or NIL when there is no file or
symlink there."
  (let ((pn (fs-join fs dir rel)))
    (cond ((fs-symlink-p fs pn) (%node-entry fs (%make-node rel :symlink pn)))
          ((fs-file-p fs pn) (%node-entry fs (%make-node rel :file pn)))
          (t nil))))

;;; Diffs.

(defun %entry= (x y)
  (or (and (null x) (null y))
      (and x y
           (eq (snapshot-entry-mode x) (snapshot-entry-mode y))
           (string= (snapshot-entry-hash x) (snapshot-entry-hash y)))))

(defun %make-change (kind old new)
  (make-instance 'fs-change
                 :kind kind :path (snapshot-entry-path (or new old))
                 :old-mode (and old (snapshot-entry-mode old))
                 :new-mode (and new (snapshot-entry-mode new))
                 :old-hash (and old (snapshot-entry-hash old))
                 :new-hash (and new (snapshot-entry-hash new))))

(defun %entry-change (old new)
  "FS-CHANGE from the entry OLD to NEW (either may be NIL), or NIL when equal."
  (cond ((%entry= old new) nil)
        ((null old) (%make-change :added nil new))
        ((null new) (%make-change :removed old nil))
        ((and (string= (snapshot-entry-hash old) (snapshot-entry-hash new))
              (member (snapshot-entry-mode old) '(:file :executable))
              (member (snapshot-entry-mode new) '(:file :executable)))
         (%make-change :mode-changed old new))
        (t (%make-change :modified old new))))

(defun %diff-entries (old new)
  "FS-CHANGE list from OLD to NEW, both SNAPSHOT-ENTRY lists sorted by path."
  (let ((out '()))
    (loop while (or old new)
          do (let ((o (first old)) (n (first new)))
               (cond ((and o n (string= (snapshot-entry-path o) (snapshot-entry-path n)))
                      (let ((c (%entry-change o n))) (when c (push c out)))
                      (pop old) (pop new))
                     ((or (null n) (and o (string< (snapshot-entry-path o) (snapshot-entry-path n))))
                      (push (%entry-change o nil) out)
                      (pop old))
                     (t
                      (push (%entry-change nil n) out)
                      (pop new)))))
    (nreverse out)))

(defun %change-new-entry (change)
  "The SNAPSHOT-ENTRY CHANGE leads to, or NIL for :REMOVED."
  (and (fs-change-new-hash change)
       (make-snapshot-entry (fs-change-path change) (fs-change-new-mode change)
                            (fs-change-new-hash change))))

;;; Applying changes (restore, commit).

(defun %prune-empty-parents (fs root rel)
  "Remove the now empty parent directories of REL under ROOT, bottom-up;
never ROOT itself."
  (loop for k = (position #\/ rel :from-end t) then (position #\/ rel :from-end t :end k)
        while (and k (plusp k))
        do (let ((d (uiop:ensure-directory-pathname (fs-join fs root (subseq rel 0 k)))))
             (if (and (not (fs-symlink-p fs (fs-join fs root (subseq rel 0 k))))
                      (fs-directory-p fs d)
                      (null (fs-iterdir fs d)))
                 (fs-rmdir fs d)
                 (return)))))

(defun %write-entry (fs root entry octets)
  "Make REL under ROOT hold ENTRY, whose content (file bytes or UTF-8 symlink
target) is OCTETS."
  (let ((pn (fs-join fs root (snapshot-entry-path entry))))
    ;; FS-WRITE-ATOMIC writes through symlinks: drop a link first.
    (when (fs-symlink-p fs pn) (fs-unlink fs pn))
    (ecase (snapshot-entry-mode entry)
      (:symlink
       (when (fs-file-p fs pn) (fs-unlink fs pn))
       (fs-mkdir fs (fs-parent fs pn) :parents t :exist-ok t)
       (fs-create-symlink fs pn (utf8-octets-to-string octets)))
      ((:file :executable)
       (fs-write-atomic fs pn octets)
       ;; Local atomic writes keep an existing file's mode: set it both ways.
       (%fs-set-executable fs pn (eq (snapshot-entry-mode entry) :executable))))))

(defun %apply-changes (fs root changes octets-of)
  "Apply CHANGES (FS-CHANGE list, sorted by path) to the tree under ROOT:
removals first (pruning directories they leave empty), then writes and mode
changes. OCTETS-OF maps a SNAPSHOT-ENTRY to its content octets."
  (dolist (c (reverse changes))
    (when (eq (fs-change-kind c) :removed)
      (fs-unlink fs (fs-join fs root (fs-change-path c)) :missing-ok t)
      (%prune-empty-parents fs root (fs-change-path c))))
  (dolist (c changes)
    (case (fs-change-kind c)
      ((:added :modified)
       (let ((new (%change-new-entry c)))
         (%write-entry fs root new (funcall octets-of new))))
      (:mode-changed
       (%fs-set-executable fs (fs-join fs root (fs-change-path c))
                           (eq (fs-change-new-mode c) :executable))))))

;;; --- default methods -----------------------------------------------------------

(defmethod fs-snapshot ((fs filesystem) root &key ignore (store *snapshot-store*))
  (let ((root (%dir-root fs root)))
    (unless (fs-directory-p fs root)
      (error 'path-not-found :filesystem fs :path root :message "snapshot root does not exist"))
    (snapshot-store-put store
                        (snapshot-manifest-octets
                         (%tree-entries fs root (%resolve-ignore fs root ignore) :store store)))))

(defmethod fs-restore ((fs filesystem) root snapshot &key (store *snapshot-store*) ignore)
  (check-type snapshot string)
  (let* ((root (%dir-root fs root))
         (target (%manifest-entries store snapshot)))
    ;; Every blob must be there before anything is touched (ROOT included).
    (dolist (e target)
      (unless (snapshot-store-contains-p store (snapshot-entry-hash e))
        (error 'snapshot-not-found :store store :id (snapshot-entry-hash e)
                                   :message (format nil "blob of ~S" (snapshot-entry-path e)))))
    (let* ((exists (fs-directory-p fs root))
           (matcher (cond (exists (%resolve-ignore fs root ignore))
                          ((typep ignore 'ignore-matcher) ignore)
                          (t nil)))
           (target (if matcher
                       (remove-if (lambda (e) (ignore-path-p matcher (snapshot-entry-path e))) target)
                       target))
           (current (if exists (%tree-entries fs root matcher) '()))
           (changes (%diff-entries current target)))
      (unless exists
        (fs-mkdir fs root :parents t :exist-ok t))
      (%apply-changes fs root changes
                      (lambda (entry) (snapshot-store-get store (snapshot-entry-hash entry))))
      changes)))

(defmethod fs-snapshot-changes ((store snapshot-store) a b)
  (let* ((old (%manifest-entries store a))
         (new (%manifest-entries store b)))
    (%diff-entries old new)))

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
