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

;;; URI scheme dispatch — "file://…", "memory://…", "zip://archive!/entry".
;;; Backends register a handler that returns a PATH for the full URI string.

(defvar *uri-schemes* (make-hash-table :test #'equalp)
  "SCHEME-STRING → (lambda (uri) path).")

(defun register-uri-scheme (scheme handler)
  "Register HANDLER for URI SCHEME (e.g. \"zip\").
   HANDLER is a function of the full URI string that returns a PATH."
  (check-type scheme string)
  (check-type handler function)
  (setf (gethash scheme *uri-schemes*) handler)
  scheme)

(defun uri-scheme-handler (scheme)
  (gethash scheme *uri-schemes*))

(defun list-uri-schemes ()
  (loop for k being the hash-keys of *uri-schemes* collect k))

(defun uri-scheme (string)
  "Return the RFC 3986 scheme of STRING, or NIL.
   Rejects single-letter schemes so Windows `C:\\…` is not a URI."
  (when (stringp string)
    (let ((colon (position #\: string)))
      (when (and colon (>= colon 2) (<= colon 16)
                 (< (1+ colon) (length string))
                 (alpha-char-p (char string 0))
                 (loop for i from 1 below colon
                       always (let ((c (char string i)))
                                (or (alphanumericp c)
                                    (find c "+-." :test #'char=)))))
        (subseq string 0 colon)))))

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
(defgeneric fs-glob (fs pathname pattern &key recursive ignore)
  (:documentation "Paths under PATHNAME matching PATTERN. IGNORE is NIL, an
IGNORE-MATCHER (patterns relative to PATHNAME) or :AUTO (read .gitignore and
.ignore files found under PATHNAME); ignored entries are dropped and ignored
directories are not descended."))
(defgeneric fs-walk (fs pathname &key top-down follow-symlinks ignore)
  (:documentation "List of (dir files dirs) triples under PATHNAME. IGNORE as
for FS-GLOB: ignored files are left out of FILES, ignored directories are left
out of DIRS and not walked."))

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

;;; --- H2a: ranged reads, hashes, atomic + CAS writes, stat, grep ------------
;;; Portable defaults live on FILESYSTEM (src/extensions.lisp) over
;;; FS-READ-BYTES / FS-ITERDIR / FS-FILE-SIZE / FS-LAST-MODIFIED; backends
;;; specialize for speed (local: file-position, streaming hash, rename(2)).

(defgeneric fs-read-range (fs pathname start end)
  (:documentation "Octets [START, END) of the file at PATHNAME as a fresh
(unsigned-byte 8) vector. END NIL means end of file; END past the size is
clamped; START at or past the size gives an empty vector. START > END
signals an ERROR. A missing file signals PATH-NOT-FOUND."))

(defgeneric fs-read-lines (fs pathname &key start end)
  (:documentation "Lines START..END (1-based, both inclusive; END NIL = to the
last line) of the UTF-8 file at PATHNAME, without terminators. Lines split at
LF; one trailing CR per line is removed; a final LF does not start an extra
empty line. Returns (values lines more-p), MORE-P true when lines exist after
END."))

(defgeneric fs-open (fs pathname &key direction element-type)
  (:documentation "Open PATHNAME for streaming reads. DIRECTION must be :INPUT
(anything else signals UNSUPPORTED-OPERATION). ELEMENT-TYPE is
'(UNSIGNED-BYTE 8) (default) or 'CHARACTER (UTF-8). The caller closes the
stream; prefer WITH-OPEN-PATH."))

(defgeneric fs-content-hash (fs pathname)
  (:documentation "Digest string of the file content, same format as
OCTETS-HASH (\"sha256:\" + 64 lowercase hex digits). A missing file signals
PATH-NOT-FOUND; a directory signals PATH-ERROR."))

(defgeneric fs-write-atomic (fs pathname bytes &key expected-hash)
  (:documentation "Replace the file at PATHNAME with BYTES so that readers see
the old or the new content, never a mix. Local: a temp file in the same
directory, then rename(2); no temp file survives success or failure. Missing
parent directories are created. Writes through a symlink (the link stays).

EXPECTED-HASH: NIL (no check), a digest string (the current content must have
that hash) or :ABSENT (the file must not exist). On mismatch signals
CONTENT-HASH-MISMATCH with restarts OVERWRITE (write anyway) and USE-CURRENT
(write nothing).

Returns (values pathname hash written-p): HASH is the digest of the content
now at PATHNAME (BYTES, or the current content after USE-CURRENT)."))

(defgeneric fs-stat (fs pathname &key follow-symlinks)
  (:documentation "A FILE-STAT for PATHNAME (FOLLOW-SYMLINKS default T; NIL
describes a symlink itself). A missing path signals PATH-NOT-FOUND."))

(defgeneric fs-grep (fs root pattern &key ignore include regex case-insensitive max-results)
  (:documentation "Search the files under ROOT (or the file ROOT) line by line.
PATTERN: a string (literal substring, or a regex when REGEX is true, compiled
through FS-GREP-MATCHER) or a function of a line string returning the match
start index or NIL. IGNORE: as FS-WALK, default :AUTO. INCLUDE: NIL, a string
or a list of strings in .gitignore glob syntax relative to ROOT; a file is
searched when the INCLUDE matcher ignores it (i.e. a pattern matches it).
Files with a NUL octet in their first 8000 octets, or not valid UTF-8, are
skipped. Returns (values matches truncated-p): GREP-MATCH objects sorted by
path namestring (STRING<) then line; at most MAX-RESULTS of them, TRUNCATED-P
true when at least one more match existed."))

(defgeneric fs-grep-matcher (fs pattern &key case-insensitive)
  (:documentation "Hook: compile the regex string PATTERN into a function of a
line returning the match start or NIL. The default signals
UNSUPPORTED-OPERATION (cl-stack-pathlib carries no regex engine); a system
that loads one specializes this."))

(defun %h2a-pending (operation)
  "Contract stub. Remove with the H2a implementation."
  (error "cl-stack-pathlib: ~S is not implemented yet (H2a contract stub)" operation))

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
