(in-package #:cl-stack-pathlib)

;;; In-memory VFS — proves the filesystem protocol; useful for tests & virtual trees.
;;; Entries: :dir | (:file vector timestamp) keyed by posix absolute path string.

(defclass memory-filesystem (filesystem)
  ((root :initform (make-hash-table :test #'equal) :reader memory-fs-root)
   (cwd :initform "/" :accessor memory-fs-cwd)
   (home :initform "/home" :accessor memory-fs-home))
  (:default-initargs :name "memory"))

(defun %abs-posix-cwd (s)
  (cond ((string= s "") "/")
        ((char= (char s 0) #\/) s)
        (t (format nil "/~A" (string-left-trim "/" s)))))

(defun make-memory-filesystem (&key (cwd "/") (home "/home"))
  (let ((fs (make-instance 'memory-filesystem)))
    (setf (memory-fs-cwd fs) (%abs-posix-cwd cwd)
          (memory-fs-home fs) home)
    (setf (gethash "/" (memory-fs-root fs)) :dir)
    (fs-mkdir fs home :parents t :exist-ok t)
    fs))

(defun %mem-lexical-key (fs pathname)
  "Absolute posix key of PATHNAME as spelled (no symlink resolution)."
  (let* ((pn (if (pathnamep pathname) pathname (fs-parse fs pathname)))
         (abs (if (uiop:absolute-pathname-p pn)
                  pn
                  (fs-join fs (memory-fs-cwd fs) pn)))
         (s (or (uiop:unix-namestring (uiop:ensure-pathname abs :want-pathname t))
                (namestring abs))))
    (cond ((or (string= s "") (string= s ".")) (memory-fs-cwd fs))
          ((char= (char s 0) #\/) (if (and (> (length s) 1) (char= (char s (1- (length s))) #\/))
                                      (subseq s 0 (1- (length s)))
                                      (if (string= s "/") "/" s)))
          (t s))))

(defun %mem-dir-key (key)
  (if (or (string= key "/") (char= (char key (1- (length key))) #\/))
      key
      key))

(defvar *mem-link-depth* 0)

(defun %mem-symlink-loop (fs key)
  (error 'symlink-loop :filesystem fs :path key :message "too many levels of symbolic links"))

(defun %mem-follow-symlink (fs key &optional visited)
  "KEY (parents already resolved) with symlinks at the leaf followed."
  (let ((ent (gethash key (memory-fs-root fs))))
    (if (and (consp ent) (eq (car ent) :symlink))
        (let ((*mem-link-depth* (1+ *mem-link-depth*)))
          (when (or (member key visited :test #'string=) (> *mem-link-depth* 40))
            (%mem-symlink-loop fs key))
          (%mem-follow-symlink fs (%mem-parent-resolved-key fs (second ent)) (cons key visited)))
        key)))

(defun %mem-parent-resolved-key (fs key)
  "KEY with every symlink among its parent directories resolved (the leaf is
kept, as lstat(2) does)."
  (let ((slash (position #\/ key :from-end t)))
    (if (or (null slash) (string= key "/"))
        key
        (let ((parent (if (zerop slash) "/" (subseq key 0 slash)))
              (name (subseq key (1+ slash))))
          (if (string= parent "/")
              key
              (let ((real (%mem-follow-symlink fs (%mem-parent-resolved-key fs parent))))
                (if (string= real "/")
                    (concatenate 'string "/" name)
                    (concatenate 'string real "/" name))))))))

(defun %mem-key (fs pathname)
  "Table key of the entry PATHNAME names: parents resolved, leaf as is."
  (%mem-parent-resolved-key fs (%mem-lexical-key fs pathname)))

(defun %mem-real-key (fs pathname)
  "Table key of what PATHNAME finally refers to (every symlink resolved)."
  (%mem-follow-symlink fs (%mem-key fs pathname)))

(defmethod fs-parse ((fs memory-filesystem) designator &key directory)
  (let ((pn (etypecase designator
              (pathname designator)
              (string (%parse-posix designator))
              (path (path-pathname designator)))))
    (if directory (uiop:ensure-directory-pathname pn) pn)))

(defmethod fs-join ((fs memory-filesystem) base relative)
  (uiop:merge-pathnames*
   (fs-parse fs relative)
   (uiop:ensure-directory-pathname (fs-parse fs base))))

(defmethod fs-parent ((fs memory-filesystem) pathname)
  (%logical-parent (fs-parse fs pathname)))

(defmethod fs-name ((fs memory-filesystem) pathname)
  (%file-namestring* (fs-parse fs pathname)))

(defmethod fs-absolute ((fs memory-filesystem) pathname &key (defaults nil))
  (let ((pn (fs-parse fs pathname))
        (base (fs-parse fs (or defaults (memory-fs-cwd fs)) :directory t)))
    (if (uiop:absolute-pathname-p pn) pn (uiop:merge-pathnames* pn base))))

(defmethod fs-resolve ((fs memory-filesystem) pathname &key (strict t))
  (let* ((abs (fs-absolute fs pathname))
         (key (%mem-key fs abs))
         (root (memory-fs-root fs))
         (ent (gethash key root)))
    (when (and strict (null ent))
      (return-from fs-resolve
        (%restart-path-not-found fs abs
                                 :message "path does not exist"
                                 :allow-create-file t
                                 :allow-create-directory t
                                 :allow-ignore nil)))
    (let ((resolved (%mem-follow-symlink fs key)))
      (if (or (gethash resolved root) (not strict))
          (fs-parse fs resolved)
          (return-from fs-resolve
            (%restart-path-not-found fs abs
                                     :message "path does not exist"
                                     :allow-create-file t
                                     :allow-create-directory t
                                     :allow-ignore nil))))))(defmethod fs-normpath ((fs memory-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (multiple-value-bind (abs? stack dir?) (%collapse-dot-segments pn)
      (fs-parse fs
                (cond ((and abs? (null stack)) "/")
                      (abs? (format nil "/~{~A~^/~}" stack))
                      ((null stack) ".")
                      (t (format nil "~{~A~^/~}" stack)))
                :directory dir?))))

(defmethod fs-expanduser ((fs memory-filesystem) pathname)
  (let* ((pn (fs-parse fs pathname))
         (s (or (uiop:unix-namestring pn) "")))
    (if (and (plusp (length s)) (char= (char s 0) #\~))
        (fs-join fs (memory-fs-home fs)
                 (if (and (> (length s) 1) (char= (char s 1) #\/))
                     (subseq s 2)
                     (subseq s 1)))
        pn)))

(defun %mem-target-entry (fs pathname)
  "(values entry found-p) of what PATHNAME refers to, every symlink followed
(stat(2)): NIL NIL for a missing path or a dangling link. Signals
SYMLINK-LOOP for a link cycle."
  (gethash (%mem-real-key fs (fs-absolute fs pathname)) (memory-fs-root fs)))

(defun %mem-probe (fs pathname)
  "As %MEM-TARGET-ENTRY, but a link cycle is absent (NIL NIL), as a failing
stat(2) is for the predicates."
  (handler-case (%mem-target-entry fs pathname)
    (symlink-loop () (values nil nil))))

(defun %mem-target-file (fs pathname)
  "The (:file …) entry PATHNAME refers to (links followed), else PATH-NOT-FOUND."
  (let ((ent (%mem-target-entry fs pathname)))
    (unless (and (consp ent) (eq (car ent) :file))
      (error 'path-not-found :filesystem fs :path pathname
                             :message "file does not exist"))
    ent))

;;; Probes follow a symlink at the leaf, as stat(2) and the local backend do;
;;; FS-SYMLINK-P (and FS-STAT :FOLLOW-SYMLINKS NIL) is the lstat(2) view.

(defmethod fs-exists-p ((fs memory-filesystem) pathname)
  (nth-value 1 (%mem-probe fs pathname)))

(defmethod fs-file-p ((fs memory-filesystem) pathname)
  (let ((ent (%mem-probe fs pathname)))
    (and (consp ent) (eq (car ent) :file))))

(defmethod fs-directory-p ((fs memory-filesystem) pathname)
  (eq (%mem-probe fs pathname) :dir))

(defmethod fs-symlink-p ((fs memory-filesystem) pathname)
  (let ((ent (gethash (%mem-key fs (fs-absolute fs pathname)) (memory-fs-root fs))))
    (and (consp ent) (eq (car ent) :symlink))))

(defmethod fs-readable-p ((fs memory-filesystem) pathname)
  (fs-exists-p fs pathname))

(defmethod fs-writable-p ((fs memory-filesystem) pathname)
  (or (fs-exists-p fs pathname)
      (fs-directory-p fs (fs-parent fs (fs-absolute fs pathname)))))

(defmethod fs-executable-p ((fs memory-filesystem) pathname)
  (declare (ignore pathname))
  nil)

(defmethod fs-file-size ((fs memory-filesystem) pathname)
  (length (second (%mem-target-file fs pathname))))

(defmethod fs-last-modified ((fs memory-filesystem) pathname)
  (third (%mem-target-file fs pathname)))

(defmethod fs-iterdir ((fs memory-filesystem) pathname)
  "Entries of the directory PATHNAME (a symlink to a directory is followed),
named under PATHNAME as given."
  (let* ((abs (fs-absolute fs pathname :defaults (memory-fs-cwd fs)))
         (real (%mem-real-key fs abs))
         (prefix (if (string= real "/") "/" (concatenate 'string real "/")))
         (spelled (%mem-lexical-key fs abs))
         (spelled (if (string= spelled "/") "/" (concatenate 'string spelled "/")))
         (out '()))
    (unless (eq (gethash real (memory-fs-root fs)) :dir)
      (error 'path-not-found :filesystem fs :path abs))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (uiop:string-prefix-p prefix k)
                          (not (string= k real))
                          (not (find #\/ k :start (length prefix))))
                 (push (fs-parse fs (concatenate 'string spelled (subseq k (length prefix))))
                       out)))
             (memory-fs-root fs))
    (nreverse out)))

(defmethod fs-walk ((fs memory-filesystem) pathname &key (top-down t) follow-symlinks ignore)
  (let ((root (fs-absolute fs pathname)))
    (%walk-tree fs root
                (lambda (dir)
                  ;; A link is listed under DIRS when it leads to a directory,
                  ;; otherwise (file or dangling link) under FILES, as on local.
                  (let ((files '()) (dirs '()))
                    (dolist (e (fs-iterdir fs dir))
                      (if (eq :dir (gethash (ignore-errors (%mem-real-key fs e))
                                            (memory-fs-root fs)))
                          (push e dirs)
                          (push e files)))
                    (values (nreverse files) (nreverse dirs))))
                :top-down top-down :follow-symlinks follow-symlinks
                :ignored-p (%ignore-predicate fs root ignore))))

(defmethod fs-mkdir ((fs memory-filesystem) pathname &key (parents t) (exist-ok t))
  (let* ((abs (uiop:ensure-directory-pathname (fs-absolute fs pathname)))
         (key (%mem-key fs abs)))
    ;; A dangling link is there too: mkdir(2) fails with EEXIST.
    (when (or (fs-exists-p fs abs) (fs-symlink-p fs abs))
      (unless (and exist-ok (fs-directory-p fs abs))
        (restart-case
            (error 'path-exists-error :filesystem fs :path abs)
          (overwrite ()
            :report (lambda (s) (format s "Keep existing path ~A" abs))
            (return-from fs-mkdir abs))))
      (return-from fs-mkdir abs))
    (let ((parent (fs-parent fs abs)))
      (if parents
          (unless (or (%pathname-root-p parent) (fs-directory-p fs parent))
            (fs-mkdir fs parent :parents t :exist-ok t))
          (unless (or (%pathname-root-p parent) (fs-directory-p fs parent))
            (%restart-create-parents fs abs))))
    (setf (gethash key (memory-fs-root fs)) :dir)
    abs))

(defmethod fs-rmdir ((fs memory-filesystem) pathname)
  (let ((key (%mem-key fs (fs-absolute fs pathname))))
    ;; Not through a link to a directory: rmdir(2) gives ENOTDIR.
    (unless (and (fs-directory-p fs pathname) (not (fs-symlink-p fs pathname)))
      (return-from fs-rmdir
        (%restart-path-not-found fs pathname
                                 :message "directory does not exist"
                                 :allow-create-file nil
                                 :allow-create-directory t
                                 :allow-ignore nil)))
    (when (fs-iterdir fs pathname)
      (error 'directory-not-empty :filesystem fs :path pathname
             :message "directory not empty"))
    (remhash key (memory-fs-root fs))
    t))

(defmethod fs-unlink ((fs memory-filesystem) pathname &key (missing-ok nil))
  (let* ((key (%mem-key fs (fs-absolute fs pathname)))
         (ent (gethash key (memory-fs-root fs))))
    (unless ent
      (return-from fs-unlink
        (if missing-ok
            nil
            (%restart-path-not-found fs pathname
                                     :message "file does not exist"
                                     :allow-create-file nil
                                     :allow-create-directory nil
                                     :allow-ignore t))))
    (when (eq ent :dir)
      (error 'path-error :filesystem fs :path pathname :message "is a directory"))
    (remhash key (memory-fs-root fs))
    t))

(defmethod fs-touch ((fs memory-filesystem) pathname &key (exist-ok t))
  (let* ((abs (fs-absolute fs pathname))
         (key (%mem-key fs abs)))
    (if (gethash key (memory-fs-root fs))
        (progn
          (unless exist-ok
            (restart-case
                (error 'path-exists-error :filesystem fs :path abs)
              (overwrite ()
                :report (lambda (s) (format s "Keep existing file ~A" abs))
                (return-from fs-touch abs))))
          abs)
        (progn
          (let ((parent (fs-parent fs abs)))
            (unless (or (%pathname-root-p parent) (fs-directory-p fs parent))
              ;; default: create parents; also offer restart if someone forces otherwise
              (fs-mkdir fs parent :parents t :exist-ok t)))
          (setf (gethash key (memory-fs-root fs))
                (list :file (make-array 0 :element-type '(unsigned-byte 8)) (get-universal-time)))
          abs))))

(defmethod fs-rename ((fs memory-filesystem) source target &key (replace t))
  (let* ((sk (%mem-key fs (fs-absolute fs source)))
         (tk (%mem-key fs (fs-absolute fs target)))
         (root (memory-fs-root fs))
         (ent (gethash sk root)))
    (unless ent
      (%restart-path-not-found fs source
                               :message "rename source does not exist"
                               :allow-create-file nil
                               :allow-create-directory nil
                               :allow-ignore nil))
    (when (and (not replace) (gethash tk root))
      (restart-case
          (error 'path-exists-error :filesystem fs :path target)
        (overwrite ()
          :report (lambda (s) (format s "Replace existing ~A" target))
          (return-from fs-rename (fs-rename fs source target :replace t)))))
    (when (string= sk tk)
      (return-from fs-rename (fs-parse fs tk)))
    (fs-mkdir fs (fs-parent fs (fs-absolute fs target)) :parents t :exist-ok t)
    (when (eq ent :dir)
      (let* ((src-prefix (if (string= sk "/") "/" (concatenate 'string sk "/")))
             (tgt-prefix (if (string= tk "/") "/" (concatenate 'string tk "/")))
             (updates '()))
        (when (uiop:string-prefix-p src-prefix tgt-prefix)
          (error 'path-error :filesystem fs :path source
                 :message "cannot rename directory into its descendant"))
        (maphash (lambda (k v)
                   (when (and (uiop:string-prefix-p src-prefix k)
                              (not (string= k sk)))
                     (push (cons k v) updates)))
                 root)
        (dolist (kv updates)
          (let ((nk (concatenate 'string tgt-prefix (subseq (car kv) (length src-prefix)))))
            (setf (gethash nk root) (cdr kv))
            (remhash (car kv) root)))))
    (setf (gethash tk root) ent)
    (remhash sk root)
    (fs-parse fs tk)))

(defmethod fs-copy ((fs memory-filesystem) source target &key (replace t))
  (let* ((sk (%mem-key fs (fs-absolute fs source)))
         (tk (%mem-key fs (fs-absolute fs target)))
         (root (memory-fs-root fs))
         (ent (gethash sk root)))
    (unless ent
      (%restart-path-not-found fs source
                               :message "copy source does not exist"
                               :allow-create-file nil
                               :allow-create-directory nil
                               :allow-ignore nil))
    (when (and (not replace) (gethash tk root))
      (restart-case
          (error 'path-exists-error :filesystem fs :path target)
        (overwrite ()
          :report (lambda (s) (format s "Replace existing ~A" target))
          (return-from fs-copy (fs-copy fs source target :replace t)))))
    (fs-mkdir fs (fs-parent fs (fs-absolute fs target)) :parents t :exist-ok t)
    (cond ((eq ent :dir)
           (setf (gethash tk root) :dir)
           (let* ((src-prefix (if (string= sk "/") "/" (concatenate 'string sk "/")))
                  (tgt-prefix (if (string= tk "/") "/" (concatenate 'string tk "/")))
                  (updates '()))
             (when (uiop:string-prefix-p src-prefix tgt-prefix)
               (error 'path-error :filesystem fs :path source
                      :message "cannot copy directory into its descendant"))
             (maphash (lambda (k v)
                        (when (and (uiop:string-prefix-p src-prefix k)
                                   (not (string= k sk)))
                          (push (cons k v) updates)))
                      root)
             (dolist (kv updates)
               (let ((nk (concatenate 'string tgt-prefix
                                      (subseq (car kv) (length src-prefix)))))
                 (setf (gethash nk root)
                       (let ((v (cdr kv)))
                         (if (and (consp v) (eq (car v) :file))
                             (list :file (copy-seq (second v)) (get-universal-time))
                             v)))))))
          ((and (consp ent) (eq (car ent) :file))
           (setf (gethash tk root)
                 (list :file (copy-seq (second ent)) (get-universal-time))))
          (t (setf (gethash tk root) ent)))
    (fs-parse fs tk)))

(defmethod fs-create-symlink ((fs memory-filesystem) link target)
  (let ((key (%mem-key fs (fs-absolute fs link))))
    (setf (gethash key (memory-fs-root fs))
          (list :symlink (%mem-lexical-key fs (fs-absolute fs target))))
    (fs-parse fs key)))

(defmethod fs-read-symlink ((fs memory-filesystem) pathname)
  (let ((ent (gethash (%mem-key fs (fs-absolute fs pathname)) (memory-fs-root fs))))
    (unless (and (consp ent) (eq (car ent) :symlink))
      (error 'path-error :filesystem fs :path pathname :message "not a symlink"))
    (fs-parse fs (second ent))))

(defmethod fs-read-bytes ((fs memory-filesystem) pathname)
  (let* ((abs (fs-absolute fs pathname))
         (key (%mem-follow-symlink fs (%mem-key fs abs)))
         (ent (gethash key (memory-fs-root fs))))
    (unless (and (consp ent) (eq (car ent) :file))
      (return-from fs-read-bytes
        (%restart-path-not-found fs abs
                                 :message "file does not exist"
                                 :allow-create-file t
                                 :allow-create-directory nil
                                 :allow-ignore nil)))
    (copy-seq (second ent))))
(defmethod fs-write-bytes ((fs memory-filesystem) pathname bytes
                           &key (append nil) if-exists if-does-not-exist)
  (declare (ignore if-exists if-does-not-exist))
  (let* ((abs (fs-absolute fs pathname))
         (key (%mem-key fs abs))
         (old (gethash key (memory-fs-root fs)))
         (vec (if (and append (consp old) (eq (car old) :file))
                  (concatenate '(vector (unsigned-byte 8)) (second old) bytes)
                  (coerce bytes '(vector (unsigned-byte 8))))))
    (when (eq old :dir)
      (error 'path-error :filesystem fs :path abs :message "is a directory"))
    (fs-mkdir fs (fs-parent fs abs) :parents t :exist-ok t)
    (setf (gethash key (memory-fs-root fs)) (list :file vec (get-universal-time)))
    abs))

(defmethod fs-make-temp ((fs memory-filesystem) &key (directory "/tmp") (prefix "tmp-") (suffix ""))
  (fs-mkdir fs directory :parents t :exist-ok t)
  (let ((dir (string-right-trim "/" (%mem-lexical-key fs (fs-absolute fs directory)))))
    (loop
      (let ((pn (fs-parse fs (format nil "~A/~A~A~A" dir prefix (%temp-name-token) suffix))))
        (unless (or (fs-exists-p fs pn) (fs-symlink-p fs pn))
          (return pn))))))

(defmethod fs-same-p ((fs memory-filesystem) a b)
  (string= (%mem-key fs (fs-absolute fs a))
           (%mem-key fs (fs-absolute fs b))))

(defmethod fs-as-uri ((fs memory-filesystem) pathname)
  (format nil "memory://~A" (%mem-lexical-key fs (fs-absolute fs pathname))))

;;; --- H2a: atomic writes and streams ----------------------------------------

(defun %mem-file-entry (fs key)
  (let ((ent (gethash key (memory-fs-root fs))))
    (and (consp ent) (eq (car ent) :file) ent)))

(defmethod fs-write-atomic ((fs memory-filesystem) pathname bytes &key expected-hash)
  ;; One table update: readers see the old entry or the new one.
  (let* ((abs (fs-absolute fs pathname))
         (vec (copy-seq (%coerce-octets bytes)))   ; checked + unaliased before any change
         (key (%mem-follow-symlink fs (%mem-key fs abs)))
         (root (memory-fs-root fs)))
    (when (eq (gethash key root) :dir)
      (error 'path-error :filesystem fs :path abs :message "target is a directory"))
    (multiple-value-bind (proceed current)
        (%check-expected-hash fs abs expected-hash
                              (lambda ()
                                (let ((ent (%mem-file-entry fs key)))
                                  (and ent (octets-hash (second ent))))))
      (unless proceed
        (return-from fs-write-atomic (values abs current nil))))
    (fs-mkdir fs (fs-parent fs (fs-parse fs key)) :parents t :exist-ok t)
    (setf (gethash key root) (list :file vec (get-universal-time)))
    (values abs (octets-hash vec) t)))

(defmethod fs-open ((fs memory-filesystem) pathname &key (direction :input) (element-type '(unsigned-byte 8)))
  ;; Entries are replaced, never mutated in place, so the stored vector is a
  ;; stable snapshot for the stream: no copy.
  (let* ((kind (%check-open-args fs pathname direction element-type))
         (abs (fs-absolute fs pathname))
         (ent (%mem-file-entry fs (%mem-follow-symlink fs (%mem-key fs abs)))))
    (unless ent
      (error 'path-not-found :filesystem fs :path abs :message "file does not exist"))
    (%octets-input-stream fs abs (second ent) kind)))
