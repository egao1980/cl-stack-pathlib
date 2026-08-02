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

(defun %mem-key (fs pathname)
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

(defun %mem-follow-symlink (fs key &optional visited)
  (let ((ent (gethash key (memory-fs-root fs))))
    (if (and (consp ent) (eq (car ent) :symlink))
        (let ((target (second ent)))
          (when (member target visited :test #'string=)
            (error 'path-error :filesystem fs :path key :message "symlink loop"))
          (%mem-follow-symlink fs target (cons key visited)))
        key)))

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

(defmethod fs-exists-p ((fs memory-filesystem) pathname)
  (let ((key (%mem-key fs (fs-absolute fs pathname))))
    (nth-value 1 (gethash key (memory-fs-root fs)))))

(defmethod fs-file-p ((fs memory-filesystem) pathname)
  (let ((ent (gethash (%mem-key fs (fs-absolute fs pathname)) (memory-fs-root fs))))
    (and (consp ent) (eq (car ent) :file))))

(defmethod fs-directory-p ((fs memory-filesystem) pathname)
  (let ((key (%mem-key fs (fs-absolute fs pathname))))
    (eq (gethash key (memory-fs-root fs)) :dir)))

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
  (let ((ent (gethash (%mem-key fs (fs-absolute fs pathname)) (memory-fs-root fs))))
    (unless (and (consp ent) (eq (car ent) :file))
      (error 'path-not-found :filesystem fs :path pathname))
    (length (second ent))))

(defmethod fs-last-modified ((fs memory-filesystem) pathname)
  (let ((ent (gethash (%mem-key fs (fs-absolute fs pathname)) (memory-fs-root fs))))
    (unless (and (consp ent) (eq (car ent) :file))
      (error 'path-not-found :filesystem fs :path pathname))
    (third ent)))

(defmethod fs-iterdir ((fs memory-filesystem) pathname)
  (let* ((abs (fs-absolute fs pathname :defaults (memory-fs-cwd fs)))
         (prefix (%mem-key fs abs))
         (prefix (if (string= prefix "/") "/" (concatenate 'string prefix "/")))
         (out '()))
    (unless (fs-directory-p fs abs)
      (error 'path-not-found :filesystem fs :path abs))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (uiop:string-prefix-p prefix k)
                          (not (string= k (%mem-key fs abs)))
                          (not (find #\/ k :start (length prefix))))
                 (push (fs-parse fs k) out)))
             (memory-fs-root fs))
    (nreverse out)))

(defmethod fs-glob ((fs memory-filesystem) pathname pattern &key recursive)
  (let* ((base-key (%mem-key fs (fs-absolute fs pathname)))
         (prefix (if (string= base-key "/")
                     "/"
                     (concatenate 'string base-key "/")))
         (out '()))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (when (uiop:string-prefix-p prefix k)
         (let* ((rel (subseq k (length prefix)))
                (nm (file-namestring k))
                (nested-p (find #\/ rel)))
           (when (and (plusp (length rel))
                      (or recursive (find #\/ pattern) (not nested-p))
                      (if (find #\/ pattern)
                          (pathname-match-p rel (%wild-pattern pattern))
                          (and nm (%name-matches-p nm pattern))))
             (push (fs-parse fs k) out)))))
     (memory-fs-root fs))
    (nreverse out)))

(defmethod fs-walk ((fs memory-filesystem) pathname &key (top-down t) follow-symlinks)
  (declare (ignore follow-symlinks))
  (let ((out '()))
    (labels ((walk* (dir)
               (let* ((entries (fs-iterdir fs dir))
                      (files (remove-if-not (lambda (p) (fs-file-p fs p)) entries))
                      (dirs (remove-if-not (lambda (p) (fs-directory-p fs p)) entries)))
                 (when top-down (push (list dir files dirs) out))
                 (dolist (d dirs) (walk* d))
                 (unless top-down (push (list dir files dirs) out)))))
      (walk* (fs-absolute fs pathname))
      (nreverse out))))

(defmethod fs-mkdir ((fs memory-filesystem) pathname &key (parents t) (exist-ok t))
  (let* ((abs (uiop:ensure-directory-pathname (fs-absolute fs pathname)))
         (key (%mem-key fs abs)))
    (when (fs-exists-p fs abs)
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
    (unless (fs-directory-p fs pathname)
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
          (list :symlink (%mem-key fs (fs-absolute fs target))))
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
  (fs-parse fs (format nil "~A/~A~A~A"
                       (string-right-trim "/" (%mem-key fs (fs-absolute fs directory)))
                       prefix (get-universal-time) suffix)))

(defmethod fs-same-p ((fs memory-filesystem) a b)
  (string= (%mem-key fs (fs-absolute fs a))
           (%mem-key fs (fs-absolute fs b))))

(defmethod fs-as-uri ((fs memory-filesystem) pathname)
  (format nil "memory://~A" (%mem-key fs (fs-absolute fs pathname))))
