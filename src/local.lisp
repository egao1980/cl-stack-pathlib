(in-package #:cl-stack-pathlib)

(defclass local-filesystem (filesystem)
  ()
  (:default-initargs :name "file"))

(defun make-local-filesystem ()
  (make-instance 'local-filesystem))

(defmethod fs-parse ((fs local-filesystem) designator &key directory)
  (let ((pn (etypecase designator
              (pathname designator)
              (string (%parse-posix designator))
              (path (path-pathname designator)))))
    (if directory (uiop:ensure-directory-pathname pn) pn)))

(defmethod fs-join ((fs local-filesystem) base relative)
  (uiop:merge-pathnames*
   (fs-parse fs relative)
   (uiop:ensure-directory-pathname (fs-parse fs base))))

(defmethod fs-parent ((fs local-filesystem) pathname)
  (%logical-parent (fs-parse fs pathname)))

(defmethod fs-name ((fs local-filesystem) pathname)
  (%file-namestring* (fs-parse fs pathname)))

(defmethod fs-absolute ((fs local-filesystem) pathname &key (defaults (uiop:getcwd)))
  (let ((pn (fs-parse fs pathname))
        (base (uiop:ensure-directory-pathname (fs-parse fs defaults))))
    (if (uiop:absolute-pathname-p pn) pn (uiop:merge-pathnames* pn base))))

(defmethod fs-resolve ((fs local-filesystem) pathname &key (strict t))
  "Resolve to a physical path. STRICT signals if the leaf does not exist.
UIOP TRUENAMIZE alone is not enough — it succeeds for missing leaves."
  (let* ((abs (fs-absolute fs pathname))
         (true (uiop:probe-file* abs :truename t)))
    (cond (true true)
          (strict
           (%restart-path-not-found fs abs
                                    :message "path does not exist"
                                    :allow-create-file t
                                    :allow-create-directory t
                                    :allow-ignore nil))
          (t (or (ignore-errors (uiop:truenamize abs)) abs)))))

(defmethod fs-normpath ((fs local-filesystem) pathname)
  "Collapse `.` / `..` in PATHNAME (NIO normalize / pathlib-ish)."
  (let ((pn (fs-parse fs pathname)))
    (multiple-value-bind (abs? stack dir?) (%collapse-dot-segments pn)
      (fs-parse fs
                (cond ((and abs? (null stack)) "/")
                      (abs? (format nil "/~{~A~^/~}" stack))
                      ((null stack) ".")
                      (t (format nil "~{~A~^/~}" stack)))
                :directory dir?))))

(defun %passwd-home (username)
  (with-open-file (in "/etc/passwd" :if-does-not-exist nil)
    (loop for line = (read-line in nil nil) while line
          do (let ((parts (uiop:split-string line :separator ":")))
               (when (and (>= (length parts) 7) (string= username (first parts)))
                 (return (uiop:parse-native-namestring (nth 5 parts))))))))

(defun %expand-tilde-string (s)
  (cond ((= (length s) 1) (user-homedir-pathname))
        ((char= (char s 1) #\/) (merge-pathnames (subseq s 2) (user-homedir-pathname)))
        (t (let* ((slash (position #\/ s :start 1))
                  (username (subseq s 1 slash))
                  (rest (if slash (subseq s slash) ""))
                  (home (%passwd-home username)))
             (if home
                 (merge-pathnames rest home)
                 (uiop:parse-native-namestring s))))))

(defmethod fs-expanduser ((fs local-filesystem) pathname)
  (let* ((pn (fs-parse fs pathname))
         (s (namestring pn)))
    (if (and (plusp (length s)) (char= (char s 0) #\~))
        (fs-parse fs (uiop:parse-native-namestring (uiop:native-namestring (%expand-tilde-string s))))
        pn)))

(defmethod fs-exists-p ((fs local-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (or (uiop:file-exists-p pn) (uiop:directory-exists-p pn))))

(defmethod fs-file-p ((fs local-filesystem) pathname)
  (and (uiop:file-exists-p (fs-parse fs pathname)) t))

(defmethod fs-directory-p ((fs local-filesystem) pathname)
  (and (uiop:directory-exists-p (fs-parse fs pathname)) t))

(defmethod fs-symlink-p ((fs local-filesystem) pathname)
  #+sbcl
  (handler-case
      (let ((stat (sb-posix:lstat (namestring (fs-parse fs pathname)))))
        (sb-posix:s-islnk (sb-posix:stat-mode stat)))
    (error () nil))
  #-sbcl nil)

(defmethod fs-readable-p ((fs local-filesystem) pathname)
  (ignore-errors
    (with-open-file (s (fs-parse fs pathname) :direction :input :if-does-not-exist nil)
      (and s t))))

(defmethod fs-writable-p ((fs local-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (or (ignore-errors
          (with-open-file (s pn :direction :output :if-exists :append :if-does-not-exist nil)
            (and s t)))
        (let ((parent (fs-parent fs pn)))
          (and (fs-directory-p fs parent)
               (ignore-errors
                 (let ((tmp (merge-pathnames (format nil ".cl-stack-pathlib-~A" (random 1e9)) parent)))
                   (unwind-protect
                        (progn (with-open-file (s tmp :direction :output :if-exists :error
                                                  :if-does-not-exist :create))
                               t)
                     (ignore-errors (delete-file tmp))))))))))

(defmethod fs-executable-p ((fs local-filesystem) pathname)
  #+sbcl
  (handler-case
      (let ((mode (sb-posix:stat-mode (sb-posix:stat (namestring (fs-parse fs pathname))))))
        (plusp (logand mode sb-posix:s-ixusr)))
    (error () nil))
  #-sbcl nil)

(defmethod fs-file-size ((fs local-filesystem) pathname)
  (with-open-file (s (fs-parse fs pathname) :direction :input :element-type '(unsigned-byte 8)
                      :if-does-not-exist :error)
    (file-length s)))

(defmethod fs-last-modified ((fs local-filesystem) pathname)
  (file-write-date (fs-parse fs pathname)))

(defmethod fs-iterdir ((fs local-filesystem) pathname)
  (let ((dir (uiop:ensure-directory-pathname (fs-parse fs pathname))))
    (unless (uiop:directory-exists-p dir)
      (error 'path-not-found :filesystem fs :path dir))
    (append (uiop:directory-files dir) (uiop:subdirectories dir))))

(defmethod fs-glob ((fs local-filesystem) pathname pattern &key recursive)
  (let* ((base (uiop:ensure-directory-pathname (fs-absolute fs pathname)))
         (pat (if recursive
                  (merge-pathnames (uiop:parse-unix-namestring
                                    (format nil "**/~A" pattern))
                                   base)
                  (merge-pathnames (uiop:parse-unix-namestring pattern) base))))
    (directory pat)))

(defmethod fs-walk ((fs local-filesystem) pathname &key (top-down t) (follow-symlinks nil))
  (declare (ignore follow-symlinks))
  (let ((out '()))
    (labels ((walk* (dir)
               (let* ((dir (uiop:ensure-directory-pathname dir))
                      (files (ignore-errors (uiop:directory-files dir)))
                      (subs (ignore-errors (uiop:subdirectories dir))))
                 (when top-down (push (list dir files subs) out))
                 (dolist (s subs) (walk* s))
                 (unless top-down (push (list dir files subs) out)))))
      (walk* (fs-absolute fs pathname))
      (nreverse out))))

(defmethod fs-mkdir ((fs local-filesystem) pathname &key (parents t) (exist-ok t))
  (let ((dir (uiop:ensure-directory-pathname (fs-parse fs pathname))))
    (cond ((uiop:directory-exists-p dir)
           (unless exist-ok
             (restart-case
                 (error 'path-exists-error :filesystem fs :path dir)
               (overwrite ()
                 :report (lambda (s) (format s "Keep existing directory ~A" dir))
                 dir)))
           dir)
          (parents
           (ensure-directories-exist (merge-pathnames (make-pathname :name "x") dir))
           dir)
          (t
           (let ((parent (uiop:pathname-parent-directory-pathname dir)))
             (unless (uiop:directory-exists-p parent)
               (%restart-create-parents fs dir))
             #+sbcl (sb-posix:mkdir (namestring dir) #o755)
             #-sbcl (uiop:run-program (list "mkdir" (uiop:unix-namestring dir))
                                      :ignore-error-status nil))
           dir))))

(defmethod fs-rmdir ((fs local-filesystem) pathname)
  (uiop:delete-empty-directory (uiop:ensure-directory-pathname (fs-parse fs pathname))))

(defmethod fs-unlink ((fs local-filesystem) pathname &key (missing-ok nil))
  (let ((pn (fs-parse fs pathname)))
    (handler-case (progn (delete-file pn) t)
      (file-error ()
        (return-from fs-unlink
          (if missing-ok
              nil
              (%restart-path-not-found fs pn
                                       :message "file does not exist"
                                       :allow-create-file nil
                                       :allow-create-directory nil
                                       :allow-ignore t)))))))

(defmethod fs-touch ((fs local-filesystem) pathname &key (exist-ok t))
  (let ((pn (fs-parse fs pathname)))
    (cond ((uiop:file-exists-p pn)
           (unless exist-ok
             (restart-case
                 (error 'path-exists-error :filesystem fs :path pn)
               (overwrite ()
                 :report (lambda (s) (format s "Keep existing file ~A" pn))
                 pn)))
           pn)
          (t
           (let ((parent (uiop:pathname-parent-directory-pathname pn)))
             (unless (or (null parent) (uiop:directory-exists-p parent))
               (%restart-create-parents fs pn)))
           (handler-case
               (with-open-file (s pn :direction :output :if-exists :error
                                  :if-does-not-exist :create)
                 pn)
             (file-error ()
               (%restart-create-parents fs pn)))))))

(defmethod fs-rename ((fs local-filesystem) source target &key (replace t))
  (let ((from (fs-parse fs source))
        (to (fs-parse fs target)))
    (unless (fs-exists-p fs from)
      (%restart-path-not-found fs from
                               :message "rename source does not exist"
                               :allow-create-file nil
                               :allow-create-directory nil
                               :allow-ignore nil))
    (when (and (not replace) (fs-exists-p fs to))
      (restart-case
          (error 'path-exists-error :filesystem fs :path to)
        (overwrite ()
          :report (lambda (s) (format s "Replace existing ~A" to))
          (return-from fs-rename (fs-rename fs from to :replace t)))))
    (rename-file from to)
    to))

(defmethod fs-copy ((fs local-filesystem) source target &key (replace t))
  (let ((from (fs-parse fs source))
        (to (fs-parse fs target)))
    (unless (fs-exists-p fs from)
      (%restart-path-not-found fs from
                               :message "copy source does not exist"
                               :allow-create-file nil
                               :allow-create-directory nil
                               :allow-ignore nil))
    (when (and (not replace) (fs-exists-p fs to))
      (restart-case
          (error 'path-exists-error :filesystem fs :path to)
        (overwrite ()
          :report (lambda (s) (format s "Replace existing ~A" to))
          (return-from fs-copy (fs-copy fs from to :replace t)))))
    (let ((parent (uiop:pathname-parent-directory-pathname to)))
      (unless (or (null parent) (uiop:directory-exists-p parent))
        (%restart-create-parents fs to)))
    (uiop:copy-file from to)
    to))
(defmethod fs-create-symlink ((fs local-filesystem) link target)
  #+sbcl
  (progn
    (sb-posix:symlink (namestring (fs-parse fs target))
                      (namestring (fs-parse fs link)))
    (fs-parse fs link))
  #-sbcl
  (%unsupported fs 'fs-create-symlink link))

(defmethod fs-read-symlink ((fs local-filesystem) pathname)
  #+sbcl
  (pathname (sb-posix:readlink (namestring (fs-parse fs pathname))))
  #-sbcl
  (%unsupported fs 'fs-read-symlink pathname))

(defmethod fs-read-bytes ((fs local-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (handler-case
        (with-open-file (s pn :direction :input
                           :element-type '(unsigned-byte 8) :if-does-not-exist :error)
          (let* ((len (file-length s))
                 (buf (make-array len :element-type '(unsigned-byte 8))))
            (read-sequence buf s)
            buf))
      (file-error ()
        (return-from fs-read-bytes
          (%restart-path-not-found fs pn
                                   :message "file does not exist"
                                   :allow-create-file t
                                   :allow-create-directory nil
                                   :allow-ignore nil))))))

(defmethod fs-write-bytes ((fs local-filesystem) pathname bytes
                           &key (append nil)
                                (if-exists (if append :append :supersede))
                                (if-does-not-exist :create))
  (let ((pn (fs-parse fs pathname)))
    (let ((parent (uiop:pathname-parent-directory-pathname pn)))
      (unless (or (null parent) (uiop:directory-exists-p parent))
        (%restart-create-parents fs pn)))
    (handler-case
        (with-open-file (s pn :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists if-exists
                           :if-does-not-exist if-does-not-exist)
          (write-sequence bytes s))
      (file-error ()
        (%restart-create-parents fs pn)))
    pn))
(defmethod fs-make-temp ((fs local-filesystem) &key (directory nil) (prefix "cl-stack-") (suffix ""))
  (let* ((dir (uiop:ensure-directory-pathname
               (or directory (uiop:temporary-directory))))
         (name (format nil "~A~A~A" prefix (get-universal-time) (random 100000))))
    (if (and suffix (plusp (length suffix)))
        (merge-pathnames (make-pathname :name name :type (string-left-trim "." suffix)) dir)
        (merge-pathnames (make-pathname :name name) dir))))

(defmethod fs-same-p ((fs local-filesystem) a b)
  (handler-case
      (equal (uiop:truenamize (fs-absolute fs a))
             (uiop:truenamize (fs-absolute fs b)))
    (error ()
      (equal (namestring (fs-absolute fs a))
             (namestring (fs-absolute fs b))))))

(defmethod fs-as-uri ((fs local-filesystem) pathname)
  (format nil "file://~A" (uiop:unix-namestring (fs-absolute fs pathname))))

(setf *filesystem* (make-local-filesystem))
