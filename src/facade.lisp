(in-package #:cl-stack-pathlib)

;;; Keyword-heavy facade — pathlib / NIO Path+Files DX over the filesystem protocol.

(defun %fs (designator)
  (if (path-p designator) (path-filesystem designator) (%current-fs)))

(defun %pn (designator)
  (path-pathname (ensure-path designator)))

(defun %wrap (fs pathname)
  (make-path pathname :filesystem fs))

(defun path (designator &key directory (filesystem nil filesystem-p))
  (if filesystem-p
      (ensure-path designator :filesystem filesystem :directory directory)
      (ensure-path designator :directory directory)))

(defun join (base &rest parts)
  (let* ((p0 (ensure-path base))
         (fs (path-filesystem p0))
         (acc (path-pathname p0)))
    (dolist (part parts (%wrap fs acc))
      (when part
        (setf acc (fs-join fs acc part))))))

(setf (fdefinition 'joinpath) (fdefinition 'join))

(defun from-list (components &key (filesystem *filesystem*) absolute)
  (let* ((comps (mapcar #'string components))
         (s (if absolute
                (format nil "/~{~A~^/~}" comps)
                (format nil "~{~A~^/~}" comps))))
    (ensure-path s :filesystem filesystem)))

(defun components (designator)
  (let* ((pn (%pn designator))
         (dir (pathname-directory pn))
         (out '()))
    (when (and (consp dir) (eq (first dir) :absolute))
      (push "/" out))
    (when (consp dir)
      (dolist (c (rest dir))
        (when (stringp c) (push c out))))
    (let ((fn (%file-namestring* pn)))
      (when fn (push fn out)))
    (nreverse out)))

(defun parts (designator)
  (remove "/" (components designator) :test #'string=))

(defun parent (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p) (fs-parent (path-filesystem p) (path-pathname p)))))

(defun parents (designator)
  (loop for p = (parent designator) then (parent p)
        until (root-p p)
        collect p))

(defun with-parent (designator new-parent)
  (join new-parent (name designator)))

(defun name (designator)
  (let ((p (ensure-path designator)))
    (fs-name (path-filesystem p) (path-pathname p))))

(defun with-name (designator new-name)
  (let* ((p (ensure-path designator))
         (fs (path-filesystem p))
         (dir (uiop:pathname-directory-pathname (path-pathname p))))
    (%wrap fs (fs-join fs dir new-name))))

(defun base (designator)
  (pathname-name (%pn designator)))

(setf (fdefinition 'stem) (fdefinition 'base))

(defun with-base (designator new-base)
  (let ((pn (%pn designator)))
    (with-name designator
               (make-pathname :name new-base :type (pathname-type pn) :defaults ""))))

(setf (fdefinition 'with-stem) (fdefinition 'with-base))

(defun extension (designator)
  (let ((type (pathname-type (%pn designator))))
    (if (and type (not (eq type :unspecific))) (string type) "")))

(defun suffix (designator)
  (let ((ext (extension designator)))
    (if (plusp (length ext)) (concatenate 'string "." ext) "")))

(defun suffixes (designator)
  (let* ((n (or (name designator) ""))
         (parts (uiop:split-string n :separator ".")))
    (if (<= (length parts) 1)
        '()
        (mapcar (lambda (s) (concatenate 'string "." s)) (rest parts)))))

(defun with-extension (designator new-ext)
  (let* ((ext (string new-ext))
         (type (if (and (plusp (length ext)) (char= (char ext 0) #\.))
                   (subseq ext 1)
                   ext)))
    (with-name designator
               (make-pathname :name (base designator) :type (if (plusp (length type)) type nil)
                              :defaults ""))))

(setf (fdefinition 'with-suffix) (fdefinition 'with-extension))

(defun add-extension (designator new-ext)
  (let* ((ext (string-left-trim "." (string new-ext)))
         (n (name designator)))
    (with-name designator (format nil "~A.~A" n ext))))

(defun drop-extension (designator)
  (with-name designator (base designator)))

(defun root-p (designator)
  (%pathname-root-p (%pn designator)))

(defun empty-p (designator)
  (%pathname-empty-p (%pn designator)))

(defun absolute-p (designator)
  (uiop:absolute-pathname-p (%pn designator)))

(defun relative-p (designator)
  (uiop:relative-pathname-p (%pn designator)))

(defun directory-pathname-p (designator)
  (uiop:directory-pathname-p (%pn designator)))

(defun starts-with-p (designator prefix)
  (let ((a (components designator))
        (b (components prefix)))
    (and (<= (length b) (length a))
         (equal (subseq a 0 (length b)) b))))

(defun %tail (list n)
  (nthcdr (max 0 (- (length list) n)) list))

(defun ends-with-p (designator suffix)
  (let* ((a (components designator))
         (b (components suffix)))
    (and (<= (length b) (length a))
         (equal (%tail a (length b)) b))))

(defun match-p (designator pattern)
  (pathname-match-p (%pn designator) (uiop:parse-unix-namestring pattern)))

(defun absolute (designator &key (defaults nil defaults-p))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (if defaults-p
               (fs-absolute (path-filesystem p) (path-pathname p)
                            :defaults (%pn defaults))
               (fs-absolute (path-filesystem p) (path-pathname p))))))

(defun resolve (designator &key (strict t))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-resolve (path-filesystem p) (path-pathname p) :strict strict))))

(defun expanduser (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-expanduser (path-filesystem p) (path-pathname p)))))

(defun normpath (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-normpath (path-filesystem p) (path-pathname p)))))

(defun cwd (&key (filesystem *filesystem*))
  (%wrap filesystem
         (if (typep filesystem 'memory-filesystem)
             (fs-parse filesystem (memory-fs-cwd filesystem) :directory t)
             (uiop:getcwd))))

(defun home (&key (filesystem *filesystem*))
  (%wrap filesystem
         (if (typep filesystem 'memory-filesystem)
             (fs-parse filesystem (memory-fs-home filesystem) :directory t)
             (user-homedir-pathname))))

(defun as-namestring (designator &key (style :native))
  (let ((pn (%pn designator)))
    (ecase style
      ((:native) (namestring pn))
      ((:posix :codegen) (or (uiop:unix-namestring pn) (namestring pn))))))

(defun as-posix (designator)
  (as-namestring designator :style :posix))

(defun as-uri (designator)
  (let ((p (ensure-path designator)))
    (fs-as-uri (path-filesystem p) (path-pathname p))))

(defun to-string (designator)
  (as-posix designator))

(defun from-string (string &key (filesystem *filesystem*))
  (ensure-path string :filesystem filesystem))

(defun ensure-directory (designator)
  (ensure-path designator :directory t))

(defun ensure-string (designator)
  (as-posix designator))

(defun relative-to (designator base)
  (let* ((p (ensure-path designator))
         (b (ensure-path base :filesystem (path-filesystem p)))
         (rel (uiop:enough-pathname
               (fs-absolute (path-filesystem p) (path-pathname p))
               (uiop:ensure-directory-pathname
                (fs-absolute (path-filesystem b) (path-pathname b))))))
    (unless (uiop:relative-pathname-p rel)
      (error 'not-relative-error :path p :filesystem (path-filesystem p)
             :message (format nil "~A is not relative to ~A" p b)))
    (%wrap (path-filesystem p) rel)))

(defun relative-to-p (designator base)
  (handler-case (progn (relative-to designator base) t)
    (not-relative-error () nil)))

(defun under (root relative)
  (join (ensure-directory root) relative))

(defun same-p (a b)
  (let ((pa (ensure-path a)))
    (fs-same-p (path-filesystem pa) (path-pathname pa)
               (path-pathname (ensure-path b :filesystem (path-filesystem pa))))))

(defun path-equal (a b)
  (and (eq (path-filesystem (ensure-path a))
           (path-filesystem (ensure-path b)))
       (equal (namestring (%pn a)) (namestring (%pn b)))))

(macrolet ((def-probe (name fs-name)
             `(defun ,name (designator)
                (let ((p (ensure-path designator)))
                  (,fs-name (path-filesystem p) (path-pathname p))))))
  (def-probe exists-p fs-exists-p)
  (def-probe file-p fs-file-p)
  (def-probe directory-p fs-directory-p)
  (def-probe symlink-p fs-symlink-p)
  (def-probe readable-p fs-readable-p)
  (def-probe writable-p fs-writable-p)
  (def-probe executable-p fs-executable-p)
  (def-probe file-size fs-file-size)
  (def-probe last-modified fs-last-modified))

(defun iterdir (designator)
  (let ((p (ensure-path designator)))
    (mapcar (lambda (pn) (%wrap (path-filesystem p) pn))
            (fs-iterdir (path-filesystem p) (path-pathname p)))))

(defun glob (designator pattern &key recursive)
  (let ((p (ensure-path designator)))
    (mapcar (lambda (pn) (%wrap (path-filesystem p) pn))
            (fs-glob (path-filesystem p) (path-pathname p) pattern :recursive recursive))))

(defun rglob (designator pattern)
  (glob designator pattern :recursive t))

(defun walk (designator &key (top-down t) (follow-symlinks nil))
  (let ((p (ensure-path designator))
        (fs (path-filesystem (ensure-path designator))))
    (mapcar (lambda (triple)
              (destructuring-bind (dir files dirs) triple
                (list (%wrap fs dir)
                      (mapcar (lambda (f) (%wrap fs f)) files)
                      (mapcar (lambda (d) (%wrap fs d)) dirs))))
            (fs-walk fs (path-pathname p) :top-down top-down
                                          :follow-symlinks follow-symlinks))))

(defun mkdir (designator &key (parents t) (exist-ok t))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-mkdir (path-filesystem p) (path-pathname p)
                     :parents parents :exist-ok exist-ok))))

(defun rmdir (designator)
  (let ((p (ensure-path designator)))
    (fs-rmdir (path-filesystem p) (path-pathname p))))

(defun unlink (designator &key (missing-ok nil))
  (let ((p (ensure-path designator)))
    (fs-unlink (path-filesystem p) (path-pathname p) :missing-ok missing-ok)))

(setf (fdefinition 'delete-path) (fdefinition 'unlink))

(defun touch (designator &key (exist-ok t))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-touch (path-filesystem p) (path-pathname p) :exist-ok exist-ok))))

(defun rename-path (source target &key (replace t))
  (let ((s (ensure-path source)))
    (%wrap (path-filesystem s)
           (fs-rename (path-filesystem s) (path-pathname s)
                      (path-pathname (ensure-path target :filesystem (path-filesystem s)))
                      :replace replace))))

(defun replace-path (source target)
  (rename-path source target :replace t))

(defun copy-path (source target &key (replace t))
  (let ((s (ensure-path source)))
    (%wrap (path-filesystem s)
           (fs-copy (path-filesystem s) (path-pathname s)
                    (path-pathname (ensure-path target :filesystem (path-filesystem s)))
                    :replace replace))))

(defun move-path (source target &key (replace t))
  (copy-path source target :replace replace)
  (unlink source)
  (ensure-path target :filesystem (path-filesystem (ensure-path source))))

(defun create-symlink (link target)
  (let ((l (ensure-path link)))
    (%wrap (path-filesystem l)
           (fs-create-symlink (path-filesystem l) (path-pathname l)
                              (path-pathname (ensure-path target :filesystem (path-filesystem l)))))))

(defun read-symlink (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-read-symlink (path-filesystem p) (path-pathname p)))))

(defun make-temp-file (&key directory (prefix "cl-stack-") (suffix ".tmp")
                            (filesystem *filesystem*))
  (let ((pn (fs-make-temp filesystem :directory (when directory (%pn directory))
                                     :prefix prefix :suffix suffix)))
    (fs-touch filesystem pn)
    (%wrap filesystem pn)))

(defun make-temp-directory (&key directory (prefix "cl-stack-") (filesystem *filesystem*))
  (let ((pn (fs-make-temp filesystem :directory (when directory (%pn directory))
                                     :prefix prefix :suffix "")))
    (%wrap filesystem (fs-mkdir filesystem pn :parents t :exist-ok t))))

(defun read-bytes (designator)
  (let ((p (ensure-path designator)))
    (fs-read-bytes (path-filesystem p) (path-pathname p))))

(defun write-bytes (designator bytes &key append)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-write-bytes (path-filesystem p) (path-pathname p) bytes :append append))))

(defun read-text (designator &key (encoding :utf-8))
  (declare (ignore encoding))
  (babel-or-octets (read-bytes designator)))

(defun write-text (designator text &key (encoding :utf-8) append)
  (declare (ignore encoding))
  (write-bytes designator (string-to-octets* text) :append append))

(defun babel-or-octets (bytes)
  (handler-case
      (map 'string #'code-char bytes) ; UTF-8 ascii-safe fallback without babel dep
    (error ()
      (let ((s (make-string (length bytes))))
        (dotimes (i (length bytes) s)
          (setf (char s i) (code-char (aref bytes i))))))))

(defun string-to-octets* (text)
  (let ((v (make-array (length text) :element-type '(unsigned-byte 8))))
    (dotimes (i (length text) v)
      (setf (aref v i) (char-code (char text i))))))
