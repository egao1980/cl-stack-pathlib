(in-package #:cl-stack-pathlib/tests)

(deftest file-uri-roundtrip
  (ok (member "file" (list-uri-schemes) :test #'string-equal))
  (let ((p (ensure-path "file:///tmp/cl-stack-pathlib-uri.txt")))
    (ok (typep (path-filesystem p) 'local-filesystem))
    (ok (posix= p "/tmp/cl-stack-pathlib-uri.txt"))))

(deftest zip-stored-roundtrip
  (clear-zip-filesystem-cache)
  (let* ((bytes (write-zip-bytes
                 '(("countries/index.sexp" "((\"DE\" \"Germany\" 3))")
                   ("countries/DE.sexp" "(:code \"DE\" :name \"Germany\")")
                   ("empty.txt" ""))))
         (fs (make-zip-filesystem "<mem>" :bytes bytes :cache nil)))
    (with-filesystem (fs)
      (ok (directory-p "/countries"))
      (ok (file-p "/countries/DE.sexp"))
      (ok (file-p "/empty.txt"))
      (ok (zerop (file-size "/empty.txt")))
      (ok (string= (read-text "/countries/DE.sexp")
                   "(:code \"DE\" :name \"Germany\")"))
      (ok (string= (read-text "/countries/index.sexp")
                   "((\"DE\" \"Germany\" 3))"))
      (let ((kids (mapcar #'as-posix (iterdir "/countries"))))
        (ok (member "/countries/DE.sexp" kids :test #'string=))
        (ok (member "/countries/index.sexp" kids :test #'string=)))
      (ok (signals (write-text "/nope" "x") 'unsupported-operation)))))

(deftest zip-uri-and-join
  (clear-zip-filesystem-cache)
  (let ((tmp (uiop:with-temporary-file (:pathname pn :type "zip" :keep t)
               (write-zip-file pn '(("a/b.txt" "hello")
                                    ("a/c.txt" "world")))
               pn)))
    (unwind-protect
         (let* ((via-path (zip-path tmp "a/b.txt"))
                (uri (as-uri via-path))
                (p (ensure-path uri)))
           (ok (uiop:string-prefix-p "zip://" uri))
           (ok (search "!/a/b.txt" uri))
           (when (uiop:os-windows-p)
             (let ((bang (or (position #\! uri) (length uri))))
               (ok (search ":/" (subseq uri 6 bang)))))
           (ok (zip-filesystem-p (path-filesystem p)))
           (ok (string= (read-text p) "hello"))
           (ok (string= (read-text (join (parent p) "c.txt")) "world"))
           (let ((root (zip-path tmp)))
             (ok (directory-p root))
             (ok (string= (read-text (join root "a/b.txt")) "hello"))))
      (ignore-errors (delete-file tmp))
      (clear-zip-filesystem-cache))))

(deftest zip-uri-rfc8089-windows-shape
  "Archive half is RFC 8089 path-absolute: /C:/… not unix-namestring (drops drive)."
  (dolist (pair '(("/C:/Users/foo/data.zip" "C:/Users/foo/data.zip")
                  ("C:/Users/foo/data.zip" "C:/Users/foo/data.zip")
                  ("file:///C:/Users/foo/data.zip" "C:/Users/foo/data.zip")
                  ("file:/C:/Users/foo/data.zip" "C:/Users/foo/data.zip")
                  ("//C:/Users/foo/data.zip" "C:/Users/foo/data.zip")
                  ("file://localhost/C:/app/data.zip" "C:/app/data.zip")
                  ("c|/Windows/data.zip" "C:/Windows/data.zip")
                  ("/tmp/data.zip" "/tmp/data.zip")))
    (destructuring-bind (in expected) pair
      (ok (string= (cl-stack-pathlib::%normalize-archive-string in) expected)
          (format nil "~A → ~A" in expected))))
  (let ((tmp (uiop:with-temporary-file (:pathname pn :type "zip" :keep t) pn)))
    (unwind-protect
         (let ((uri-path (cl-stack-pathlib::%file-uri-path tmp)))
           (ok (and (plusp (length uri-path)) (char= (char uri-path 0) #\/)))
           (when (uiop:os-windows-p)
             (ok (and (>= (length uri-path) 3)
                      (alpha-char-p (char uri-path 1))
                      (char= (char uri-path 2) #\:)))))
      (ignore-errors (delete-file tmp)))))

(deftest zip-uri-file-composition
  "VFS/jar shape: zip:file:///<rfc8089-path>!/entry."
  (clear-zip-filesystem-cache)
  (let ((tmp (uiop:with-temporary-file (:pathname pn :type "zip" :keep t)
               (write-zip-file pn '(("x.txt" "ok")))
               pn)))
    (unwind-protect
         (let* ((path (cl-stack-pathlib::%file-uri-path tmp))
                (uri (format nil "zip:file://~A!/x.txt" path)))
           (ok (string= (read-text uri) "ok")))
      (ignore-errors (delete-file tmp))
      (clear-zip-filesystem-cache))))

(deftest zip-glob
  (let ((fs (make-zip-filesystem
             "<glob>"
             :bytes (write-zip-bytes
                     '(("exchanges/XNYS.sexp" "ny")
                       ("exchanges/XLON.sexp" "ln")
                       ("readme.txt" "x")))
             :cache nil)))
    (with-filesystem (fs)
      (let ((hits (mapcar #'as-posix (glob "/exchanges" "*.sexp"))))
        (ok (= 2 (length hits)))
        (ok (every (lambda (s) (search ".sexp" s)) hits))))))

(deftest local-glob-star-extension
  "SBCL DIRECTORY needs :WILD, not a literal * name."
  (uiop:with-temporary-file (:pathname tmp :keep t)
    (delete-file tmp)
    (let ((dir (uiop:ensure-directory-pathname tmp)))
      (unwind-protect
           (progn
             (ensure-directories-exist dir)
             (write-text (merge-pathnames "a.sexp" dir) "a")
             (write-text (merge-pathnames "b.sexp" dir) "b")
             (write-text (merge-pathnames "c.txt" dir) "c")
             (let ((hits (mapcar #'name (glob dir "*.sexp"))))
               (ok (= 2 (length hits)))
               (ok (find "a.sexp" hits :test #'string=))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest zip-tree-roundtrip
  (uiop:with-temporary-file (:pathname dir :keep t)
    (delete-file dir)
    (let* ((root (uiop:ensure-directory-pathname dir))
           (zip (merge-pathnames "tree.zip" root)))
      (unwind-protect
           (progn
             (ensure-directories-exist (merge-pathnames "sub/" root))
             (write-text (merge-pathnames "sub/a.txt" root) "alpha")
             (write-text (merge-pathnames "b.txt" root) "beta")
             (zip-tree root zip)
             (ok (string= (read-text (zip-path zip "sub/a.txt")) "alpha"))
             (ok (string= (read-text (as-uri (zip-path zip "b.txt"))) "beta")))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
        (clear-zip-filesystem-cache)))))
