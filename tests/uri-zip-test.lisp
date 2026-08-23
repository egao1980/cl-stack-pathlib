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
  (let* ((tmp (uiop:with-temporary-file (:pathname pn :type "zip" :keep t)
                (write-zip-file pn '(("a/b.txt" "hello")
                                     ("a/c.txt" "world")))
                pn))
         (abs (uiop:unix-namestring (uiop:ensure-absolute-pathname tmp (uiop:getcwd))))
         (uri (format nil "zip://~A!/a/b.txt" abs)))
    (unwind-protect
         (let ((p (ensure-path uri)))
           (ok (zip-filesystem-p (path-filesystem p)))
           (ok (string= (read-text p) "hello"))
           (ok (string= (read-text (join (parent p) "c.txt")) "world"))
           (ok (uiop:string-prefix-p "zip://" (as-uri p)))
           (ok (search "!/a/b.txt" (as-uri p)))
           (let ((root (zip-path tmp)))
             (ok (directory-p root))
             (ok (string= (read-text (join root "a/b.txt")) "hello"))))
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
             (ok (string= (read-text (format nil "zip://~A!/b.txt"
                                             (uiop:unix-namestring zip)))
                          "beta")))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
        (clear-zip-filesystem-cache)))))
