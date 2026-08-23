(in-package #:cl-stack-pathlib/tests)

;;; Structural / pure-path cases (inspired by pathlib PurePath tests).

(deftest join-empty-and-multi
  (ok (posix= (join "/a" "b" "c") "/a/b/c"))
  (ok (posix= (join "a" "b") "a/b"))
  (ok (posix= (joinpath "/x" "y.txt") "/x/y.txt"))
  (ok (posix= (join "/a/b" "../c") "/a/b/../c"))
  (ok (posix= (join "/a/b" "./c") "/a/b/c"))
  (ok (posix= (join (path "a/b") (path "c")) "a/b/c")))

(deftest join-absolute-replaces
  ;; pathlib: joining an absolute segment discards the base.
  (ok (posix= (join "/a/b" "/c") "/c"))
  (ok (posix= (join "rel" "/abs/x") "/abs/x"))
  (ok (posix= (join "/a" "/b" "c") "/b/c")))

(deftest join-equivalences
  ;; CPython PurePathTest.equivalences (posix flavour), adapted.
  (dolist (args '(("a" "b") ("a/" "b") ("a" "b/") ("a/" "b/")
                  ("a/b/") ("a//b") ("a//b//")
                  ("" "a" "b") ("a" "" "b") ("a" "b" "")))
    (ok (posix= (apply #'join args) "a/b")
        (format nil "join~S → a/b" args)))
  (dolist (args '(("a" "/b/c" "d") ("/a" "/b/c" "d")
                  ("/" "b" "" "c/d") ("/" "" "b/c/d") ("" "/b/c/d")))
    (ok (posix= (apply #'join args) "/b/c/d")
        (format nil "join~S → /b/c/d" args))))

(deftest from-list-and-components
  (let ((p (from-list '("foo" "bar" "baz.txt") :absolute t)))
    (ok (absolute-p p))
    (ok (equal (parts p) '("foo" "bar" "baz.txt")))
    (ok (equal (components p) '("/" "foo" "bar" "baz.txt"))))
  (let ((p (from-list '("rel" "x"))))
    (ok (relative-p p))
    (ok (equal (parts p) '("rel" "x"))))
  (let ((p (path "/a/b/../c")))
    (ok (equal (parts p) '("a" "b" ".." "c")))
    (ok (equal (components p) '("/" "a" "b" ".." "c")))))

(deftest name-stem-suffix-suffixes
  (let ((p (path "/dir/archive.tar.gz")))
    (ok (string= (name p) "archive.tar.gz"))
    (ok (string= (base p) "archive.tar"))
    (ok (string= (stem p) "archive.tar"))
    (ok (string= (extension p) "gz"))
    (ok (string= (suffix p) ".gz"))
    (ok (equal (suffixes p) '(".tar" ".gz"))))
  (let ((p (path "/dir/README")))
    (ok (string= (name p) "README"))
    (ok (string= (suffix p) ""))
    (ok (null (suffixes p))))
  (let ((p (path "/dir/.gitignore")))
    (ok (string= (name p) ".gitignore"))))

(deftest with-name-stem-suffix
  (let ((p (path "/a/b/c.txt")))
    (ok (posix= (with-name p "d.md") "/a/b/d.md"))
    (ok (posix= (with-base p "z") "/a/b/z.txt"))
    (ok (posix= (with-stem p "z") "/a/b/z.txt"))
    (ok (posix= (with-extension p "lisp") "/a/b/c.lisp"))
    (ok (posix= (with-suffix p ".lisp") "/a/b/c.lisp"))
    (ok (posix= (add-extension p "bak") "/a/b/c.txt.bak"))
    (ok (posix= (drop-extension p) "/a/b/c"))))

(deftest parent-and-parents
  (let ((p (path "/a/b/c.txt")))
    (ok (posix= (parent p) "/a/b"))
    (ok (posix= (with-parent p "/x/y") "/x/y/c.txt"))
    (let ((ps (mapcar #'as-posix (parents p))))
      (ok (equal ps '("/a/b" "/a" "/")))))
  (ok (posix= (parent "/") "/"))
  (ok (posix= (parent "/a") "/"))
  (ok (posix= (parent "a/b") "a"))
  (ok (posix= (parent "a") ".")))

(deftest root-empty-absolute-relative
  (ok (root-p (path "/" :directory t)))
  (ok (absolute-p "/abs/x"))
  (ok (relative-p "rel/x"))
  (ok (directory-pathname-p (path "/tmp/" :directory t)))
  (ng (directory-pathname-p (path "/tmp/file")))
  (ok (absolute-p (path "/")))
  (ng (absolute-p (path "a/b")))
  (ok (relative-p (path "a/b"))))

(deftest starts-with-ends-with
  (let ((p (path "/foo/bar/baz/zing.json")))
    (ok (starts-with-p p "/foo/bar"))
    (ok (ends-with-p p "baz/zing.json"))
    (ng (starts-with-p p "/foo/quux"))
    (ng (ends-with-p p "zing.txt")))
  (ok (starts-with-p "/a/b/c" "/a"))
  (ok (ends-with-p "x/y/z.txt" "z.txt")))

(deftest match-pattern
  ;; pathlib match: pattern against final name or full path.
  (ok (match-p "/a/b/c.txt" "*.txt"))
  (ok (match-p "/a/b/c.txt" "c.txt"))
  (ok (match-p "b.py" "b.py"))
  (ok (match-p "a/b.py" "b.py"))
  (ok (match-p "/a/b.py" "*.py"))
  (ng (match-p "/a/b/c.txt" "*.lisp"))
  (ng (match-p "a.py" "b.py"))
  (ng (match-p "b.py/c" "b.py")))

(deftest as-posix-and-string
  (let ((p (path "/foo/bar")))
    (ok (string= (as-posix p) (to-string p)))
    (ok (string= (ensure-string p) (as-posix p)))
    (ok (posix= (from-string "/foo/bar") "/foo/bar")))
  (ok (string= (as-posix "/abs") "/abs"))
  (ok (string= (as-posix "/") "/"))
  (ok (string= (as-posix "rel/x") "rel/x"))
  (ok (string= (as-posix "/a/b/../c") "/a/b/../c")))

(deftest windows-drive-posix-roundtrip
  "RFC 8089: as-posix keeps the drive so join does not hop to another volume."
  (dolist (s '("/C:/Users/foo/data.zip" "/C:/app/data/countries/DE.sexp"))
    (let ((pn (cl-stack-pathlib::%posix-string-to-pathname s)))
      (ok (string-equal (string (pathname-device pn)) "C") s)
      (ok (string= (cl-stack-pathlib::%pathname-as-posix pn) s) s)))
  (ok (string= (as-posix (join (path "/C:/tmp/data/" :directory t) "countries/ZZ.sexp"))
               "/C:/tmp/data/countries/ZZ.sexp")))

(deftest under-join
  (ok (posix= (under "/opt/pkg/" "native/lib.so") "/opt/pkg/native/lib.so"))
  (ok (posix= (under (path "/opt/pkg/" :directory t) "x") "/opt/pkg/x")))

(deftest path-equal-components
  (ok (path-equal (path "/a/b") (join "/a" "b")))
  (ok (path-equal (path "a//b") (path "a/b")))
  (ng (path-equal "/a/b" "/a/c")))
