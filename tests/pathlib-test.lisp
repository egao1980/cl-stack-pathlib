(defpackage #:cl-stack-pathlib/tests
  (:use #:cl #:rove #:cl-stack-pathlib))
(in-package #:cl-stack-pathlib/tests)

(deftest join-and-components
  (let ((p (join "/foo" "bar" "baz.txt")))
    (ok (absolute-p p))
    (ok (string= (name p) "baz.txt"))
    (ok (string= (base p) "baz"))
    (ok (string= (extension p) "txt"))
    (ok (string= (suffix p) ".txt"))
    (ok (equal (parts p) '("foo" "bar" "baz.txt")))))

(deftest absolute-vs-resolve-identity
  "absolute must not follow symlinks / change install-root identity."
  (let* ((tmp (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (p (join tmp "no-such-child" "x")))
    (ok (absolute-p (absolute p)))
    ;; resolve strict on missing should error; non-strict returns absolute
    (ok (absolute-p (resolve p :strict nil)))))

(deftest memory-filesystem-roundtrip
  (let ((fs (make-memory-filesystem)))
    (with-filesystem (fs)
      (mkdir "/data" :parents t)
      (write-text "/data/hello.txt" "hi")
      (ok (exists-p "/data/hello.txt"))
      (ok (file-p "/data/hello.txt"))
      (ok (directory-p "/data"))
      (ok (string= (read-text "/data/hello.txt") "hi"))
      (ok (string= (as-uri "/data/hello.txt") "memory:///data/hello.txt"))
      (copy-path "/data/hello.txt" "/data/hello2.txt")
      (ok (exists-p "/data/hello2.txt"))
      (create-symlink "/data/link" "/data/hello.txt")
      (ok (symlink-p "/data/link"))
      (ok (string= (as-posix (read-symlink "/data/link")) "/data/hello.txt")))))

(deftest under-preserves-root
  (let ((root (path "/opt/pkg/" :directory t)))
    (ok (string= (as-posix (under root "native/lib.so"))
                 "/opt/pkg/native/lib.so"))))

(deftest with-filesystem-isolates-default
  (let ((mem (make-memory-filesystem))
        (local *filesystem*))
    (with-filesystem (mem)
      (ok (typep *filesystem* 'memory-filesystem))
      (mkdir "/only-mem")
      (ok (exists-p "/only-mem")))
    (ok (eq *filesystem* local))))
