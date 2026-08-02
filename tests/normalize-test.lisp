(in-package #:cl-stack-pathlib/tests)

;;; absolute / resolve / normpath / relative_to (pathlib Path tests).

(deftest absolute-does-not-require-existence
  (let ((p (join (uiop:temporary-directory) "missing" "child")))
    (ok (absolute-p (absolute p)))
    (ok (search "missing" (as-posix (absolute p))))))

(deftest absolute-vs-resolve
  ;; absolute ≠ resolve: absolute never requires the leaf; resolve may.
  (with-memory-fs ()
    (mkdir "/tmp" :parents t)
    (write-text "/tmp/f.txt" "x")
    (ok (posix= (absolute "tmp/f.txt") "/tmp/f.txt"))
    (ok (posix= (resolve "tmp/f.txt" :strict t) "/tmp/f.txt"))
    (ok (signals (resolve "tmp/missing" :strict t) 'path-not-found))
    (ok (posix= (resolve "tmp/missing" :strict nil) "/tmp/missing"))))

(deftest resolve-strict-missing
  (let ((p (join (uiop:temporary-directory) (format nil "nope-~A" (random 1e9)))))
    (ok (signals (resolve p :strict t) 'path-not-found))
    (ok (absolute-p (resolve p :strict nil)))))

(deftest normpath-dot-dot
  (ok (posix= (normpath "/a/b/../c") "/a/c"))
  (ok (posix= (normpath "/a/./b") "/a/b"))
  (ok (posix= (normpath "a/b/../c") "a/c"))
  (ok (posix= (normpath "/../a") "/a"))
  (ok (posix= (normpath "/") "/"))
  (ok (posix= (normpath "/a/b/../../c") "/c"))
  (ok (posix= (normpath "a/../b/./c") "b/c"))
  ;; join keeps ..; normpath collapses.
  (ok (posix= (normpath (join "/a/b" "../c")) "/a/c")))

(deftest relative-to-happy-and-error
  (let ((p (path "/a/b/c.txt"))
        (base (path "/a/b/" :directory t)))
    (ok (posix= (relative-to p base) "c.txt"))
    (ok (relative-to-p p base))
    (ng (relative-to-p p "/x/y"))
    (ok (signals (relative-to p "/x/y") 'not-relative-error)))
  (ok (posix= (relative-to "/a/b" "/") "a/b"))
  (ok (posix= (relative-to "/a/b" "/a") "b"))
  (ok (posix= (relative-to "a/b" "a") "b"))
  (ok (signals (relative-to "/a/b" "/a/b/c") 'not-relative-error))
  (ok (signals (relative-to "/a/b" "a") 'not-relative-error)))

(deftest expanduser-memory-home
  (with-memory-fs ()
    (ok (posix= (expanduser "~/docs/x") "/home/docs/x"))
    (ok (posix= (expanduser "/abs") "/abs"))
    (ok (posix= (expanduser "~") "/home"))))

(deftest path-equal-and-same
  (with-memory-fs ()
    (write-text "/f.txt" "x")
    (ok (path-equal "/f.txt" (path "/f.txt")))
    (ok (same-p "/f.txt" "/f.txt"))
    (ng (same-p "/f.txt" "/g.txt"))))
