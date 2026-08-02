(in-package #:cl-stack-pathlib/tests)

;;; Cases inspired by Boost.Filesystem path_test.cpp (BSL-1.0).
;;; Original CL/Rove — not a copy of Boost sources.
;;;
;;; Semantic notes vs Boost (POSIX):
;;; - JOIN follows pathlib: an absolute segment replaces the base
;;;   (Boost V4 /= does the same; older append differed).
;;; - NORMPATH collapses absolute leading `..` (pathlib); Boost
;;;   lexically_normal keeps `/../f` as `/../f`.
;;; - PARENT of `/` is `/` (pathlib); Boost parent_path("/") is "".

(deftest boost-append-preserves-dot-dot
  ;; path_test.cpp POSIX append block (foo/../bar style).
  (ok (posix= (join ".." "..") "../.."))
  (ok (posix= (join "/" "..") "/.."))
  (ok (posix= (join "/.." "..") "/../.."))
  (ok (posix= (join ".." "foo") "../foo"))
  (ok (posix= (join "foo" "..") "foo/.."))
  (ok (posix= (join "foo" ".." "bar") "foo/../bar"))
  (ok (posix= (join "foo" "bar" "..") "foo/bar/.."))
  (ok (posix= (join "foo" "bar" ".." "blah") "foo/bar/../blah"))
  (ok (posix= (join "f" "b" ".." "a") "f/b/../a"))
  (ok (posix= (join "foo" (name (path "woo/bar"))) "foo/bar")))

(deftest boost-append-absolute-replaces
  ;; pathlib / Boost V4: absolute RHS replaces.
  (ok (posix= (join "/foo" "/bar") "/bar"))
  (ok (posix= (join "/" "/foo") "/foo")))

(deftest boost-decomposition-parent-name
  (ok (posix= (parent "/foo/bar.txt") "/foo"))
  (ok (posix= (parent "/foo/bar") "/foo"))
  (ok (posix= (parent "foo/bar") "foo"))
  (ok (posix= (parent "../foo") ".."))
  (ok (posix= (parent "/foo/bar/baz") "/foo/bar"))
  (ok (string= (name "/foo/bar.txt") "bar.txt"))
  (ok (string= (name "/foo/bar") "bar"))
  (ok (string= (name "foo/bar") "bar"))
  (ok (string= (name "../foo") "foo")))

(deftest boost-stem-extension
  ;; stem()/extension() vectors from path_test.cpp (generic form).
  (ok (string= (stem "a/b.txt") "b"))
  (ok (string= (suffix "a/b.txt") ".txt"))
  (ok (string= (stem "a.b.c") "a.b"))
  (ok (string= (suffix "a.b.c") ".c"))
  (ok (string= (stem "b") "b"))
  (ok (string= (suffix "a/b") ""))
  (ok (string= (stem "a.b/c") "c"))
  (ok (string= (suffix "a.b/c") ""))
  (ok (posix= (with-suffix "a/b.txt" ".md") "a/b.md"))
  (ok (posix= (with-suffix "a/b.txt" "md") "a/b.md"))
  (ok (posix= (with-stem "a/b.txt" "c") "a/c.txt")))

(deftest boost-relative
  (ok (posix= (relative-to "/abc/def" "/abc") "def"))
  (ok (posix= (relative-to "abc/def" "abc") "def"))
  (ok (posix= (relative-to "/abc/xyz/def" "/abc") "xyz/def"))
  (ok (posix= (relative-to "abc/xyz/def" "abc") "xyz/def")))

(deftest boost-lexically-normal-via-normpath
  ;; lexically_normal ≈ NORMPATH (pathlib collapse of abs `..`).
  (ok (posix= (normpath "/") "/"))
  (ok (posix= (normpath "foo") "foo"))
  (ok (posix= (normpath "/foo") "/foo"))
  (ok (posix= (normpath "/./foo") "/foo"))
  (ok (posix= (normpath "foo/bar") "foo/bar"))
  (ok (posix= (normpath "..") ".."))
  (ok (posix= (normpath "../..") "../.."))
  (ok (posix= (normpath "../foo") "../foo"))
  (ok (posix= (normpath "foo/..") "."))
  (ok (posix= (normpath "foo/../bar") "bar"))
  (ok (posix= (normpath "foo/../..") ".."))
  (ok (posix= (normpath "foo/bar/../blah") "foo/blah"))
  (ok (posix= (normpath (join "foo" ".." "bar")) "bar"))
  ;; pathlib (not Boost): `/../f` → `/f`
  (ok (posix= (normpath "/../f") "/f")))

(deftest boost-absolute-relative-queries
  (ok (absolute-p "/foo"))
  (ok (absolute-p "/"))
  (ng (absolute-p "foo/bar"))
  (ng (absolute-p "../foo"))
  (ok (relative-p "foo/bar"))
  (ok (relative-p ".."))
  (ng (relative-p "/foo")))
