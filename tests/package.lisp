(defpackage #:cl-stack-pathlib/tests
  (:use #:cl #:rove #:cl-stack-pathlib)
  (:export #:with-memory-fs))
(in-package #:cl-stack-pathlib/tests)

(defmacro with-memory-fs ((&optional (fs-var (gensym "FS"))) &body body)
  `(let ((,fs-var (make-memory-filesystem)))
     (with-filesystem (,fs-var)
       ,@body)))

(defun posix= (designator expected)
  (string= (as-posix designator) expected))
