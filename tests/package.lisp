(defpackage #:cl-stack-pathlib/tests
  (:use #:cl #:rove #:cl-stack-pathlib)
  (:import-from #:cl-stack-pathlib/journal-suite #:run-journal-suite #:journal-suite-steps)
  (:import-from #:cl-stack-pathlib/clone-suite #:run-clone-suite #:clone-suite-steps
                #:clone-suite-edits)
  (:export #:with-memory-fs))
(in-package #:cl-stack-pathlib/tests)

(defmacro with-memory-fs ((&optional (fs-var (gensym "FS"))) &body body)
  `(let ((,fs-var (make-memory-filesystem)))
     (with-filesystem (,fs-var)
       ,@body)))

(defun posix= (designator expected)
  (string= (as-posix designator) expected))
