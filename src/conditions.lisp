(in-package #:cl-stack-pathlib)

(define-condition path-error (error)
  ((path :initarg :path :reader path-error-path :initform nil)
   (filesystem :initarg :filesystem :reader path-error-filesystem :initform nil)
   (message :initarg :message :reader path-error-message :initform nil))
  (:report (lambda (c s)
             (format s "~@[~A~%~]path=~S fs=~S"
                     (path-error-message c)
                     (path-error-path c)
                     (path-error-filesystem c)))))

(define-condition path-not-found (path-error) ())
(define-condition path-exists-error (path-error) ())
(define-condition not-relative-error (path-error) ())
(define-condition unsupported-operation (path-error) ())

(defun %unsupported (fs op path)
  (error 'unsupported-operation
         :filesystem fs :path path
         :message (format nil "~A does not support ~A" fs op)))
