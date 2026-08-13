;;;; Phase 2: load + test.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(setf asdf:*compile-file-failure-behaviour* :warn)

(defun call-with-ci-muffles (fn)
  #+sbcl
  (handler-bind ((sb-ext:defconstant-uneql
                  (lambda (c)
                    (let ((r (find-restart 'continue c)))
                      (when r (invoke-restart r))))))
    (funcall fn))
  #-sbcl
  (funcall fn))

(call-with-ci-muffles (lambda () (asdf:load-system "cl-repository-client")))
(cl-repository-client/asdf-integration:configure-asdf-source-registry)
(call-with-ci-muffles
 (lambda ()
   (cl-repository-client/asdf-integration:load-system-init-files)))

(call-with-ci-muffles
 (lambda ()
   (dolist (n '("rove"))
     (unless (asdf:find-system n nil)
       (format t "~&; ci: ql fallback ~a~%" n)
       (ql:quickload n :silent t)))
   (asdf:load-system "cl-stack-pathlib")
   (asdf:test-system "cl-stack-pathlib")))

(format t "~&; ci: tests ok~%")
(uiop:quit 0)
