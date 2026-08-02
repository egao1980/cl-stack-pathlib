(in-package #:cl-stack-pathlib)

;;; Conditions + interactive/programmatic restarts (CL style, not status codes).
;;;
;;; Typical recoveries:
;;;   create-parents  — mkdir -p missing parents, then retry
;;;   create-file     — touch empty file, then retry (reads / open)
;;;   create-directory— mkdir the missing path, then retry
;;;   overwrite       — treat path-exists as replace/exist-ok, then retry
;;;   ignore-missing  — treat path-not-found as success (unlink-style)
;;;   retry           — re-run the enclosing path operation
;;;   use-value       — supply an alternate return value (CL standard name)

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
(define-condition missing-parent (path-not-found) ()
  (:documentation "Parent directory of PATH does not exist."))
(define-condition path-exists-error (path-error) ())
(define-condition not-relative-error (path-error) ())
(define-condition unsupported-operation (path-error) ())
(define-condition directory-not-empty (path-error) ())

(defun %unsupported (fs op path)
  (error 'unsupported-operation
         :filesystem fs :path path
         :message (format nil "~A does not support ~A" fs op)))

;;; --- restart helpers -------------------------------------------------------

(defun %report-path (stream format-control &rest args)
  (apply #'format stream format-control args))

(defun call-with-path-restarts (thunk)
  "Establish RETRY around THUNK. Recovery restarts (CREATE-PARENTS, …)
invoke RETRY after mutating the filesystem."
  (tagbody
   :retry
     (return-from call-with-path-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry the path operation"
           (go :retry))))))

(defmacro with-path-restarts (&body body)
  `(call-with-path-restarts (lambda () ,@body)))

(defun %invoke-retry ()
  (let ((r (find-restart 'retry)))
    (if r
        (invoke-restart r)
        (error "RETRY restart not active; wrap the call in WITH-PATH-RESTARTS"))))

(defun %ensure-parent-dirs (fs path)
  (let* ((pn (if (pathnamep path) path (fs-parse fs path)))
         (parent (fs-parent fs pn)))
    (unless (or (null parent) (%pathname-root-p parent))
      (fs-mkdir fs parent :parents t :exist-ok t))
    parent))

(defun %restart-create-parents (fs path)
  "Signal MISSING-PARENT; CREATE-PARENTS makes parents then RETRY."
  (restart-case
      (error 'missing-parent
             :filesystem fs :path path
             :message "parent directory does not exist")
    (create-parents ()
      :report (lambda (s)
                (%report-path s "Create missing parent directories for ~A and retry" path))
      (%ensure-parent-dirs fs path)
      (%invoke-retry))))

(defun %restart-path-not-found (fs path &key message
                                            (allow-create-file t)
                                            (allow-create-directory t)
                                            (allow-ignore nil))
  "Signal PATH-NOT-FOUND with CREATE-FILE / CREATE-DIRECTORY / IGNORE-MISSING / USE-VALUE."
  (restart-case
      (error 'path-not-found
             :filesystem fs :path path
             :message message)
    (create-file ()
      :report (lambda (s) (%report-path s "Create empty file ~A and retry" path))
      :test (lambda (c) (declare (ignore c)) allow-create-file)
      (%ensure-parent-dirs fs path)
      (fs-touch fs path :exist-ok t)
      (%invoke-retry))
    (create-directory ()
      :report (lambda (s) (%report-path s "Create directory ~A and retry" path))
      :test (lambda (c) (declare (ignore c)) allow-create-directory)
      (fs-mkdir fs path :parents t :exist-ok t)
      (%invoke-retry))
    (ignore-missing ()
      :report (lambda (s) (%report-path s "Ignore missing path ~A" path))
      :test (lambda (c) (declare (ignore c)) allow-ignore)
      nil)
    (use-value (value)
      :report "Use a supplied value instead"
      :interactive (lambda ()
                     (format *query-io* "Value to use: ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      value)))
;;; --- handler sugar ---------------------------------------------------------

(defun invoke-create-parents (&optional condition)
  (let ((r (find-restart 'create-parents condition)))
    (when r (invoke-restart r))))

(defun invoke-create-file (&optional condition)
  (let ((r (find-restart 'create-file condition)))
    (when r (invoke-restart r))))

(defun invoke-create-directory (&optional condition)
  (let ((r (find-restart 'create-directory condition)))
    (when r (invoke-restart r))))

(defun invoke-overwrite (&optional condition)
  (let ((r (find-restart 'overwrite condition)))
    (when r (invoke-restart r))))

(defun invoke-ignore-missing (&optional condition)
  (let ((r (find-restart 'ignore-missing condition)))
    (when r (invoke-restart r))))

(defun invoke-retry (&optional condition)
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun auto-create-parents (condition)
  "HANDLER-BIND function: invoke CREATE-PARENTS when available."
  (if (find-restart 'create-parents condition)
      (invoke-create-parents condition)
      (error condition)))

(defun auto-create-file (condition)
  "HANDLER-BIND function: invoke CREATE-FILE when available."
  (if (find-restart 'create-file condition)
      (invoke-create-file condition)
      (error condition)))

(defun auto-overwrite (condition)
  "HANDLER-BIND function: invoke OVERWRITE when available."
  (if (find-restart 'overwrite condition)
      (invoke-overwrite condition)
      (error condition)))

(defun auto-ignore-missing (condition)
  "HANDLER-BIND function: invoke IGNORE-MISSING when available."
  (if (find-restart 'ignore-missing condition)
      (invoke-ignore-missing condition)
      (error condition)))

(defmacro with-auto-create-parents (&body body)
  "On MISSING-PARENT with CREATE-PARENTS, create parents + retry."
  `(handler-bind ((missing-parent #'auto-create-parents))
     (with-path-restarts ,@body)))

(defmacro with-auto-create-file (&body body)
  `(handler-bind ((path-not-found #'auto-create-file))
     (with-path-restarts ,@body)))

(defmacro with-auto-overwrite (&body body)
  `(handler-bind ((path-exists-error #'auto-overwrite))
     (with-path-restarts ,@body)))

(defmacro with-auto-ignore-missing (&body body)
  `(handler-bind ((path-not-found #'auto-ignore-missing))
     (with-path-restarts ,@body)))
