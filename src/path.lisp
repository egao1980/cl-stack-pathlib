(in-package #:cl-stack-pathlib)

;;; PATH — value bound to a filesystem (java.nio.file.Path).
;;; Identity inside the FS is a CL pathname (portable structure, not always OS).

(defclass path ()
  ((filesystem :initarg :filesystem :reader path-filesystem :type filesystem)
   (pathname :initarg :pathname :reader path-pathname :type pathname)))

(defun path-p (x) (typep x 'path))

(defun make-path (pathname &key (filesystem *filesystem*))
  (check-type filesystem filesystem)
  (make-instance 'path
                 :filesystem filesystem
                 :pathname (uiop:ensure-pathname pathname :want-pathname t)))

(defmethod print-object ((p path) stream)
  (let ((s (ignore-errors (uiop:unix-namestring (path-pathname p)))))
    (if *print-readably*
        (format stream "#.~S"
                `(make-path ,(path-pathname p)
                            :filesystem ,(filesystem-name (path-filesystem p))))
        (print-unreadable-object (p stream :type t)
          (format stream "~A:~A"
                  (filesystem-name (path-filesystem p))
                  (or s (namestring (path-pathname p))))))))

(defun %current-fs ()
  (or *filesystem*
      (error 'path-error :message "*filesystem* is unbound; load local backend or bind with-filesystem")))

(defun ensure-path (designator &key (filesystem nil filesystem-p) directory)
  "Coerce string / pathname / path → PATH on FILESYSTEM (default *filesystem*)."
  (cond
    ((path-p designator)
     (when (and filesystem-p (not (eq filesystem (path-filesystem designator))))
       (error 'path-error :path designator :filesystem filesystem
              :message "path belongs to a different filesystem"))
     (if directory
         (make-path (uiop:ensure-directory-pathname (path-pathname designator))
                    :filesystem (path-filesystem designator))
         designator))
    (t
     (let* ((fs (if filesystem-p filesystem (%current-fs)))
            (pn (fs-parse fs designator :directory directory)))
       (make-path pn :filesystem fs)))))

;;; Structural helpers on pathnames (FS-agnostic; used by all backends)

(defun %pathname-root-p (pn)
  (let ((dir (pathname-directory pn)))
    (and (consp dir) (eq (first dir) :absolute) (null (rest dir))
         (null (pathname-name pn)) (null (pathname-type pn)))))

(defun %pathname-empty-p (pn)
  (and (null (pathname-directory pn))
       (or (null (pathname-name pn)) (equal (pathname-name pn) ""))
       (or (null (pathname-type pn)) (eq (pathname-type pn) :unspecific))))

(defun %file-namestring* (pn)
  (let ((n (file-namestring pn)))
    (if (and n (plusp (length n)))
        n
        (let ((dir (pathname-directory pn)))
          (when (consp dir)
            (let ((last (car (last dir))))
              (when (stringp last) last)))))))

(defun %collapse-dot-segments (pathname)
  "Pure string collapse of `.` / `..` for unix-namestring PATHNAME."
  (let* ((abs? (uiop:absolute-pathname-p pathname))
         (str (or (uiop:unix-namestring pathname) (namestring pathname) ""))
         (raw (uiop:split-string str :separator "/"))
         (stack '()))
    (dolist (p raw)
      (cond ((or (string= p "") (string= p ".")) nil)
            ((string= p "..")
             (cond ((null stack) (unless abs? (push p stack)))
                   (t (pop stack))))
            (t (push p stack))))
    (setf stack (nreverse stack))
    (values abs? stack (uiop:directory-pathname-p pathname))))
