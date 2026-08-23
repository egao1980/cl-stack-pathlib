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
  "Coerce string / pathname / path → PATH on FILESYSTEM (default *filesystem*).
   Strings with a registered URI scheme (`file:`, `zip:`, …) are dispatched
   unless :FILESYSTEM is supplied explicitly."
  (cond
    ((path-p designator)
     (when (and filesystem-p (not (eq filesystem (path-filesystem designator))))
       (error 'path-error :path designator :filesystem filesystem
              :message "path belongs to a different filesystem"))
     (if directory
         (make-path (uiop:ensure-directory-pathname (path-pathname designator))
                    :filesystem (path-filesystem designator))
         designator))
    ((and (not filesystem-p)
          (stringp designator)
          (let ((scheme (uri-scheme designator)))
            (and scheme (uri-scheme-handler scheme))))
     (let ((p (funcall (uri-scheme-handler (uri-scheme designator)) designator)))
       (unless (path-p p)
         (error 'path-error :path designator
                :message (format nil "URI handler for ~A did not return a path"
                                 (uri-scheme designator))))
       (if directory
           (make-path (uiop:ensure-directory-pathname (path-pathname p))
                      :filesystem (path-filesystem p))
           p)))
    ((and (not filesystem-p)
          (stringp designator)
          (uri-scheme designator))
     (error 'path-error :path designator
            :message (format nil "unknown URI scheme ~S (known: ~{~A~^, ~})"
                             (uri-scheme designator) (list-uri-schemes))))
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

(defun %logical-parent (pn)
  "Pathlib .parent: containing directory of a file, or parent of a directory."
  (let ((pn (uiop:ensure-pathname pn :want-pathname t)))
    (if (or (uiop:directory-pathname-p pn)
            (null (pathname-name pn)))
        (uiop:pathname-parent-directory-pathname pn)
        (uiop:pathname-directory-pathname pn))))

(defun %drive-letter-component-p (part)
  (and (stringp part)
       (= (length part) 2)
       (alpha-char-p (char part 0))
       (char= (char part 1) #\:)))

(defun %pathname-drive-letter (pn)
  (let ((d (pathname-device pn)))
    (cond ((or (null d) (eq d :unspecific)) nil)
          ((characterp d) (char-upcase d))
          ((and (stringp d) (plusp (length d))) (char-upcase (char d 0)))
          (t nil))))

(defun %posix-string-to-pathname (string &key directory)
  "Parse unix STRING keeping `..` as the string component \"..\".
SBCL collapses `:BACK` in MAKE-PATHNAME; a string \"..\" is preserved.
RFC 8089 drive: `/C:/Users/foo` and `C:/Users/foo` → device C."
  (let* ((s (substitute #\/ #\\ (or string "")))
         (abs (and (plusp (length s)) (char= (char s 0) #\/)))
         (parts (remove "" (uiop:split-string s :separator "/") :test #'string=))
         (device nil)
         (dir-comps '())
         (name nil)
         (type nil)
         (trailing-dir (or directory
                           (and (plusp (length s))
                                (char= (char s (1- (length s))) #\/)))))
    (when (and parts (%drive-letter-component-p (first parts)))
      (setf device (string-upcase (subseq (first parts) 0 1))
            parts (rest parts)
            abs t))
    (let* ((file (unless trailing-dir (car (last parts))))
           (dirs (if trailing-dir parts (butlast parts))))
      (dolist (p dirs)
        (cond ((string= p ".") nil)
              ((string= p "..") (push ".." dir-comps))
              (t (push p dir-comps))))
      (when file
        (cond ((string= file ".") nil)
              ((string= file "..") (push ".." dir-comps))
              ((and (find #\. file) (plusp (position #\. file :from-end t)))
               (let ((dot (position #\. file :from-end t)))
                 (setf name (subseq file 0 dot)
                       type (subseq file (1+ dot)))))
              (t (setf name file))))
      (make-pathname :host nil
                     :device (or device :unspecific)
                     :directory (when (or abs dir-comps)
                                  (cons (if abs :absolute :relative)
                                        (nreverse dir-comps)))
                     :name name :type type :version nil))))

(defun %pathname-as-posix (pn)
  "Unix namestring that renders `:BACK`/`:UP`/\"..\" as `..`.
Trailing slash only for the root directory (pathlib as_posix style).
Windows drive is RFC 8089 path-absolute: `/C:/Users/foo` (not unix-namestring)."
  (let* ((dir (pathname-directory pn))
         (abs (and (consp dir) (eq (first dir) :absolute)))
         (drive (%pathname-drive-letter pn))
         (comps (when (consp dir)
                  (loop for c in (rest dir)
                        collect (cond ((member c '(:back :up)) "..")
                                      ((and (stringp c) (string= c "..")) "..")
                                      ((stringp c) c)
                                      (t (princ-to-string c))))))
         (file (let ((n (pathname-name pn))
                     (ty (pathname-type pn)))
                 (cond ((and n ty (not (eq ty :unspecific)))
                        (format nil "~A.~A" n ty))
                       ((and n (plusp (length (string n)))) (string n))
                       (t nil))))
         (body (format nil "~{~A~^/~}" comps))
         (s (cond ((and abs (null comps) (null file)) "/")
                  ((and abs (null comps) file) (format nil "/~A" file))
                  ((and abs file) (format nil "/~A/~A" body file))
                  ((and abs (null file)) (format nil "/~A" body))
                  ((and (null comps) file) file)
                  ((null file) (if (plusp (length body)) body "."))
                  (t (format nil "~A/~A" body file)))))
    (if drive
        (if (and (plusp (length s)) (char= (char s 0) #\/))
            (format nil "/~A:~A" drive s)
            (format nil "/~A:/~A" drive s))
        s)))

(defun %parse-posix (string &key directory)
  "Parse STRING as a unix path without collapsing `..`."
  (let ((pn (%posix-string-to-pathname string :directory directory)))
    (if directory (uiop:ensure-directory-pathname pn) pn)))

(defun %wild-pattern (pattern)
  "Convert a glob PATTERN (e.g. \"*.txt\") to a pathname for pathname-match-p."
  (cond ((string= pattern "*") (make-pathname :name :wild :type :wild))
        ((and (>= (length pattern) 2)
              (char= (char pattern 0) #\*)
              (char= (char pattern 1) #\.))
         (make-pathname :name :wild :type (subseq pattern 2)))
        ((find #\* pattern)
         (uiop:parse-unix-namestring pattern :wilden t))
        (t (uiop:parse-unix-namestring pattern))))

(defun %name-matches-p (name pattern)
  (or (string= name pattern)
      (pathname-match-p name (%wild-pattern pattern))
      (pathname-match-p (pathname name) (%wild-pattern pattern))))

(defun %collapse-dot-segments (pathname)
  "Pure collapse of `.` / `..`. Relative leading `..` is kept (pathlib / Boost)."
  (let* ((str (%pathname-as-posix (uiop:ensure-pathname pathname :want-pathname t)))
         (abs? (and (plusp (length str)) (char= (char str 0) #\/)))
         (raw (uiop:split-string str :separator "/"))
         (stack '()))
    (dolist (p raw)
      (cond ((or (string= p "") (string= p ".")) nil)
            ((string= p "..")
             (cond ((null stack)
                    (unless abs? (push p stack)))
                   ((string= (car stack) "..")
                    (push p stack))
                   (t (pop stack))))
            (t (push p stack))))
    (setf stack (nreverse stack))
    (values abs? stack (uiop:directory-pathname-p pathname))))
