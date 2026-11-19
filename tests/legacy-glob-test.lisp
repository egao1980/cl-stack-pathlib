(in-package #:cl-stack-pathlib/tests)

;;; A third-party backend written against 0.5 that specializes FS-GLOB itself
;;; (as revision-backend-jj does) and matches names with the internal helpers
;;; %NAME-MATCHES-P / %WILD-PATTERN. 0.6.0 deleted them, so its RGLOB failed
;;; with UNDEFINED-FUNCTION. The tree mirrors that backend's test fixture.

(defclass legacy-glob-filesystem (memory-filesystem) ())

(defmethod fs-glob ((fs legacy-glob-filesystem) pathname pattern &key recursive ignore)
  (declare (ignore ignore))
  ;; The shape of revision-backend-jj 0.2.0's method, over memory's table.
  (let* ((base (string-right-trim "/" (as-posix (make-path pathname :filesystem fs))))
         (prefix (concatenate 'string base "/"))
         (out '()))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (when (and (uiop:string-prefix-p prefix k) (string/= k base))
         (let ((rel (subseq k (length prefix)))
               (nm (file-namestring k)))
           (when (and (plusp (length rel))
                      (or recursive (find #\/ pattern) (not (find #\/ rel)))
                      (if (find #\/ pattern)
                          (pathname-match-p rel (cl-stack-pathlib::%wild-pattern pattern))
                          (and nm (cl-stack-pathlib::%name-matches-p nm pattern))))
             (push k out)))))
     (cl-stack-pathlib::memory-fs-root fs))
    (mapcar (lambda (k) (fs-parse fs k)) (sort out #'string<))))

(defun call-with-revision-like-tree (fs fn)
  (with-filesystem (fs)
    (make-tree (ensure-path "/" :directory t)
               '(("src/main.lisp" "(main)") ("README" "readme") ("run.sh" "#!/bin/sh")
                 ("bin/data.bin" "123456789") ("new/name.txt" "1") ("thing/inner.txt" "2")))
    (create-symlink "/link" "/src/main.lisp")
    (funcall fn)))

(deftest legacy-backend-glob-helpers
  (call-with-revision-like-tree
   (make-instance 'legacy-glob-filesystem)
   (lambda ()
     (ok (equal '("/src/main.lisp") (mapcar #'as-posix (rglob "/" "*.lisp")))
         "a backend's own fs-glob calling %name-matches-p")
     (ok (equal '("/README") (mapcar #'as-posix (glob "/" "README"))))
     (ok (equal '("/new/name.txt" "/thing/inner.txt") (mapcar #'as-posix (rglob "/" "*.txt"))))
     (ok (equal '("/src/main.lisp") (mapcar #'as-posix (glob "/" "src/*.lisp")))
         "%wild-pattern for a pattern with a slash"))))

(deftest generic-glob-on-revision-like-tree
  (call-with-revision-like-tree
   (make-memory-filesystem)
   (lambda ()
     (ok (equal '("/src/main.lisp") (mapcar #'as-posix (rglob "/" "*.lisp")))
         "the link /link is not a match, the file is listed once")
     (ok (equal '("/link") (mapcar #'as-posix (rglob "/" "link"))))
     (ok (equal '("/src/main.lisp") (mapcar #'as-posix (glob "/" "**/*.lisp" :recursive t)))))))
