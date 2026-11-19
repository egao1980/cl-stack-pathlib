(in-package #:cl-stack-pathlib/tests)

;;; Helpers for the H2a suites. Each backend test runs against a fresh empty
;;; directory: a temp directory on the local filesystem (its truename, so
;;; paths returned by walk / grep share the prefix) or /work on a new
;;; memory filesystem. *FILESYSTEM* is bound to that backend.

(defun call-with-backend (kind fn)
  (ecase kind
    (:memory
     (let ((fs (make-memory-filesystem)))
       (with-filesystem (fs)
         (mkdir "/work" :parents t)
         (funcall fn (ensure-path "/work" :directory t)))))
    (:local
     (let ((dir (uiop:ensure-directory-pathname
                 (merge-pathnames (format nil "cl-stack-pathlib-h2a-~36R" (random (expt 36 8)))
                                  (uiop:temporary-directory)))))
       (ensure-directories-exist dir)
       (unwind-protect
            (with-filesystem (cl-stack-pathlib::*local-filesystem*)
              (funcall fn (ensure-path (truename dir) :directory t)))
         (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))))

(defmacro with-backend ((root kind) &body body)
  "Run BODY with ROOT bound to an empty directory PATH on backend KIND."
  `(call-with-backend ,kind (lambda (,root) ,@body)))

(defun octets (&rest items)
  "Octet vector from integers, strings (UTF-8) and octet vectors."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (it items (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase it
        (integer (vector-push-extend it out))
        (string (loop for b across (cl-stack-pathlib::utf8-string-to-octets it)
                      do (vector-push-extend b out)))
        (vector (loop for b across it do (vector-push-extend b out)))))))

(defun make-tree (root entries)
  "ENTRIES: (relpath content) with content a string or octet vector, or
\"dir/\" alone for an empty directory."
  (dolist (e entries root)
    (destructuring-bind (rel &optional (content "")) (if (consp e) e (list e))
      (if (and (plusp (length rel)) (char= (char rel (1- (length rel))) #\/))
          (mkdir (join root rel) :parents t :exist-ok t)
          (let ((p (join root rel)))
            (mkdir (parent p) :parents t :exist-ok t)
            (if (stringp content)
                (write-text p content)
                (write-bytes p content)))))))

(defun rel (path root)
  "POSIX path of PATH relative to ROOT, without a trailing slash."
  (let* ((r (string-right-trim "/" (as-posix root)))
         (s (string-right-trim "/" (as-posix path))))
    (assert (uiop:string-prefix-p (concatenate 'string r "/") s) ()
            "~A is not under ~A" s r)
    (subseq s (1+ (length r)))))

(defun sorted-rels (paths root)
  (sort (mapcar (lambda (p) (rel p root)) paths) #'string<))

(defun walk-files (root &rest walk-keys)
  "Sorted relative names of every file listed by WALK."
  (sorted-rels (loop for (nil files nil) in (apply #'walk root walk-keys) append files) root))

(defun walk-dirs (root &rest walk-keys)
  "Sorted relative names of every directory listed in a DIRS slot by WALK."
  (sorted-rels (loop for (nil nil dirs) in (apply #'walk root walk-keys) append dirs) root))

(defun walked-dir-names (root &rest walk-keys)
  "Sorted relative names of the directories WALK descended into (root excluded)."
  (sorted-rels (remove-if (lambda (d) (path-equal d root))
                          (mapcar #'first (apply #'walk root walk-keys)))
               root))

(defun list-names (dir)
  "Sorted names of the entries directly in DIR."
  (sort (mapcar #'name (iterdir dir)) #'string<))

(defun make-zip-fs (entries)
  "Zip filesystem over ENTRIES ((name content-string) ...)."
  (clear-zip-filesystem-cache)
  (make-zip-filesystem "<h2a>" :bytes (write-zip-bytes entries) :cache nil))

;;; gitignore corpus (tests/fixtures/gitignore-corpus.sexp; verdicts from real git)

(defvar *gitignore-corpus*
  (with-open-file (in (asdf:system-relative-pathname
                       "cl-stack-pathlib" "tests/fixtures/gitignore-corpus.sexp")
                      :external-format :utf-8)
    (let ((*read-eval* nil))
      (read in))))

(defun corpus-case-names ()
  (mapcar (lambda (c) (getf c :name)) *gitignore-corpus*))

(defun corpus-case (name)
  (or (find name *gitignore-corpus* :key (lambda (c) (getf c :name)) :test #'string=)
      (error "no gitignore corpus case ~S" name)))

(defun corpus-matcher (case)
  (apply #'merge-ignore
         (loop for (dir text) in (getf case :ignore-files)
               collect (parse-ignore text :base dir))))
