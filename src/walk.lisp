(in-package #:cl-stack-pathlib)

;;; Tree walks and glob patterns shared by every backend.
;;;
;;; Glob syntax is fnmatch / Python pathlib on each "/"-separated segment:
;;; "*", "?", "[a-z]", "[!x]" / "[^x]" and "\" escapes, none of which matches
;;; "/"; a "**" segment matches any number of directories. Segments compile
;;; with the gitignore matcher's tokenizer (ignore.lisp), so names are matched
;;; as strings and never parsed as CL pathnames ("[" or "*" in a real file
;;; name is just a character).
;;;
;;; Symlinks (os.walk / pathlib semantics): a link is listed like any other
;;; entry, a link to a directory is never descended unless FOLLOW-SYMLINKS is
;;; true, and even then a link back to a directory being walked (a cycle) is
;;; listed but not entered.

;;; --- patterns ----------------------------------------------------------------

(defun %split-pattern (pattern)
  (remove "" (uiop:split-string pattern :separator "/") :test #'string=))

(defun %compile-glob (pattern &key recursive)
  "Segment list for %MATCH-SEGMENTS. RECURSIVE (rglob) matches PATTERN at any
depth. A trailing \"**\" matches one or more segments."
  (check-type pattern string)
  (let* ((parts (%split-pattern pattern))
         (segments (loop for (part . more) on parts
                         collect (cond ((and (string= part "**") more) :globstar)
                                       ((string= part "**") :globstar+)
                                       (t (%tokenize-segment part))))))
    (if recursive (cons :globstar segments) segments)))

(defun %glob-max-depth (segments)
  "Deepest relative path SEGMENTS can match, or NIL when unbounded."
  (unless (find-if #'keywordp segments)
    (length segments)))

(defun %glob-name-match-p (name pattern)
  "Does the slash-free NAME match the glob segment PATTERN?"
  (%match-segment (%tokenize-segment pattern) name))

(defun %path-match-p (posix pattern)
  "Pathlib PurePath.match: a relative PATTERN matches the trailing segments of
the POSIX path string, an absolute one the whole path."
  (let* ((absolute (and (plusp (length pattern)) (char= (char pattern 0) #\/)))
         (segments (%compile-glob pattern :recursive (not absolute)))
         (parts (%split-pattern posix)))
    (and segments (%match-segments segments parts))))

;;; --- walking -------------------------------------------------------------------

(defun %real-key (fs pathname)
  "A string naming the physical directory behind PATHNAME (for cycle checks)."
  (let ((real (ignore-errors (fs-resolve fs pathname :strict nil))))
    (string-right-trim "/" (%pathname-as-posix (or real pathname)))))

(defun %walk-tree (fs root children &key (top-down t) follow-symlinks ignored-p)
  "(dir files dirs) triples under ROOT. CHILDREN is a function of a directory
returning (values files dirs) of its entries; IGNORED-P is NIL or a function
(pathname directory-p)."
  (let ((out '()))
    (labels ((walk* (dir ancestors)
               (multiple-value-bind (files dirs) (funcall children dir)
                 (when ignored-p
                   (setf files (remove-if (lambda (f) (funcall ignored-p f nil)) files)
                         dirs (remove-if (lambda (d) (funcall ignored-p d t)) dirs)))
                 (when top-down (push (list dir files dirs) out))
                 (dolist (d dirs)
                   (cond ((not follow-symlinks)
                          (unless (fs-symlink-p fs d) (walk* d nil)))
                         (t (let ((real (%real-key fs d)))
                              (unless (member real ancestors :test #'string=)
                                (walk* d (cons real ancestors)))))))
                 (unless top-down (push (list dir files dirs) out)))))
      (walk* root (when follow-symlinks (list (%real-key fs root))))
      (nreverse out))))

;;; --- glob ------------------------------------------------------------------------

(defmethod fs-glob ((fs filesystem) pathname pattern &key recursive ignore)
  "Entries under PATHNAME whose path relative to it matches PATTERN, files and
directories alike, as paths under PATHNAME (links are not resolved, and
directory links are not descended)."
  (let* ((base (fs-absolute fs pathname))
         (segments (%compile-glob pattern :recursive recursive))
         (max-depth (%glob-max-depth segments))
         (out '()))
    (when (and segments (fs-directory-p fs base))
      (let ((ignored-p (%ignore-predicate fs base ignore)))
        (labels ((visit (dir rel depth)
                   (dolist (e (sort (copy-list (fs-iterdir fs dir)) #'string<
                                    :key #'%entry-name))
                     (let* ((parts (append rel (list (%entry-name e))))
                            (dir-p (fs-directory-p fs e)))
                       (unless (and ignored-p (funcall ignored-p e dir-p))
                         (when (%match-segments segments parts)
                           (push e out))
                         (when (and dir-p
                                    (or (null max-depth) (< depth max-depth))
                                    (not (fs-symlink-p fs e)))
                           (visit e parts (1+ depth))))))))
          (visit base '() 1))))
    (nreverse out)))
