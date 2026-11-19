(in-package #:cl-stack-pathlib/tests)

;;; FS-GREP / GREP: literal search, function patterns, the regex hook,
;;; ignore / include filters, binary and non-UTF-8 skipping, MAX-RESULTS.

(defun grep-tree (root)
  (make-tree root `(("a.txt" ,(format nil "alpha~%beta~%alphabet~%"))
                    ("sub/b.txt" ,(format nil "no match~%ALPHA upper~%  alpha indented~%"))
                    ("bin.dat" ,(octets "alpha" 0 "alpha"))
                    ("bad.txt" ,(octets "alpha" 10 #xE2))
                    ("ignored.log" ,(format nil "alpha~%"))
                    (".gitignore" ,(format nil "*.log~%")))))

(defun grep-rows (root matches)
  "(relpath line column text) per match, in result order."
  (mapcar (lambda (m)
            (list (rel (grep-match-path m) root) (grep-match-line m)
                  (grep-match-column m) (grep-match-text m)))
          matches))

(deftest-parametrize grep-literal
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (grep-tree root)
    (multiple-value-bind (matches truncated) (grep root "alpha")
      (ok (equal (grep-rows root matches)
                 '(("a.txt" 1 0 "alpha")
                   ("a.txt" 3 0 "alphabet")
                   ("sub/b.txt" 3 2 "  alpha indented")))
          "sorted by path then line; ignore :AUTO; binary and bad UTF-8 skipped")
      (ng truncated))
    (ok (every (lambda (m) (path-p (grep-match-path m))) (grep root "alpha"))
        "facade GREP returns PATH objects")
    (ok (string= (read-text (grep-match-path (first (grep root "beta"))))
                 (format nil "alpha~%beta~%alphabet~%")))
    (ok (equal (grep-rows root (grep root "alpha" :case-insensitive t))
               '(("a.txt" 1 0 "alpha")
                 ("a.txt" 3 0 "alphabet")
                 ("sub/b.txt" 2 0 "ALPHA upper")
                 ("sub/b.txt" 3 2 "  alpha indented"))))
    (ok (equal (grep-rows root (grep root "alpha" :ignore nil))
               '(("a.txt" 1 0 "alpha")
                 ("a.txt" 3 0 "alphabet")
                 ("ignored.log" 1 0 "alpha")
                 ("sub/b.txt" 3 2 "  alpha indented"))))
    (ok (null (grep root "no such text")))
    ;; Protocol call: pathnames, same rows.
    (let ((raw (fs-grep (path-filesystem root) (path-pathname root) "alphabet")))
      (ok (= 1 (length raw)))
      (ok (pathnamep (grep-match-path (first raw)))))))

(deftest-parametrize grep-include-and-limits
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (grep-tree root)
    (ok (equal (mapcar #'first (grep-rows root (grep root "alpha" :include "sub/")))
               '("sub/b.txt")))
    (ok (equal (mapcar #'first (grep-rows root (grep root "alpha" :include '("a.txt"))))
               '("a.txt" "a.txt")))
    (ok (= 3 (length (grep root "alpha" :include "*.txt"))))
    (ok (null (grep root "alpha" :include "*.md")))
    (multiple-value-bind (matches truncated) (grep root "alpha" :max-results 2)
      (ok (equal (grep-rows root matches)
                 '(("a.txt" 1 0 "alpha") ("a.txt" 3 0 "alphabet"))))
      (ok truncated))
    (multiple-value-bind (matches truncated) (grep root "alpha" :max-results 3)
      (ok (= 3 (length matches)))
      (ng truncated "exactly MAX-RESULTS matches is not truncated"))
    ;; ROOT may be a single file.
    (ok (equal (grep-rows root (grep (join root "a.txt") "beta"))
               '(("a.txt" 2 0 "beta"))))))

(deftest-parametrize grep-pattern-kinds
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (grep-tree root)
    (ok (equal (grep-rows root (grep root (lambda (line) (search "bet" line))))
               '(("a.txt" 2 0 "beta") ("a.txt" 3 5 "alphabet")))
        "function pattern returns the match start")
    ;; pathlib has no regex engine: the default hook refuses.
    (ok (signals (grep root "al.ha" :regex t) 'unsupported-operation))
    (ok (signals (fs-grep-matcher (path-filesystem root) "al.ha") 'unsupported-operation))))

(defclass toy-regex-fs (memory-filesystem) ()
  (:documentation "Specializes the regex hook: \".\" matches any character."))

(defmethod fs-grep-matcher ((fs toy-regex-fs) pattern &key case-insensitive)
  (declare (ignore case-insensitive))
  (lambda (line)
    (loop for i from 0 to (- (length line) (length pattern))
          when (loop for j below (length pattern)
                     always (or (char= (char pattern j) #\.)
                                (char= (char pattern j) (char line (+ i j)))))
            return i)))

(deftest grep-regex-hook
  (let ((fs (make-instance 'toy-regex-fs)))
    (setf (gethash "/" (cl-stack-pathlib::memory-fs-root fs)) :dir)
    (with-filesystem (fs)
      (write-text "/t.txt" (format nil "cat~%cut~%dog~%"))
      (ok (equal (mapcar #'grep-match-line (grep "/" "c.t" :regex t)) '(1 2))))))

(deftest grep-zip
  (let ((fs (make-zip-fs (list (list "a.txt" (format nil "x~%needle~%")) (list "b/c.txt" "needle")))))
    (with-filesystem (fs)
      (let ((root (ensure-path "/" :directory t)))
        (ok (equal (grep-rows root (grep root "needle"))
                   '(("a.txt" 2 0 "needle") ("b/c.txt" 1 0 "needle"))))))))
