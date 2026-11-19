(in-package #:cl-stack-pathlib/tests)

;;; Ignore matcher: .gitignore syntax. The corpus verdicts come from real git
;;; (tests/fixtures/gitignore-corpus.sexp); the other cases pin pathlib's own
;;; API (status values, BASE, MERGE-IGNORE order, .ignore precedence).

(deftest-parametrize gitignore-corpus
    ((case-name) :ids (corpus-case-names) :rows (corpus-case-names))
  (let* ((case (corpus-case case-name))
         (m (corpus-matcher case)))
    (ok (ignore-matcher-p m))
    (loop for (path expected) in (getf case :paths)
          do (ok (eq expected (and (ignore-path-p m path) t))
                 (format nil "~A: ~S ~:[included~;ignored~] as in git" case-name path expected)))))

(deftest-parametrize gitignore-corpus-loaded
    ((case-name) :ids (corpus-case-names) :rows (corpus-case-names))
  ;; Same verdicts when the ignore files are found on a filesystem.
  (with-backend (root :memory)
    (let ((case (corpus-case case-name)))
      (make-tree root (loop for (path nil) in (getf case :paths) collect path))
      (loop for (dir text) in (getf case :ignore-files)
            do (make-tree root (list (list (concatenate 'string dir ".gitignore") text))))
      (let ((m (load-ignore-files (path-filesystem root) (path-pathname root))))
        (loop for (path expected) in (getf case :paths)
              do (ok (eq expected (and (ignore-path-p m path) t))
                     (format nil "~A (loaded): ~S" case-name path)))))))

(deftest ignore-path-status-values
  (let ((m (parse-ignore (format nil "*.log~%!keep.log~%build/~%!build/keep.txt~%"))))
    (ok (eq :ignored (ignore-path-status m "x.log")))
    (ok (eq :included (ignore-path-status m "keep.log")))
    (ok (null (ignore-path-status m "a.txt")))
    ;; Own status is :INCLUDED, but the parent directory is ignored.
    (ok (eq :included (ignore-path-status m "build/keep.txt")))
    (ok (ignore-path-p m "build/keep.txt"))
    (ng (ignore-path-p m "keep.log"))
    (ng (ignore-path-p m "a.txt"))))

(deftest ignore-directory-flag
  (let ((m (parse-ignore "build/")))
    (ok (ignore-path-p m "build" :directory t))
    (ok (ignore-path-p m "build/"))
    (ng (ignore-path-p m "build") "a file named build is not matched by build/")
    (ok (ignore-path-p m "build/x.o") "contents of an ignored directory")
    (ok (eq :ignored (ignore-path-status m "src/build/")))))

(deftest ignore-source-forms
  (let ((from-text (parse-ignore (format nil "*.log~%# comment~%~%!keep.log~%")))
        (from-lines (parse-ignore '("*.log" "# comment" "" "!keep.log")))
        (crlf (parse-ignore (format nil "*.log~C~%/top~C~%" #\Return #\Return))))
    (dolist (m (list from-text from-lines))
      (ok (ignore-path-p m "a.log"))
      (ng (ignore-path-p m "keep.log")))
    (ok (ignore-path-p crlf "a.log") "CRLF ignore file")
    (ok (ignore-path-p crlf "top"))
    (ng (ignore-path-p (parse-ignore "") "anything"))))

(deftest ignore-base-scoping
  (let ((m (parse-ignore (format nil "/x.txt~%*.tmp~%") :base "sub/")))
    (ok (ignore-path-p m "sub/x.txt"))
    (ng (ignore-path-p m "x.txt") "rules of sub/.gitignore do not reach the root")
    (ng (ignore-path-p m "sub/a/x.txt") "anchored to sub/")
    (ok (ignore-path-p m "sub/a/b.tmp"))
    (ng (ignore-path-p m "b.tmp"))))

(deftest merge-ignore-precedence
  (let ((all-log (parse-ignore "*.log"))
        (keep (parse-ignore "!a.log")))
    (ng (ignore-path-p (merge-ignore all-log keep) "a.log") "later matcher wins")
    (ok (ignore-path-p (merge-ignore keep all-log) "a.log"))
    (ok (ignore-path-p (merge-ignore nil all-log nil) "b.log") "NIL skipped")
    (ok (ignore-matcher-p (merge-ignore)))
    (ng (ignore-path-p (merge-ignore) "a.log"))))

(deftest load-ignore-files-precedence-and-dot-git
  (with-backend (root :memory)
    (make-tree root `((".gitignore" ,(format nil "*.log~%"))
                      (".ignore" ,(format nil "!keep.log~%"))
                      (".git/.gitignore" ,(format nil "!*.log~%"))
                      (".git/z.log" "")
                      ("a.log" "") ("keep.log" "") ("a.txt" "")
                      ("sub/.gitignore" ,(format nil "*.txt~%"))
                      ("sub/b.txt" "")
                      ("vendor/.gitignore" "*")
                      ("vendor/c.txt" "")))
    (let ((m (load-ignore-files (path-filesystem root) (path-pathname root))))
      (ok (ignore-path-p m "a.log"))
      (ng (ignore-path-p m "keep.log") ".ignore beats .gitignore")
      (ok (ignore-path-p m ".git/z.log") ".git/.gitignore is never read")
      (ok (ignore-path-p m "sub/b.txt"))
      (ng (ignore-path-p m "b.txt") "sub/.gitignore only applies under sub/")
      (ok (ignore-path-p m "vendor/c.txt")))
    (let ((only-git (load-ignore-files (path-filesystem root) (path-pathname root)
                                       :names '(".gitignore"))))
      (ok (ignore-path-p only-git "keep.log") ":NAMES limits the files read"))))
