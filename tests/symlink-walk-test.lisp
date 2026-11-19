(in-package #:cl-stack-pathlib/tests)

;;; Symlinks in walks, globs, grep and ignore-file loading (#22, #23, #24).
;;; A symlink is listed (FILES for a link to a file or a dangling link, DIRS
;;; for a link to a directory) and a directory link is not descended unless
;;; :FOLLOW-SYMLINKS is true; a link cycle never recurses forever.
;;;
;;; WORK/inner is the root; WORK/outside is a directory the links point at.

(defmacro with-link-tree ((root outside kind) &body body)
  `(if (and (eq ,kind :local) (uiop:os-windows-p))
       (skip "local symlinks need unix")
       (with-backend (work ,kind)
         (let ((,root (ensure-directory (join work "inner")))
               (,outside (ensure-directory (join work "outside"))))
           (declare (ignorable ,outside))
           (make-tree work `(("inner/real/a.txt" "alpha")
                             ("outside/secret.txt" "s3cret")
                             ("outside/.gitignore" ,(format nil "*.txt~%"))))
           (create-symlink (join ,root "out") ,outside)
           (create-symlink (join ,root "f") (join ,outside "secret.txt"))
           ,@body))))

(deftest-parametrize walk-lists-symlinks
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-link-tree (root outside kind)
    (ok (equal '("f" "real/a.txt") (walk-files root)) "a file link is in FILES (#22)")
    (ok (equal '("out" "real") (walk-dirs root)) "a directory link is in DIRS")
    (ok (equal '("real") (walked-dir-names root)) "and is not descended (#23)")
    (ok (equal '("real") (walked-dir-names root :follow-symlinks nil)))
    (ok (equal '("real") (walked-dir-names root :top-down nil)))
    (ok (equal '("f" "real/a.txt") (walk-files root :ignore :auto)))
    (ok (equal '("out" "real") (walked-dir-names root :follow-symlinks t)))
    (ok (equal '("f" "out/.gitignore" "out/secret.txt" "real/a.txt")
               (walk-files root :follow-symlinks t))
        "followed links keep paths under the root")))

(deftest-parametrize symlink-p-on-listings
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-link-tree (root outside kind)
    (flet ((entry (name) (find name (iterdir root) :key #'name :test #'string=)))
      (ok (symlink-p (entry "out")) "directory link listed by iterdir (#23)")
      (ok (symlink-p (entry "f")))
      (ng (symlink-p (entry "real")))
      (ok (symlink-p (ensure-directory (join root "out"))) "directory form")
      (ok (symlink-p (join root "out"))))))

(deftest-parametrize glob-and-grep-stay-under-root
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-link-tree (root outside kind)
    (ok (equal '("f" "out" "real" "real/a.txt") (sorted-rels (rglob root "*") root)))
    (ok (equal '("f" "out" "real" "real/a.txt")
               (sorted-rels (glob root "**/*" :recursive t) root))
        "glob results stay under the base (#23)")
    (ok (null (grep root "s3cret")) "grep does not read through links")
    (ok (null (grep root "s3cret" :ignore nil)))
    (ok (equal '("real/a.txt") (mapcar (lambda (m) (rel (grep-match-path m) root))
                                       (grep root "alpha"))))))

(deftest-parametrize ignore-files-not-read-through-links
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-link-tree (root outside kind)
    (let ((m (load-ignore-files (path-filesystem root) (path-pathname root))))
      (ng (ignore-path-p m "real/a.txt") "outside/.gitignore is not read (#24)")
      (ng (ignore-path-p m "out/secret.txt")))))

(deftest-parametrize symlink-cycles-terminate
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-link-tree (root outside kind)
    (create-symlink (join root "loop") root)
    (create-symlink (join root "real/up") root)
    (write-text (join root ".gitignore") (format nil "*.log~%"))
    (let ((m (load-ignore-files (path-filesystem root) (path-pathname root))))
      (ok (ignore-path-p m "x.log") "load-ignore-files terminates on a cycle (#24)"))
    (ok (equal '("loop" "out" "real" "real/up") (walk-dirs root)))
    (ok (equal '("out" "real") (walked-dir-names root :follow-symlinks t))
        "a link back to an ancestor is listed, not walked")
    (ok (equal '("real/a.txt") (sorted-rels (rglob root "a.txt") root)))
    (ok (= 1 (length (grep root "alpha"))))))
