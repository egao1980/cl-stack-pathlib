(in-package #:cl-stack-pathlib/tests)

;;; WALK / GLOB / RGLOB with :IGNORE (a matcher or :AUTO). Ignored
;;; directories are pruned: not listed and not descended.

(defun ignore-tree (root)
  (make-tree root `((".gitignore" ,(format nil "*.log~%!keep.log~%build/~%"))
                    ("a.txt" "a") ("a.log" "l") ("keep.log" "k")
                    ("build/out.o" "o") ("build/sub/x.o" "x")
                    ("src/b.lisp" "b") ("src/c.log" "c") ("src/gen.lisp" "g")
                    ("src/.ignore" ,(format nil "gen.lisp~%"))
                    (".git/config" "") (".git/HEAD" ""))))

(deftest-parametrize walk-with-ignore
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (ignore-tree root)
    ;; Unchanged without :IGNORE.
    (ok (equal (walk-files root)
               '(".git/HEAD" ".git/config" ".gitignore" "a.log" "a.txt" "build/out.o"
                 "build/sub/x.o" "keep.log" "src/.ignore" "src/b.lisp" "src/c.log" "src/gen.lisp")))
    ;; An explicit matcher, relative to the walk root.
    (let ((m (parse-ignore (format nil "*.log~%build/~%"))))
      (ok (equal (walk-files root :ignore m)
                 '(".git/HEAD" ".git/config" ".gitignore" "a.txt" "src/.ignore" "src/b.lisp" "src/gen.lisp")))
      (ok (equal (walk-dirs root :ignore m) '(".git" "src")))
      (ok (equal (walked-dir-names root :ignore m) '(".git" "src")) "build/ not descended")
      (ok (equal (walk-files root :ignore m :top-down nil) (walk-files root :ignore m))))
    ;; :AUTO reads .gitignore / .ignore under the root and prunes .git.
    (ok (equal (walk-files root :ignore :auto)
               '(".gitignore" "a.txt" "keep.log" "src/.ignore" "src/b.lisp")))
    (ok (equal (walk-dirs root :ignore :auto) '("src")))
    (ok (equal (walked-dir-names root :ignore :auto) '("src")))
    ;; The protocol call takes the same key.
    (ok (= 2 (length (fs-walk (path-filesystem root) (path-pathname root) :ignore :auto))))))

(deftest-parametrize glob-with-ignore
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (ignore-tree root)
    (ok (equal (sorted-rels (rglob root "*.o") root) '("build/out.o" "build/sub/x.o")))
    (ok (null (rglob root "*.o" :ignore :auto)))
    (ok (equal (sorted-rels (rglob root "*.log" :ignore :auto) root) '("keep.log")))
    (ok (equal (sorted-rels (rglob root "*.lisp" :ignore :auto) root) '("src/b.lisp")))
    (ok (null (glob root "*.log" :ignore (parse-ignore "*.log"))))
    (ok (equal (sorted-rels (glob root "*.log" :ignore (parse-ignore (format nil "*.log~%!keep.log~%"))) root)
               '("keep.log")))
    (ok (equal (sorted-rels (glob root "*.txt" :ignore :auto) root) '("a.txt")))))

(deftest walk-with-ignore-zip
  (let ((fs (make-zip-fs '(("a.txt" "x") ("b.log" "y") ("d/c.log" "z") ("d/e.txt" "w")))))
    (with-filesystem (fs)
      (let ((root (ensure-path "/" :directory t)))
        (ok (equal (walk-files root :ignore (parse-ignore "*.log")) '("a.txt" "d/e.txt")))
        (ok (equal (walk-files root :ignore (parse-ignore "d/")) '("a.txt" "b.log")))
        (ok (equal (sorted-rels (rglob root "*.txt" :ignore (parse-ignore "d/")) root) '("a.txt")))))))
