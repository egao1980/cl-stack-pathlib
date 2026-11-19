(in-package #:cl-stack-pathlib/tests)

;;; GLOB / RGLOB / FS-GLOB / MATCH-P pattern syntax (#28): fnmatch rules on
;;; each "/"-separated segment ("*", "?", "[a-z]", "[!x]", "\" escapes; none
;;; matches "/"), "**" for any number of directories, on every backend.

(defun glob-tree-entries ()
  '(("keep.log" "k") ("main.lisp" "m") ("file1.txt" "f") ("ax.c" "a") ("readme.md" "r")
    ("[x].txt" "b") ("sub/m2.lisp" "s") ("sub/deep/m3.lisp" "d")))

(defun call-with-glob-tree (kind fn)
  (if (eq kind :zip)
      (with-filesystem ((make-zip-fs (glob-tree-entries)))
        (funcall fn (ensure-path "/" :directory t)))
      (with-backend (root kind)
        (make-tree root (glob-tree-entries))
        (funcall fn root))))

(defparameter +glob-cases+
  '(("*.l*"      ("keep.log" "main.lisp"))
    ("*.lisp"    ("main.lisp"))
    ("file?.txt" ("file1.txt"))
    ("[ab]*.c"   ("ax.c"))
    ("[!a]*.c"   ())
    ("[km]*"     ("keep.log" "main.lisp"))
    ("m*.lisp"   ("main.lisp"))
    ("f*.txt"    ("file1.txt"))
    ("*.*"       ("[x].txt" "ax.c" "file1.txt" "keep.log" "main.lisp" "readme.md"))
    ("*"         ("[x].txt" "ax.c" "file1.txt" "keep.log" "main.lisp" "readme.md" "sub"))
    ("*l*"       ("file1.txt" "keep.log" "main.lisp"))
    ("main.*"    ("main.lisp"))
    ("[[]x].txt" ("[x].txt"))
    ("\\[x].txt" ("[x].txt"))
    ("readme.md" ("readme.md"))
    ("nothing"   ())
    ("sub/*.lisp" ("sub/m2.lisp"))
    ("*/*.lisp"  ("sub/m2.lisp"))
    ("s?b/*"     ("sub/deep" "sub/m2.lisp"))
    ("**/*.lisp" ("main.lisp" "sub/deep/m3.lisp" "sub/m2.lisp"))
    ("sub/**/*.lisp" ("sub/deep/m3.lisp" "sub/m2.lisp"))))

(defparameter +rglob-cases+
  '(("m*.lisp"   ("main.lisp" "sub/deep/m3.lisp" "sub/m2.lisp"))
    ("*.l*"      ("keep.log" "main.lisp" "sub/deep/m3.lisp" "sub/m2.lisp"))
    ("deep"      ("sub/deep"))
    ("deep/*"    ("sub/deep/m3.lisp"))
    ("?2.lisp"   ("sub/m2.lisp"))))

(defun %name-of (pathname)
  (cl-stack-pathlib::%entry-name pathname))

(deftest-parametrize glob-patterns
    ((kind) :ids ("local" "memory" "zip") (:local) (:memory) (:zip))
  (call-with-glob-tree
   kind
   (lambda (root)
     (loop for (pattern expected) in +glob-cases+
           do (ok (equal expected (sorted-rels (glob root pattern) root))
                  (format nil "glob ~S" pattern)))
     (loop for (pattern expected) in +rglob-cases+
           do (ok (equal expected (sorted-rels (rglob root pattern) root))
                  (format nil "rglob ~S" pattern))
              (ok (equal expected (sorted-rels (rglob root pattern :ignore :auto) root))
                  (format nil "rglob ~S :ignore :auto" pattern)))
     (ok (equal '("main.lisp")
                (sort (mapcar (lambda (pn) (%name-of pn))
                              (fs-glob (path-filesystem root) (path-pathname root) "m*.lisp"))
                      #'string<))
         "fs-glob")
     (ok (null (glob (join root "missing/") "*")) "missing base"))))

(deftest match-p-patterns
  (ok (match-p "/a/file1.txt" "file?.txt"))
  (ok (match-p "/a/keep.log" "*.l*"))
  (ng (match-p "/a/x.lisp" "*.l"))
  (ok (match-p "/a/main.lisp" "[km]*"))
  (ng (match-p "/a/ax.c" "[!a]*"))
  (ok (match-p "a/b/c.lisp" "b/*.lisp"))
  (ng (match-p "a/b/c.lisp" "a/*.lisp"))
  (ok (match-p "/a/b/c.lisp" "/a/*/c.lisp"))
  (ng (match-p "/x/a/b/c.lisp" "/a/*/c.lisp") "an absolute pattern matches the whole path")
  (ok (match-p "[x].txt" "\\[x].txt"))
  (ok (match-p "x.txt" "[x].txt")))
