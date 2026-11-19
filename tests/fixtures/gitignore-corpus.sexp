;;; gitignore corpus. Expected verdicts recorded from real git (git version 2.54.0 (Apple Git-157)) with core.ignorecase=false:
;;; `git check-ignore -q PATH` per path, cross-checked against
;;; `git ls-files --others --ignored --exclude-standard` for files.
;;; Regenerate: tests/fixtures/gen-gitignore-corpus.py (see PROVENANCE.md).
;;; Each case: (:name N :ignore-files ((DIR TEXT) ...) :paths ((PATH IGNORED-P) ...)).
;;; DIR is "" for the root or "sub/"; PATH ending in / is a directory.
(
 (:name "basic"
  :ignore-files (("" "*.log
"))
  :paths (("a.log" t)
          ("a.txt" nil)
          ("sub/b.log" t)
          ("sub/deep/c.log" t)
          ("log" nil)))
 (:name "negation"
  :ignore-files (("" "*.log
!keep.log
"))
  :paths (("x.log" t)
          ("keep.log" nil)
          ("sub/keep.log" nil)
          ("sub/y.log" t)))
 (:name "directory-only"
  :ignore-files (("" "build/
"))
  :paths (("build/" t)
          ("build/out.o" t)
          ("src/build/" t)
          ("src/build/x.o" t)
          ("lib/build" nil)))
 (:name "anchored-leading-slash"
  :ignore-files (("" "/top.txt
"))
  :paths (("top.txt" t)
          ("sub/top.txt" nil)))
 (:name "anchored-middle-slash"
  :ignore-files (("" "doc/frotz/
"))
  :paths (("doc/frotz/" t)
          ("doc/frotz/a.txt" t)
          ("a/doc/frotz/b.txt" nil)))
 (:name "leading-double-star"
  :ignore-files (("" "**/foo
**/qux/bar
"))
  :paths (("foo" t)
          ("a/foo" t)
          ("a/b/foo" t)
          ("qux/bar" t)
          ("x/qux/bar" t)
          ("x/bar" nil)))
 (:name "trailing-double-star"
  :ignore-files (("" "abc/**
"))
  :paths (("abc/x" t)
          ("abc/y/z" t)
          ("x/abc/y" nil)
          ("abcd/x" nil)))
 (:name "middle-double-star"
  :ignore-files (("" "a/**/b
"))
  :paths (("a/b" t)
          ("a/x/b" t)
          ("a/x/y/b" t)
          ("a/xb" nil)
          ("c/a/b" nil)))
 (:name "parent-excluded"
  :ignore-files (("" "build/
!build/keep.txt
"))
  :paths (("build/keep.txt" t)
          ("build/other.txt" t)))
 (:name "reinclude-via-star"
  :ignore-files (("" "/*
!/foo
/foo/*
!/foo/bar
"))
  :paths (("top.txt" t)
          ("foo/bar" nil)
          ("foo/baz" t)
          ("other/x" t)))
 (:name "escapes-and-comments"
  :ignore-files (("" "# a comment
\\#hash
\\!bang
trail\\ 
plain   
"))
  :paths (("#hash" t)
          ("!bang" t)
          ("trail " t)
          ("plain" t)
          ("a comment" nil)
          ("# a comment" nil)))
 (:name "wildcards"
  :ignore-files (("" "file?.txt
[ab]*.c
[!a]x.c
"))
  :paths (("file1.txt" t)
          ("file10.txt" nil)
          ("fileX.txt" t)
          ("a1.c" t)
          ("b.c" t)
          ("c.c" nil)
          ("bx.c" t)
          ("ax.c" t)))
 (:name "star-no-slash"
  :ignore-files (("" "a/*.c
"))
  :paths (("a/x.c" t)
          ("a/b/x.c" nil)
          ("b/a/x.c" nil)))
 (:name "bare-name-matches-directory"
  :ignore-files (("" "node_modules
"))
  :paths (("node_modules/" t)
          ("node_modules/x/y.js" t)
          ("pkg/node_modules/z.js" t)
          ("node_modulesx/a.js" nil)))
 (:name "star-children"
  :ignore-files (("" "foo/*
"))
  :paths (("foo/a" t)
          ("foo/b/c" t)
          ("x/foo/a" nil)))
 (:name "nested"
  :ignore-files (("" "*.tmp
") ("sub/" "!keep.tmp
/local.txt
"))
  :paths (("a.tmp" t)
          ("local.txt" nil)
          ("sub/keep.tmp" nil)
          ("sub/other.tmp" t)
          ("sub/local.txt" t)
          ("sub/deeper/local.txt" nil)
          ("keep.tmp" t)))
 (:name "negated-directory"
  :ignore-files (("" "logs/
!logs/
"))
  :paths (("logs/a.txt" nil)))
 (:name "dir-only-negation-of-files"
  :ignore-files (("" "*
!*/
!*.lisp
"))
  :paths (("a.lisp" nil)
          ("a.txt" t)
          ("src/b.lisp" nil)
          ("src/b.txt" t)))
)
