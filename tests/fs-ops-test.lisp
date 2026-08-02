(in-package #:cl-stack-pathlib/tests)

;;; Concrete Path FS ops on memory-filesystem (pathlib Path tests shape).

(deftest exists-file-dir
  (with-memory-fs ()
    (mkdir "/d" :parents t)
    (write-text "/d/f.txt" "x")
    (ok (exists-p "/d"))
    (ok (exists-p "/d/f.txt"))
    (ok (directory-p "/d"))
    (ok (file-p "/d/f.txt"))
    (ng (file-p "/d"))
    (ng (directory-p "/d/f.txt"))
    (ng (exists-p "/missing"))))

(deftest mkdir-parents-and-exist-ok
  (with-memory-fs ()
    (mkdir "/a/b/c" :parents t)
    (ok (directory-p "/a/b/c"))
    (mkdir "/a/b/c" :exist-ok t)
    (ok (signals (mkdir "/a/b/c" :exist-ok nil) 'path-exists-error))))

(deftest touch-unlink-rmdir
  (with-memory-fs ()
    (mkdir "/t" :parents t)
    (touch "/t/new")
    (ok (file-p "/t/new"))
    (ok (zerop (file-size "/t/new")))
    (unlink "/t/new")
    (ng (exists-p "/t/new"))
    (ok (signals (unlink "/t/missing") 'path-not-found))
    (unlink "/t/missing" :missing-ok t)
    (rmdir "/t")
    (ng (exists-p "/t"))))

(deftest delete-path-alias
  (with-memory-fs ()
    (touch "/x")
    (delete-path "/x")
    (ng (exists-p "/x"))))

(deftest rename-replace-copy-move
  (with-memory-fs ()
    (write-text "/a.txt" "one")
    (rename-path "/a.txt" "/b.txt")
    (ng (exists-p "/a.txt"))
    (ok (string= (read-text "/b.txt") "one"))
    (write-text "/c.txt" "two")
    (ok (signals (rename-path "/b.txt" "/c.txt" :replace nil) 'path-exists-error))
    (replace-path "/b.txt" "/c.txt")
    (ok (string= (read-text "/c.txt") "one"))
    (write-text "/src.txt" "copy-me")
    (copy-path "/src.txt" "/dst.txt")
    (ok (string= (read-text "/dst.txt") "copy-me"))
    (ok (exists-p "/src.txt"))
    (move-path "/src.txt" "/moved.txt")
    (ng (exists-p "/src.txt"))
    (ok (string= (read-text "/moved.txt") "copy-me"))))

(deftest read-write-bytes-and-text
  (with-memory-fs ()
    (write-bytes "/bin.dat" #(1 2 3 4))
    (ok (equalp (read-bytes "/bin.dat") #(1 2 3 4)))
    (ok (= 4 (file-size "/bin.dat")))
    (write-text "/t.txt" "hello")
    (ok (string= (read-text "/t.txt") "hello"))
    (write-text "/t.txt" "!" :append t)
    (ok (string= (read-text "/t.txt") "hello!"))))

(deftest symlink-ops
  (with-memory-fs ()
    (write-text "/real.txt" "data")
    (create-symlink "/link" "/real.txt")
    (ok (symlink-p "/link"))
    (ok (posix= (read-symlink "/link") "/real.txt"))
    (ng (symlink-p "/real.txt"))))

(deftest iterdir-walk-glob
  (with-memory-fs ()
    (mkdir "/root/sub" :parents t)
    (write-text "/root/a.txt" "a")
    (write-text "/root/b.lisp" "b")
    (write-text "/root/sub/c.txt" "c")
    (let ((names (sort (mapcar #'name (iterdir "/root")) #'string<)))
      (ok (equal names '("a.txt" "b.lisp" "sub"))))
    (let ((walked (walk "/root")))
      (ok (plusp (length walked)))
      (ok (every (lambda (triple) (= 3 (length triple))) walked)))
    (let ((txts (mapcar #'name (glob "/root" "*.txt"))))
      (ok (find "a.txt" txts :test #'string=))
      (ng (find "c.txt" txts :test #'string=)))
    (let ((all-txt (mapcar #'name (rglob "/root" "*.txt"))))
      (ok (find "a.txt" all-txt :test #'string=))
      (ok (find "c.txt" all-txt :test #'string=)))))

(deftest uri-and-cwd-home-memory
  (with-memory-fs ()
    (write-text "/u.txt" "u")
    (ok (string= (as-uri "/u.txt") "memory:///u.txt"))
    (ok (directory-p (cwd)))
    (ok (directory-p (home)))
    (ok (posix= (home) "/home"))
    (ok (posix= (cwd) "/"))))

(deftest last-modified-present
  (with-memory-fs ()
    (write-text "/m.txt" "m")
    (ok (integerp (last-modified "/m.txt")))))

(deftest nested-write-via-join
  (with-memory-fs ()
    (mkdir "/pkg/native" :parents t)
    (let ((p (join "/pkg/native" "lib.so")))
      (write-bytes p #(7 8 9))
      (ok (file-p p))
      (ok (equalp (read-bytes p) #(7 8 9)))
      (ok (posix= (parent p) "/pkg/native")))))

(deftest permissions-smoke-memory
  (with-memory-fs ()
    (write-text "/rw.txt" "x")
    (ok (readable-p "/rw.txt"))
    (ok (writable-p "/rw.txt"))))
