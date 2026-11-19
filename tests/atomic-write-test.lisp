(in-package #:cl-stack-pathlib/tests)

;;; Atomic writes (temp file + rename in the same directory) and
;;; compare-and-swap writes (:EXPECTED-HASH) with CONTENT-HASH-MISMATCH and
;;; its OVERWRITE / USE-CURRENT restarts.

(deftest-parametrize atomic-write-success-leaves-no-temp
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let ((p (join root "f.txt")))
      (multiple-value-bind (path hash written-p) (write-text p "one" :atomic t)
        (ok (path-p path))
        (ok (path-equal path p))
        (ok (string= hash (octets-hash (octets "one"))))
        (ok (eq written-p t)))
      (ok (string= (read-text p) "one"))
      (ok (equal (list-names root) '("f.txt")) "no temp file after create")
      (write-text p "two" :atomic t)
      (ok (string= (read-text p) "two"))
      (ok (equal (list-names root) '("f.txt")) "no temp file after replace")
      ;; The protocol call returns the pathname and the new hash.
      (multiple-value-bind (pn hash written-p)
          (fs-write-atomic (path-filesystem p) (path-pathname p) (octets "three"))
        (ok (pathnamep pn))
        (ok (string= hash (octets-hash (octets "three"))))
        (ok written-p))
      (ok (string= (read-text p) "three"))
      (ok (equal (list-names root) '("f.txt"))))))

(deftest-parametrize atomic-write-any-name-create-and-replace
    ((kind) :ids ("local" "memory") (:local) (:memory))
  ;; Names without a type (dotfiles, store blobs), with several dots, and
  ;; non-ASCII: the temp file's name must never leak into the target's
  ;; (CL:RENAME-FILE merges the temp's type into a typeless target), and the
  ;; rename must see the same bytes as OPEN. Replacing must work on every OS.
  (with-backend (root kind)
    (let ((names (list ".gitignore" "Makefile" "24cf3d897fd680e0258c1c" "a.b.c"
                       (format nil "~C.txt" (code-char #xE9))
                       (format nil "d~C" (code-char #x4E2D)))))
      (dolist (n names)
        (let ((p (join root n)))
          (write-text p "one" :atomic t)
          (ok (string= "one" (read-text p)) (format nil "created ~S" n))
          (write-text p "two" :atomic t)
          (ok (string= "two" (read-text p)) (format nil "replaced ~S" n))))
      (ok (equal (sort (copy-list names) #'string<) (list-names root))
          "exactly the targets, no temp files"))))

(deftest-parametrize atomic-write-creates-parents
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let ((p (join root "a/b/c.txt")))
      (write-text p "deep" :atomic t)
      (ok (string= (read-text p) "deep"))
      (ok (equal (list-names (join root "a/b")) '("c.txt"))))))

(deftest-parametrize atomic-write-failure-leaves-no-temp
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let ((p (join root "f.txt")))
      (write-text p "orig" :atomic t)
      ;; Bad content: the write fails; the old content and the directory stay.
      (ok (eq :failed (handler-case (write-bytes p (vector 1 2 :not-an-octet) :atomic t)
                        (error () :failed))))
      (ok (string= (read-text p) "orig"))
      (ok (equal (list-names root) '("f.txt")) "no temp file after a failed write")
      ;; Target is a directory: refused after the temp file would exist.
      (mkdir (join root "d") :parents t)
      (ok (signals (write-text (join root "d") "x" :atomic t) 'path-error))
      (ok (directory-p (join root "d")))
      (ok (equal (list-names root) '("d" "f.txt")) "no temp file after a refused rename"))))

(deftest-parametrize cas-write-match
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let ((p (join root "f.txt")))
      (write-text p "v1")
      (let ((h1 (content-hash p)))
        (multiple-value-bind (path h2 written-p) (write-text p "v2" :expected-hash h1)
          (declare (ignore path))
          (ok written-p)
          (ok (string= h2 (octets-hash (octets "v2"))))
          (ok (string= (read-text p) "v2"))
          ;; The returned hash is the token for the next CAS write.
          (write-text p "v3" :expected-hash h2)
          (ok (string= (read-text p) "v3"))))
      ;; :ABSENT = create only.
      (let ((q (join root "new.txt")))
        (ok (nth-value 2 (write-text q "fresh" :expected-hash :absent)))
        (ok (string= (read-text q) "fresh")))
      (ok (equal (list-names root) '("f.txt" "new.txt"))))))

(deftest-parametrize cas-write-mismatch
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let* ((p (join root "f.txt"))
           (stale (progn (write-text p "v1") (content-hash p))))
      (write-text p "changed elsewhere")
      (let ((current (content-hash p))
            (c (handler-case (progn (write-text p "mine" :expected-hash stale) nil)
                 (content-hash-mismatch (c) c))))
        (ok (typep c 'content-hash-mismatch))
        (ok (typep c 'path-error))
        (when c
          (ok (string= (content-hash-mismatch-expected c) stale))
          (ok (string= (content-hash-mismatch-actual c) current))
          (ok (search "f.txt" (princ-to-string (path-error-path c))))))
      (ok (string= (read-text p) "changed elsewhere") "mismatch writes nothing")
      (ok (equal (list-names root) '("f.txt")))
      ;; :ABSENT on an existing file.
      (let ((c (handler-case (progn (write-text p "x" :expected-hash :absent) nil)
                 (content-hash-mismatch (c) c))))
        (ok (typep c 'content-hash-mismatch))
        (when c
          (ok (eq (content-hash-mismatch-expected c) :absent))
          (ok (stringp (content-hash-mismatch-actual c)))))
      ;; A hash for a file that does not exist: ACTUAL is NIL.
      (let ((c (handler-case (progn (write-text (join root "gone.txt") "x" :expected-hash stale) nil)
                 (content-hash-mismatch (c) c))))
        (ok (typep c 'content-hash-mismatch))
        (when c (ok (null (content-hash-mismatch-actual c)))))
      (ng (exists-p (join root "gone.txt"))))))

(deftest-parametrize cas-restarts
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let* ((p (join root "f.txt"))
           (stale (progn (write-text p "v1") (content-hash p)))
           (seen '()))
      (write-text p "theirs")
      ;; Both restarts are offered.
      (handler-bind ((content-hash-mismatch
                       (lambda (c)
                         (setf seen (list (and (find-restart 'overwrite c) t)
                                          (and (find-restart 'use-current c) t)))
                         (invoke-use-current c))))
        (write-text p "mine" :expected-hash stale))
      (ok (equal seen '(t t)))
      ;; USE-CURRENT: nothing written, current hash returned.
      (multiple-value-bind (path hash written-p)
          (handler-bind ((content-hash-mismatch #'invoke-use-current))
            (write-text p "mine" :expected-hash stale))
        (ok (path-equal path p))
        (ok (string= hash (octets-hash (octets "theirs"))))
        (ok (null written-p)))
      (ok (string= (read-text p) "theirs"))
      ;; OVERWRITE: written anyway.
      (multiple-value-bind (path hash written-p)
          (handler-bind ((content-hash-mismatch #'invoke-overwrite))
            (write-text p "mine" :expected-hash stale))
        (declare (ignore path))
        (ok (string= hash (octets-hash (octets "mine"))))
        (ok written-p))
      (ok (string= (read-text p) "mine"))
      ;; AUTO-OVERWRITE works through the generic helper too.
      (handler-bind ((content-hash-mismatch #'auto-overwrite))
        (write-text p "again" :expected-hash stale))
      (ok (string= (read-text p) "again"))
      ;; Restarts on the protocol call, outside the facade.
      (multiple-value-bind (pn hash written-p)
          (handler-bind ((content-hash-mismatch (lambda (c) (invoke-restart (find-restart 'use-current c)))))
            (fs-write-atomic (path-filesystem p) (path-pathname p) (octets "zzz") :expected-hash stale))
        (ok (pathnamep pn))
        (ok (string= hash (octets-hash (octets "again"))))
        (ng written-p))
      (ok (equal (list-names root) '("f.txt"))))))

(deftest atomic-write-local-symlink-and-mode
  #+(and sbcl unix)
  (with-backend (root :local)
    (let ((real (join root "real.txt"))
          (link (join root "link.txt")))
      (write-text real "old")
      (sb-posix:chmod (as-posix real) #o640)
      (create-symlink link real)
      (write-text link "new" :atomic t)
      (ok (symlink-p link) "the link stays a link")
      (ok (string= (read-text real) "new") "written through to the target")
      (ok (= #o640 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (as-posix real)))))
          "permission bits kept")
      (ok (equal (list-names root) '("link.txt" "real.txt")))))
  #-(and sbcl unix)
  (skip "needs SBCL on unix"))

(deftest atomic-write-zip-unsupported
  (let ((fs (make-zip-fs '(("a.txt" "x")))))
    (with-filesystem (fs)
      (ok (signals (write-text "/a.txt" "y" :atomic t) 'unsupported-operation))
      (ok (signals (fs-write-atomic fs (fs-parse fs "/a.txt") (octets "y")) 'unsupported-operation)))))
