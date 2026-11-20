(in-package #:cl-stack-pathlib/tests)

;;; 0.7.0 (#37): FS-CHMOD / (SETF FS-MODE), :MODE on FS-WRITE-ATOMIC and
;;; FS-RESTORE, and :AUTO ignoring a .git file (linked worktree) as well as a
;;; .git directory.

(defun %mode-of (path)
  (fs-mode (path-filesystem path) (path-pathname path)))

(deftest fs-chmod-local
  (with-backend (root :local)
    (let* ((p (join root "run.sh"))
           (fs (path-filesystem p)))
      (write-text p "echo hi")
      (cond
        ((unix-modes-p)
         (ok (eq t (fs-chmod fs (path-pathname p) #o755)))
         (ok (= #o755 (%mode-of p)))
         (ok (executable-p p))
         (setf (fs-mode fs (path-pathname p)) #o600)
         (ok (= #o600 (%mode-of p)) "(setf fs-mode)")
         (ok (not (executable-p p)))
         (fs-chmod fs (path-pathname p) #o100644)
         (ok (= #o644 (%mode-of p)) "only the permission bits are used"))
        (t
         (ok (null (fs-chmod fs (path-pathname p) #o755)) "no POSIX modes: a no-op")))
      (ok (signals (fs-chmod fs (path-pathname (join root "missing")) #o644) 'path-not-found)))))

(deftest fs-chmod-memory
  (with-backend (root :memory)
    (let* ((p (join root "f"))
           (fs (path-filesystem p)))
      (write-text p "x")
      (ok (null (fs-chmod fs (path-pathname p) #o755)) "memory: no modes")
      (ok (null (%mode-of p)))
      (ok (signals (fs-chmod fs (path-pathname (join root "missing")) #o644) 'path-not-found))
      (multiple-value-bind (pn hash written-p)
          (fs-write-atomic fs (path-pathname p) (octets "y") :mode #o755)
        (declare (ignore pn hash))
        (ok written-p ":mode is accepted and ignored"))
      (ok (string= "y" (read-text p))))))

(deftest write-atomic-mode
  (when (unix-modes-p)
    (with-backend (root :local)
      (let* ((p (join root "tool"))
             (fs (path-filesystem p)))
        (fs-write-atomic fs (path-pathname p) (octets "#!/bin/sh") :mode #o755)
        (ok (= #o755 (%mode-of p)) "a new file gets MODE")
        (fs-write-atomic fs (path-pathname p) (octets "#!/bin/sh v2"))
        (ok (= #o755 (%mode-of p)) "no MODE: an existing file keeps its mode")
        (fs-write-atomic fs (path-pathname p) (octets "plain") :mode #o640)
        (ok (= #o640 (%mode-of p)) "MODE replaces the mode of an existing file")
        (ok (string= "plain" (read-text p)))
        (write-bytes p (octets "via facade") :mode #o700)
        (ok (= #o700 (%mode-of p)) "write-bytes :mode")
        (ok (signals (write-bytes p (octets "x") :append t :mode #o700) 'error))
        (ok (equal '("tool") (list-names root)) "no temp file")))))

(deftest restore-mode
  (when (unix-modes-p)
    (with-backend (work :local)
      (let ((root (subdir work "tree"))
            (store (make-memory-snapshot-store)))
        (make-tree root '(("bin/run.sh" "#!/bin/sh") ("a.txt" "a") ("b.txt" "b")))
        (chmod (join root "bin/run.sh") #o755)
        (let ((id (snapshot root :store store)))
          (uiop:delete-directory-tree (path-pathname root) :validate t)
          (restore-snapshot root id :store store :mode #o600)
          (ok (= #o600 (%mode-of (join root "a.txt"))) "files written get MODE")
          (ok (= #o700 (%mode-of (join root "bin/run.sh")))
              "an :executable entry gets the execute bit on top")
          (testing "a function of the relative path"
            (uiop:delete-directory-tree (path-pathname root) :validate t)
            (restore-snapshot root id :store store
                                      :mode (lambda (rel) (and (string= rel "a.txt") #o640)))
            (ok (= #o640 (%mode-of (join root "a.txt"))))
            (ok (= #o644 (logand #o644 (%mode-of (join root "b.txt")))) "NIL: the default")
            (ok (logtest #o100 (%mode-of (join root "bin/run.sh")))))
          (testing "files the restore does not write keep their mode"
            (chmod (join root "b.txt") #o604)
            (restore-snapshot root id :store store :mode #o600)
            (ok (= #o604 (%mode-of (join root "b.txt"))))))))))

(deftest-parametrize auto-ignores-a-git-file
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-r4a (work store kind :memory)
    (let ((root (subdir work "tree")))
      ;; A linked worktree: .git is a file pointing at the main repository.
      (make-tree root '((".git" "gitdir: /elsewhere/.git/worktrees/wt") ("a.txt" "a")))
      (let ((id (snapshot root :store store :ignore :auto)))
        (ok (equal '("a.txt") (mapcar #'snapshot-entry-path (snapshot-entries id :store store)))
            "not in the snapshot")
        (ok (equal '("a.txt") (walk-files root :ignore :auto))
            "not walked")
        (write-text (join root "b.txt") "b")
        (restore-snapshot root id :store store :ignore :auto)
        (ok (file-p (join root ".git")) "restore :auto keeps the .git file")
        (ok (string= "gitdir: /elsewhere/.git/worktrees/wt" (read-text (join root ".git"))))
        (ok (not (exists-p (join root "b.txt"))))
        (testing "a .git directory is still pruned"
          (let ((root2 (subdir work "tree2")))
            (make-tree root2 '((".git/HEAD" "ref") ("a.txt" "a")))
            (ok (equal '("a.txt")
                       (mapcar #'snapshot-entry-path
                               (snapshot-entries (snapshot root2 :store store :ignore :auto)
                                                 :store store))))))))))
