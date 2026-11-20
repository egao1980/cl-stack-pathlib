(in-package #:cl-stack-pathlib/tests)

;;; Stat-like probes follow a symlink at the end of the path, as stat(2) does
;;; (#30): FILE-P, DIRECTORY-P, EXISTS-P, READABLE-P, FILE-SIZE,
;;; LAST-MODIFIED and STAT. A dangling link is absent (NIL, or PATH-NOT-FOUND
;;; from FILE-SIZE / LAST-MODIFIED / STAT); a link cycle is absent for the
;;; predicates and SYMLINK-LOOP for the others. SYMLINK-P and
;;; STAT :FOLLOW-SYMLINKS NIL stay the lstat(2) view. Same tree, same answers
;;; on memory and local.

(defmacro with-final-link-tree ((root kind) &body body)
  `(if (and (eq ,kind :local) (uiop:os-windows-p))
       (skip "local symlinks need unix")
       (with-backend (,root ,kind)
         (make-tree ,root '(("r.txt" "abc") "d/"))
         (create-symlink (join ,root "lf") (join ,root "r.txt"))
         (create-symlink (join ,root "llf") (join ,root "lf"))
         (create-symlink (join ,root "ld") (join ,root "d"))
         (create-symlink (join ,root "dangling") (join ,root "nope"))
         (create-symlink (join ,root "loop-a") (join ,root "loop-b"))
         (create-symlink (join ,root "loop-b") (join ,root "loop-a"))
         ,@body)))

(deftest-parametrize final-link-predicates-follow
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-final-link-tree (root kind)
    (flet ((probe (name)
             (let ((p (join root name)))
               (list (and (exists-p p) t) (and (file-p p) t) (and (directory-p p) t)
                     (and (readable-p p) t) (and (symlink-p p) t)))))
      ;;                      exists file  dir   read  symlink
      (ok (equal '(t   t   nil t   t)   (probe "lf")) "link to a file")
      (ok (equal '(t   t   nil t   t)   (probe "llf")) "link to a link to a file")
      (ok (equal '(t   nil t   t   t)   (probe "ld")) "link to a directory")
      (ok (equal '(nil nil nil nil t)   (probe "dangling")) "dangling link")
      (ok (equal '(nil nil nil nil t)   (probe "loop-a")) "link cycle")
      (ok (directory-p (ensure-directory (join root "ld"))) "directory form")
      (ng (executable-p (join root "dangling")))
      (ng (executable-p (join root "loop-a"))))))

(deftest-parametrize final-link-size-and-mtime-follow
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-final-link-tree (root kind)
    (let ((real (join root "r.txt")))
      (ok (= 3 (file-size (join root "lf"))))
      (ok (= 3 (file-size (join root "llf"))))
      (ok (eql 0 (file-size (join root "d"))) "a directory is 0 (#33)")
      (ok (eql 0 (file-size (join root "ld"))) "a link to a directory is 0")
      (ok (eql 0 (file-size (ensure-directory (join root "d")))) "directory form")
      (ok (eql (file-stat-size (stat (join root "d"))) (file-size (join root "d")))
          "file-size agrees with stat :size")
      (ok (eql (last-modified real) (last-modified (join root "lf"))))
      (ok (signals (file-size (join root "dangling")) 'path-not-found))
      (ok (signals (last-modified (join root "dangling")) 'path-not-found))
      (ok (signals (file-size (join root "loop-a")) 'symlink-loop))
      (ok (signals (last-modified (join root "loop-a")) 'symlink-loop))
      (ok (typep (make-condition 'symlink-loop) 'path-error)))))

(deftest-parametrize final-link-stat
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-final-link-tree (root kind)
    (ok (eq :file (file-stat-kind (stat (join root "llf")))))
    (ok (= 3 (file-stat-size (stat (join root "llf")))))
    (ok (eq :directory (file-stat-kind (stat (join root "ld")))))
    (ok (signals (stat (join root "dangling")) 'path-not-found))
    (ok (signals (stat (join root "loop-a")) 'symlink-loop))
    (dolist (name '("lf" "ld" "dangling" "loop-a"))
      (ok (eq :symlink (file-stat-kind (stat (join root name) :follow-symlinks nil)))
          (format nil "lstat view of ~A" name)))))
