(in-package #:cl-stack-pathlib/tests)

;;; #35 SYMLINK-P on the local filesystem answers without signalling: a
;;; symlink (unix) or junction (Windows, a reparse point) is a link; a plain
;;; file or directory is not.

(defun %native-link-name (path)
  (let ((s (uiop:native-namestring path)))
    (string-right-trim "/\\" s)))

(defun %make-local-link (link target)
  "LINK → TARGET (a directory): a junction on Windows (no privilege needed),
else a symlink."
  (if (uiop:os-windows-p)
      (uiop:run-program (list "cmd" "/c" "mklink" "/J" (%native-link-name link) (%native-link-name target))
                        :output nil :error-output :interactive)
      (create-symlink link target)))

(defun %remove-local-link (link)
  (when (uiop:os-windows-p)
    (uiop:run-program (list "cmd" "/c" "rmdir" (%native-link-name link))
                      :output nil :error-output nil :ignore-error-status t)))

(deftest local-symlink-p-portable
  (with-backend (root :local)
    (make-tree root '(("f.txt" "abc") "d/"))
    (let ((link (join root "j")))
      (%make-local-link link (join root "d"))
      (unwind-protect
           (progn
             (ok (not (symlink-p (join root "f.txt"))) "a file is not a link")
             (ok (not (symlink-p (join root "d"))) "a directory is not a link")
             (ok (not (symlink-p (join root "missing"))) "a missing path is not a link")
             (ok (symlink-p link) "the link (junction on Windows) is a link"))
        (%remove-local-link link)))))
