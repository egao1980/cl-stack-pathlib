(in-package #:cl-stack-pathlib/tests)

;;; Condition restarts: create-parents, create-file, overwrite, ignore-missing.
;;; Note: do not nest restarting ops inside ROVE OK — it handler-cases errors
;;; before outer HANDLER-BIND recoveries run.

(deftest restart-create-parents-mkdir
  (with-memory-fs ()
    (ok (signals (mkdir "/x/y/z" :parents nil) 'missing-parent))
    (with-auto-create-parents
      (mkdir "/x/y/z" :parents nil))
    (ok (directory-p "/x/y/z"))
    (ok (directory-p "/x/y"))
    (ok (directory-p "/x"))))

(deftest restart-create-parents-via-invoke
  (with-memory-fs ()
    (handler-bind ((missing-parent #'invoke-create-parents))
      (with-path-restarts
        (mkdir "/p/q" :parents nil)))
    (ok (directory-p "/p/q"))))

(deftest restart-create-file-on-read
  (with-memory-fs ()
    (ok (signals (read-bytes "/missing.txt") 'path-not-found))
    (let ((bytes (with-auto-create-file (read-bytes "/missing.txt"))))
      (ok (equalp bytes #()))
      (ok (file-p "/missing.txt")))))

(deftest restart-create-file-nested-parents
  (with-memory-fs ()
    (let ((bytes (with-auto-create-file (read-bytes "/deep/nest/f.bin"))))
      (ok (equalp bytes #()))
      (ok (directory-p "/deep/nest"))
      (ok (file-p "/deep/nest/f.bin")))))

(deftest restart-overwrite-on-exists
  (with-memory-fs ()
    (mkdir "/d" :parents t)
    (ok (signals (mkdir "/d" :exist-ok nil) 'path-exists-error))
    (let ((p (with-auto-overwrite (mkdir "/d" :exist-ok nil))))
      (ok (directory-p p)))
    (write-text "/a.txt" "one")
    (write-text "/b.txt" "two")
    (with-auto-overwrite
      (rename-path "/a.txt" "/b.txt" :replace nil))
    (ok (string= (read-text "/b.txt") "one"))
    (ng (exists-p "/a.txt"))))

(deftest restart-ignore-missing-unlink
  (with-memory-fs ()
    (ok (signals (unlink "/nope") 'path-not-found))
    (let ((result (with-auto-ignore-missing (unlink "/nope"))))
      (ok (null result)))
    (unlink "/nope" :missing-ok t)
    (ok t)))

(deftest restart-use-value-on-read
  (with-memory-fs ()
    (let ((got
            (handler-bind ((path-not-found
                            (lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'use-value #(9 9)))))
              (read-bytes "/absent"))))
      (ok (equalp got #(9 9))))))
