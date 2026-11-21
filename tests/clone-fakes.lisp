(in-package #:cl-stack-pathlib/tests)

;;; R4c-0 fake clone mechanisms, registered only inside WITH-CLONE-FAKES (a
;;; LET binding of *CLONE-MECHANISMS*), so no other test sees them. Every
;;; fake "clones" by copying through the public protocol (FS-READ-BYTES /
;;; FS-WRITE-BYTES), except :FAKE-SHARED, which hard-links (the source and
;;; the clone then share data, and the clone suite must catch it).
;;;
;;;   :FAKE-CLONE         exact, rank 10, always available
;;;   :FAKE-TREE          rank 10; a CLONE-TREE hook that clones the whole
;;;                       tree at once (through FS-CLONE-TREE :FAKE-CLONE)
;;;   :FAKE-DECLINE       rank 10; a CLONE-TREE hook that always declines
;;;   :FAKE-SHARED        SBCL unix, local only: a hard link (shares data)
;;;   :FAKE-LOSSY         a CLONE-TREE hook that loses mtimes and modes
;;;   :FAKE-PARTIAL       hands files over 1 MiB back to the copy rung
;;;   :FAKE-SIGNAL        signals CLONE-ERROR for every file named a.txt
;;;   :FAKE-CROSS-VOLUME  unavailable (:CROSS-VOLUME) when only the
;;;                       destination is under a directory "other-volume"
;;;   :FAKE-UNSUPPORTED   rank 50, never available (:UNSUPPORTED-VOLUME)
;;;   :FAKE-UNAVAILABLE   rank 40, never available, no reason given
;;;   :FAKE-LOCAL-ONLY    available on a LOCAL-FILESYSTEM only
;;;                       (:UNSUPPORTED-FILESYSTEM elsewhere)
;;;   :FAKE-RECORDER      rank 5; logs every call with its arguments
;;;
;;; *FAKE-LOG* collects (what name . arguments) entries, newest first.

(defvar *fake-log* '())

(defun fake-log (&rest entry) (push entry *fake-log*))

(defun fake-posix (fs pathname)
  (as-posix (make-path pathname :filesystem fs)))

(defun fake-copy-file (fs source destination)
  (fs-write-bytes fs destination (fs-read-bytes fs source))
  t)

(defun fake-clone-file (name)
  (lambda (fs source destination)
    (fake-log :clone-file name (fake-posix fs source) (fake-posix fs destination))
    (fake-copy-file fs source destination)))

(defun fake-tree-hook (fs source destination matcher)
  (fake-log :clone-tree :fake-tree (fake-posix fs source) (fake-posix fs destination) matcher)
  (clone-report-cloned (fs-clone-tree fs source destination :ignore matcher :mechanism :fake-clone)))

(defun fake-decline-hook (fs source destination matcher)
  (fake-log :clone-tree :fake-decline (fake-posix fs source) (fake-posix fs destination) matcher)
  nil)

(defun fake-lossy-hook (fs source destination matcher)
  ;; The right tree, then every regular file gets a new mtime and mode.
  (let ((report (fs-clone-tree fs source destination :ignore matcher :mechanism :fake-clone)))
    (dolist (triple (fs-walk fs destination))
      (dolist (f (second triple))
        (unless (fs-symlink-p fs f)
          (fs-set-mtime fs f (get-universal-time))
          (fs-chmod fs f #o666))))
    (clone-report-cloned report)))

(defun fake-hard-link (fs source destination)
  (declare (ignorable fs source destination))
  #+(and sbcl unix)
  (progn (sb-posix:link (uiop:native-namestring source) (uiop:native-namestring destination))
         t)
  #-(and sbcl unix)
  (error "hard links need SBCL on unix"))

(defun fake-partial-file (fs source destination)
  (if (> (fs-file-size fs source) (* 1024 1024))
      (values nil :clone-failed)
      (fake-copy-file fs source destination)))

(defun fake-signal-file (fs source destination)
  (if (string= "a.txt" (fs-name fs source))
      (error 'clone-error :filesystem fs :path source :mechanism :fake-signal
                          :operation "fake-clone" :code 5
                          :message "the fake refuses a.txt")
      (fake-copy-file fs source destination)))

(defun fake-cross-volume-p (fs source destination)
  (if (and (search "other-volume" (fake-posix fs destination))
           (not (search "other-volume" (fake-posix fs source))))
      (values nil :cross-volume)
      t))

(defun fake-recorder-p (fs source destination)
  (fake-log :available-p :fake-recorder (fake-posix fs source) (fake-posix fs destination))
  t)

(defun register-fake-clone (name)
  (ecase name
    (:fake-clone (register-clone-mechanism :fake-clone (fake-clone-file :fake-clone) :rank 10))
    (:fake-tree
     (register-clone-mechanism :fake-clone (fake-clone-file :fake-clone) :rank 10)
     (register-clone-mechanism :fake-tree (fake-clone-file :fake-tree) :rank 10
                                                                       :clone-tree 'fake-tree-hook))
    (:fake-decline
     (register-clone-mechanism :fake-decline (fake-clone-file :fake-decline) :rank 10
                                                                             :clone-tree 'fake-decline-hook))
    (:fake-shared
     (register-clone-mechanism :fake-shared 'fake-hard-link
                               :available-p (lambda (fs s d) (declare (ignore s d))
                                              (if (typep fs 'local-filesystem)
                                                  t
                                                  (values nil :unsupported-filesystem)))))
    (:fake-lossy
     (register-clone-mechanism :fake-clone (fake-clone-file :fake-clone) :rank 10)
     (register-clone-mechanism :fake-lossy (fake-clone-file :fake-lossy) :clone-tree 'fake-lossy-hook))
    (:fake-partial (register-clone-mechanism :fake-partial 'fake-partial-file))
    (:fake-signal (register-clone-mechanism :fake-signal 'fake-signal-file))
    (:fake-cross-volume
     (register-clone-mechanism :fake-cross-volume (fake-clone-file :fake-cross-volume)
                               :available-p 'fake-cross-volume-p))
    (:fake-unsupported
     (register-clone-mechanism :fake-unsupported
                               (lambda (fs s d) (declare (ignore fs s d))
                                 (error "the unsupported fake was called"))
                               :rank 50
                               :available-p (lambda (fs s d) (declare (ignore fs s d))
                                              (values nil :unsupported-volume))))
    (:fake-unavailable
     (register-clone-mechanism :fake-unavailable
                               (lambda (fs s d) (declare (ignore fs s d))
                                 (error "the unavailable fake was called"))
                               :rank 40
                               :available-p (lambda (fs s d) (declare (ignore fs s d)) nil)))
    (:fake-local-only
     (register-clone-mechanism :fake-local-only (fake-clone-file :fake-local-only)
                               :available-p (lambda (fs s d) (declare (ignore s d))
                                              (if (typep fs 'local-filesystem)
                                                  t
                                                  (values nil :unsupported-filesystem)))))
    (:fake-recorder
     (register-clone-mechanism :fake-recorder (fake-clone-file :fake-recorder) :rank 5
                                                                               :available-p 'fake-recorder-p))))

(defmacro with-clone-fakes ((&rest names) &body body)
  "Run BODY with the fake clone mechanisms NAMES registered (in that order)
in a LET binding of *CLONE-MECHANISMS*, and a fresh *FAKE-LOG*."
  `(let ((*clone-mechanisms* *clone-mechanisms*)
         (*fake-log* '()))
     (dolist (n ',names) (register-fake-clone n))
     ,@body))

(defun fake-log-of (what &optional name)
  "Entries of *FAKE-LOG* for WHAT (and NAME), oldest first."
  (reverse (remove-if-not (lambda (e) (and (eq (first e) what) (or (null name) (eq (second e) name))))
                          *fake-log*)))

;;; --- tree helpers for the clone tests ------------------------------------------------

(defparameter +old-time+ (+ 1000000000 2208988800))

(defun clone-base (base)
  "A small base tree: .gitignore (*.log, build/), a.txt, b.txt, run.sh,
src/c.lisp, an empty directory, x.log and build/out.o (ignored by :AUTO), a
.git directory; plus big.bin (2 MiB) for the partial fake."
  (make-tree base `((".gitignore" ,(format nil "*.log~%build/~%"))
                    ("a.txt" "aaa") ("b.txt" "bbb") ("run.sh" "echo")
                    ("src/c.lisp" "(c)") "empty/"
                    ("x.log" "log") ("build/out.o" "o") (".git/HEAD" "ref")
                    ("big.bin" ,(make-array (* 2 1024 1024) :element-type '(unsigned-byte 8)
                                                            :initial-element 7)))))

(defparameter +clone-auto-files+
  '(".gitignore" "a.txt" "b.txt" "big.bin" "run.sh" "src/c.lisp"))

(defun tree-id (root &optional ignore)
  (snapshot root :ignore ignore :store (make-memory-snapshot-store)))

(defun local-mtimes-p ()
  #+(and sbcl unix) t
  #-(and sbcl unix) nil)
