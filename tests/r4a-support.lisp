(in-package #:cl-stack-pathlib/tests)

;;; Helpers for the R4a suites (snapshots, areas). Built on h2a-support:
;;; WITH-BACKEND gives an empty work directory; trees, stores and areas are
;;; made in subdirectories of it so everything is cleaned up together.

;;; --- the locked fixed tree and its recorded ids ------------------------------
;;;
;;; Ids recorded from the locked encoding (brief, "R4a — locked"), computed
;;; with printf + shasum -a 256 and cross-checked with OCTETS-HASH.

(defun fixed-tree-entries ()
  `(("README.md" ,(format nil "hello~%"))
    ("src/a.lisp" ,(format nil "(defun a () 1)~%"))
    ("src/b/c.txt" "")
    (,(format nil "dir with space/~C.txt" (code-char #xE9)) ,(format nil "~C~%" (code-char #xFC)))
    "empty/"))

(defparameter +empty-tree-id+
  "sha256:1b284e6bd3fc9dfcfe2a2b144e85535a790497a1671d5a5f9c6577fb7feeaba5")

(defparameter +fixed-tree-id+
  "sha256:2939372bedfa08ff2051af61095ae505cf23e2e5e8f73be092e55d11790c93c5")

;;; The fixed tree plus bin/run.sh "#!/bin/sh\necho hi\n" with the execute bit.
(defparameter +fixed-exec-tree-id+
  "sha256:1ead55ccb6ef37ce2bb4d759da189ff2dd27720195d455c1437bce84e5d48cba")

(defun run-sh-content () (format nil "#!/bin/sh~%echo hi~%"))

(defun h (string)
  "Digest of the UTF-8 octets of STRING."
  (octets-hash (octets string)))

(defun fixed-tree-manifest-string (&key exec)
  "The locked manifest text of the fixed tree (EXEC: with bin/run.sh)."
  (let ((e-acute (code-char #xE9)) (u-uml (code-char #xFC)))
    (with-output-to-string (s)
      (format s "cl-stack-pathlib tree v1~C" #\Newline)
      (flet ((line (mode content path n)
               (format s "~A ~A ~D ~A~C" mode (h content) n path #\Newline)))
        (line "100644" (format nil "hello~%") "README.md" 9)
        (when exec (line "100755" (run-sh-content) "bin/run.sh" 10))
        (line "100644" (format nil "~C~%" u-uml) (format nil "dir with space/~C.txt" e-acute) 21)
        (line "100644" (format nil "(defun a () 1)~%") "src/a.lisp" 10)
        (line "100644" "" "src/b/c.txt" 11)))))

(defun fixed-tree-entry-summary ()
  (list (list "README.md" :file (h (format nil "hello~%")))
        (list (format nil "dir with space/~C.txt" (code-char #xE9)) :file
              (h (format nil "~C~%" (code-char #xFC))))
        (list "src/a.lisp" :file (h (format nil "(defun a () 1)~%")))
        (list "src/b/c.txt" :file (h ""))))

;;; --- backends × stores -----------------------------------------------------------

(defun subdir (work name)
  "Directory PATH WORK/NAME/ (not created)."
  (ensure-directory (join work name)))

(defun make-store (store-kind work)
  (ecase store-kind
    (:memory (make-memory-snapshot-store))
    (:directory (make-directory-snapshot-store (subdir work "store")))))

(defmacro with-r4a ((work store kind store-kind) &body body)
  "WITH-BACKEND KIND, then STORE bound to a fresh store of STORE-KIND (a
directory store lives in WORK/store/)."
  `(with-backend (,work ,kind)
     (let ((,store (make-store ,store-kind ,work)))
       ,@body)))

(defun unix-modes-p ()
  "True when the local backend has permission bits (SBCL / ECL on unix)."
  #+(and unix (or sbcl ecl)) t
  #-(and unix (or sbcl ecl)) nil)

(defun chmod (path mode)
  (cl-stack-pathlib::%local-chmod (path-pathname path) mode))

;;; --- edits that bypass pathlib -------------------------------------------------------

(defun native (path)
  (uiop:native-namestring (path-pathname path)))

(defun external-write (path content)
  "Write CONTENT (string, UTF-8) to PATH without the pathlib write API: CL:OPEN
on the local backend, the raw FS-WRITE-BYTES protocol call on memory."
  (let ((fs (path-filesystem path)))
    (etypecase fs
      (local-filesystem
       (ensure-directories-exist (path-pathname path))
       (with-open-file (s (path-pathname path) :direction :output :element-type '(unsigned-byte 8)
                                               :if-exists :supersede :if-does-not-exist :create)
         (write-sequence (octets content) s)))
      (memory-filesystem
       (fs-mkdir fs (path-pathname (parent path)) :parents t :exist-ok t)
       (fs-write-bytes fs (path-pathname path) (octets content))))
    path))

(defun external-delete (path)
  (let ((fs (path-filesystem path)))
    (etypecase fs
      (local-filesystem (delete-file (path-pathname path)))
      (memory-filesystem (fs-unlink fs (path-pathname path))))))

(defun run-sh (dir script)
  "Run SCRIPT with /bin/sh in the directory DIR (a separate process)."
  (uiop:run-program (list "/bin/sh" "-c" (format nil "cd \"$1\" && ~A" script) "sh" (native dir))
                    :output nil :error-output :string))

;;; --- summaries for comparisons -------------------------------------------------------

(defun change-summary (changes)
  "((kind path) ...) of FS-CHANGE values, in order."
  (mapcar (lambda (c) (list (fs-change-kind c) (fs-change-path c))) changes))

(defun change-details (changes)
  "((kind path old-mode new-mode old-hash new-hash) ...)."
  (mapcar (lambda (c) (list (fs-change-kind c) (fs-change-path c)
                            (fs-change-old-mode c) (fs-change-new-mode c)
                            (fs-change-old-hash c) (fs-change-new-hash c)))
          changes))

(defun entry-summary (entries)
  "((path mode hash) ...) of SNAPSHOT-ENTRY values, in order."
  (mapcar (lambda (e) (list (snapshot-entry-path e) (snapshot-entry-mode e) (snapshot-entry-hash e)))
          entries))

(defun text-at (root relpath)
  (read-text (join root relpath)))

(defun all-files (root)
  "Sorted relative names of every file under ROOT (empty when ROOT is missing)."
  (if (exists-p root) (walk-files root) '()))

(defun blob-file (store id)
  "The file a directory store keeps the blob ID in (locked layout)."
  (let ((hex (subseq id (length "sha256:"))))
    (join (directory-snapshot-store-root store) "blobs" "sha256" (subseq hex 0 2) (subseq hex 2))))

;;; --- areas ---------------------------------------------------------------------------

(defun area-tree (base)
  "The base tree of the area suites. With :IGNORE :AUTO the area gets
.gitignore a.txt b.txt run.sh src/c.lisp (and the empty directory empty/)."
  (make-tree base `((".gitignore" ,(format nil "*.log~%build/~%"))
                    ("a.txt" "aaa") ("b.txt" "bbb") ("run.sh" "echo")
                    ("src/c.lisp" "(c)") "empty/"
                    ("x.log" "log") ("build/out.o" "o") (".git/HEAD" "ref"))))

(defparameter +area-tree-files+
  '(".git/HEAD" ".gitignore" "a.txt" "b.txt" "build/out.o" "run.sh" "src/c.lisp" "x.log"))

(defparameter +area-copied-files+
  '(".gitignore" "a.txt" "b.txt" "run.sh" "src/c.lisp"))

(defun fresh-area (work tag &rest open-keys)
  "Make WORK/base-TAG/ with AREA-TREE and open an area over it in
WORK/area-TAG/ with :IGNORE :AUTO (unless OPEN-KEYS say otherwise).
Returns (values area base)."
  (let ((base (subdir work (format nil "base-~A" tag))))
    (area-tree base)
    (values (apply #'open-area base
                   (append open-keys
                           (list :ignore :auto
                                 :directory (subdir work (format nil "area-~A" tag)))))
            base)))

(defun same-directory-p (a b)
  (string= (string-right-trim "/" (as-posix a)) (string-right-trim "/" (as-posix b))))

(defun under-p (path dir)
  (uiop:string-prefix-p (concatenate 'string (string-right-trim "/" (as-posix dir)) "/")
                        (as-posix path)))
