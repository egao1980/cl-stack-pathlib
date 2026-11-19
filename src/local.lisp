(in-package #:cl-stack-pathlib)

(defclass local-filesystem (filesystem)
  ()
  (:default-initargs :name "file"))

(defun make-local-filesystem ()
  (make-instance 'local-filesystem))

;;; ECL has no lstat/readlink/symlink bindings in EXT/SI. A :CSTRING argument
;;; only passes base characters, so names go through %ECL-C (UTF-8 octets as a
;;; base-string, the bytes OPEN uses for the same name) and readlink's result
;;; comes back through %ECL-LISP.
#+(and ecl unix)
(progn
  (ffi:clines "#include <unistd.h>" "#include <limits.h>"
              "#include <stdio.h>" "#include <sys/types.h>" "#include <sys/stat.h>")
  (defun %ecl-c (string)
    "STRING as a base-string holding its UTF-8 octets, one per character."
    (map 'base-string #'code-char (ext:string-to-octets string :external-format :utf-8)))
  (defun %ecl-lisp (base-string)
    "Inverse of %ECL-C."
    (ext:octets-to-string (map '(vector (unsigned-byte 8)) #'char-code base-string)
                          :external-format :utf-8))
  (defun %ecl-rename (from to)
    (let ((from (%ecl-c from)) (to (%ecl-c to)))
      (ffi:c-inline (from to) (:cstring :cstring) :bool "rename(#0, #1) == 0" :one-liner t)))
  (defun %ecl-chmod (namestring mode)
    (let ((namestring (%ecl-c namestring)))
      (ffi:c-inline (namestring mode) (:cstring :int) :bool
                    "chmod(#0, (mode_t)#1) == 0" :one-liner t)))
  (defun %ecl-stat-mode (namestring follow)
    "st_mode of NAMESTRING (stat, or lstat when FOLLOW is NIL), or -1."
    (let ((namestring (%ecl-c namestring)))
      (ffi:c-inline (namestring follow) (:cstring :bool) :int
        "{ struct stat st; int r = (#1) ? stat(#0, &st) : lstat(#0, &st);
           @(return) = (r == 0) ? (int)st.st_mode : -1; }")))
  (defun %ecl-access-x-p (namestring)
    (let ((namestring (%ecl-c namestring)))
      (ffi:c-inline (namestring) (:cstring) :bool "access(#0, X_OK) == 0" :one-liner t)))
  (defun %ecl-symlink (target link)
    (let ((target (%ecl-c target)) (link (%ecl-c link)))
      (ffi:c-inline (target link) (:cstring :cstring) :bool "symlink(#0, #1) == 0" :one-liner t)))
  (defun %ecl-readlink (namestring)
    (let* ((namestring (%ecl-c namestring))
           (raw (ffi:c-inline (namestring) (:cstring) :object
                  "{ char buf[PATH_MAX]; ssize_t n = readlink(#0, buf, sizeof(buf) - 1);
                     if (n < 0) @(return) = ECL_NIL;
                     else { buf[n] = 0; @(return) = ecl_make_simple_base_string(buf, n); } }")))
      (and raw (%ecl-lisp raw)))))

(defmethod fs-parse ((fs local-filesystem) designator &key directory)
  (let ((pn (etypecase designator
              (pathname designator)
              (string (%parse-posix designator))
              (path (path-pathname designator)))))
    (if directory (uiop:ensure-directory-pathname pn) pn)))

(defmethod fs-join ((fs local-filesystem) base relative)
  (uiop:merge-pathnames*
   (fs-parse fs relative)
   (uiop:ensure-directory-pathname (fs-parse fs base))))

(defmethod fs-parent ((fs local-filesystem) pathname)
  (%logical-parent (fs-parse fs pathname)))

(defmethod fs-name ((fs local-filesystem) pathname)
  (%file-namestring* (fs-parse fs pathname)))

(defmethod fs-absolute ((fs local-filesystem) pathname &key (defaults (uiop:getcwd)))
  (let ((pn (fs-parse fs pathname))
        (base (uiop:ensure-directory-pathname (fs-parse fs defaults))))
    (if (uiop:absolute-pathname-p pn) pn (uiop:merge-pathnames* pn base))))

(defmethod fs-resolve ((fs local-filesystem) pathname &key (strict t))
  "Resolve to a physical path. STRICT signals if the leaf does not exist.
UIOP TRUENAMIZE alone is not enough — it succeeds for missing leaves."
  (let* ((abs (fs-absolute fs pathname))
         (true (uiop:probe-file* abs :truename t)))
    (cond (true true)
          (strict
           (%restart-path-not-found fs abs
                                    :message "path does not exist"
                                    :allow-create-file t
                                    :allow-create-directory t
                                    :allow-ignore nil))
          (t (or (ignore-errors (uiop:truenamize abs)) abs)))))

(defmethod fs-normpath ((fs local-filesystem) pathname)
  "Collapse `.` / `..` in PATHNAME (NIO normalize / pathlib-ish)."
  (let ((pn (fs-parse fs pathname)))
    (multiple-value-bind (abs? stack dir?) (%collapse-dot-segments pn)
      (fs-parse fs
                (cond ((and abs? (null stack)) "/")
                      (abs? (format nil "/~{~A~^/~}" stack))
                      ((null stack) ".")
                      (t (format nil "~{~A~^/~}" stack)))
                :directory dir?))))

(defun %passwd-home (username)
  (with-open-file (in "/etc/passwd" :if-does-not-exist nil)
    (loop for line = (read-line in nil nil) while line
          do (let ((parts (uiop:split-string line :separator ":")))
               (when (and (>= (length parts) 7) (string= username (first parts)))
                 (return (uiop:parse-native-namestring (nth 5 parts))))))))

(defun %expand-tilde-string (s)
  (cond ((= (length s) 1) (user-homedir-pathname))
        ((char= (char s 1) #\/) (merge-pathnames (subseq s 2) (user-homedir-pathname)))
        (t (let* ((slash (position #\/ s :start 1))
                  (username (subseq s 1 slash))
                  (rest (if slash (subseq s slash) ""))
                  (home (%passwd-home username)))
             (if home
                 (merge-pathnames rest home)
                 (uiop:parse-native-namestring s))))))

(defmethod fs-expanduser ((fs local-filesystem) pathname)
  (let* ((pn (fs-parse fs pathname))
         (s (namestring pn)))
    (if (and (plusp (length s)) (char= (char s 0) #\~))
        (fs-parse fs (uiop:parse-native-namestring (uiop:native-namestring (%expand-tilde-string s))))
        pn)))

(defmethod fs-exists-p ((fs local-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (or (uiop:file-exists-p pn) (uiop:directory-exists-p pn))))

(defmethod fs-file-p ((fs local-filesystem) pathname)
  (and (uiop:file-exists-p (fs-parse fs pathname)) t))

(defmethod fs-directory-p ((fs local-filesystem) pathname)
  (and (uiop:directory-exists-p (fs-parse fs pathname)) t))

(defmethod fs-symlink-p ((fs local-filesystem) pathname)
  #+sbcl
  (handler-case
      (let ((stat (sb-posix:lstat (namestring (fs-parse fs pathname)))))
        (sb-posix:s-islnk (sb-posix:stat-mode stat)))
    (error () nil))
  #+ecl
  (eq (ignore-errors (ext:file-kind (namestring (fs-parse fs pathname)) nil)) :link)
  #-(or sbcl ecl) nil)

(defmethod fs-readable-p ((fs local-filesystem) pathname)
  (ignore-errors
    (with-open-file (s (fs-parse fs pathname) :direction :input :if-does-not-exist nil)
      (and s t))))

(defmethod fs-writable-p ((fs local-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (or (ignore-errors
          (with-open-file (s pn :direction :output :if-exists :append :if-does-not-exist nil)
            (and s t)))
        (let ((parent (fs-parent fs pn)))
          (and (fs-directory-p fs parent)
               (ignore-errors
                 (let ((tmp (merge-pathnames (format nil ".cl-stack-pathlib-~A" (random 1e9)) parent)))
                   (unwind-protect
                        (progn (with-open-file (s tmp :direction :output :if-exists :error
                                                  :if-does-not-exist :create))
                               t)
                     (ignore-errors (delete-file tmp))))))))))

(defmethod fs-executable-p ((fs local-filesystem) pathname)
  #+sbcl
  (handler-case
      (let ((mode (sb-posix:stat-mode (sb-posix:stat (namestring (fs-parse fs pathname))))))
        (plusp (logand mode sb-posix:s-ixusr)))
    (error () nil))
  #+ecl
  (%ecl-access-x-p (namestring (fs-parse fs pathname)))
  #-(or sbcl ecl) nil)

(defmethod fs-file-size ((fs local-filesystem) pathname)
  (with-open-file (s (fs-parse fs pathname) :direction :input :element-type '(unsigned-byte 8)
                      :if-does-not-exist :error)
    (file-length s)))

(defmethod fs-last-modified ((fs local-filesystem) pathname)
  (file-write-date (fs-parse fs pathname)))

(defun %local-files (dir)
  "Non-directory entries of the directory DIR, as DIR/name. A symlink is
listed under its own name, never as its target: ECL's UIOP:DIRECTORY-FILES
resolves links (a link to real.txt shows up as a second real.txt), so ECL
asks DIRECTORY not to, and rebuilds each entry on DIR (ECL's DIRECTORY
returns DIR's truename, /private/var/... for /var/... on macOS)."
  #+ecl
  (loop for e in (directory (merge-pathnames uiop:*wild-file-for-directory* dir)
                            :resolve-symlinks nil)
        unless (uiop:directory-pathname-p e)
          collect (make-pathname :name (pathname-name e) :type (pathname-type e)
                                 :version nil :defaults dir))
  #-ecl
  (uiop:directory-files dir))

(defun %local-subdirectories (dir)
  "Subdirectories of the directory DIR, as DIR/name/ (see %LOCAL-FILES)."
  #+ecl
  (loop for e in (directory (merge-pathnames (make-pathname :directory '(:relative :wild)) dir)
                            :resolve-symlinks nil)
        collect (merge-pathnames
                 (make-pathname :directory (list :relative (car (last (pathname-directory e))))
                                :name nil :type nil :version nil)
                 dir))
  #-ecl
  (uiop:subdirectories dir))

(defmethod fs-iterdir ((fs local-filesystem) pathname)
  (let ((dir (uiop:ensure-directory-pathname (fs-parse fs pathname))))
    (unless (uiop:directory-exists-p dir)
      (error 'path-not-found :filesystem fs :path dir))
    (append (%local-files dir) (%local-subdirectories dir))))

(defmethod fs-glob ((fs local-filesystem) pathname pattern &key recursive ignore)
  (let* ((base (uiop:ensure-directory-pathname (fs-absolute fs pathname)))
         (pat (if recursive
                  (merge-pathnames
                   (make-pathname :directory '(:relative :wild-inferiors)
                                  :defaults (%wild-pattern pattern))
                   base)
                  (merge-pathnames (%wild-pattern pattern) base)))
         ;; CL:DIRECTORY returns truenames: strip the base's truename.
         (ignored-p (%ignore-predicate fs base ignore
                                       :prefix-of (or (probe-file base) base)))
         (found (directory pat)))
    (if ignored-p
        (remove-if (lambda (p) (funcall ignored-p p (uiop:directory-pathname-p p))) found)
        found)))

(defmethod fs-walk ((fs local-filesystem) pathname &key (top-down t) (follow-symlinks nil) ignore)
  (declare (ignore follow-symlinks))
  (let* ((out '())
         (root (uiop:ensure-directory-pathname (fs-absolute fs pathname)))
         (ignored-p (%ignore-predicate fs root ignore)))
    (labels ((walk* (dir)
               (let* ((dir (uiop:ensure-directory-pathname dir))
                      (files (ignore-errors (%local-files dir)))
                      (subs (ignore-errors (%local-subdirectories dir))))
                 (when ignored-p
                   (setf files (remove-if (lambda (f) (funcall ignored-p f nil)) files)
                         subs (remove-if (lambda (d) (funcall ignored-p d t)) subs)))
                 (when top-down (push (list dir files subs) out))
                 (dolist (s subs) (walk* s))
                 (unless top-down (push (list dir files subs) out)))))
      (walk* root)
      (nreverse out))))

(defmethod fs-mkdir ((fs local-filesystem) pathname &key (parents t) (exist-ok t))
  (let ((dir (uiop:ensure-directory-pathname (fs-parse fs pathname))))
    (cond ((uiop:directory-exists-p dir)
           (unless exist-ok
             (restart-case
                 (error 'path-exists-error :filesystem fs :path dir)
               (overwrite ()
                 :report (lambda (s) (format s "Keep existing directory ~A" dir))
                 dir)))
           dir)
          (parents
           (ensure-directories-exist (merge-pathnames (make-pathname :name "x") dir))
           dir)
          (t
           (let ((parent (uiop:pathname-parent-directory-pathname dir)))
             (unless (uiop:directory-exists-p parent)
               (%restart-create-parents fs dir))
             #+sbcl (sb-posix:mkdir (namestring dir) #o755)
             #+ecl (si:mkdir (namestring dir) #o755)
             #-(or sbcl ecl) (uiop:run-program (list "mkdir" (uiop:unix-namestring dir))
                                      :ignore-error-status nil))
           dir))))

(defmethod fs-rmdir ((fs local-filesystem) pathname)
  (uiop:delete-empty-directory (uiop:ensure-directory-pathname (fs-parse fs pathname))))

(defmethod fs-unlink ((fs local-filesystem) pathname &key (missing-ok nil))
  (let ((pn (fs-parse fs pathname)))
    (handler-case (progn (delete-file pn) t)
      (file-error ()
        (return-from fs-unlink
          (if missing-ok
              nil
              (%restart-path-not-found fs pn
                                       :message "file does not exist"
                                       :allow-create-file nil
                                       :allow-create-directory nil
                                       :allow-ignore t)))))))

(defmethod fs-touch ((fs local-filesystem) pathname &key (exist-ok t))
  (let ((pn (fs-parse fs pathname)))
    (cond ((uiop:file-exists-p pn)
           (unless exist-ok
             (restart-case
                 (error 'path-exists-error :filesystem fs :path pn)
               (overwrite ()
                 :report (lambda (s) (format s "Keep existing file ~A" pn))
                 pn)))
           pn)
          (t
           (let ((parent (uiop:pathname-parent-directory-pathname pn)))
             (unless (or (null parent) (uiop:directory-exists-p parent))
               (%restart-create-parents fs pn)))
           (handler-case
               (with-open-file (s pn :direction :output :if-exists :error
                                  :if-does-not-exist :create)
                 pn)
             (file-error (c)
               (let ((parent (uiop:pathname-parent-directory-pathname pn)))
                 (if (or (null parent) (uiop:directory-exists-p parent))
                     (error c)
                     (%restart-create-parents fs pn)))))))))

(defmethod fs-rename ((fs local-filesystem) source target &key (replace t))
  (let ((from (fs-parse fs source))
        (to (fs-parse fs target)))
    (unless (fs-exists-p fs from)
      (%restart-path-not-found fs from
                               :message "rename source does not exist"
                               :allow-create-file nil
                               :allow-create-directory nil
                               :allow-ignore nil))
    (when (and (not replace) (fs-exists-p fs to))
      (restart-case
          (error 'path-exists-error :filesystem fs :path to)
        (overwrite ()
          :report (lambda (s) (format s "Replace existing ~A" to))
          (return-from fs-rename (fs-rename fs from to :replace t)))))
    (rename-file from to)
    to))

(defmethod fs-copy ((fs local-filesystem) source target &key (replace t))
  (let ((from (fs-parse fs source))
        (to (fs-parse fs target)))
    (unless (fs-exists-p fs from)
      (%restart-path-not-found fs from
                               :message "copy source does not exist"
                               :allow-create-file nil
                               :allow-create-directory nil
                               :allow-ignore nil))
    (when (and (not replace) (fs-exists-p fs to))
      (restart-case
          (error 'path-exists-error :filesystem fs :path to)
        (overwrite ()
          :report (lambda (s) (format s "Replace existing ~A" to))
          (return-from fs-copy (fs-copy fs from to :replace t)))))
    (let ((parent (uiop:pathname-parent-directory-pathname to)))
      (unless (or (null parent) (uiop:directory-exists-p parent))
        (%restart-create-parents fs to)))
    (uiop:copy-file from to)
    to))
(defmethod fs-create-symlink ((fs local-filesystem) link target)
  ;; sb-posix:symlink is Unix-only (absent on Windows SBCL).
  #+(and sbcl unix)
  (progn
    (sb-posix:symlink (namestring (fs-parse fs target))
                      (namestring (fs-parse fs link)))
    (fs-parse fs link))
  #+(and ecl unix)
  (progn
    (unless (%ecl-symlink (namestring (fs-parse fs target))
                          (namestring (fs-parse fs link)))
      (error 'path-error :filesystem fs :path link :message "symlink failed"))
    (fs-parse fs link))
  #-(and (or sbcl ecl) unix)
  (%unsupported fs 'fs-create-symlink link))

(defmethod fs-read-symlink ((fs local-filesystem) pathname)
  #+(and sbcl unix)
  (pathname (sb-posix:readlink (namestring (fs-parse fs pathname))))
  #+(and ecl unix)
  (let ((target (%ecl-readlink (namestring (fs-parse fs pathname)))))
    (if target
        (pathname target)
        (error 'path-error :filesystem fs :path pathname :message "readlink failed")))
  #-(and (or sbcl ecl) unix)
  (%unsupported fs 'fs-read-symlink pathname))

(defmethod fs-read-bytes ((fs local-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (handler-case
        (with-open-file (s pn :direction :input
                           :element-type '(unsigned-byte 8) :if-does-not-exist :error)
          (let* ((len (file-length s))
                 (buf (make-array len :element-type '(unsigned-byte 8))))
            (read-sequence buf s)
            buf))
      (file-error ()
        (return-from fs-read-bytes
          (%restart-path-not-found fs pn
                                   :message "file does not exist"
                                   :allow-create-file t
                                   :allow-create-directory nil
                                   :allow-ignore nil))))))

(defmethod fs-write-bytes ((fs local-filesystem) pathname bytes
                           &key (append nil)
                                (if-exists (if append :append :supersede))
                                (if-does-not-exist :create))
  (let ((pn (fs-parse fs pathname)))
    (let ((parent (uiop:pathname-parent-directory-pathname pn)))
      (unless (or (null parent) (uiop:directory-exists-p parent))
        (%restart-create-parents fs pn)))
    (handler-case
        (with-open-file (s pn :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists if-exists
                           :if-does-not-exist if-does-not-exist)
          (write-sequence bytes s))
      (file-error ()
        (%restart-create-parents fs pn)))
    pn))
(defmethod fs-make-temp ((fs local-filesystem) &key (directory nil) (prefix "cl-stack-") (suffix ""))
  (let* ((dir (uiop:ensure-directory-pathname
               (or directory (uiop:temporary-directory))))
         (name (format nil "~A~A~A" prefix (get-universal-time) (random 100000))))
    (if (and suffix (plusp (length suffix)))
        (merge-pathnames (make-pathname :name name :type (string-left-trim "." suffix)) dir)
        (merge-pathnames (make-pathname :name name) dir))))

(defmethod fs-same-p ((fs local-filesystem) a b)
  (handler-case
      (equal (uiop:truenamize (fs-absolute fs a))
             (uiop:truenamize (fs-absolute fs b)))
    (error ()
      (equal (namestring (fs-absolute fs a))
             (namestring (fs-absolute fs b))))))

(defmethod fs-as-uri ((fs local-filesystem) pathname)
  (format nil "file://~A" (uiop:unix-namestring (fs-absolute fs pathname))))

;;; --- H2a: ranged / streaming reads, hashes, atomic writes, stat -------------

(defun %native (pathname)
  "OS namestring of PATHNAME (no wildcard escapes, unlike NAMESTRING)."
  (uiop:native-namestring pathname))

(defun %local-not-found (fs pathname)
  (error 'path-not-found :filesystem fs :path pathname :message "file does not exist"))

(defun %local-raw-stat (pathname follow)
  "(values st_mode size mtime) of PATHNAME via stat(2) (lstat(2) when FOLLOW
is NIL), or NIL when it fails or the implementation has no binding. SIZE
and MTIME may be NIL (then the caller asks the portable way)."
  #+(and sbcl unix)
  (let ((st (handler-case (if follow
                              (sb-posix:stat (%native pathname))
                              (sb-posix:lstat (%native pathname)))
              (sb-posix:syscall-error () nil))))
    (when st
      (values (sb-posix:stat-mode st)
              (sb-posix:stat-size st)
              ;; Unix epoch → universal time.
              (+ (sb-posix:stat-mtime st) 2208988800))))
  #+(and ecl unix)
  (let ((mode (%ecl-stat-mode (%native pathname) follow)))
    (when (>= mode 0) (values mode nil nil)))
  #-(and (or sbcl ecl) unix)
  (progn pathname follow nil))

(defun %local-chmod (pathname mode)
  #+(and sbcl unix) (sb-posix:chmod (%native pathname) mode)
  #+(and ecl unix) (%ecl-chmod (%native pathname) mode)
  #-(and (or sbcl ecl) unix) (progn pathname mode nil))

(defun %local-rename (fs from to)
  "Rename FROM over TO, replacing TO: rename(2) on POSIX, MoveFileExW with
MOVEFILE_REPLACE_EXISTING on Windows SBCL. Never CL:RENAME-FILE there,
which merges pathname defaults into TO."
  #+(and sbcl unix)
  (handler-case (sb-posix:rename (%native from) (%native to))
    (sb-posix:syscall-error (e)
      (error 'path-error :filesystem fs :path to
                         :message (format nil "rename(2) failed: ~A" e))))
  #+(and ecl unix)
  (unless (%ecl-rename (%native from) (%native to))
    (error 'path-error :filesystem fs :path to :message "rename(2) failed"))
  ;; Windows SBCL: SB-UNIX:UNIX-RENAME is MoveFileExW with
  ;; MOVEFILE_REPLACE_EXISTING (a same-volume replace, which is what an
  ;; atomic write needs) on the native names.
  #+(and sbcl (or win32 windows))
  (multiple-value-bind (ok err)
      (sb-unix:unix-rename (coerce (%native from) 'simple-string)
                           (coerce (%native to) 'simple-string))
    (unless ok
      (error 'path-error :filesystem fs :path to
                         :message (format nil "MoveFileEx failed: ~A" (sb-int:strerror err)))))
  #-(or (and (or sbcl ecl) unix) (and sbcl (or win32 windows)))
  ;; CL:RENAME-FILE merges FROM into a typeless TO (".gitignore" would become
  ;; ".gitignore.tmp"); an :UNSPECIFIC type stops that.
  (progn fs (uiop:rename-file-overwriting-target
             from (if (pathname-type to) to (make-pathname :type :unspecific :defaults to)))))

(defmethod fs-read-range ((fs local-filesystem) pathname start end)
  (%check-range start end)
  (let ((pn (fs-parse fs pathname)))
    (with-open-file (s pn :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (unless s (%local-not-found fs pn))
      (let* ((n (file-length s))
             (b (min start n))
             (e (max b (min (or end n) n)))
             (buf (make-array (- e b) :element-type '(unsigned-byte 8))))
        (file-position s b)
        (let ((got (read-sequence buf s)))
          (if (= got (length buf)) buf (subseq buf 0 got)))))))

(defmethod fs-read-lines ((fs local-filesystem) pathname &key (start 1) end)
  ;; Octet stream split at LF, each line decoded like READ-TEXT: same result
  ;; as the portable default, but reading stops at line END + 1.
  (check-type start (integer 1))
  (check-type end (or null (integer 0)))
  (let ((pn (fs-parse fs pathname)))
    (with-open-file (s pn :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (unless s (%local-not-found fs pn))
      (let ((buf (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
            (out '())
            (no 0))
        (flet ((next-line ()
                 ;; Octets of the next line without its LF, or NIL at EOF.
                 (setf (fill-pointer buf) 0)
                 (loop for b = (read-byte s nil nil)
                       do (cond ((null b) (return (and (plusp (fill-pointer buf)) buf)))
                                ((= b 10) (return buf))
                                (t (vector-push-extend b buf))))))
          (loop for line = (next-line)
                while line
                do (incf no)
                   (cond ((and end (> no end))
                          (return-from fs-read-lines (values (nreverse out) t)))
                         ((>= no start)
                          (let ((len (fill-pointer line)))
                            (when (and (plusp len) (= 13 (aref line (1- len))))
                              (decf (fill-pointer line))))
                          (push (coerce (utf8-octets-to-string line) 'simple-string) out))))
          (values (nreverse out) nil))))))

(defmethod fs-content-hash ((fs local-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (when (fs-directory-p fs pn)
      (error 'path-error :filesystem fs :path pn :message "is a directory"))
    (with-open-file (s pn :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (unless s (%local-not-found fs pn))
      (let ((state (%sha256-init))
            (buf (make-array 65536 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence buf s)
              while (plusp n)
              do (%sha256-update state buf :end n))
        (%sha256-final state)))))

(defmethod fs-open ((fs local-filesystem) pathname &key (direction :input) (element-type '(unsigned-byte 8)))
  (let ((kind (%check-open-args fs pathname direction element-type))
        (pn (fs-parse fs pathname)))
    (when (fs-directory-p fs pn)
      (error 'path-error :filesystem fs :path pn :message "is a directory"))
    (or (if (eq kind :characters)
            (open pn :direction :input :element-type 'character
                     :external-format :utf-8 :if-does-not-exist nil)
            (open pn :direction :input :element-type '(unsigned-byte 8)
                     :if-does-not-exist nil))
        (%local-not-found fs pn))))

(defvar *atomic-random-state* nil
  "Random state for temp file names, seeded on first use (so images started
from one core do not pick the same names).")

(defun %atomic-temp-name (target)
  (let ((state (or *atomic-random-state*
                   (setf *atomic-random-state* (make-random-state t)))))
    (format nil ".~A.~(~8,'0x~).tmp" (%entry-name target) (random #x100000000 state))))

(defun %local-write-target (fs pathname)
  "PATHNAME, or the file its symlink chain ends in (a dangling link's target
included), so atomic writes go through links."
  (let ((pn pathname))
    (loop repeat 40
          while (fs-symlink-p fs pn)
          do (let ((to (fs-read-symlink fs pn)))
               (setf pn (if (uiop:absolute-pathname-p to)
                            to
                            (uiop:merge-pathnames* to (uiop:pathname-directory-pathname pn))))))
    pn))

(defmethod fs-write-atomic ((fs local-filesystem) pathname bytes &key expected-hash)
  (let* ((pn (fs-parse fs pathname))
         (vec (%coerce-octets bytes))
         (target (%local-write-target fs (fs-absolute fs pn))))
    (when (fs-directory-p fs target)
      (error 'path-error :filesystem fs :path pn :message "target is a directory"))
    (multiple-value-bind (proceed current)
        (%check-expected-hash fs pn expected-hash
                              (lambda ()
                                (and (fs-file-p fs target) (fs-content-hash fs target))))
      (unless proceed
        (return-from fs-write-atomic (values pn current nil))))
    (ensure-directories-exist target)
    (let ((mode (let ((m (%local-raw-stat target t)))
                  (and m (logand m #o7777))))
          (tmp (uiop:merge-pathnames* (uiop:parse-unix-namestring (%atomic-temp-name target))
                                      (uiop:pathname-directory-pathname target)))
          (created nil)
          (done nil))
      (unwind-protect
           (progn
             (with-open-file (s tmp :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :error :if-does-not-exist :create)
               (setf created t)
               (write-sequence vec s)
               (finish-output s))
             (when mode (%local-chmod tmp mode))
             (%local-rename fs tmp target)
             (setf done t))
        (when (and created (not done))
          (handler-case (delete-file tmp)
            (file-error () nil)))))
    (values pn (octets-hash vec) t)))

(defmethod fs-stat ((fs local-filesystem) pathname &key (follow-symlinks t))
  (let ((pn (fs-parse fs pathname)))
    (multiple-value-bind (raw size mtime) (%local-raw-stat pn follow-symlinks)
      (cond ((null raw)
             (if (or (fs-exists-p fs pn) (fs-symlink-p fs pn))
                 (call-next-method)     ; no stat binding: portable, mode NIL
                 (error 'path-not-found :filesystem fs :path pn
                                        :message "path does not exist")))
            (t
             (let ((kind (case (logand raw #o170000)
                           (#o040000 :directory)
                           (#o100000 :file)
                           (#o120000 :symlink)
                           (t :other))))
               (make-instance 'file-stat
                              :path pn :kind kind
                              :size (if (eq kind :file) (or size (fs-file-size fs pn)) 0)
                              :mtime (or mtime
                                         (and (not (eq kind :symlink))
                                              (file-write-date pn)))
                              :mode (logand raw #o7777)
                              :filesystem fs)))))))

(defvar *local-filesystem* (make-local-filesystem)
  "Process-wide local FS used by file:// URIs and the default *FILESYSTEM*.")

(defun %parse-file-uri (uri)
  (let* ((rest (if (and (>= (length uri) 7)
                        (string-equal (subseq uri 0 7) "file://"))
                   (subseq uri 7)
                   uri))
         (path (cond ((and (>= (length rest) 9)
                           (string-equal (subseq rest 0 9) "localhost"))
                      (subseq rest 9))
                     (t rest))))
    (when (zerop (length path))
      (error 'path-error :path uri :message "file:// URI missing path"))
    (make-path (fs-parse *local-filesystem* path) :filesystem *local-filesystem*)))

(register-uri-scheme "file" #'%parse-file-uri)

(setf *filesystem* *local-filesystem*)
