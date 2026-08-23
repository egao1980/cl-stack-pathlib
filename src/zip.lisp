(in-package #:cl-stack-pathlib)

;;; Read-only ZIP filesystem + zip:// URI.
;;;
;;; Archive half is an RFC 8089 path-absolute (VFS / jar:file: shape):
;;;   zip:///tmp/data.zip!/countries/DE.sexp
;;;   zip:///C:/Users/foo/data.zip!/countries/DE.sexp
;;;
;;; Also accepted: zip://C:/… (do not treat C as host), zip:file:///C:/…!/…
;;; Never uiop:unix-namestring the archive — it drops the drive letter.
;;; Entries are posix paths rooted at / inside the archive (APPNOTE: / only, no drive).
;;; Method 0 (stored) and 8 (deflate via chipz).

(defparameter +zip-local-sig+ #x04034b50)
(defparameter +zip-central-sig+ #x02014b50)
(defparameter +zip-eocd-sig+ #x06054b50)

(defclass zip-filesystem (filesystem)
  ((archive :initarg :archive :reader zip-filesystem-archive)
   (entries :initform (make-hash-table :test #'equal) :reader zip-filesystem-entries)
   (bytes :initarg :bytes :initform nil :accessor zip-filesystem-bytes))
  (:default-initargs :name "zip"))

(defun zip-filesystem-p (x) (typep x 'zip-filesystem))

(defstruct (%zip-entry (:constructor %make-zip-entry))
  name method crc compressed-size uncompressed-size header-offset dos-date dos-time)

(defvar *zip-filesystem-cache* (make-hash-table :test #'equal)
  "Absolute archive namestring → ZIP-FILESYSTEM.")

(defun clear-zip-filesystem-cache ()
  (clrhash *zip-filesystem-cache*))

(defun %u16 (bytes offset)
  (logior (aref bytes offset)
          (ash (aref bytes (1+ offset)) 8)))

(defun %u32 (bytes offset)
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun %put-u16 (out value)
  (vector-push-extend (logand value #xff) out)
  (vector-push-extend (logand (ash value -8) #xff) out))

(defun %put-u32 (out value)
  (vector-push-extend (logand value #xff) out)
  (vector-push-extend (logand (ash value -8) #xff) out)
  (vector-push-extend (logand (ash value -16) #xff) out)
  (vector-push-extend (logand (ash value -24) #xff) out))

(defun %read-file-bytes (pathname)
  (with-open-file (s pathname :direction :input :element-type '(unsigned-byte 8)
                     :if-does-not-exist :error)
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      buf)))

(defun %zip-normalize-name (name)
  (let* ((s (if (and (plusp (length name)) (char= (char name 0) #\/))
                name
                (concatenate 'string "/" name)))
         (s (string-right-trim "/" s)))
    (if (zerop (length s)) "/" s)))

(defun %find-eocd (bytes)
  (let ((len (length bytes))
        (sig (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(#x50 #x4b #x05 #x06))))
    (loop for i from (max 0 (- len 22 65535)) below (- len 21)
          when (and (= (aref bytes i) (aref sig 0))
                     (= (aref bytes (+ i 1)) (aref sig 1))
                     (= (aref bytes (+ i 2)) (aref sig 2))
                     (= (aref bytes (+ i 3)) (aref sig 3)))
            do (return i)
          finally (error 'path-error :message "not a zip archive (EOCD missing)"))))

(defun %zip-load-central-directory (fs)
  (let* ((bytes (or (zip-filesystem-bytes fs)
                    (setf (zip-filesystem-bytes fs)
                          (%read-file-bytes (zip-filesystem-archive fs)))))
         (eocd (%find-eocd bytes))
         (count (%u16 bytes (+ eocd 10)))
         (cd-offset (%u32 bytes (+ eocd 16)))
         (table (zip-filesystem-entries fs)))
    (clrhash table)
    (setf (gethash "/" table) :dir)
    (let ((off cd-offset))
      (dotimes (_ count)
        (unless (= (%u32 bytes off) +zip-central-sig+)
          (error 'path-error :filesystem fs
                 :message "corrupt zip central directory"))
        (let* ((method (%u16 bytes (+ off 10)))
               (dos-time (%u16 bytes (+ off 12)))
               (dos-date (%u16 bytes (+ off 14)))
               (crc (%u32 bytes (+ off 16)))
               (comp (%u32 bytes (+ off 20)))
               (uncomp (%u32 bytes (+ off 24)))
               (name-len (%u16 bytes (+ off 28)))
               (extra-len (%u16 bytes (+ off 30)))
               (comment-len (%u16 bytes (+ off 32)))
               (local-off (%u32 bytes (+ off 42)))
               (name (babel-or-utf8 bytes (+ off 46) name-len))
               (key (%zip-normalize-name name)))
          (if (and (plusp (length name))
                   (char= (char name (1- (length name))) #\/))
              (setf (gethash key table) :dir)
              (setf (gethash key table)
                    (%make-zip-entry :name key :method method :crc crc
                                     :compressed-size comp
                                     :uncompressed-size uncomp
                                     :header-offset local-off
                                     :dos-date dos-date :dos-time dos-time)))
          (%zip-ensure-parents table key)
          (incf off (+ 46 name-len extra-len comment-len))))
    table)))

(defun babel-or-utf8 (bytes start len)
  "UTF-8 decode of LEN bytes at START. ZIP stores names as CP437 or UTF-8;
   UTF-8 is a superset of the ASCII names we care about."
  (utf8-octets-to-string (subseq bytes start (+ start len))))

(defun %zip-ensure-parents (table key)
  (let ((s key))
    (loop
      (let ((slash (position #\/ s :from-end t)))
        (unless (and slash (plusp slash))
          (return))
        (setf s (subseq s 0 slash))
        (unless (gethash s table)
          (setf (gethash s table) :dir))))))

(defun %pct-decode (s)
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length s))
          do (let ((c (char s i)))
               (if (and (char= c #\%) (<= (+ i 3) (length s)))
                   (let ((code (ignore-errors
                                 (parse-integer s :start (1+ i) :end (+ i 3) :radix 16))))
                     (if code
                         (progn (write-char (code-char code) out) (incf i 3))
                         (progn (write-char c out) (incf i))))
                   (progn (write-char c out) (incf i)))))))

(defun %pct-encode-archive (s)
  (with-output-to-string (out)
    (loop for c across s
          do (if (or (char= c #\%) (char= c #\!) (char= c #\Space)
                     (char= c #\#) (char= c #\?))
                 (format out "%~2,'0X" (char-code c))
                 (write-char c out)))))

(defun %drive-letter-prefix (s)
  "If S is C:… or /C:… (colon or historic |), return (values DRIVE rest)."
  (flet ((drive-at (i)
           (and (< (1+ i) (length s))
                (alpha-char-p (char s i))
                (find (char s (1+ i)) ":|"))))
    (cond ((and (plusp (length s)) (char= (char s 0) #\/) (drive-at 1))
           (values (char-upcase (char s 1)) (subseq s 3)))
          ((drive-at 0)
           (values (char-upcase (char s 0)) (subseq s 2)))
          (t (values nil s)))))

(defun %normalize-archive-string (s)
  "Archive half of a zip: URI → native namestring (C:/… or /tmp/…).
   Accepts file: prefix, empty authority, /C:/ and C:/ (RFC 8089)."
  (let ((s (%pct-decode (substitute #\/ #\\ (string s)))))
    (when (and (>= (length s) 5) (string-equal (subseq s 0 5) "file:"))
      (setf s (subseq s 5)))
    (when (and (>= (length s) 2) (char= (char s 0) #\/) (char= (char s 1) #\/))
      (cond ((and (>= (length s) 3) (char= (char s 2) #\/))
             (setf s (subseq s 2)))
            (t
             (let ((rest (subseq s 2)))
               (setf s
                     (if (nth-value 0 (%drive-letter-prefix rest))
                         rest
                         (let ((slash (position #\/ rest)))
                           (if (and slash
                                    (member (subseq rest 0 slash)
                                            '("" "localhost") :test #'string-equal))
                               (subseq rest slash)
                               (if slash (subseq rest slash) rest)))))))))
    (multiple-value-bind (drive rest) (%drive-letter-prefix s)
      (if drive
          (format nil "~A:~A" drive
                  (cond ((zerop (length rest)) "/")
                        ((char= (char rest 0) #\/) rest)
                        (t (concatenate 'string "/" rest))))
          s))))

(defun %archive-pathname (archive)
  "Native pathname we can OPEN. Never unix-namestring (drops the drive)."
  (etypecase archive
    (pathname
     (uiop:ensure-absolute-pathname archive (uiop:getcwd)))
    (string
     (uiop:ensure-absolute-pathname
      (uiop:parse-native-namestring (%normalize-archive-string archive))
      (uiop:getcwd)))))

(defun %archive-key (archive)
  (let ((pn (%archive-pathname archive)))
    (namestring (or (ignore-errors (truename pn)) pn))))

(defun make-zip-filesystem (archive &key (cache t) bytes)
  "Open ARCHIVE (pathname/string) as a ZIP-FILESYSTEM.
   :BYTES supplies the archive contents (tests / already-read buffers)."
  (let* ((pn (unless bytes (%archive-pathname archive)))
         (key (if bytes
                  (format nil "bytes:~A" (sxhash bytes))
                  (%archive-key pn))))
    (or (and cache (not bytes) (gethash key *zip-filesystem-cache*))
        (let ((fs (make-instance 'zip-filesystem
                                 :archive (if bytes
                                              (or archive (format nil "<bytes:~A>" (length bytes)))
                                              pn)
                                 :bytes bytes)))
          (%zip-load-central-directory fs)
          (when (and cache (not bytes))
            (setf (gethash key *zip-filesystem-cache*) fs))
          fs))))

(defun %file-uri-path (archive)
  "RFC 8089 path-absolute of a local archive (no scheme).
   Unix: /tmp/x.zip   Windows: /C:/Users/foo/x.zip"
  (cond
    ((and (stringp archive)
          (plusp (length archive))
          (char= (char archive 0) #\<))
     archive)
    (t
     (let* ((pn (if (pathnamep archive)
                    (uiop:ensure-absolute-pathname archive (uiop:getcwd))
                    (%archive-pathname archive)))
            (native (substitute #\/ #\\ (uiop:native-namestring pn))))
       (cond
         ((and (>= (length native) 2)
               (alpha-char-p (char native 0))
               (char= (char native 1) #\:))
          (concatenate 'string "/" native))
         ((and (plusp (length native)) (char= (char native 0) #\/))
          native)
         (t (concatenate 'string "/" native)))))))

(defun %zip-archive-uri (archive)
  (%pct-encode-archive (%file-uri-path archive)))

(defun parse-zip-uri (uri)
  (let* ((rest (cond ((and (>= (length uri) 6)
                           (string-equal (subseq uri 0 6) "zip://"))
                      (subseq uri 6))
                     ((and (>= (length uri) 4)
                           (string-equal (subseq uri 0 4) "zip:"))
                      (subseq uri 4))
                     (t uri)))
         (bang (position #\! rest))
         (archive (if bang (subseq rest 0 bang) rest))
         (entry (if bang (subseq rest (1+ bang)) "/")))
    (when (zerop (length archive))
      (error 'path-error :path uri :message "zip:// URI missing archive path"))
    (let ((fs (make-zip-filesystem (%normalize-archive-string archive))))
      (make-path (fs-parse fs (if (plusp (length entry)) entry "/"))
                 :filesystem fs))))

(defun zip-path (archive &optional (entry "/"))
  "PATH on ARCHIVE's zip filesystem at ENTRY (default `/`)."
  (let ((fs (make-zip-filesystem archive)))
    (make-path (fs-parse fs entry) :filesystem fs)))

(register-uri-scheme "zip" #'parse-zip-uri)

(defun %zip-key (fs pathname)
  (%zip-normalize-name
   (let* ((pn (if (pathnamep pathname) pathname (fs-parse fs pathname)))
          (s (%pathname-as-posix pn)))
     (if (and (plusp (length s)) (char= (char s 0) #\/)) s (concatenate 'string "/" s)))))

(defmethod fs-parse ((fs zip-filesystem) designator &key directory)
  (let ((pn (etypecase designator
              (pathname designator)
              (string (%parse-posix designator))
              (path (path-pathname designator)))))
    (if directory (uiop:ensure-directory-pathname pn) pn)))

(defmethod fs-join ((fs zip-filesystem) base relative)
  (uiop:merge-pathnames*
   (fs-parse fs relative)
   (uiop:ensure-directory-pathname (fs-parse fs base))))

(defmethod fs-parent ((fs zip-filesystem) pathname)
  (%logical-parent (fs-parse fs pathname)))

(defmethod fs-name ((fs zip-filesystem) pathname)
  (%file-namestring* (fs-parse fs pathname)))

(defmethod fs-absolute ((fs zip-filesystem) pathname &key (defaults "/"))
  (let ((pn (fs-parse fs pathname))
        (base (fs-parse fs defaults :directory t)))
    (if (uiop:absolute-pathname-p pn) pn (uiop:merge-pathnames* pn base))))

(defmethod fs-resolve ((fs zip-filesystem) pathname &key (strict t))
  (let ((abs (fs-absolute fs pathname)))
    (if (or (not strict) (fs-exists-p fs abs))
        abs
        (%restart-path-not-found fs abs
                                 :message "path does not exist"
                                 :allow-create-file nil
                                 :allow-create-directory nil
                                 :allow-ignore nil))))

(defmethod fs-normpath ((fs zip-filesystem) pathname)
  (let ((pn (fs-parse fs pathname)))
    (multiple-value-bind (abs? stack dir?) (%collapse-dot-segments pn)
      (fs-parse fs
                (cond ((and abs? (null stack)) "/")
                      (abs? (format nil "/~{~A~^/~}" stack))
                      ((null stack) ".")
                      (t (format nil "~{~A~^/~}" stack)))
                :directory dir?))))

(defmethod fs-expanduser ((fs zip-filesystem) pathname)
  (fs-parse fs pathname))

(defmethod fs-exists-p ((fs zip-filesystem) pathname)
  (nth-value 1 (gethash (%zip-key fs (fs-absolute fs pathname))
                        (zip-filesystem-entries fs))))

(defmethod fs-file-p ((fs zip-filesystem) pathname)
  (let ((ent (gethash (%zip-key fs (fs-absolute fs pathname))
                      (zip-filesystem-entries fs))))
    (%zip-entry-p ent)))

(defmethod fs-directory-p ((fs zip-filesystem) pathname)
  (eq (gethash (%zip-key fs (fs-absolute fs pathname))
               (zip-filesystem-entries fs))
      :dir))

(defmethod fs-readable-p ((fs zip-filesystem) pathname)
  (fs-exists-p fs pathname))

(defmethod fs-writable-p ((fs zip-filesystem) pathname)
  (declare (ignore pathname))
  nil)

(defmethod fs-executable-p ((fs zip-filesystem) pathname)
  (declare (ignore pathname))
  nil)

(defmethod fs-file-size ((fs zip-filesystem) pathname)
  (let ((ent (gethash (%zip-key fs (fs-absolute fs pathname))
                      (zip-filesystem-entries fs))))
    (unless (%zip-entry-p ent)
      (error 'path-not-found :filesystem fs :path pathname))
    (%zip-entry-uncompressed-size ent)))

(defun %dos-datetime-universal (date time)
  (let ((day (logand date #x1f))
        (month (logand (ash date -5) #x0f))
        (year (+ 1980 (ash date -9)))
        (sec (* 2 (logand time #x1f)))
        (min (logand (ash time -5) #x3f))
        (hour (ash time -11)))
    (if (or (zerop day) (zerop month))
        0
        (encode-universal-time sec min hour day month year 0))))

(defmethod fs-last-modified ((fs zip-filesystem) pathname)
  (let ((ent (gethash (%zip-key fs (fs-absolute fs pathname))
                      (zip-filesystem-entries fs))))
    (unless (%zip-entry-p ent)
      (error 'path-not-found :filesystem fs :path pathname))
    (%dos-datetime-universal (%zip-entry-dos-date ent) (%zip-entry-dos-time ent))))

(defmethod fs-iterdir ((fs zip-filesystem) pathname)
  (let* ((abs (fs-absolute fs pathname))
         (key (%zip-key fs abs))
         (prefix (if (string= key "/") "/" (concatenate 'string key "/")))
         (out '()))
    (unless (fs-directory-p fs abs)
      (error 'path-not-found :filesystem fs :path abs))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (uiop:string-prefix-p prefix k)
                          (not (string= k key))
                          (not (find #\/ k :start (length prefix))))
                 (push (fs-parse fs k) out)))
             (zip-filesystem-entries fs))
    (nreverse out)))

(defmethod fs-glob ((fs zip-filesystem) pathname pattern &key recursive)
  (let* ((base-key (%zip-key fs (fs-absolute fs pathname)))
         (prefix (if (string= base-key "/") "/" (concatenate 'string base-key "/")))
         (out '()))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (when (and (uiop:string-prefix-p prefix k) (not (string= k base-key)))
         (let* ((rel (subseq k (length prefix)))
                (nm (file-namestring k))
                (nested-p (find #\/ rel)))
           (when (and (plusp (length rel))
                      (or recursive (find #\/ pattern) (not nested-p))
                      (if (find #\/ pattern)
                          (pathname-match-p rel (%wild-pattern pattern))
                          (and nm (%name-matches-p nm pattern))))
             (push (fs-parse fs k) out)))))
     (zip-filesystem-entries fs))
    (nreverse out)))

(defmethod fs-walk ((fs zip-filesystem) pathname &key (top-down t) follow-symlinks)
  (declare (ignore follow-symlinks))
  (let ((out '()))
    (labels ((walk* (dir)
               (let* ((entries (fs-iterdir fs dir))
                      (files (remove-if-not (lambda (p) (fs-file-p fs p)) entries))
                      (dirs (remove-if-not (lambda (p) (fs-directory-p fs p)) entries)))
                 (when top-down (push (list dir files dirs) out))
                 (dolist (d dirs) (walk* d))
                 (unless top-down (push (list dir files dirs) out)))))
      (walk* (fs-absolute fs pathname))
      (nreverse out))))

(defmethod fs-mkdir ((fs zip-filesystem) pathname &key parents exist-ok)
  (declare (ignore parents exist-ok))
  (%unsupported fs 'fs-mkdir pathname))

(defmethod fs-rmdir ((fs zip-filesystem) pathname)
  (%unsupported fs 'fs-rmdir pathname))

(defmethod fs-unlink ((fs zip-filesystem) pathname &key missing-ok)
  (declare (ignore missing-ok))
  (%unsupported fs 'fs-unlink pathname))

(defmethod fs-touch ((fs zip-filesystem) pathname &key exist-ok)
  (declare (ignore exist-ok))
  (%unsupported fs 'fs-touch pathname))

(defmethod fs-rename ((fs zip-filesystem) source target &key replace)
  (declare (ignore target replace))
  (%unsupported fs 'fs-rename source))

(defmethod fs-copy ((fs zip-filesystem) source target &key replace)
  (declare (ignore target replace))
  (%unsupported fs 'fs-copy source))

(defmethod fs-write-bytes ((fs zip-filesystem) pathname bytes &key append if-exists if-does-not-exist)
  (declare (ignore bytes append if-exists if-does-not-exist))
  (%unsupported fs 'fs-write-bytes pathname))

(defmethod fs-make-temp ((fs zip-filesystem) &key directory prefix suffix)
  (declare (ignore directory prefix suffix))
  (%unsupported fs 'fs-make-temp nil))

(defun %inflate-deflate (compressed)
  (chipz:decompress nil 'chipz:deflate compressed))

(defun %zip-read-payload (fs entry)
  (let* ((bytes (zip-filesystem-bytes fs))
         (off (%zip-entry-header-offset entry)))
    (unless (= (%u32 bytes off) +zip-local-sig+)
      (error 'path-error :filesystem fs :path (%zip-entry-name entry)
             :message "corrupt zip local header"))
    (let* ((name-len (%u16 bytes (+ off 26)))
           (extra-len (%u16 bytes (+ off 28)))
           (data-off (+ off 30 name-len extra-len))
           (comp (subseq bytes data-off (+ data-off (%zip-entry-compressed-size entry))))
           (method (%zip-entry-method entry)))
      (cond ((= method 0) comp)
            ((= method 8) (%inflate-deflate comp))
            (t (error 'path-error :filesystem fs :path (%zip-entry-name entry)
                      :message (format nil "unsupported zip method ~A" method)))))))

(defmethod fs-read-bytes ((fs zip-filesystem) pathname)
  (let* ((abs (fs-absolute fs pathname))
         (ent (gethash (%zip-key fs abs) (zip-filesystem-entries fs))))
    (unless (%zip-entry-p ent)
      (return-from fs-read-bytes
        (%restart-path-not-found fs abs
                                 :message "file does not exist"
                                 :allow-create-file nil
                                 :allow-create-directory nil
                                 :allow-ignore nil)))
    (%zip-read-payload fs ent)))

(defmethod fs-same-p ((fs zip-filesystem) a b)
  (string= (%zip-key fs (fs-absolute fs a))
           (%zip-key fs (fs-absolute fs b))))

(defmethod fs-as-uri ((fs zip-filesystem) pathname)
  (format nil "zip://~A!~A"
          (%zip-archive-uri (zip-filesystem-archive fs))
          (%zip-key fs (fs-absolute fs pathname))))

;;; --- stored ZIP writer (tests / bundling) --------------------------------

(defparameter *crc32-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (_ 8)
          (setf c (if (oddp c)
                      (logxor #xedb88320 (ash c -1))
                      (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (bytes)
  (let ((crc #xffffffff))
    (loop for b across bytes
          do (setf crc (logxor (aref *crc32-table* (logand (logxor crc b) #xff))
                               (ash crc -8))))
    (logxor crc #xffffffff)))

(defun write-zip-bytes (entries)
  "Build a stored (method 0) ZIP as a byte vector.
   ENTRIES is a list of (posix-name string-or-octets)."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (centrals '()))
    (dolist (pair entries)
      (destructuring-bind (name data) pair
        (let* ((name (string-left-trim "/" (string name)))
               (name-octets (utf8-string-to-octets name))
               (payload (if (stringp data) (utf8-string-to-octets data)
                            (coerce data '(vector (unsigned-byte 8)))))
               (crc (crc32 payload))
               (local-off (length out)))
          (%put-u32 out +zip-local-sig+)
          (%put-u16 out 20)             ; version
          (%put-u16 out 0)              ; flags
          (%put-u16 out 0)              ; stored
          (%put-u16 out 0) (%put-u16 out 0) ; time/date
          (%put-u32 out crc)
          (%put-u32 out (length payload))
          (%put-u32 out (length payload))
          (%put-u16 out (length name-octets))
          (%put-u16 out 0)
          (loop for b across name-octets do (vector-push-extend b out))
          (loop for b across payload do (vector-push-extend b out))
          (push (list name-octets crc (length payload) local-off) centrals))))
    (setf centrals (nreverse centrals))
    (let ((cd-off (length out)))
      (dolist (c centrals)
        (destructuring-bind (name-octets crc size local-off) c
          (%put-u32 out +zip-central-sig+)
          (%put-u16 out 20) (%put-u16 out 20)
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u32 out crc)
          (%put-u32 out size) (%put-u32 out size)
          (%put-u16 out (length name-octets))
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u32 out 0)
          (%put-u32 out local-off)
          (loop for b across name-octets do (vector-push-extend b out))))
      (let ((cd-size (- (length out) cd-off)))
        (%put-u32 out +zip-eocd-sig+)
        (%put-u16 out 0) (%put-u16 out 0)
        (%put-u16 out (length centrals)) (%put-u16 out (length centrals))
        (%put-u32 out cd-size)
        (%put-u32 out cd-off)
        (%put-u16 out 0)))
    out))

(defun write-zip-file (pathname entries)
  "Write ENTRIES (see WRITE-ZIP-BYTES) to PATHNAME. Returns PATHNAME."
  (let ((pn (uiop:ensure-pathname pathname :want-pathname t)))
    (ensure-directories-exist pn)
    (with-open-file (s pn :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede :if-does-not-exist :create)
      (write-sequence (write-zip-bytes entries) s))
    pn))

(defun zip-tree (root &optional destination)
  "Zip every file under ROOT (directory). Writes DESTINATION or returns bytes.
   Paths inside the archive are relative to ROOT (no leading slash).
   ROOT is resolved first so Windows 8.3 vs long names agree."
  (let* ((root (ensure-directory (resolve root :strict t)))
         (entries '()))
    (dolist (triple (walk root))
      (destructuring-bind (_dir files __dirs) triple
        (declare (ignore _dir __dirs))
        (dolist (f files)
          (push (list (as-posix (relative-to f root)) (read-bytes f)) entries))))
    (if destination
        (write-zip-file destination (nreverse entries))
        (write-zip-bytes (nreverse entries)))))
