(in-package #:cl-stack-pathlib)

;;; Keyword-heavy facade — pathlib / NIO Path+Files DX over the filesystem protocol.

(defun %fs (designator)
  (if (path-p designator) (path-filesystem designator) (%current-fs)))

(defun %pn (designator)
  (path-pathname (ensure-path designator)))

(defun %wrap (fs pathname)
  (make-path pathname :filesystem fs))

(defun path (designator &key directory (filesystem nil filesystem-p))
  (if filesystem-p
      (ensure-path designator :filesystem filesystem :directory directory)
      (ensure-path designator :directory directory)))

(defun %designator-posix (designator)
  (etypecase designator
    (path (as-posix designator))
    (pathname (or (uiop:unix-namestring designator) (namestring designator)))
    (string designator)
    (symbol (string designator))))

(defun join (base &rest parts)
  "Join like pathlib `/` — does not normalize `.` / `..` (use NORMPATH)."
  (let* ((p0 (ensure-path base))
         (fs (path-filesystem p0))
         (acc (string-right-trim "/" (%designator-posix p0))))
    (dolist (part parts)
      (when part
        (let ((seg (%designator-posix part)))
          (setf acc
                (if (and (plusp (length seg)) (char= (char seg 0) #\/))
                    (string-right-trim "/" seg)
                    (let ((seg (string-left-trim "/" seg)))
                      (cond ((zerop (length acc)) (format nil "/~A" seg))
                            ((zerop (length seg)) acc)
                            (t (format nil "~A/~A" acc seg)))))))))
    (%wrap fs (fs-parse fs (if (zerop (length acc)) "." acc)))))

(setf (fdefinition 'joinpath) (fdefinition 'join))

(defun from-list (components &key (filesystem *filesystem*) absolute)
  (let* ((comps (mapcar #'string components))
         (s (if absolute
                (format nil "/~{~A~^/~}" comps)
                (format nil "~{~A~^/~}" comps))))
    (ensure-path s :filesystem filesystem)))

(defun components (designator)
  (let* ((pn (%pn designator))
         (dir (pathname-directory pn))
         (out '()))
    (when (and (consp dir) (eq (first dir) :absolute))
      (push "/" out))
    (when (consp dir)
      (dolist (c (rest dir))
        (when (stringp c) (push c out))))
    (let ((fn (%file-namestring* pn)))
      (when fn (push fn out)))
    (nreverse out)))

(defun parts (designator)
  (remove "/" (components designator) :test #'string=))

(defun parent (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (uiop:ensure-directory-pathname
            (%logical-parent (path-pathname p))))))

(defun parents (designator)
  "Logical ancestors, including the filesystem root (pathlib .parents).
Stops when PARENT is a fixed point (root), so broken backends cannot loop."
  (loop named walk
        with p = (parent designator)
        with acc = '()
        do (when (equal (%pn p) (%pn (parent p)))
             (unless (equal (%pn p) (%pn designator))
               (push p acc))
             (return-from walk (nreverse acc)))
           (push p acc)
           (setf p (parent p))))

(defun with-parent (designator new-parent)
  (join new-parent (name designator)))

(defun name (designator)
  (let ((p (ensure-path designator)))
    (fs-name (path-filesystem p) (path-pathname p))))

(defun with-name (designator new-name)
  (let* ((p (ensure-path designator))
         (fs (path-filesystem p))
         (dir (uiop:pathname-directory-pathname (path-pathname p))))
    (%wrap fs (fs-join fs dir new-name))))

(defun base (designator)
  (pathname-name (%pn designator)))

(setf (fdefinition 'stem) (fdefinition 'base))

(defun with-base (designator new-base)
  (let ((pn (%pn designator)))
    (with-name designator
               (make-pathname :name new-base :type (pathname-type pn) :defaults ""))))

(setf (fdefinition 'with-stem) (fdefinition 'with-base))

(defun extension (designator)
  (let ((type (pathname-type (%pn designator))))
    (if (and type (not (eq type :unspecific))) (string type) "")))

(defun suffix (designator)
  (let ((ext (extension designator)))
    (if (plusp (length ext)) (concatenate 'string "." ext) "")))

(defun suffixes (designator)
  (let* ((n (or (name designator) ""))
         (parts (uiop:split-string n :separator ".")))
    (if (<= (length parts) 1)
        '()
        (mapcar (lambda (s) (concatenate 'string "." s)) (rest parts)))))

(defun with-extension (designator new-ext)
  (let* ((ext (string new-ext))
         (type (if (and (plusp (length ext)) (char= (char ext 0) #\.))
                   (subseq ext 1)
                   ext)))
    (with-name designator
               (make-pathname :name (base designator) :type (if (plusp (length type)) type nil)
                              :defaults ""))))

(setf (fdefinition 'with-suffix) (fdefinition 'with-extension))

(defun add-extension (designator new-ext)
  (let* ((ext (string-left-trim "." (string new-ext)))
         (n (name designator)))
    (with-name designator (format nil "~A.~A" n ext))))

(defun drop-extension (designator)
  (with-name designator (base designator)))

(defun root-p (designator)
  (%pathname-root-p (%pn designator)))

(defun empty-p (designator)
  (%pathname-empty-p (%pn designator)))

(defun absolute-p (designator)
  (uiop:absolute-pathname-p (%pn designator)))

(defun relative-p (designator)
  (uiop:relative-pathname-p (%pn designator)))

(defun directory-pathname-p (designator)
  (uiop:directory-pathname-p (%pn designator)))

(defun starts-with-p (designator prefix)
  (let ((a (components designator))
        (b (components prefix)))
    (and (<= (length b) (length a))
         (equal (subseq a 0 (length b)) b))))

(defun %tail (list n)
  (nthcdr (max 0 (- (length list) n)) list))

(defun ends-with-p (designator suffix)
  (let* ((a (components designator))
         (b (components suffix)))
    (and (<= (length b) (length a))
         (equal (%tail a (length b)) b))))

(defun match-p (designator pattern)
  "Pathlib-like match: PATTERN against the full path or the final name."
  (let ((nm (name designator))
        (pn (%pn designator))
        (wild (%wild-pattern pattern)))
    (or (and nm (%name-matches-p nm pattern))
        (pathname-match-p pn wild))))

(defun absolute (designator &key (defaults nil defaults-p))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (if defaults-p
               (fs-absolute (path-filesystem p) (path-pathname p)
                            :defaults (%pn defaults))
               (fs-absolute (path-filesystem p) (path-pathname p))))))

(defun resolve (designator &key (strict t))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-resolve (path-filesystem p) (path-pathname p) :strict strict))))

(defun expanduser (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-expanduser (path-filesystem p) (path-pathname p)))))

(defun normpath (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-normpath (path-filesystem p) (path-pathname p)))))

(defun cwd (&key (filesystem *filesystem*))
  (%wrap filesystem
         (if (typep filesystem 'memory-filesystem)
             (fs-parse filesystem (memory-fs-cwd filesystem) :directory t)
             (uiop:getcwd))))

(defun home (&key (filesystem *filesystem*))
  (%wrap filesystem
         (if (typep filesystem 'memory-filesystem)
             (fs-parse filesystem (memory-fs-home filesystem) :directory t)
             (user-homedir-pathname))))

(defun as-namestring (designator &key (style :native))
  (let ((pn (%pn designator)))
    (ecase style
      ((:native) (namestring pn))
      ((:posix :codegen) (%pathname-as-posix pn)))))

(defun as-posix (designator)
  (as-namestring designator :style :posix))

(defun as-uri (designator)
  (let ((p (ensure-path designator)))
    (fs-as-uri (path-filesystem p) (path-pathname p))))

(defun to-string (designator)
  (as-posix designator))

(defun from-string (string &key (filesystem *filesystem*))
  (ensure-path string :filesystem filesystem))

(defun ensure-directory (designator)
  (ensure-path designator :directory t))

(defun ensure-string (designator)
  (as-posix designator))

(defun relative-to (designator base)
  (let* ((p (ensure-path designator))
         (b (ensure-path base :filesystem (path-filesystem p)))
         (rel (uiop:enough-pathname
               (fs-absolute (path-filesystem p) (path-pathname p))
               (uiop:ensure-directory-pathname
                (fs-absolute (path-filesystem b) (path-pathname b))))))
    (unless (uiop:relative-pathname-p rel)
      (error 'not-relative-error :path p :filesystem (path-filesystem p)
             :message (format nil "~A is not relative to ~A" p b)))
    (%wrap (path-filesystem p) rel)))

(defun relative-to-p (designator base)
  (handler-case (progn (relative-to designator base) t)
    (not-relative-error () nil)))

(defun under (root relative)
  (join (ensure-directory root) relative))

(defun same-p (a b)
  (let ((pa (ensure-path a)))
    (fs-same-p (path-filesystem pa) (path-pathname pa)
               (path-pathname (ensure-path b :filesystem (path-filesystem pa))))))

(defun path-equal (a b)
  (and (eq (path-filesystem (ensure-path a))
           (path-filesystem (ensure-path b)))
       (equal (namestring (%pn a)) (namestring (%pn b)))))

(macrolet ((def-probe (name fs-name)
             `(defun ,name (designator)
                (let ((p (ensure-path designator)))
                  (,fs-name (path-filesystem p) (path-pathname p))))))
  (def-probe exists-p fs-exists-p)
  (def-probe file-p fs-file-p)
  (def-probe directory-p fs-directory-p)
  (def-probe symlink-p fs-symlink-p)
  (def-probe readable-p fs-readable-p)
  (def-probe writable-p fs-writable-p)
  (def-probe executable-p fs-executable-p)
  (def-probe file-size fs-file-size)
  (def-probe last-modified fs-last-modified))

(defun iterdir (designator)
  (let ((p (ensure-path designator)))
    (mapcar (lambda (pn) (%wrap (path-filesystem p) pn))
            (fs-iterdir (path-filesystem p) (path-pathname p)))))

(defun glob (designator pattern &key recursive)
  (let ((p (ensure-path designator)))
    (mapcar (lambda (pn) (%wrap (path-filesystem p) pn))
            (fs-glob (path-filesystem p) (path-pathname p) pattern :recursive recursive))))

(defun rglob (designator pattern)
  (glob designator pattern :recursive t))

(defun walk (designator &key (top-down t) (follow-symlinks nil))
  (let ((p (ensure-path designator))
        (fs (path-filesystem (ensure-path designator))))
    (mapcar (lambda (triple)
              (destructuring-bind (dir files dirs) triple
                (list (%wrap fs dir)
                      (mapcar (lambda (f) (%wrap fs f)) files)
                      (mapcar (lambda (d) (%wrap fs d)) dirs))))
            (fs-walk fs (path-pathname p) :top-down top-down
                                          :follow-symlinks follow-symlinks))))

(defun mkdir (designator &key (parents t) (exist-ok t))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-mkdir (path-filesystem p) (path-pathname p)
                     :parents parents :exist-ok exist-ok))))

(defun rmdir (designator)
  (let ((p (ensure-path designator)))
    (fs-rmdir (path-filesystem p) (path-pathname p))))

(defun unlink (designator &key (missing-ok nil))
  (let ((p (ensure-path designator)))
    (fs-unlink (path-filesystem p) (path-pathname p) :missing-ok missing-ok)))

(setf (fdefinition 'delete-path) (fdefinition 'unlink))

(defun touch (designator &key (exist-ok t))
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-touch (path-filesystem p) (path-pathname p) :exist-ok exist-ok))))

(defun rename-path (source target &key (replace t))
  (let ((s (ensure-path source)))
    (%wrap (path-filesystem s)
           (fs-rename (path-filesystem s) (path-pathname s)
                      (path-pathname (ensure-path target :filesystem (path-filesystem s)))
                      :replace replace))))

(defun replace-path (source target)
  (rename-path source target :replace t))

(defun copy-path (source target &key (replace t))
  (let ((s (ensure-path source)))
    (%wrap (path-filesystem s)
           (fs-copy (path-filesystem s) (path-pathname s)
                    (path-pathname (ensure-path target :filesystem (path-filesystem s)))
                    :replace replace))))

(defun move-path (source target &key (replace t))
  (rename-path source target :replace replace))

(defun create-symlink (link target)
  (let ((l (ensure-path link)))
    (%wrap (path-filesystem l)
           (fs-create-symlink (path-filesystem l) (path-pathname l)
                              (path-pathname (ensure-path target :filesystem (path-filesystem l)))))))

(defun read-symlink (designator)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-read-symlink (path-filesystem p) (path-pathname p)))))

(defun make-temp-file (&key directory (prefix "cl-stack-") (suffix ".tmp")
                            (filesystem *filesystem*))
  (let ((pn (fs-make-temp filesystem :directory (when directory (%pn directory))
                                     :prefix prefix :suffix suffix)))
    (fs-touch filesystem pn)
    (%wrap filesystem pn)))

(defun make-temp-directory (&key directory (prefix "cl-stack-") (filesystem *filesystem*))
  (let ((pn (fs-make-temp filesystem :directory (when directory (%pn directory))
                                     :prefix prefix :suffix "")))
    (%wrap filesystem (fs-mkdir filesystem pn :parents t :exist-ok t))))

(defun read-bytes (designator)
  (let ((p (ensure-path designator)))
    (fs-read-bytes (path-filesystem p) (path-pathname p))))

(defun write-bytes (designator bytes &key append)
  (let ((p (ensure-path designator)))
    (%wrap (path-filesystem p)
           (fs-write-bytes (path-filesystem p) (path-pathname p) bytes :append append))))

(defun read-text (designator &key (encoding :utf-8))
  (ecase encoding
    (:utf-8 (utf8-octets-to-string (read-bytes designator)))
    (otherwise (error "unsupported text encoding ~S" encoding))))

(defun write-text (designator text &key (encoding :utf-8) append)
  (ecase encoding
    (:utf-8 (write-bytes designator (utf8-string-to-octets text) :append append))
    (otherwise (error "unsupported text encoding ~S" encoding))))

(defun utf8-octets-to-string (bytes)
  (labels ((need (n)
             (unless (<= (+ i n) len)
               (error "invalid UTF-8: truncated sequence at byte ~D" i))))
    (let ((len (length bytes))
          (out (make-string 0 :adjustable t :fill-pointer 0))
          (i 0))
      (loop while (< i len)
            do (let ((b0 (aref bytes i)))
                 (cond ((< b0 #x80)
                        (vector-push-extend (code-char b0) out)
                        (incf i))
                       ((< b0 #xE0)
                        (need 2)
                        (vector-push-extend
                         (code-char (logior (ash (logand b0 #x1F) 6)
                                            (logand (aref bytes (+ i 1)) #x3F)))
                         out)
                        (incf i 2))
                       ((< b0 #xF0)
                        (need 3)
                        (vector-push-extend
                         (code-char (logior (ash (logand b0 #x0F) 12)
                                            (ash (logand (aref bytes (+ i 1)) #x3F) 6)
                                            (logand (aref bytes (+ i 2)) #x3F)))
                         out)
                        (incf i 3))
                       (t
                        (need 4)
                        (vector-push-extend
                         (code-char (logior (ash (logand b0 #x07) 18)
                                            (ash (logand (aref bytes (+ i 1)) #x3F) 12)
                                            (ash (logand (aref bytes (+ i 2)) #x3F) 6)
                                            (logand (aref bytes (+ i 3)) #x3F)))
                         out)
                        (incf i 4)))))
      out)))

(defun utf8-string-to-octets (text)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((push-byte (b) (vector-push-extend b out)))
      (dotimes (i (length text))
        (let ((c (char-code (char text i))))
          (cond ((< c #x80) (push-byte c))
                ((< c #x800)
                 (push-byte (logior #xC0 (ash c -6)))
                 (push-byte (logior #x80 (logand c #x3F))))
                ((< c #x10000)
                 (push-byte (logior #xE0 (ash c -12)))
                 (push-byte (logior #x80 (logand (ash c -6) #x3F)))
                 (push-byte (logior #x80 (logand c #x3F))))
                (t
                 (push-byte (logior #xF0 (ash c -18)))
                 (push-byte (logior #x80 (logand (ash c -12) #x3F)))
                 (push-byte (logior #x80 (logand (ash c -6) #x3F)))
                 (push-byte (logior #x80 (logand c #x3F))))))))
    out))
