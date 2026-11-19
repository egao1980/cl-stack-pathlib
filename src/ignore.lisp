(in-package #:cl-stack-pathlib)

;;; Ignore matcher (H2a): .gitignore / .ignore syntax, gitignore(5) semantics.
;;;
;;; A matcher is an ordered list of rules; a later rule wins over an earlier
;;; one. Each rule keeps its BASE (the directory, relative to the walk root,
;;; holding the file it came from: "" or "a/b/"), so nested ignore files
;;; compose by MERGE-IGNORE in root-to-leaf order.
;;;
;;; Pattern syntax (gitignore(5)):
;;;   blank line / "#..."      no rule;  "\#" and "\!" escape a leading # or !
;;;   trailing spaces          dropped unless escaped ("foo\ ")
;;;   trailing CR              dropped (CRLF files)              [pathlib choice]
;;;   "!pat"                   negation: re-include
;;;   "pat/"                   directories only
;;;   "/" at start or middle   anchored to BASE; otherwise matches at any depth
;;;   "*" "?" "[a-z]" "[!x]" "[^x]"  never match "/"
;;;   "**/x"  "x/**"  "a/**/b" any number of directories (zero included);
;;;                            any other "**" is a plain "*"
;;; Matching is case-sensitive. A path under an ignored directory is ignored
;;; whatever later rules say (git cannot re-include it either).
;;;
;;; Representation: a rule's pattern is a list of segments matched against
;;; the "/"-split path below BASE. A segment is :GLOBSTAR ("**", zero or more
;;; path segments), :GLOBSTAR+ (a trailing "**": one or more) or a simple
;;; vector of tokens (characters, :ANY for "?", :STAR for "*", %CHAR-CLASS).
;;; An unanchored pattern gets a leading :GLOBSTAR ("foo" = "**/foo"), so a
;;; token can never see a "/" and "matches at any depth" needs no special case.

(defclass ignore-matcher ()
  ((rules :initarg :rules :initform #() :reader %ignore-matcher-rules
          :documentation "Simple vector of rules in precedence order, lowest first."))
  (:documentation "Parsed .gitignore / .ignore rules. Build with PARSE-IGNORE,
combine with MERGE-IGNORE, query with IGNORE-PATH-P / IGNORE-PATH-STATUS."))

(defun ignore-matcher-p (x) (typep x 'ignore-matcher))

(defmethod print-object ((m ignore-matcher) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~D rule~:P" (length (%ignore-matcher-rules m)))))

(defstruct (%ignore-rule (:constructor %make-ignore-rule (pattern negated dir-only base segments))
                         (:copier nil))
  (pattern "" :type string)            ; source text, for debugging
  (negated nil)
  (dir-only nil)
  (base "" :type string)               ; "" or "sub/"
  (segments '() :type list))

(defstruct (%char-class (:constructor %make-char-class (negated items)) (:copier nil))
  (negated nil)
  (items '() :type list))              ; characters and (lo . hi) conses

;;; --- parsing ---------------------------------------------------------------

(defun %parse-char-class (pattern start)
  "Parse a bracket expression at PATTERN[START] = #\\[. Returns
(values class index-after-closing-bracket), or NIL when unterminated."
  (let ((n (length pattern))
        (i (1+ start))
        (negated nil)
        (items '()))
    (when (and (< i n) (member (char pattern i) '(#\! #\^)))
      (setf negated t)
      (incf i))
    (flet ((item-char ()
             ;; One (possibly escaped) character at I, advancing I; NIL at end.
             (cond ((>= i n) nil)
                   ((and (char= (char pattern i) #\\) (< (1+ i) n))
                    (prog1 (char pattern (1+ i)) (incf i 2)))
                   (t (prog1 (char pattern i) (incf i))))))
      (let ((first t))
        (loop
          (when (>= i n) (return-from %parse-char-class nil))
          (when (and (char= (char pattern i) #\]) (not first))
            (return (values (%make-char-class negated (nreverse items)) (1+ i))))
          (setf first nil)
          (let ((lo (item-char)))
            (unless lo (return-from %parse-char-class nil))
            (if (and (< (1+ i) n)
                     (char= (char pattern i) #\-)
                     (char/= (char pattern (1+ i)) #\]))
                (progn
                  (incf i)
                  (let ((hi (item-char)))
                    (unless hi (return-from %parse-char-class nil))
                    (push (cons lo hi) items)))
                (push lo items))))))))

(defun %char-class-match-p (class ch)
  (let ((hit (loop for it in (%char-class-items class)
                   thereis (if (consp it)
                               (char<= (car it) ch (cdr it))
                               (char= it ch)))))
    (if (%char-class-negated class) (not hit) hit)))

(defun %tokenize-segment (segment)
  "Token vector for one slash-free pattern SEGMENT."
  (let ((out '()) (i 0) (n (length segment)))
    (loop while (< i n)
          do (let ((c (char segment i)))
               (case c
                 (#\\ (if (< (1+ i) n)
                          (progn (push (char segment (1+ i)) out) (incf i 2))
                          (incf i)))
                 (#\? (push :any out) (incf i))
                 (#\* (unless (eq (first out) :star) (push :star out)) (incf i))
                 (#\[ (multiple-value-bind (class next) (%parse-char-class segment i)
                        (if class
                            (progn (push class out) (setf i next))
                            (progn (push #\[ out) (incf i)))))
                 (t (push c out) (incf i)))))
    (coerce (nreverse out) 'simple-vector)))

(defun %strip-trailing-spaces (line)
  "LINE without trailing spaces, except a space escaped by an odd number of
backslashes."
  (let ((end (length line)))
    (loop while (and (plusp end) (char= (char line (1- end)) #\Space))
          do (let ((backslashes (loop for k downfrom (- end 2) to 0
                                      while (char= (char line k) #\\)
                                      count t)))
               (if (oddp backslashes)
                   (return)
                   (decf end))))
    (subseq line 0 end)))

(defun %parse-ignore-line (line base)
  "An %IGNORE-RULE for LINE, or NIL for blank lines, comments and empty patterns."
  (let* ((line (if (and (plusp (length line)) (char= (char line (1- (length line))) #\Return))
                   (subseq line 0 (1- (length line)))
                   line)))
    (when (or (zerop (length line)) (char= (char line 0) #\#))
      (return-from %parse-ignore-line nil))
    (let ((pat (%strip-trailing-spaces line))
          (negated nil)
          (dir-only nil))
      (when (and (plusp (length pat)) (char= (char pat 0) #\!))
        (setf negated t pat (subseq pat 1)))
      (when (and (plusp (length pat)) (char= (char pat (1- (length pat))) #\/))
        (setf dir-only t pat (subseq pat 0 (1- (length pat)))))
      (when (zerop (length pat))
        (return-from %parse-ignore-line nil))
      (let ((anchored (find #\/ pat)))
        (when (char= (char pat 0) #\/)
          (setf pat (subseq pat 1)))
        (when (zerop (length pat))
          (return-from %parse-ignore-line nil))
        (let* ((parts (uiop:split-string pat :separator "/"))
               (segments (loop for (part . more) on parts
                               collect (cond ((and (string= part "**") more) :globstar)
                                             ((string= part "**") :globstar+)
                                             (t (%tokenize-segment part))))))
          (%make-ignore-rule line negated dir-only base
                             (if anchored segments (cons :globstar segments))))))))

(defun %split-at-newlines (text)
  (let ((out '()) (start 0) (n (length text)))
    (loop for nl = (position #\Newline text :start start)
          do (push (subseq text start (or nl n)) out)
             (if nl (setf start (1+ nl)) (return)))
    (nreverse out)))

(defun parse-ignore (source &key (base ""))
  "Parse SOURCE (the text of an ignore file, or a list of its lines) into an
IGNORE-MATCHER. BASE is the directory of that file relative to the root the
matcher is applied to: \"\" or a string ending in \"/\" (\"sub/\")."
  (check-type source (or string list))
  (check-type base string)
  (let ((base (if (or (zerop (length base)) (char= (char base (1- (length base))) #\/))
                  base
                  (concatenate 'string base "/")))
        (lines (if (stringp source) (%split-at-newlines source) source)))
    (make-instance 'ignore-matcher
                   :rules (coerce (loop for line in lines
                                        for rule = (%parse-ignore-line (string line) base)
                                        when rule collect rule)
                                  'simple-vector))))

(defun merge-ignore (&rest matchers)
  "One matcher holding the rules of MATCHERS in order; later ones take
precedence (pass parent directories before children). NIL entries are skipped."
  (make-instance 'ignore-matcher
                 :rules (apply #'concatenate 'simple-vector
                               (loop for m in matchers
                                     when m
                                       collect (progn (check-type m ignore-matcher)
                                                      (%ignore-matcher-rules m))))))

;;; --- matching --------------------------------------------------------------

(defun %match-segment (tokens string)
  "True when the token vector TOKENS matches all of the slash-free STRING."
  (declare (type simple-vector tokens) (type string string))
  (let ((nt (length tokens)) (ns (length string)))
    (labels ((m (ti si)
               (if (= ti nt)
                   (= si ns)
                   (let ((tk (svref tokens ti)))
                     (cond ((eq tk :star)
                            (if (= (1+ ti) nt)
                                t
                                (loop for k from si to ns thereis (m (1+ ti) k))))
                           ((>= si ns) nil)
                           ((eq tk :any) (m (1+ ti) (1+ si)))
                           ((characterp tk)
                            (and (char= tk (char string si)) (m (1+ ti) (1+ si))))
                           (t (and (%char-class-match-p tk (char string si))
                                   (m (1+ ti) (1+ si)))))))))
      (m 0 0))))

(defun %match-segments (patterns segments)
  (cond ((null patterns) (null segments))
        ((eq (car patterns) :globstar)
         (loop for rest on (cons nil segments)
               thereis (%match-segments (cdr patterns) (cdr rest))))
        ((eq (car patterns) :globstar+)
         (and segments t))
        ((null segments) nil)
        (t (and (%match-segment (car patterns) (car segments))
                (%match-segments (cdr patterns) (cdr segments))))))

(defun %rule-matches-p (rule rel segments directory)
  "Does RULE match the normalized relpath REL (split into SEGMENTS)?"
  (and (or directory (not (%ignore-rule-dir-only rule)))
       (let ((base (%ignore-rule-base rule)))
         (and (> (length rel) (length base))
              (uiop:string-prefix-p base rel)
              (%match-segments (%ignore-rule-segments rule)
                               (nthcdr (count #\/ base) segments))))))

(defun %normalize-relpath (relpath directory)
  "(values rel directory-p): RELPATH without leading \"./\" or \"/\" and
without a trailing \"/\" (which marks a directory)."
  (let* ((s (string relpath))
         (dir-p (or directory
                    (and (plusp (length s)) (char= (char s (1- (length s))) #\/))))
         (s (string-right-trim "/" s))
         (s (loop while (uiop:string-prefix-p "./" s)
                  do (setf s (subseq s 2))
                  finally (return (string-left-trim "/" s)))))
    (values s (and dir-p t))))

(defun %status (matcher rel directory)
  (let ((rules (%ignore-matcher-rules matcher))
        (segments (uiop:split-string rel :separator "/")))
    (loop for k from (1- (length rules)) downto 0
          for r = (svref rules k)
          when (%rule-matches-p r rel segments directory)
            return (if (%ignore-rule-negated r) :included :ignored))))

(defun ignore-path-status (matcher relpath &key directory)
  "Verdict of the last rule matching RELPATH itself (no ancestor check):
:IGNORED, :INCLUDED (a negation matched) or NIL (no rule matched). RELPATH is
a \"/\"-separated path relative to the root; a trailing \"/\" or DIRECTORY true
marks a directory."
  (check-type matcher ignore-matcher)
  (multiple-value-bind (rel dir-p) (%normalize-relpath relpath directory)
    (and (plusp (length rel)) (%status matcher rel dir-p))))

(defun ignore-path-p (matcher relpath &key directory)
  "True when RELPATH is ignored: some ancestor directory of it has status
:IGNORED, or RELPATH's own status is :IGNORED. Same RELPATH conventions as
IGNORE-PATH-STATUS."
  (check-type matcher ignore-matcher)
  (multiple-value-bind (rel dir-p) (%normalize-relpath relpath directory)
    (and (plusp (length rel))
         (or (loop for k = (position #\/ rel) then (position #\/ rel :start (1+ k))
                   while k
                   thereis (eq :ignored (%status matcher (subseq rel 0 k) t)))
             (eq :ignored (%status matcher rel dir-p))))))

;;; --- relpaths of filesystem entries ----------------------------------------

(defun %posix-prefix (fs directory)
  "Posix namestring of DIRECTORY on FS ending in \"/\": the prefix stripped by
%RELPATH."
  (let ((s (%pathname-as-posix (fs-absolute fs directory))))
    (if (and (plusp (length s)) (char= (char s (1- (length s))) #\/))
        s
        (concatenate 'string s "/"))))

(defun %relpath (prefix pathname)
  "PATHNAME relative to PREFIX (from %POSIX-PREFIX), without a trailing
\"/\"; NIL when PATHNAME is not strictly below PREFIX."
  (let ((s (string-right-trim "/" (%pathname-as-posix pathname))))
    (when (and (> (length s) (length prefix)) (uiop:string-prefix-p prefix s))
      (subseq s (length prefix)))))

(defun %entry-name (pathname)
  "Last component of PATHNAME, for file and directory pathnames alike."
  (let ((s (string-right-trim "/" (%pathname-as-posix pathname))))
    (subseq s (1+ (or (position #\/ s :from-end t) -1)))))

;;; --- loading ignore files --------------------------------------------------

(defun load-ignore-files (fs root &key (names '(".gitignore" ".ignore")))
  "Read every ignore file named in NAMES under ROOT on FS into one matcher with
per-directory BASEs. Within one directory the files apply in NAMES order (so
.ignore beats .gitignore, as in ripgrep); deeper directories beat shallower
ones. Directories ignored by rules already read are not searched, symlinks to
directories are never followed, and a directory named \".git\" is always
skipped. This is what :IGNORE :AUTO uses."
  (let ((acc (make-instance 'ignore-matcher)))
    (labels ((visit (dir base)
               (dolist (name names)
                 (let ((file (fs-join fs dir name)))
                   (when (fs-file-p fs file)
                     (setf acc (merge-ignore acc (parse-ignore
                                                  (utf8-decode (fs-read-bytes fs file))
                                                  :base base))))))
               (dolist (sub (fs-iterdir fs dir))
                 (when (and (fs-directory-p fs sub)
                            ;; Never through a link: it may leave ROOT or loop.
                            (not (fs-symlink-p fs sub)))
                   (let* ((name (%entry-name sub))
                          (rel (concatenate 'string base name)))
                     (unless (or (string= name ".git")
                                 (ignore-path-p acc rel :directory t))
                       (visit sub (concatenate 'string rel "/"))))))))
      (visit (fs-absolute fs root) "")
      acc)))

;;; --- :IGNORE designators for walk / glob -----------------------------------

(defun %resolve-ignore (fs base ignore)
  "The matcher for the :IGNORE designator IGNORE of a walk / glob at BASE:
NIL, the IGNORE-MATCHER itself, or for :AUTO the ignore files under BASE plus
a final \".git/\" rule (highest precedence, so .git is always pruned)."
  (etypecase ignore
    (null nil)
    (ignore-matcher ignore)
    ((eql :auto) (merge-ignore (load-ignore-files fs base) (parse-ignore '(".git/"))))))

(defun %ignore-predicate (fs base ignore &key (prefix-of base))
  "NIL when IGNORE is NIL; otherwise a function (PATHNAME DIRECTORY-P) that is
true when the entry PATHNAME under BASE is ignored. PREFIX-OF is the
directory whose namestring prefixes the entries (BASE unless a backend lists
truenames). BASE itself is never ignored."
  (let ((matcher (%resolve-ignore fs base ignore)))
    (when matcher
      (let ((prefix (%posix-prefix fs prefix-of)))
        (lambda (pathname directory-p)
          (let ((rel (%relpath prefix pathname)))
            (and rel (ignore-path-p matcher rel :directory directory-p))))))))
