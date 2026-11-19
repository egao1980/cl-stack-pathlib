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

(defclass ignore-matcher ()
  ((rules :initarg :rules :initform '() :reader %ignore-matcher-rules
          :documentation "Rules in precedence order, lowest first."))
  (:documentation "Parsed .gitignore / .ignore rules. Build with PARSE-IGNORE,
combine with MERGE-IGNORE, query with IGNORE-PATH-P / IGNORE-PATH-STATUS."))

(defun ignore-matcher-p (x) (typep x 'ignore-matcher))

(defun parse-ignore (source &key (base ""))
  "Parse SOURCE (the text of an ignore file, or a list of its lines) into an
IGNORE-MATCHER. BASE is the directory of that file relative to the root the
matcher is applied to: \"\" or a string ending in \"/\" (\"sub/\")."
  (declare (ignore source base))
  (%h2a-pending 'parse-ignore))

(defun merge-ignore (&rest matchers)
  "One matcher holding the rules of MATCHERS in order; later ones take
precedence (pass parent directories before children). NIL entries are skipped."
  (declare (ignore matchers))
  (%h2a-pending 'merge-ignore))

(defun ignore-path-status (matcher relpath &key directory)
  "Verdict of the last rule matching RELPATH itself (no ancestor check):
:IGNORED, :INCLUDED (a negation matched) or NIL (no rule matched). RELPATH is
a \"/\"-separated path relative to the root; a trailing \"/\" or DIRECTORY true
marks a directory."
  (declare (ignore matcher relpath directory))
  (%h2a-pending 'ignore-path-status))

(defun ignore-path-p (matcher relpath &key directory)
  "True when RELPATH is ignored: some ancestor directory of it has status
:IGNORED, or RELPATH's own status is :IGNORED. Same RELPATH conventions as
IGNORE-PATH-STATUS."
  (declare (ignore matcher relpath directory))
  (%h2a-pending 'ignore-path-p))

(defun load-ignore-files (fs root &key (names '(".gitignore" ".ignore")))
  "Read every ignore file named in NAMES under ROOT on FS into one matcher with
per-directory BASEs. Within one directory the files apply in NAMES order (so
.ignore beats .gitignore, as in ripgrep); deeper directories beat shallower
ones. Directories ignored by rules already read are not searched, and a
directory named \".git\" is always skipped. This is what :IGNORE :AUTO uses."
  (declare (ignore fs root names))
  (%h2a-pending 'load-ignore-files))
