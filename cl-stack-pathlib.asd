(defsystem "cl-stack-pathlib"
  :version "0.1.0"
  :description "CLOS paths + pluggable filesystems (pathlib/NIO-style) for cl-stack"
  :author "egao1980"
  :license "MIT"
  :depends-on ("uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "path")
               (:file "local")
               (:file "memory")
               (:file "facade"))
  :in-order-to ((test-op (test-op "cl-stack-pathlib/tests"))))

(defsystem "cl-stack-pathlib/tests"
  :depends-on ("cl-stack-pathlib" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "pure-path-test")
               (:file "boost-path-test")
               (:file "normalize-test")
               (:file "fs-ops-test"))
  :perform (test-op (o c) (symbol-call :rove :run c)))
