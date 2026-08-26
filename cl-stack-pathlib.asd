(defsystem "cl-stack-pathlib"
  :version "0.2.1"
  :description "CLOS paths + pluggable filesystems (pathlib/NIO-style) for cl-stack"
  :author "egao1980"
  :license "MIT"
  :depends-on ("uiop" "chipz")

  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "path")
               (:file "local")
               (:file "memory")
               (:file "facade")
               (:file "zip"))
  :in-order-to ((test-op (test-op "cl-stack-pathlib/tests"))))

(defsystem "cl-stack-pathlib/tests"
  :depends-on ("cl-stack-pathlib" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "pure-path-test")
               (:file "boost-path-test")
               (:file "normalize-test")
               (:file "fs-ops-test")
               (:file "restarts-test")
               (:file "uri-zip-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
