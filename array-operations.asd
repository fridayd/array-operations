(asdf:defsystem :array-operations
  :description "Simple array operations library for Common Lisp."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :depends-on (#:alexandria
               #:optima)
  :pathname "src/"
  :components ((:file "package")
               (:file "generic")
               (:file "reducing")
               (:file "matrices"     :depends-on ("generic"))
               (:file "utilities"    :depends-on ("generic"))
               (:file "creating"     :depends-on ("generic" "utilities"))
               (:file "indexing"     :depends-on ("generic" "utilities"))
               (:file "displacing"   :depends-on ("generic" "utilities"))
               (:file "transforming" :depends-on ("generic" "utilities" "displacing"))
               (:file "stacking"     :depends-on ("generic" "displacing")))
  :in-order-to ((test-op (load-op :array-operations/tests)))
  :perform (test-op (o c) (uiop:symbol-call :array-operations/tests :run)))

(asdf:defsystem :array-operations/tests
  :description "Unit tests for the ARRAY-OPERATIONS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :depends-on (:array-operations       ; loads everything else
               :clunit)
  :pathname "tests/"
  :components ((:file "tests")))
