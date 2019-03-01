(asdf:defsystem :array-operations
  :description "Simple array operations library for Common Lisp."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src/"
  :depends-on (:array-operations/all))

(asdf:defsystem :array-operations-tests
  :serial t
  :description "Unit tests for the ARRAY-OPERATIONS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :depends-on (:array-operations       ; loads everything else
               :alexandria
               :clunit)
  :pathname "tests/"
  :components ((:file "tests")))
