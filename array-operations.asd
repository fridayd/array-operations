;;;; array-operations.asd

(asdf:defsystem #:array-operations
  :serial t
  :description "Simple array operations library for Common Lisp."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :depends-on (#:alexandria
               #:optima)
  :pathname #P"src/"
  :components ((:file "package")
               (:file "generic")
               (:file "reducing")
               (:file "utilities"    :depends-on ("generic"))
               (:file "creating"     :depends-on ("generic" "utilities"))
               (:file "indexing"     :depends-on ("generic" "utilities"))
               (:file "displacing"   :depends-on ("generic" "utilities"))
               (:file "transforming" :depends-on ("generic" "utilities" "displacing"))
               (:file "stacking"     :depends-on ("generic" "displacing"))))

(asdf:defsystem #:array-operations-tests
  :serial t
  :description "Unit tests for the ARRAY-OPERATIONS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :depends-on (#:array-operations       ; loads everything else
               #:clunit)
  :pathname #P"tests/"
  :components ((:file "tests")))
