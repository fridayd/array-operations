;;;; array-operations.asd

(asdf:defsystem #:array-operations
  :serial t
  :description "Simple array operations library for Common Lisp."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :maintainer "Ben Dudson <http://github.com/bendudson>"
  :homepage "https://github.com/bendudson/array-operations"
  :license "MIT"
  :depends-on (#:alexandria)
  :pathname #P"src/"
  :components ((:file "package")
               (:file "creating")
               (:file "utilities")
               (:file "generic")
               (:file "displacing")
               (:file "transforming")
               (:file "reducing")
               (:file "indexing")
               (:file "stacking")))

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
