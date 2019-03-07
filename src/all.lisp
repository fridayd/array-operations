(uiop/package:define-package :array-operations/all
  (:nicknames :array-operations :aops)
  (:use-reexport :array-operations/generic
                 :array-operations/reducing
                 :array-operations/matrices
                 :array-operations/creating
                 :array-operations/indexing
                 :array-operations/displacing
                 :array-operations/transforming
                 :array-operations/stacking))
;; Note: array-operations/utilities *not* exported for external use.
