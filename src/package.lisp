(defpackage :array-operations
  (:use :cl)
  (:import-from :alexandria
                :compose
                :curry
                :ensure-list
                :length=
                :with-unique-names)
  (:import-from :optima
                :ematch)
  (:nicknames :aops)
  (:shadow :flatten)
  ;; Generic
  (:export :as-array
           :element-type
           :size
           :rank
           :dim :dims :&dims
           :nrow :ncol)
  ;; Utilities
  (:export :walk-subscripts
           :walk-subscripts-list
           :nested-loop)
  ;; Creating
  (:export :fill!
           :zeros    :zeros*    :zeros!
           :ones     :ones*     :ones!
           :rand     :rand*     :rand!
           :randn    :randn*    :randn!
           :linspace :linspace* :linspace!
           :similar-array)
  ;; Indexing
  (:export :each-index :each-index* :each-index!
           :sum-index
           :reduce-index)
  ;; Displacing
  (:export :displace
           :flatten
           :split
           :copy-into
           :sub
           :partition
           :combine
           :subvec
           :reshape
           :reshape-col :reshape-row)
  ;; Reducing
  (:export :most
           :best
           :argmax
           :argmin
           :vectorize-reduce)
  ;; Transforming
  (:export :coercing
           :generate*  :generate
           :each*      :each
           :margin*    :margin
           :outer*     :outer
           :vectorize* :vectorize :vectorize!
           :invert-permutation
           :complete-permutation
           :complement-permutation
           :identity-permutation-p
           :identity-permutation? ; deprecated alias for above
           :permutation-invalid-index
           :permutation-repeated-index
           :permutation-incompatible-rank
           :permute
           :recycle)
  ;; Stacking
  (:export :copy-row-major-block
           :stack-rows-copy :stack-rows* :stack-rows
           :stack-cols-copy :stack-cols* :stack-cols
           :stack* :stack)
  ;; Matrices
  (:export :array-matrix
           :matrixp
           :square-matrix-p
           ;; These next two are deprecated aliases for the previous two.
           :matrix?
           :square-matrix?))
