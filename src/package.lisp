;;;; package.lisp

(defpackage #:array-operations
  (:use #:cl)
  (:import-from #:alexandria
                #:compose
                #:curry
                #:ensure-list
                #:length=
                #:with-unique-names)
  (:import-from #:optima
                #:ematch)
  (:nicknames #:aops)
  (:shadow #:flatten)
  (:export ; creation
   #:fill!
   #:zeros
   #:zeros*
   #:zeros!
   #:ones
   #:ones*
   #:ones!
   #:rand
   #:rand*
   #:rand!
   #:randn
   #:randn*
   #:randn!
   #:linspace
   #:linspace*
   #:linspace!
   #:similar-array)
  (:export ; utilities
   #:walk-subscripts
   #:walk-subscripts-list
   #:nested-loop)
  (:export ; general
   #:as-array
   #:element-type
   #:dims
   #:size
   #:rank
   #:dim
   #:&dims
   #:nrow
   #:ncol
   #:array-matrix
   #:matrixp
   #:square-matrix-p
   ;; These next two are deprecated aliases for the previous two.
   #:matrix?
   #:square-matrix?)
  (:export ; displacement
   #:displace
   #:flatten
   #:split
   #:copy-into
   #:sub
   #:partition
   #:combine
   #:subvec
   #:reshape
   #:reshape-col
   #:reshape-row)
  (:export ; transformations
   #:coercing
   #:generate*
   #:generate
   #:permutation-repeated-index
   #:permutation-invalid-index
   #:permutation-incompatible-rank
   #:complement-permutation
   #:complete-permutation
   #:invert-permutation
   #:identity-permutation-p
   #:identity-permutation? ; deprecated alias for above
   #:permute
   #:each*
   #:each
   #:margin*
   #:margin
   #:recycle
   #:outer*
   #:outer
   #:vectorize!
   #:vectorize*
   #:vectorize)
  (:export ; reductions
   #:most
   #:best
   #:argmax
   #:argmin
   #:vectorize-reduce)
  (:export ; indexing
   #:each-index
   #:each-index*
   #:each-index!
   #:sum-index
   #:reduce-index)
  (:export ; stack
   #:copy-row-major-block
   #:stack-rows-copy
   #:stack-rows*
   #:stack-rows
   #:stack-cols-copy
   #:stack-cols*
   #:stack-cols
   #:stack*
   #:stack))
