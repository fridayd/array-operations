#!/usr/local/bin/sbcl --script

;;;; Generate coverage report, using SBCL's sb-cover module.

(load "~/.sbclrc")

(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(asdf:load-system :array-operations :force '(:array-operations/creating
                                             :array-operations/matrices
                                             :array-operations/stacking
                                             :array-operations/reducing
                                             :array-operations/indexing
                                             :array-operations/utilities
                                             :array-operations/displacing
                                             :array-operations/transforming))

(declaim (optimize (sb-cover:store-coverage-data 0)))

(asdf:test-system :array-operations)

(sb-cover:report "coverage/")

(sb-cover:clear-coverage)
