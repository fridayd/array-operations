#!/usr/local/bin/sbcl --script

;;;; Generate coverage report, using SBCL's sb-cover module.

(load "~/.sbclrc")
(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(asdf:compile-system :array-operations/all :force t)

(declaim (optimize (sb-cover:store-coverage-data 0)))

(asdf:test-system :array-operations)

(sb-cover:report "coverage/")

(sb-cover:clear-coverage)
