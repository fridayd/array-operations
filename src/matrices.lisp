;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; representing matrices as 2D arrays

(deftype array-matrix ()
  "A rank-2 array."
  '(array * (* *)))

(declaim (inline matrixp square-matrix-p))

(defun matrixp (matrix)
  "Test if MATRIX has rank 2."
  (length= (dims matrix) 2))

(defun square-matrix-p (matrix)
  "Test if MATRIX has two dimensions and that they are equal."
  (let ((dims (dims matrix)))
    (and (length= dims 2)
         (= (first dims) (second dims)))))

;; Aliases for deprecated names 'matrix?' and 'square-matrix?'
(setf (fdefinition 'matrix?) #'matrixp)
(setf (fdefinition 'square-matrix?) #'square-matrix-p)
