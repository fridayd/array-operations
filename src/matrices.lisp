;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; representing matrices as 2D arrays

(deftype array-matrix ()
  "A rank-2 array."
  '(array * (* *)))

(declaim (inline matrix? square-matrix?))
(defun matrix? (matrix)
  "Test if MATRIX has rank 2."
  (length= (dims matrix) 2))

(defun square-matrix? (matrix)
  "Test if MATRIX has two dimensions and that they are equal."
  (let ((dims (dims matrix)))
    (and (length= dims 2)
         (= (first dims) (second dims)))))
