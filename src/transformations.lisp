;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; creating arrays

(defun generate* (element-type function dimensions &optional arguments)
  "Return an array with given DIMENSIONS and ELEMENT-TYPE, with elements
generated by calling FUNCTION with

 - no arguments, when ARGUMENTS is nil
 - the position (= row major index), when ARGUMENTS is :POSITION
 - a list of subscripts, when ARGUMENTS is :SUBSCRIPTS
 - both when ARGUMENTS is :POSITION-AND-SUBSCRIPTS

The traversal order is unspecified and may be nonlinear."
  (aprog1 (make-array dimensions :element-type element-type)
    (ecase arguments
      ((nil)
       (dotimes (position (array-total-size it))
         (setf (row-major-aref it position)
               (funcall function))))
      (:position
       (walk-subscripts (dimensions subscripts position)
         (setf (row-major-aref it position) (funcall function position))))
      (:subscripts
       (walk-subscripts-list (dimensions subscripts position)
         (setf (row-major-aref it position)
               (funcall function subscripts))))
      (:position-and-subscripts
       (walk-subscripts-list (dimensions subscripts position)
         (setf (row-major-aref it position)
               (funcall function position subscripts)))))))

(defun generate (function dimensions &optional arguments)
  "Like GENERATE*, with ELEMENT-TYPE T."
  (generate* t function dimensions arguments))



;;; permutations
;;;
;;; A permutation is a list of nonnegative, non-repeated integers, below some
;;; rank (of the array it is applied to).

(define-condition permutation-repeated-index (error)
  ((index :initarg :index)))

(define-condition permutation-invalid-index (error)
  ((index :initarg :index)))

(define-condition permutation-incompatible-rank (error)
  ())

(defun permutation-flags (permutation &optional (rank (length permutation)))
  "Make a bit vector of flags with indexes from PERMUTATION, signalling errors
for invalid and repeated indices.  NOT EXPORTED."
  (aprog1 (make-array rank :element-type 'bit :initial-element 0)
    (map nil (lambda (p)
               (assert (and (integerp p) (< -1 p rank)) ()
                       'permutation-invalid-index :index p)
               (assert (zerop (aref it p)) ()
                       'permutation-repeated-index :index p)
               (setf (aref it p) 1))
         permutation)))

(defun check-permutation (permutation
                          &optional (rank (length permutation) rank?))
  "Check if PERMUTATION is a valid permutation (of the given RANK), and signal
an error if necessary."
  (when rank?
    (assert (= rank (length permutation)) ()
            'permutation-incompatible-rank ))
  (assert (every #'plusp (permutation-flags permutation)) ()
          'permutation-incompatible-rank))

(defun complement-permutation (permutation rank)
  "Return a list of increasing indices that complement PERMUTATION, ie form a
permutation when appended.  Atoms are accepted and treated as lists of a
single element."
  (loop for f across (permutation-flags (ensure-list permutation) rank)
        for index from 0
        when (zerop f)
        collect index))

(defun complete-permutation (permutation rank)
  "Return a completed version of permutation, appending it to its complement."
  (let ((permutation (ensure-list permutation)))
    (append permutation (complement-permutation permutation rank))))

(defun invert-permutation (permutation)
  "Invert a permutation."
  (check-permutation permutation)
  (coerce (aprog1 (make-array (length permutation) :element-type 'fixnum)
            (map nil (let ((index 0))
                       (lambda (p)
                         (setf (aref it p) index)
                  (incf index)))
                 permutation))
          'list))

(defun identity-permutation? (permutation
                              &optional (rank (length permutation)))
  "Test if PERMUTATION is the identity permutation, ie a sequence of
consecutive integers starting at 0.  Note that permutation is otherwise not
checked, ie it may not be a permutation."
  (let ((index 0))
    (and
     (every
      (lambda (p)
        (prog1 (= index p)
          (incf index)))
      permutation)
     (= index rank))))

(defun permute (permutation array)
  "Return ARRAY with the axes permuted by PERMUTATION, which is a sequence of
indexes.  Specifically, an array A is transformed to B, where

  B[b_1,...,b_n] = A[a_1,...,a_n] with b_i=a_{P[i]}

P is the permutation.

Array element type is preserved."
  (let ((rank (array-rank array)))
    (if (identity-permutation? permutation rank)
        array
        (let+ ((dimensions (array-dimensions array))
               ((&flet map-subscripts (subscripts-vector)
                  (map 'list (curry #'aref subscripts-vector) permutation))))
          (check-permutation permutation rank)
          (aprog1 (make-array (map-subscripts (coerce dimensions 'vector))
                              :element-type (array-element-type array))
            (walk-subscripts (dimensions subscripts position)
              (setf (apply #'aref it (map-subscripts subscripts))
                    (row-major-aref array position))))))))



;;; margin

(defun each* (element-type function array &rest other-arrays)
  "Apply function to the array arguments elementwise, and return the result as
an array with the given ELEMENT-TYPE.  Arguments are checked for dimension
compatibility."
  (aprog1 (make-array (array-dimensions array) :element-type element-type)
    (assert (apply #'same-dimensions? array other-arrays))
    (apply #'map-into (flatten it) function
           (flatten array) (mapcar #'flatten other-arrays))))

(defun each (function array &rest other-arrays)
  "Like EACH*, with ELEMENT-TYPE T."
  (apply #'each* t function array other-arrays))

(defun margin* (element-type function array inner
                &optional (outer (complement-permutation inner
                                                         (array-rank array))))
  "PERMUTE ARRAY with `(,@OUTER ,@INNER), split the inner subarrays, apply
FUNCTION to each, return the results in an array of dimensions OUTER, with the
given ELEMENT-TYPE."
  (let ((outer (ensure-list outer)))
    (each* element-type function
           (split (permute (append outer (ensure-list inner)) array)
                  (length outer)))))

(defun margin (function array inner
               &optional (outer (complement-permutation inner
                                                        (array-rank array))))
  "Like MARGIN*, with ELEMENT-TYPE T."
  (margin* t function array inner outer))
