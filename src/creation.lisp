;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; Routines for creating arrays
;;; 

(in-package #:array-operations)

(defun zeros* (element-type dimensions)
  "Makes an array of shape DIMENSIONS and type ELEMENT-TYPE, filled with zeros
   coerced to the specified type ELEMENT-TYPE."
  (make-array (ensure-dimensions dimensions)
              :element-type element-type
              :initial-element (coerce 0 element-type)))

(defun zeros (dimensions)
  "Makes an array of shape DIMENSIONS and type T, filled with zeros"
  (make-array (ensure-dimensions dimensions)
              :initial-element 0))

(defun ones* (element-type dimensions)
  "Makes an array of shape DIMENSIONS and type ELEMENT-TYPE, filled with ones
   coerced to the specified type ELEMENT-TYPE."
  (make-array (ensure-dimensions dimensions)
              :element-type element-type
              :initial-element (coerce 1 element-type)))

(defun ones (dimensions)
  "Makes an array of shape DIMENSIONS and type T, filled with ones"
  (make-array (ensure-dimensions dimensions)
              :initial-element 1))

(defun rand* (element-type dimensions)
  "Makes an array of shape DIMENSIONS and type ELEMENT-TYPE, filled with random numbers
   uniformly distributed between 0 and 1.

   Uses the built-in RANDOM function.
   
   (rand 3)  -> #(0.39319038 0.69693553 0.5021677)
   (rand '(2 2)) -> #2A((0.91003513 0.23208928) (0.5577954 0.94657767))

   NOTE: If it's important that these numbers are really random
   (e.g. cryptographic applications), then you should probably
   not use this function.
   "
  (let* ((arr (make-array (ensure-dimensions dimensions)
                          :element-type element-type))
         (size (array-total-size arr)))
    (dotimes (i size)
      (setf (row-major-aref arr i) (coerce (random 1.0) element-type)))
    arr))

(defun rand (dimensions)
  "Makes an array of shape DIMENSIONS and type T, filled with random numbers
   uniformly distributed between 0 and 1.

   Uses the built-in RANDOM function.
   
   (rand 3)  -> #(0.39319038 0.69693553 0.5021677)
   (rand '(2 2)) -> #2A((0.91003513 0.23208928) (0.5577954 0.94657767))

   NOTE: If it's important that these numbers are really random
   (e.g. cryptographic applications), then you should probably
   not use this function.
   "
  (rand* t dimensions))

(defun randn* (element-type dimensions)
  "Creates an array of shape DIMENSIONS and type ELEMENT-TYPE,
   and fills with normally distributed numbers
   with a mean of zero and standard deviation of 1

   Uses the Box-Muller algorithm and built-in random number generator.

   (rand 3)   -> #(-0.82067037 -0.60068226 -0.21494178)
   (randn '(2 2)) -> #2A((1.6905352 -2.5379088) (0.8461403 -1.505984))

   NOTE: If it's important that these numbers are really random
   (e.g. cryptographic applications), then you should probably
   not use this function.
   "
  (let* ((arr (make-array (ensure-dimensions dimensions)
                          :element-type element-type))
         (size (array-total-size arr)))
    (do ((i 0 (+ 2 i)))
        ((>= i (- size 1)))   
      ;; Box-Muller algorithm
      ;; Generate two uniform random numbers, u1 and u2
      ;;
      ;;  r = sqrt(-2 log(u1))
      ;; then two normally-distributed numbers are
      ;;  z0 = r * cos(2pi u2)
      ;;  z1 = r * sin(2pi u2)
      (let* ((u1 (random 1.0))
             (2piu2 (* 2 pi (random 1.0))) ; 2 * pi * u2
             (r (sqrt (* -2 (log u1)))))
        (setf (row-major-aref arr i) (coerce (* r (cos 2piu2)) element-type))
        (setf (row-major-aref arr (1+ i)) (coerce (* r (sin 2piu2)) element-type))))
    ;; If size is odd then one extra random number is needed
    (if (not (zerop (logand size 1)))
        (let* ((u1 (random 1.0))
               (2piu2 (* 2 pi (random 1.0)))
               (r (sqrt (* -2 (log u1)))))
          (setf (row-major-aref arr (1- size)) (coerce (* r (cos 2piu2)) element-type))))
    arr))

(defun randn (dimensions)
  "Creates an array of shape DIMENSIONS and type T,
   and fills with normally distributed numbers
   with a mean of zero and standard deviation of 1

   Uses the Box-Muller algorithm and built-in random number generator.

   (rand 3)   -> #(-0.82067037 -0.60068226 -0.21494178)
   (randn '(2 2)) -> #2A((1.6905352 -2.5379088) (0.8461403 -1.505984))

   NOTE: If it's important that these numbers are really random
   (e.g. cryptographic applications), then you should probably
   not use this function.
   "
  (randn* t dimensions))

(defun linspace* (element-type start stop n)
  "Make a vector of N elements and type ELEMENT-TYPE, containing evenly spaced numbers over an interval.
   The first element is equal to START and last element STOP, 
   with constant difference between consecutive elements."
  (let ((delta (/ (- stop start) (- n 1))) ; Difference between values
        (result (make-array n :element-type element-type)))
    (dotimes (i n)
      (setf (aref result i) (coerce (* delta i) element-type)))
    result))

(defun linspace (start stop n)
  "Make a vector of N elements and type T, containing evenly spaced numbers over an interval.
   The first element is equal to START and last element STOP, 
   with constant difference between consecutive elements.

  (linspace 0 4 5) -> #(0 1 2 3 4)
  (linspace 1 3 5) -> #(0 1/2 1 3/2 2)
  (linspace 0 4d0 3) -> #(0.0d0 2.0d0 4.0d0)
  "
  (linspace* t start stop n))
