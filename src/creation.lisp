;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; Routines for creating arrays
;;; 

(in-package #:array-operations)

(defun zeros (dimensions &key (element-type 'single-float))
  "Makes an array of shape DIMENSIONS, filled with zeros
   coerced to the specified type ELEMENT-TYPE."
  (make-array dimensions :element-type element-type
              :initial-element (coerce 0 element-type)))

(defun ones (dimensions &key (element-type 'single-float))
  "Makes an array of shape DIMENSIONS, filled with ones
   coerced to the specified type ELEMENT-TYPE."
  (make-array dimensions :element-type element-type
              :initial-element (coerce 1 element-type)))

(defun rand (dimensions &key (element-type 'single-float))
  "Makes an array of shape DIMENSIONS, filled with random numbers
   uniformly distributed between 0 and 1.

   Uses the built-in RANDOM function.
   
   (rand 3)  -> #(0.39319038 0.69693553 0.5021677)
   (rand '(2 2)) -> #2A((0.91003513 0.23208928) (0.5577954 0.94657767))

   NOTE: If it's important that these numbers are really random
   (e.g. cryptographic applications), then you should probably
   not use this function.
   "
  (let* ((arr (make-array dimensions :element-type element-type))
         (size (array-total-size arr)))
    (dotimes (i size)
      (setf (row-major-aref arr i) (coerce (random 1.0) element-type)))
    arr))

(defun randn (dimensions &key (element-type 'single-float))
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
  (let* ((arr (make-array dimensions :element-type element-type))
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

(defun linspace (start stop n)
  "Make a vector of N elements with first element START and last element STOP
  
  The element type is set by the type of (STOP - START) / (N - 1)

  (linspace 0 4 5) -> #(0 1 2 3 4)
  (linspace 1 3 5) -> #(0 1/2 1 3/2 2)
  (linspace 0 4d0 3) -> #(0.0d0 2.0d0 4.0d0)
  "
  (let* ((delta (/ (- stop start) (- n 1))) ; Difference between values
         (type (case (type-of delta)
                 ('bit 'integer)   ; If delta is 1 then becomes bit rather than integer
                 (otherwise (type-of delta))))
         (result (make-array n :element-type type)))
    (dotimes (i n)
      (setf (aref result i) (* delta i)))
    result))
