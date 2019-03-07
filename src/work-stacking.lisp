;;; helpers
;; - displaced arrays
;; - flatten
;; - replace

;;; algo
;; - checks
;; - make result, flat-result
;; - loop over arguments
;; -- case
;; --- (fill flat-result arg :start x :end y)
;; --- (replace flat-result (flatten arg) :start1 x :end1 y)

;; Note: Change #'dim to report scalars as 0 in the 0th dimension
;;       Would let me...?

(defun dim* (object n)
  "Return Nth dimension of array or scalar. Scalars always return 1."
  (etypecase object
    (array (dim object n))
    (number 1)))


;; TODO: Move #'rows and #'cols to "matrices.lisp"
;; TODO: ...and then, perhaps, 2D stacking goes there also?
; >2D generalization: (_ (dim object (1- _)))

;; or call this 'height'?
(defun rows (object)
  "Return the number of rows of an array or scalar. Scalars return 1."
  (etypecase object
    (array (ecase (rank object)
             (1 1)
             (2 (dim object 0))))
    (number 1)))

;; or call this 'width'?
(defun cols (object)
  "Return the number of columns of an array or scalar. Scalars return 1."
  (etypecase object
    (array (ecase (rank object)
             (1 (dim object 0))
             (2 (dim object 1))))
    (number 1)))

;; TODO: Extend/generalize to tensors of rank >2?
(defun stack-rows* (element-type &rest objects)
  "Stack OBJECTS, row-wise, into a simple 2D array of given ELEMENT-TYPE.

   Objects may be arrays or scalars.
   Array arguments must all have the same number of columns.
   Scalars are repeated to fill their row in the result."
  (let* ((array-objects (remove-if-not #'arrayp objects))
         (array-objects-cols (mapcar #'cols array-objects)))
    (when array-objects
      ;; FIXME: add message to this assert
      (assert (apply #'= array-objects-cols))) ; Any arrays should be = width.
    (let* ((height (reduce #'+ (mapcar #'rows objects)))
           (width (or (first array-objects-cols) 1)) ; 1 when only scalars.
           (result (make-array (list height width)
                               :element-type element-type))
           (flat-result (flatten result)))
      (loop as object in objects
            with cursor = 0
            when (arrayp object)
              ;; If we didn't need to coerce, we could just use REPLACE.
              do (loop for object-element across (flatten object)
                       for idx from cursor
                       do (setf (aref flat-result idx)
                                (coerce object-element element-type)))
                 (incf cursor (array-total-size object))
            else
              do (fill flat-result
                       (coerce object element-type)
                       :start cursor
                       :end (+ cursor width))
                 (incf cursor width))
      result)))
