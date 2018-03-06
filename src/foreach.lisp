(in-package #:array-operations)

(defun walk-array-indices (expr)
  "Walks an expression tree EXPR, finds AREF calls. 
   Returns a list of (symbol, array, index).

   Example: 
     (walk-array-indices '(+ (aref a i) (* 2 (aref b j k))))
   
   -> ((I A . 0) (K B . 1) (J B . 0))
    
  "
  (cond
    ;; If EXPR is not a list, nothing to return
    ((not (listp expr)) nil)

    ;; If EXPR is an AREF
    ((equalp (first expr) 'aref)
     ;; Parse this AREF for indices
     (let ((arr (second expr)))
       (do ((ind 0 (1+ ind))
            (symlist (cddr expr) (cdr symlist))
            ;; Build list of index constraints (symbol, array, index)
            (result nil (if (symbolp (first symlist))
                            (cons (cons (first symlist) (cons arr ind)) result)
                            result)))
           ((not symlist) result))))
    
    ;; Otherwise, walk elements of the list
    ;; join together the alist
    (t (mapcan #'walk-array-indices expr))))

(defmacro foreach (syms &key (sum nil) &body body)
  
