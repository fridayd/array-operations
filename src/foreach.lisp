(in-package #:array-operations)

(defun find-array-dimensions (expr)
  "Walks an expression tree EXPR, finds AREF and ROW-MAJOR-AREF calls. 
   Returns a list of (symbol, expr)

   Example: 
     (find-array-dimensions '(+ (aref a i) (* 2 (aref b j k))))
   
   -> ((I ARRAY-DIMENSION A 0) (K ARRAY-DIMENSION B 1) (J ARRAY-DIMENSION B 0))
    
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
                            (cons (list (first symlist) 'array-dimension arr ind) result)
                            result)))
           ((not symlist) result))))

    ;; If EXPR is ROW-MAJOR-AREF
    ((equalp (first expr) 'row-major-aref)
     (list (list (third expr) 'array-total-size (second expr))))
    
    ;; Otherwise, walk elements of the list
    ;; join together the alist
    (t (mapcan #'find-array-dimensions expr))))

(defmacro foreach (&key (index nil) (sum nil) ((:value body) nil))
  "Examples:
  
   Matrix-matrix multiply
   
    (foreach :index (i j) :sum k 
        :value (* (aref A i k) (aref B k j)))

   Sum over vector

    (foreach :sum i :value (aref A i))
  "
  (let ((dim-exprs (find-array-dimensions body))
        (index (if (listp index) index
                   (list index)))  ; Ensure that INDEX is a list
        (sum (if (listp sum) sum
                 (list sum))))  ; Ensure that SUM is a list
    (flet ((get-dim-expr (sym)
             ;; Get an expression to determine the range of index SYM
             ;; Check that SYM is a symbol
             (unless (symbolp sym) (error "Index must be a symbol ~S" sym))
             
             ;; Find an expression which sets the range of SYM
             (let ((dim-expr (assoc sym dim-exprs)))
               (unless dim-expr (error "Cannot determine range of index ~S" sym))
               (rest dim-expr))))
      (let ((index-sizes (loop for i from 0 below (length index) collecting (gensym)))
            (sum-sizes (loop for i from 0 below (length sum) collecting (gensym)))
            (let-list nil)   ; let environment
            (result-array (gensym))  ; The array to be returned
            (result body))   ; The result of this macro

        (loop for sym in (reverse sum) for size in sum-sizes do
           ;; Add a dimension to be set in the `let` environment
             (push (list size (get-dim-expr sym)) let-list)
           ;; Wrap a summation loop around the inner expression
             (setf result
                   `(loop for ,sym from 0 below ,size summing
                         ,result)))

        (when index
          ;; Set elements of an array
          (setf result
                `(setf (aref ,result-array ,@index) ,result))
        
          (loop for sym in (reverse index) for size in index-sizes do
             ;; Add a dimension to be set in the `let` environment
               (push (list size (get-dim-expr sym)) let-list)
             ;; Wrap a loop around result
               (setf result
                     `(loop for ,sym from 0 below ,size do
                           ,result)))
          (setf result
                `(let ((,result-array (make-array (list ,@(reverse index-sizes)))))
                   ,result
                   ,result-array)))
        
        (list 'let let-list
              result)))))
      
;;;
;;; Alternative form:
;;;
;;; (each-index (i j) expr)
;;;
;;; and
;;;
;;; (sum-index k expr)
;;;
;;; could be combined e.g.
;;;
;;; (each-index (i j)
;;;   (sum-index k
;;;     (* (aref A i k) (aref B k j))))
;;;
