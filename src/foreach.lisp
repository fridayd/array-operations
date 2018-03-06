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

(defmacro foreach (syms &key (sum nil) ((:do body) nil))
  (let* ((dim-exprs (find-array-dimensions body))
         (syms (if (listp syms) syms
                   (list syms)))) ; Ensure that syms is a list
    (dolist (sym syms)
      ;; Check that SYM is a symbol
      (unless (symbolp sym) (error "Index must be a symbol ~S" sym))
      
      ;; Find an expression which sets the range of SYM
      (let ((dim-expr (assoc sym dim-exprs)))
        (unless dim-expr (error "Cannot determine range of index ~S" sym))
        (format t "~a -> ~a~%" sym (rest dim-expr))))))
