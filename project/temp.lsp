(load "main.lsp")

(defparameter *const-table* (make-hash-table :test #'equalp))

(defun write-calc-exprs (op n1 n2)
  (let ((num1 (get-number n1))
        (num2 (get-number n2)))
    (parse-expr (format nil "~A" (funcall op num1 num2)))))

(defun calc-exprs (term)
  (/. term ((num ?n1 ?n2))
      (#! expr "?n1+?n2" (write-calc-exprs #'+ ?n1 ?n2))
      (#! expr "?n1-?n2" (write-calc-exprs #'- ?n1 ?n2))
      (#! expr "?n1*?n2" (write-calc-exprs #'* ?n1 ?n2))
      (#! expr "?n1/?n2" (write-calc-exprs #'/ ?n1 ?n2))))

;; ------

(defun get-constants (term)
  (setf *const-table* (make-hash-table :test #'equalp))
  (forall term ((id ?i) (expr ?e))
          (#! stmt "const ?i := ?e;"
            (setf (gethash ?i *const-table*) ?e)))
  (/. term ((id ?i) (expr ?e) (stmt* ?s))
      (#! stmt* "const ?i := ?e; ?s"
        ?s)))

(defun constant-folding (term)
  (/. term ((id ?i))
      (#! expr "?i" 
        (gethash ?i *const-table*))))

(defun constant-folding-loop (term)
  (let ((new-term (constant-folding term)))
    (if (equalp new-term term)
      term
      (constant-folding-loop new-term))))

;; ------

(defun change-order (term)
  (/. term ((id ?i) (expr ?n))
      (#! expr "?n+?i"
        #! expr "?i+?n")
      (#! expr "?n-?i"
        #! expr "0-?i+?n")
      (#! expr "?n*?i"
        #! expr "?i*?n")
      (#! expr "?n/?i"
        #! expr "1/?i*?n")))

(defun change-order-loop (term)
  (let ((new-term (change-order term)))
    (if (equalp new-term term)
      term
      (change-order new-term))))

;; ------

(defun simplify (term)
  (print-tree 
    (calc-exprs
      (change-order-loop
        (constant-folding-loop 
          (get-constants term))))))
