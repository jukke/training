(defstruct mrational
  "Struct for rational number."
  (num 1)
  (den 1))
 
(defun add-rat (x1 x2)
  "Summ of rationals X1 and X2. Parameters can be rational."
  (let ((num1 (mrational-num x1))
        (num2 (mrational-num x2))
        (den1 (mrational-den x1))
        (den2 (mrational-den x2)))
    (make-mrational
      :num (+ (* num1 den2) (* num2 den1))
      :den (* den1 den2))))
 
(defun sub-rat (x1 x2)
  "Subtraction of X1 and X2."
  (let ((num1 (mrational-num x1))
        (num2 (mrational-num x2))
        (den1 (mrational-den x1))
        (den2 (mrational-den x2)))
    (make-mrational
      :num (- (* num1 den2) (* num2 den1))
      :den (* den1 den2))))

(defun mul-rat (x1 x2)
  "Multiply of X1 and X2."
  (let ((num1 (mrational-num x1))
        (num2 (mrational-num x2))
        (den1 (mrational-den x1))
        (den2 (mrational-den x2)))
    (make-mrational
      :num (* num1 num2)
      :den (* den1 den2))))
 
 (defun div-rat (x1 x2)
   "Division of X1 and X2."
   (let ((num1 (mrational-num x1))
         (num2 (mrational-num x2))
         (den1 (mrational-den x1))
         (den2 (mrational-den x2)))
     (make-mrational
       :num (* num1 den2)
       :den (* den1 num2))))

(defun check-elem (elem)
  "Check and return numberic value of element ELEM using hashtable with variables TABLE."
  (cond
    ((listp elem) (calc elem))
    (t (make-mrational :num elem :den 1))))

(defun perform-operation (func lst)
  "Perform binary operation FUNC to list LST using variables from list of hashtables TABLE."
  (reduce
    (lambda (res next)
      (funcall func res (check-elem next)))
    (cdr lst)
    :initial-value (check-elem (car lst))))

(defun calc (lst)
  "Calculate list LSP. Operations can be overloaded. List TABLES contains hashtables with variables."
  (cond
    ((numberp lst) lst)
    ((eq '+ (car lst)) (perform-operation #'add-rat (cdr lst)))
    ((eq '* (car lst)) (perform-operation #'mul-rat (cdr lst)))
    ((eq '- (car lst)) (perform-operation #'sub-rat (cdr lst)))
    ((eq '/ (car lst)) (perform-operation #'div-rat (cdr lst)))
    (t (eval lst))))

(defun get-value (elem tables)
  "Get value of variable ELEM from list of hashtables TABLES."
  (if tables
    (let ((res (gethash elem (car tables))))
      (or
        res
        (get-value elem (cdr tables))))
    elem))

(defun open-let (expr &optional tables)
  "Replace variables in expression EXPR to it's values from list of hashtables TABLES."
  (if (eq 'let (car expr))
    (apply #'my-let-inner (append (cdr expr) (list tables)))
    (reduce 
      (lambda (res x)
        (cond
          ((listp x) (append res (list (open-let x tables))))
          ((numberp x) (append res (list x)))
          ((symbolp x) (append res (list (get-value x tables))))))
      (cdr expr)
      :initial-value (list (car expr)))))

(defun add-table (lst tables)
  "Add hashtable with variables from list LST to list TABLES."
  (let ((table (make-hash-table)))
    (mapcar
      (lambda (var)
        (setf (gethash (car var) table) (cadr var)))
      lst)
    (cons table tables)))

(defun my-let-inner (args expr &optional tables)
  "New implementation of function LET with arguments ARGS and expression EXPR. List TABLES contains hashtables with variables for each call of LET."
  (open-let
    expr
    (add-table args tables)))

(defun my-let (args expr)
  "Calculate expression EXPR using arguments ARGS."
  (calc (my-let-inner args expr)))
