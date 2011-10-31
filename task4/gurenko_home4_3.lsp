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
    ((mrational-p elem) elem)
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
    ((mrational-p lst) lst)
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

(defun optimization-1 (expr &optional tables)
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

(defun optimization-2-inner (expr)
  "Calculate all possible in expression EXPR."
  (let* ((func (car expr))
         (forms (append
                  (mapcar #'(lambda (x) (make-mrational :num x :den 1)) (remove-if-not #'numberp (cdr expr)))
                  (mapcar #'optimization-2-inner (remove-if-not #'listp (cdr expr)))
                  (remove-if-not #'mrational-p (cdr expr))
                  (remove-if-not #'symbolp (cdr expr))))
         (numbers (remove-if-not #'mrational-p forms))
         (notnumbers (remove-if #'mrational-p forms)))
    (if (null notnumbers)
      (funcall #'calc (cons func numbers))
      (append
        (list func)
        (list (funcall #'calc (cons func numbers)))
        notnumbers))))

(defun optimization-2 (expr)
  "Open all possible brackets in expression EXPR."
  (let ((new-expr (optimization-2-inner expr)))
    (if (mrational-p new-expr)
      (list '+ new-expr)
      new-expr)))

(defun optimization-3 (expr &optional out-func)
  "Open possible brackets is expression EXPR."
  (let* ((func (car expr))
         (forms (append
                  (remove-if-not #'mrational-p (cdr expr))
                  (car (mapcar #'(lambda (x) (optimization-3 x func)) (remove-if-not #'listp (cdr expr))))
                  (remove-if-not #'symbolp (cdr expr))
                  (remove-if-not #'numberp (cdr expr)))))
    (if out-func
      (if (equal out-func func)
        forms
        (list (cons func forms)))
      (optimization-2 (cons func forms)))))

(defun open-let (expr &optional tables)
  "Optimize expression EXPR."
  (optimization-3 (optimization-2 (optimization-1 expr tables))))

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

(defun traverse (func lst)
  "Traverse expression LST and apply function FUNC to each element."
  (reduce
    (lambda (res x)
      (cond
        ((listp x) (append res (list (traverse func x))))
        ((atom x) (append res (list (funcall func x))))))
      lst
      :initial-value nil))

(defmacro make-calc (expr)
  "Macros make-calc returns lambda with undefined parameters in EXPR."
  (let* ((reserved '(+ - * / make-mrational :num :den))
         (params nil)
         (new-expr (traverse 
                     (lambda (val)
                       (if (and (symbolp val) (not (member val reserved)))
                         (let ((tmp (gensym)))
                           (push tmp params)
                           tmp)
                         val))
                     (open-let expr))))
    `(lambda ,(reverse params) (calc ',new-expr))))

;; tests
(format t "~A -> ~%  ~A~%" '(macroexpand-1 (make-calc (+ 1 2))) (macroexpand-1 '(make-calc (+ 1 2))))
(format t "~A -> ~%  ~A~%" '(macroexpand-1 (make-calc (+ 1 a))) (macroexpand-1 '(make-calc (+ 1 a))))
(format t "~A -> ~%  ~A~%" '(macroexpand-1 (make-calc (+ 1 (let ((b 1)) (+ 1 b)) a))) (macroexpand-1 '(make-calc (+ 1 (let ((b 1)) (+ 1 b)) a))))
(format t "~A -> ~%  ~A~%" '(macroexpand-1 (make-calc (+ 1 (+ 1 (+ 1 a))))) (macroexpand-1 '(make-calc (+ 1 (+ 1 (+ 1 a))))))
(format t "~A -> ~%  ~A~%" '(macroexpand-1 (make-calc (+ 1 (+ 1 (* 1 a))))) (macroexpand-1 '(make-calc (+ 1 (+ 1 (* 1 a))))))
