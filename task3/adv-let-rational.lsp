;; Define struct mrational that describes rational number.
;; Overload basic operations for mrational.
;; Add operation / to calculator.
;; Rewrite advanced LET to work with mrational.

(defstruct mrational
  "Struct for rational number."
  (num 1)
  (den 1))

(defun add-rat (x1 x2)
  "Summ of rationals X1 and X2. Parameters can be rational."
  (let ((num1 (mrational-num x1)) (num2 (mrational-num x2)) (den1 (mrational-den x1)) (den2 (mrational-den x2))) 
    (make-mrational 
      :num (+ (* num1 den2) (* num2 den1))
      :den (* den1 den2))))

(defun sub-rat (x1 x2)
  "Subtraction of X1 and X2."
  (let ((num1 (mrational-num x1)) (num2 (mrational-num x2)) (den1 (mrational-den x1)) (den2 (mrational-den x2))) 
    (make-mrational 
      :num (- (* num1 den2) (* num2 den1))
      :den (* den1 den2))))

(defun mul-rat (x1 x2)
  "Multiply of X1 and X2."
  (let ((num1 (mrational-num x1)) (num2 (mrational-num x2)) (den1 (mrational-den x1)) (den2 (mrational-den x2))) 
    (make-mrational 
      :num (* num1 num2)
      :den (* den1 den2))))

(defun div-rat (x1 x2)
  "Division of X1 and X2."
  (let ((num1 (mrational-num x1)) (num2 (mrational-num x2)) (den1 (mrational-den x1)) (den2 (mrational-den x2))) 
    (make-mrational 
      :num (* num1 den2)
      :den (* den1 num2))))

(defun get-value (elem tables)
  "Get value of variable ELEM from list of hashtables TABLES."
  (if tables
    (let ((res (gethash elem (car tables))))
      (if res
        res
        (get-value elem (cdr tables))))
    nil))

(defun check-elem (elem tables)
  "Check and return numberic value of element ELEM using hashtable with variables TABLE."
  (cond
    ((numberp elem) 
      (make-mrational :num elem :den 1))
    ((symbolp elem) 
      (let ((val (get-value elem tables)))
        (if (mrational-p val)
          val
          (make-mrational :num val :den 1))))
    ((listp elem) 
      (calc elem tables))))

(defun perform-operation (func lst tables)
  "Perform binary operation FUNC to list LST using variables from list of hashtables TABLE."
  (reduce 
    (lambda (res next)
      (funcall func res (check-elem next tables)))
    (cdr lst) 
    :initial-value (check-elem (car lst) tables)))

(defun calc (lst &optional tables)
  "Calculate list LSP. Operations can be overloaded. List TABLES contains hashtables with variables."
  (cond
    ((eq '+ (car lst)) (perform-operation #'add-rat (cdr lst) tables))
    ((eq '* (car lst)) (perform-operation #'mul-rat (cdr lst) tables))
    ((eq '- (car lst)) (perform-operation #'sub-rat (cdr lst) tables))
    ((eq '/ (car lst)) (perform-operation #'div-rat (cdr lst) tables))
    ((eq 'adv-let-rational (car lst)) (apply #'adv-let-rational (append (cdr lst) (list tables))))
    (t (eval lst))))

(defun add-table (lst tables)
  "Add hashtable with variables from list LST to list TABLES."
  (let ((table (make-hash-table)))
    (mapcar 
      (lambda (var)
        (setf (gethash (car var) table) (cadr var)))
      lst)
    (cons table tables)))

(defun adv-let-rational (args expr &optional tables)
  "New implementation of function LET with arguments ARGS and expression EXPR. List TABLES contains hashtables with variables for each call of LET."
  (if (null args)
    (calc expr tables)
    (calc expr (add-table args tables))))

;; tests
(format t "~A -> ~A~%" '(adv-let-rational () (/ 1 2)) (adv-let-rational '() '(/ 1 2)))
(format t "~A -> ~A~%" '(adv-let-rational ((a 1)) (/ a 2)) (adv-let-rational '() '(/ 1 2)))
(format t "~A -> ~A~%" '(adv-let-rational ((a 1)) (/ a (adv-let-rational ((b 2)) (+ a b)))) (adv-let-rational '((a 1)) '(/ a (adv-let-rational ((b 2)) (+ a b)))))
(format t "~A -> ~A~%" '(adv-let-rational ((a 1)) (/ a (adv-let-rational ((a 2)) (+ a 1)) a)) (adv-let-rational '((a 1)) '(/ a (adv-let-rational ((a 2)) (+ a 1)) a)))

