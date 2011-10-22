;; Write advanced fuction LET.
;; Nested LET uses last variables in expression.
;; Use list of hashtables.

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
    ((numberp elem) elem)
    ((symbolp elem) (get-value elem tables))
    ((listp elem) (calc elem tables))))

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
    ((eq '+ (car lst)) (perform-operation #'+ (cdr lst) tables))
    ((eq '* (car lst)) (perform-operation #'* (cdr lst) tables))
    ((eq '- (car lst)) (perform-operation #'- (cdr lst) tables))
    ((eq 'adv-let (car lst)) (apply #'adv-let (append (cdr lst) (list tables))))
    (t (eval lst))))

(defun add-table (lst tables)
  "Add hashtable with variables from list LST to list TABLES."
  (let ((table (make-hash-table)))
    (mapcar 
      (lambda (var)
        (setf (gethash (car var) table) (cadr var)))
      lst)
    (cons table tables)))

(defun adv-let (args expr &optional tables)
  "New implementation of function LET with arguments ARGS and expression EXPR. List TABLES contains hashtables with variables for each call of LET."
  (if (null args)
    (calc expr tables)
    (calc expr (add-table args tables))))

;; tests
(format t "~A -> ~A~%" '(adv-let () (+ 1 2)) (adv-let '() '(+ 1 2)))
(format t "~A -> ~A~%" '(adv-let ((a 1)) (+ a 2)) (adv-let '() '(+ 1 2)))
(format t "~A -> ~A~%" '(adv-let ((a 1)) (+ a (adv-let ((b 2)) (+ a b)))) (adv-let '((a 1)) '(+ a (adv-let ((b 2)) (+ a b)))))
(format t "~A -> ~A~%" '(adv-let ((a 1)) (+ a (adv-let ((a 2)) (+ a 1)) a)) (adv-let '((a 1)) '(+ a (adv-let ((a 2)) (+ a 1)) a)))
