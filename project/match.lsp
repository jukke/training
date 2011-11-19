;; Lisp training, 2011
;; Author: Gurenko Oleg
;;
;; Utilites for matching.

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
        (if it ,then-form ,else-form)))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (val (gensym))
          (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
                            (if (or ,val ,win)
                              (let ((it ,val)) (null it) ,@(cdr cl1))
                              (acond2 ,@(cdr clauses)))))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun myassoc (x binds)
  (when binds
    (if (eq x (first (car binds)))
      (car binds)
      (myassoc x (cdr binds)))))

(defun binding (x binds)
  (labels ((recbind (x binds)
                    (aif (myassoc x binds)
                         (or (recbind (second it) binds)
                             it))))
    (let ((b (recbind x binds)))
      (values (second b) b))))

(defun match (x y &optional binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
    ((binding x binds) (match it y binds))
    ((binding y binds) (match x it binds))
    ((varsym? x) (values (cons (list x y) binds) t))
    ((varsym? y) (values (cons (list y x) binds) t))
    ((and (consp x) (consp y) (match (car x) (car y) binds))
     (match (cdr x) (cdr y) it))
    (t (values nil nil))))
