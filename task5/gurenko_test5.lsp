;; -----
;; Macro OUR-LET*
;; -----

(defun build-our-let1 (args body)
  "Build code for macro our-let1*."
  (if args
    `((lambda ,(list (caar args)) ,(build-our-let1 (cdr args) body)) ,(cadar args))
    (car body)))

(defmacro our-let1* (args &body body)
  "Macro works as function let*."
  (build-our-let1 args body))

(format t "~A -> ~A~%" '(our-let1* ((x 1) (y (+ x 2))) (+ x y)) (our-let1* ((x 1) (y (+ x 2))) (+ x y)))

;; -----

(defun build-our-let2 (args res)
  "Build code for argument values calculation."
  (if args
    `(let ((,(caar args) ,(cadar args)))
      ,(build-our-let2 (cdr args) (append res (list (caar args)))))
    `(list ,@res)))

(defmacro our-let2* (args &body body)
  "Macro works as function let*."
  `((lambda ,(mapcar #'car args) ,@body)
    ,@(eval (build-our-let2 args nil))))

(format t "~A -> ~A~%" '(our-let2* ((x 1) (y (+ x 2))) (+ x y)) (our-let2* ((x 1) (y (+ x 2))) (+ x y)))

;; -----
;; Macro WITH-GENSYM
;; -----

(defun build-gensym1 (args)
  "Build code for argument values."
  (if args
    (append
      (list `(,(car args) (gensym)))
      (build-gensym1 (cdr args)))
    nil))

(defmacro with-gensym1 (args &body body)
  "Macro allows using arguments ARGS with generated values in BODY."
  `(let ,(build-gensym1 args) ,@body))

(format t "~A -> ~A~%" '(with-gensym1 (x y z) (list x y z)) (with-gensym1 (x y z) (list x y z)))

;; -----

(defmacro with-gensym2 (args &body body)
  "Macro allows using arguments ARGS with generated values in BODY."
  `(let ,(mapcar 
           (lambda (x)
             `(,x (gensym)))
           args)
     ,@body))

(format t "~A -> ~A~%" '(with-gensym2 (x y z) (list x y z)) (with-gensym2 (x y z) (list x y z)))

;; -----
;; Macro DEFANAPH
;; -----

(defun build-defanaph (args syms fn)
  "Build code for macro DEFANAPH."
  (if args
    (let ((sym (gensym)))
      `(let* ((,sym ,(car args))
              (it ,sym))
         ,(build-defanaph (cdr args)
                      (append syms
                              (list sym))
                      fn)))
    `(,fn ,@syms)))

(defmacro defanaph (name fn)
  "Macro build another macro named NAME for calculation with function FN."
  `(defmacro ,name (&rest args)
     `,(build-defanaph args nil ,fn)))

(format t "~A -> ~A~%" '(defanaph a* '*) (defanaph a* '*))
(format t "~A -> ~A~%" '(a* 1 2 (+ it 1)) (a* 1 2 (+ it 1)))
