;; Write function (lexer str). Argument STR is a string.
;; Return a list for this grammar:
;;
;; Expr ::= Expr + Expr |
;;          Expr * Expr |
;;          Expr - Expr |
;;          Expr / Expr |
;;          (Expr) |
;;          Num
;; Num ::= [0..9]*
;;
;; For example: 1*(2+3) -> ((num 1) mul obt (num 2) plus (num 3) cbt)

;; characters for number
(setq *numbers* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;; characters for separator
(setq *separators* '(#\+ #\- #\* #\/ #\( #\)))

;; hashtable with numbers
(setq table-numbers (make-hash-table))
(setf (gethash #\0 table-numbers) 0)
(setf (gethash #\1 table-numbers) 1)
(setf (gethash #\2 table-numbers) 2)
(setf (gethash #\3 table-numbers) 3)
(setf (gethash #\4 table-numbers) 4)
(setf (gethash #\5 table-numbers) 5)
(setf (gethash #\6 table-numbers) 6)
(setf (gethash #\7 table-numbers) 7)
(setf (gethash #\8 table-numbers) 8)
(setf (gethash #\9 table-numbers) 9)

;; hashtable with separators
(setq table-separators (make-hash-table))
(setf (gethash #\+ table-separators) 'plus) 
(setf (gethash #\- table-separators) 'minus)
(setf (gethash #\* table-separators) 'mul)
(setf (gethash #\/ table-separators) 'div)
(setf (gethash #\( table-separators) 'obt)
(setf (gethash #\) table-separators) 'cbt)

(defun number-former (lst &optional (res 0))
  "Form a number from head of list LST and returns it with tail."
  (if (member (car lst) *numbers*)
    (number-former 
      (cdr lst) 
      (+ (gethash (car lst) table-numbers) (* 10 res)))
    (values 
      (list 'num res) 
      lst)))

(defun separator-former (lst)
  "Form a separator from head of list LST and returns it with tail."
  (if (member (car lst) *separators*)
    (values 
      (gethash (car lst) table-separators)
      (cdr lst))))

(defun error-former (lst)
  "Form an error from head of list LST and returns it with tail."
  (values
    (list 'undefined (car lst))
    (cdr lst)))

(defun dispatcher (lst)
  "Dispatcher of lexer. Check a head of list LST and call former-function."
  (if lst
    (multiple-value-bind (formed tail)
      (cond
        ((member (car lst) *numbers*) (number-former lst)) 
        ((member (car lst) *separators*) (separator-former lst))
        (t (error-former lst)))
      (cons formed (dispatcher tail)))
    nil))

(defun lexer (str)
  "Lexer for string STR."
  (dispatcher (coerce str 'list)))

;; tests
(format t "~A -> ~A~%" '(lexer "1*(2+3)") (lexer "1*(2+3)"))
(format t "~A -> ~A~%" '(lexer "1*(2=3)") (lexer "1*(2=3)"))
(format t "~A -> ~A~%" '(lexer "1*(a+3)") (lexer "1*(a+3)"))
