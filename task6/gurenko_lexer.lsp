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

(defvar *whitespaces* '(#\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout #\Space)
  "Whitespace characters.")

(defvar *letters* '(#\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F 
                    #\g #\G #\h #\H #\i #\I #\j #\J #\k #\K #\l #\L 
                    #\m #\M #\n #\N #\o #\P #\q #\Q #\r #\R #\s #\S 
                    #\t #\T #\u #\U #\v #\V #\w #\W #\x #\X #\y #\Y 
                    #\z #\Z)
  "Characters for variable.")

(defvar *numbers* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  "Characters for number.")

(defvar *table-numbers* (make-hash-table :test #'eq)
  "Hashtable with numbers.")

(defvar *separators* '(#\+ #\- #\* #\/ #\( #\))
  "Characters for separator.")

(defvar *table-separators* (make-hash-table :test #'eq)
  "Hashtable with separators.")

(defun init-table-numbers ()
  "Initialize hashtable with numbers."
  (setf (gethash #\0 *table-numbers*) 0)
  (setf (gethash #\1 *table-numbers*) 1)
  (setf (gethash #\2 *table-numbers*) 2)
  (setf (gethash #\3 *table-numbers*) 3)
  (setf (gethash #\4 *table-numbers*) 4)
  (setf (gethash #\5 *table-numbers*) 5)
  (setf (gethash #\6 *table-numbers*) 6)
  (setf (gethash #\7 *table-numbers*) 7)
  (setf (gethash #\8 *table-numbers*) 8)
  (setf (gethash #\9 *table-numbers*) 9))

(defun init-table-separators ()
  "Initialize hashtable with separators."
  (setf (gethash #\+ *table-separators*) 'add) 
  (setf (gethash #\- *table-separators*) 'sub)
  (setf (gethash #\* *table-separators*) 'mul)
  (setf (gethash #\/ *table-separators*) 'div)
  (setf (gethash #\( *table-separators*) 'obt)
  (setf (gethash #\) *table-separators*) 'cbt))

(defun whitespace-former (lst)
  "Form a whitespace from head of list LST and return it with tail."
  (values
    nil
    (cdr lst)))

(defun variable-former (lst &optional (res nil))
  "Form a variable from head of list LST and returns it with tail."
  (if (or (member (car lst) *numbers*) (member (car lst) *letters*)) 
    (variable-former
      (cdr lst)
      (cons (car lst) res))
    (values
       (list 'var (intern (string-upcase (coerce (cons '#\? (reverse res)) 'string))))
       lst)))

(defun number-former (lst &optional (res 0))
  "Form a number from head of list LST and returns it with tail."
  (if (member (car lst) *numbers*)
    (number-former 
      (cdr lst) 
      (+ (gethash (car lst) *table-numbers*) (* 10 res)))
    (values 
      (list 'num res) 
      lst)))

(defun separator-former (lst)
  "Form a separator from head of list LST and returns it with tail."
  (and 
    (member (car lst) *separators*)
    (values 
      (gethash (car lst) *table-separators*)
      (cdr lst))))

(defun error-former (lst)
  "Form an error from head of list LST and returns it with tail."
  (values
    (list 'undefined (car lst))
    (cdr lst)))

(defun dispatcher (lst)
  "Dispatcher of lexer. Check a head of list LST and call former-function."
  (and 
    lst
    (multiple-value-bind (formed tail)
      (cond
        ((member (car lst) *whitespaces*) (whitespace-former lst))
        ((and (eq (car lst) #\?) (member (cadr lst) *letters*)) (variable-former (cdr lst)))
        ((member (car lst) *numbers*) (number-former lst)) 
        ((member (car lst) *separators*) (separator-former lst))
        (t (error-former lst)))
      (if formed
        (cons formed (dispatcher tail))
        (dispatcher tail)))))

(defun lexer (str)
  "Lexer for string STR."
  (init-table-numbers)
  (init-table-separators)
  (dispatcher (coerce str 'list)))

;; tests
(format t "~A -> ~A~%" '(lexer "1*(2+3)") (lexer "1*(2+3)"))
(format t "~A -> ~A~%" '(lexer "1*(2=3)") (lexer "1*(2=3)"))
(format t "~A -> ~A~%" '(lexer "1*(a+3)") (lexer "1*(a+3)"))
(format t "~A -> ~A~%" '(lexer "1 * ( a +3)") (lexer "1 * ( a +3)"))
(format t "~A -> ~A~%" '(lexer "1*(?a+ 3)") (lexer "1*(?a+ 3)"))
