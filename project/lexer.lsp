;; Lisp training, 2011
;; Author: Gurenko Oleg
;;
;; Lexer for string.
;; Return a list of lexems for this grammar:
;;
;; Expr ::= Expr + Expr |
;;          Expr * Expr |
;;          Expr - Expr |
;;          Expr / Expr |
;;          (Expr) |
;;          Num |
;;          Var |
;;          ID
;; Num ::= [0..9]*
;; Var ::= ?[a..zA..Z][a..zA..Z0..9]*
;; Id ::= [a..zA..Z][a..zA..Z0..9]*

;; --------------------
;; LEXER INITIALIZATION
;; --------------------

(defvar *whitespaces* '(#\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout #\Space #\NewLine)
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

(defvar *separators* '(#\+ #\- #\* #\/ #\( #\) #\: #\= #\;)
  "Characters for separator.")

(defvar *table-separators* (make-hash-table :test #'equalp)
  "Hashtable with separators.")

(defvar *table-keywords* (make-hash-table :test #'equalp)
  "Hashtable with keywords.")

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
  (setf (gethash "+" *table-separators*) 'add) 
  (setf (gethash "-" *table-separators*) 'sub)
  (setf (gethash "*" *table-separators*) 'mul)
  (setf (gethash "/" *table-separators*) 'div)
  (setf (gethash "(" *table-separators*) 'obt)
  (setf (gethash ")" *table-separators*) 'cbt)
  (setf (gethash ";" *table-separators*) 'semicol)
  (setf (gethash ":=" *table-separators*) 'assign))

(defun init-table-keywords ()
  "Initialize hashtable with separators."
  (setf (gethash "const" *table-keywords*) 'const))

;; -------------
;; LEXER FORMERS
;; -------------

(defun whitespace-former (lst)
  "Form a whitespace from head of list LST and return it with tail."
  (values
    nil
    (cdr lst)))

(defun variable-former (lst &optional (res nil))
  "Form a variable from head of list LST and returns it with tail."
  (if (or (member (car lst) *numbers*) 
          (member (car lst) *letters*)) 
    (variable-former
      (cdr lst)
      (cons (car lst) res))
    (values
       (list 'var (intern (string-upcase (coerce (cons '#\? (reverse res)) 'string))))
       lst)))

(defun id-former (lst &optional (res nil))
  "Form an id from head of list LST and returns it with tail."
  (if (or (member (car lst) *numbers*)
          (member (car lst) *letters*))
    (id-former
      (cdr lst)
      (cons (car lst) res))
    (let* ((str-id (coerce (reverse res) 'string))
           (sym-key (gethash str-id *table-keywords*))
           (sym-id (intern (string-upcase str-id))))
      (if sym-key
        (values
          sym-key
          lst)
        (values
          (list 'id sym-id)
          lst)))))

(defun number-former (lst &optional (res 0))
  "Form a number from head of list LST and returns it with tail."
  (if (member (car lst) *numbers*)
    (number-former 
      (cdr lst) 
      (+ (gethash (car lst) *table-numbers*) (* 10 res)))
    (values 
      (list 'num res) 
      lst)))

(defun separator-former (lst &optional (res nil))
  "Form a separator from head of list LST and returns it with tail."
  (let* ((str-sep (coerce (reverse res) 'string))
         (sym-sep (gethash str-sep *table-separators*))
         (next (car lst)))
    (cond
      ((not (null sym-sep))
       (values 
         sym-sep 
         lst))
      ((and (null sym-sep) (member next *separators*)) 
       (separator-former 
         (cdr lst) 
         (cons next res)))
      ((and (null sym-sep) (not (member next *separators*)))
       (values 
         (list 'undefined-separator str-sep)
         lst)))))

(defun error-former (lst)
  "Form an error from head of list LST and returns it with tail."
  (values
    (list 'undefined (car lst))
    (cdr lst)))

;; -----
;; LEXER
;; -----

(defun dispatcher (lst)
  "Dispatcher of lexer. Check a head of list LST and call former-function."
  (and 
    lst
    (multiple-value-bind (formed tail)
      (cond
        ((member (car lst) *whitespaces*) (whitespace-former lst))
        ((member (car lst) *letters*) (id-former lst))
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
  (init-table-keywords)
  (dispatcher (coerce str 'list)))
