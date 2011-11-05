;; Write function (parser lst). Argument LST is a result of function (lexer str).
;; Grammar to parse:
;; 
;; <expression> ::= <expr>
;; <expr> ::= <term> <expr-list>
;; <expr-list> ::= <empty> | + <expr> | - <expr>
;; <term> ::= <factor> <term-list>
;; <term-list> ::= <empty> | * <term> | / <term>
;; <factor> ::= ( <expr> ) | <num>

(defparameter *ts* nil
  "Next readed value from lexed string.")

(defparameter *lst* nil
  "Non-readed part of lexed string.")

(defun scan ()
  "Scan next value from lexed string."
  (let ((ts (car *lst*)))
    (setq *lst* (cdr *lst*))
    (setq *ts* ts)))

(defun factor ()
  "Rule <factor>."
  (cond
    ((eq 'obt *ts*) (progn
                      (scan)
                      (let ((xpr (expr)))
                        (if (eq 'cbt *ts*)
                          xpr))))
    ((eq 'num (car *ts*)) *ts*)))

(defun term-list (fctr)
  "Rule <term-list>."
  (if (or (eq 'mul *ts*) (eq 'div *ts*))
    (list 'expr *ts* fctr (progn (scan) (term)))
    fctr))

(defun term ()
  "Rule <term>."
  (let ((fctr (factor)))
    (scan)
    (term-list fctr)))

(defun expr-list (trm)
  "Rule <expr-list>."
  (if (or (eq 'plus *ts*) (eq 'minus *ts*))
    (list 'expr *ts* trm (progn (scan) (expr)))
    trm))

(defun expr ()
  "Rule <expr>."
  (let ((trm (term)))
    (expr-list trm)))

(defun parser (lst)
  "Parser for lexed string."
  (setq *ts* nil)
  (setq *lst* lst)
  (scan)
  (expr))

;; tests
(format t "~A~%   -> ~A~%" '(parser ((num 1))) (parser '((num 1))))
(format t "~A~%   -> ~A~%" '(parser ((num 1) plus (num 2))) (parser '((num 1) plus (num 2))))
(format t "~A~%   -> ~A~%" '(parser (obt (num 1) plus (num 2) cbt)) (parser '(obt (num 1) plus (num 2) cbt)))
(format t "~A~%   -> ~A~%" '(parser ((num 3) mul (num 1) plus (num 2))) (parser '((num 3) mul (num 1) plus (num 2))))
(format t "~A~%   -> ~A~%" '(parser ((num 2) mul obt (num 4) minus (num 5) cbt plus (num 1))) (parser '((num 2) mul obt (num 4) minus (num 5) cbt plus (num 1))))
