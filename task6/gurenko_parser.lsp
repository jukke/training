;; Write function (parser lst). Argument LST is a result of function (lexer str).
;; Grammar to parse:
;; 
;; <expression> ::= <expr>
;; <expr> ::= <term> <expr-list>
;; <expr-list> ::= <empty> | + <expr> | - <expr>
;; <term> ::= <factor> <term-list>
;; <term-list> ::= <empty> | * <term> | / <term>
;; <factor> ::= ( <expr> ) | <num> | <var>

(defun fact-expr-in-brt (lst)
  "Rule: ( <expr> )."
  (if (eq 'obt (car lst))
    (multiple-value-bind (expression tail err)
      (expr (cdr lst))
      (if (null err)
        (if (eq 'cbt (car tail))
          (values expression 'expr-in-brt (cdr tail) nil)
          (values nil nil lst `(Unclosed bracket -> ,lst)))
        (values nil nil lst err)))
    (values nil nil lst `(Not expression in brackets -> ,lst))))

(defun fact-num (lst)
  "Rule: <num>."
  (if (and 
        (listp (car lst)) 
        (eq 'num (caar lst)))
    (values (car lst) 'num (cdr lst) nil)
    (values nil nil lst `(Not number -> ,lst))))

(defun fact-var (lst)
  "Rule: <var>."
  (if (and 
        (listp (car lst)) 
        (eq 'var (caar lst)))
    (values (car lst) 'var (cdr lst) nil)
    (values nil nil lst `(Not variable -> ,lst))))

(defun factor (lst)
  "Rule: <factor>."
  (multiple-value-bind (fact fact-type tail err)
    (cond
      ((eq 'obt (car lst)) (fact-expr-in-brt lst))
      ((and (listp (car lst)) (eq 'num (caar lst))) (fact-num lst))
      ((and (listp (car lst)) (eq 'var (caar lst))) (fact-var lst)))
    (if (null fact)
      (if (null err)
        (values nil lst `(Not factor -> ,lst))
        (values nil lst err))
      (if (not (eq 'expr-in-brt fact-type))
        (values (list 'expr fact-type fact) tail nil)
        (values fact tail nil)))))

(defun term-list (lst)
  "Rule: <term-list>."
  (if (or 
        (eq 'mul (car lst)) 
        (eq 'div (car lst)))
    (multiple-value-bind (trm tail err)
      (term (cdr lst))
      (if (null err)
        (values (car lst) trm tail nil)
        (values nil nil (cdr lst) err)))
    (values nil nil lst nil)))

(defun term (lst)
  "Rule: <term>."
  (multiple-value-bind (fct tail err)
    (factor lst)
    (if (null err)
      (multiple-value-bind (op trm-lst tail err)
        (term-list tail)
        (if (null err)
          (if (null op)
            (values fct tail nil)
            (values (list 'expr op fct trm-lst) tail nil))
          (values nil lst err)))
      (values nil lst err))))

(defun expr-list (lst)
  "Rule: <expr-list>."
  (if (or
        (eq 'add (car lst))
        (eq 'sub (car lst)))
    (multiple-value-bind (xpr tail err)
      (expr (cdr lst))
      (if (null err)
        (values (car lst) xpr tail nil)
        (values nil nil (cdr lst) err)))
    (values nil nil lst nil)))

(defun expr (lst)
  "Rule: <expr>."
  (multiple-value-bind (trm tail err)
    (term lst)
    (if (null err)
      (multiple-value-bind (op xpr-lst tail err)
        (expr-list tail)
        (if (null err)
          (if (null op)
            (values trm tail nil)
            (values (list 'expr op trm xpr-lst) tail nil))
          (values nil lst err)))
      (values nil lst err))))

(defun parser (lst)
  "Parser for string of lexems LST."
  (multiple-value-bind (expression tail err)
    (expr lst)
    (if (and
          (null tail)
          (null err))
      expression
      `(Unable to parse ,err))))

;; tests
(format t "~A~%   -> ~A~%" '(parser ((num 1))) (parser '((num 1))))
(format t "~A~%   -> ~A~%" '(parser ((num 1) add (num 2))) (parser '((num 1) add (num 2))))
(format t "~A~%   -> ~A~%" '(parser (obt (num 1) add (num 2) cbt)) (parser '(obt (num 1) add (num 2) cbt)))
(format t "~A~%   -> ~A~%" '(parser ((num 3) mul (num 1) add (num 2))) (parser '((num 3) mul (num 1) add (num 2))))
(format t "~A~%   -> ~A~%" '(parser ((var ?a) mul obt (num 4) sub (num 5) cbt add (num 1))) (parser '((var ?a) mul obt (num 4) sub (num 5) cbt add (num 1))))
(format t "~A~%   -> ~A~%" '(parser ((var ?a) mul mul (num 1))) (parser '((var ?a) mul mul (num 1))))
(format t "~A~%   -> ~A~%" '(parser ((var ?a) mul obt (num 1))) (parser '((var ?a) mul obt (num 1))))
