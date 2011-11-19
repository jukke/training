;; Lisp training, 2011
;; Author: Gurenko Oleg
;;
;; Parser for list of lexems.
;; Grammar to parse:
;;
;; <grammar> ::= <statements>
;; <statements> ::= <stmt> <statements>
;; <stmt> ::= <empty> | <expr> semicol | <definition> semicol
;; <definition> ::= const <id> assign <expr> semicol
;; <expr> ::= <term> <expr-list>
;; <expr-list> ::= <empty> | add <expr> | sub <expr>
;; <term> ::= <factor> <term-list>
;; <term-list> ::= <empty> | mul <term> | div <term>
;; <factor> ::= obt <expr> cbt | <num> | <var> | <id>

(defmacro or-values (&rest vals)
  "Return first values with not-nill first component."
  (let ((head (gensym))
        (tail (gensym)))
    (if (car vals)
      `(multiple-value-bind (,head ,tail)
         ,(car vals)
         (if ,head
           (values ,head ,tail)
           (or-values ,@(cdr vals))))
    `(values nil nil))))

(defun fact-expr-in-brt (lst &key no-errors)
  "Rule: obt <expr> cbt."
  (if (eq 'obt (car lst))
    (multiple-value-bind (expression tail)
      (expr (cdr lst))
      (if (eq 'cbt (car tail))
        (values
          expression
          (cdr tail))
        (throw-error "PARSER/FACT-EXPR-IN-BRT: unclosed bracket -> ~S" lst no-errors)))
    (throw-error "PARSER/FACT-EXPR-IN-BRT: not expression in bracket -> ~S" lst no-errors)))

(defun fact-num (lst &key no-errors)
  "Rule: <num>."
  (if (listp (car lst))
    (let* ((lst-num (car lst))
           (num-type (first lst-num))
           (num-val (second lst-num))
           (tail (cdr lst)))
      (if (and (eq 'num num-type)
               (numberp num-val))
        (values
          lst-num
          tail)
        (throw-error "PARSER/FACT-NUM: not number -> ~S" lst no-errors)))
    (throw-error "PARSER/FACT-NUM: not number -> ~S" lst no-errors)))

(defun fact-var (lst &key no-errors)
  "Rule: <var>."
  (if (listp (car lst))
    (let* ((lst-var (car lst))
           (var-type (first lst-var))
           (var-val (second lst-var))
           (tail (cdr lst)))
      (if (and (eq 'var var-type)
               (symbolp var-val))
        (values
          lst-var
          tail)
        (throw-error "PARSER/FACT-VAR: not variable -> ~S" lst no-errors)))
    (throw-error "PARSER/FACT-VAR: not variable -> ~S" lst no-errors)))

(defun fact-id (lst &key no-errors)
  "Rule: <id>."
  (if (listp (car lst))
    (let* ((lst-id (car lst))
           (id-type (first lst-id))
           (id-val (second lst-id))
           (tail (cdr lst)))
      (if (and (eq 'id id-type)
               (symbolp id-val))
        (values
          lst-id
          tail)
        (throw-error "PARSER/FACT-ID: not id -> ~S" lst no-errors)))
    (throw-error "PARSER/FACT-ID: not id -> ~S" lst no-errors)))

(defun factor (lst &key no-errors)
  "Rule: <factor>."
  (multiple-value-bind (fact tail)
    (or-values (fact-expr-in-brt lst :no-errors t)
               (fact-num lst :no-errors t)
               (fact-var lst :no-errors t)
               (fact-id lst :no-errors t))
    (if fact
      (let ((fact-type (car fact)))
        (case fact-type
          (expr (values fact tail))
          (num (values (make-node 'expr (list fact-type fact) :no-errors no-errors) tail))
          (var (values (make-node 'var (list fact) :no-errors no-errors) tail))
          (id (values (make-node 'expr (list fact-type fact) :no-errors no-errors) tail))))
      (throw-error "PARSER/FACTOR: invalid factor -> ~S" lst no-errors))))

(defun term-list (lst &key no-errors)
  "Rule: <term-list>."
  (let ((op (car lst))
        (tail (cdr lst)))
    (if (or (eq 'mul op)
            (eq 'div op))
      (multiple-value-bind (trm tail)
        (term tail :no-errors no-errors)
        (values trm op tail))
      (values nil nil lst))))

(defun term (lst &key no-errors)
  "Rule: <term>."
  (multiple-value-bind (fct tail)
    (factor lst :no-errors no-errors)
    (if fct
      (multiple-value-bind (trm-lst op tail)
        (term-list tail :no-errors no-errors)
        (if (and trm-lst 
                 op)
          (values (make-node 'expr (list op fct trm-lst) :no-errors no-errors) tail)
          (values fct tail)))
      (values nil lst))))

(defun expr-list (lst &key no-errors)
  "Rule: <expr-list>."
  (let ((op (car lst))
        (tail (cdr lst)))
    (if (or (eq 'add op)
            (eq 'sub op))
      (multiple-value-bind (xpr tail)
        (expr tail :no-errors no-errors)
        (values xpr op tail))
      (values nil nil lst))))

(defun expr (lst &key no-errors)
  "Rule: <expr>."
  (multiple-value-bind (trm tail)
    (term lst :no-errors no-errors)
    (if trm
      (multiple-value-bind (xpr-lst op tail)
        (expr-list tail :no-errors no-errors)
        (if (and xpr-lst
                 op)
          (values (make-node 'expr (list op trm xpr-lst) :no-errors no-errors) tail)
          (values trm tail)))
      (values nil lst))))

(defun definition (lst &key no-errors)
  "Rule: <definition>."
  (let ((def-type (car lst))
        (tail (cdr lst)))
    (case def-type
      (const (multiple-value-bind (id tail)
               (or-values (fact-id tail :no-errors no-errors)
                          (fact-var tail :no-errors no-errors))
               (if (eq 'assign (car tail))
                 (multiple-value-bind (val tail)
                   (expr (cdr tail) :no-errors no-errors)
                   (if val
                     (values (make-node 'def (list def-type id val) :no-errors no-errors) tail)
                     (throw-error "PARSER/DEFINITION: value not found -> ~S" lst no-errors)))
                 (throw-error "PARSER/DEFINITION: assign not found -> ~S" lst no-errors))))
      (otherwise (throw-error "PARSER/DEFINITION: undefined definition -> ~S" lst no-errors)))))

(defun stmt (lst &key no-errors)
  "Rule: <stmt>."
  (multiple-value-bind (stm tail)
    (or-values (expr lst :no-errors t)
               (definition lst :no-errors t)
               (fact-var lst :no-errors t))
    (cond
      ((and stm (eq 'semicol (car tail))) (values (make-node 'stmt (list stm) :no-errors no-errors) (cdr tail)))
      ((node-var-p stm) (values stm tail))
      (t (throw-error "PARSER/STMT: statement not found -> ~S" lst no-errors)))))

(defun stmt-list (lst &key no-errors)
  "Rule: <stmt-list>."
  (if lst
    (multiple-value-bind (stm tail)
      (stmt lst :no-errors no-errors)
      (if stm
        (multiple-value-bind (stm-lst tail)
          (stmt-list tail :no-errors no-errors)
          (values (make-node 'stmt* (list stm stm-lst) :no-errors no-errors) tail))
        (throw-error "PARSER/STMT-LIST: statement not found -> ~S" lst no-errors)))
    (values nil nil)))

(defun parser (lst &key no-errors)
  "Parser for string of lexems LST."
  (multiple-value-bind (tree tail)
    (stmt-list lst :no-errors no-errors)
    (if tail
      (throw-error "Can't parse -> ~S" tail no-errors)
      tree)))
