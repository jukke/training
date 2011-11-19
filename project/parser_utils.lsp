;; Lisp training, 2011
;; Author: Gurenko Oleg
;;
;; Utilites for parser.

;; -------------------------
;; CONTEXT VARIABLE UTILITES
;; -------------------------

(defvar *context* nil
  "List of hashtables with values of variables.")

(defstruct var-info
  "Structure of variable to save."
  kind
  val)

(defmacro aif (test-form then-form &optional else-form)
  "Macro if with variable IT."
  `(let ((it ,test-form))
        (if it ,then-form ,else-form)))

(defun find-in-context (symb &optional (current *context*))
  "Find variable SYMB in list of hashtables CURRENT."
  (if (null current)
    nil
    (aif (gethash symb (car current))
         it
         (find-in-context symb (cdr current)))))

(defun get-value (symb &optional node-name)
  "Return kind of variable SYMB or it's value if NODE-NAME is set."
  (aif (find-in-context symb)
       (let ((value (var-info-val it))
             (kind (var-info-kind it)))
         (if node-name
           (case kind
             (num (case node-name
                    (num value)
                    (otherwise (list 'expr 'num value))))
             (id (case node-name
                   (id value)
                   (otherwise (list 'expr 'id value))))
             (otherwise value))
           kind))
       (error "Variable ~A is unbound!" symb)))

(defun add-value (symb value)
  "Add VALUE of variable SYMB to first hashtable in *CONTEXT*."
  (setf (gethash symb (car *context*)) value))

;; -----------------------
;; READER-MACRO FOR PARSER
;; -----------------------

(defun quote-tree (tree current-node)
  "Add quote to TREE nodes and replace vars with it's values."
  (cond
    ((listp tree)
     (let ((node-kind (first tree)))
       (if (eq node-kind 'var)
         (list 'get-value (list 'quote (second tree)) (list 'quote current-node))
         (cons 'list (mapcar (lambda (tree-node) 
                               (quote-tree tree-node (first tree)))
                             tree)))))
    ((symbolp tree)
     (list 'quote tree))
    ((numberp tree)
     tree)))

(defun parse-expr (str)
  "Parse expression from STR."
  (expr (lexer str)))

(defun parse-num (str)
  "Parse number from STR."
  (let ((lexem-str (lexer str)))
    (multiple-value-bind (fct tail)
      (or-values (fact-num lexem-str :no-errors t)
                 (fact-var lexem-str :no-errors t))
      (if (and fct
               (null tail))
        fct
        (error "Not number -> ~S" str)))))

(defun parse-id (str)
  "Parse id from STR."
  (let ((lexem-str (lexer str)))
    (multiple-value-bind (fct tail)
      (or-values (fact-id lexem-str :no-errors t)
                 (fact-var lexem-str :no-errors t))
      (if (and fct
               (null tail))
        fct
        (error "Not id -> ~S" str)))))

(defun parse-def (str)
  "Parse definition from STR."
  (let ((lexem-str (lexer str)))
    (multiple-value-bind (def tail)
      (or-values (definition lexem-str :no-errors t)
                 (fact-var lexem-str :no-errors t))
      (if (and def
               (null tail))
        def
        (error "Not definition -> ~S" str)))))

(defun parse-stmt (str)
  "Parse statememt from STR."
  (let ((lexem-str (lexer str)))
    (multiple-value-bind (stm tail)
      (or-values (stmt lexem-str :no-errors t)
                 (fact-var lexem-str :no-errors t))
      (if (and stm
               (null tail))
        stm
        (error "Not statement -> ~S" str)))))

(defun parse-stmt* (str)
  "Parse statement-list from STR."
  (let ((lexem-str (lexer str)))
    (multiple-value-bind (stmts tail)
      (or-values (stmt-list lexem-str :no-errors t)
                 (fact-var lexem-str :no-errors t))
      (if (and stmts
               (null tail))
        stmts
        (error "Not statement-list -> ~S" str)))))

(set-dispatch-macro-character #\# #\!
                              (lambda (stream char1 char2)
                                (declare (ignore char1 char2))
                                (let ((term (read stream t nil t))
                                      (str (read stream t nil t)))
                                  (unless
                                    (and (symbolp term)
                                         (stringp str))
                                         (error "#! has following format: #! <symbol> <string>"))
                                  (case term
                                    (expr (quote-tree (parse-expr str) 'expr))
                                    (num (quote-tree (parse-num str) 'num))
                                    (id (quote-tree (parse-id str) 'id))
                                    (def (quote-tree (parse-def str) 'def))
                                    (stmt (quote-tree (parse-stmt str) 'stmt))
                                    (stmt* (quote-tree (parse-stmt* str) 'stmt*))))))

;; -----------------
;; MATCHING UTILITES
;; -----------------

(defmacro letvar (vars &body body)
  "Function LET for variables in tree."
  (let ((new-context (gensym)))
    `(let* ((,new-context (make-hash-table :test #'eq))
            (*context* (cons ,new-context *context*)))
       ,@(mapcar (lambda (var)
                   `(add-value ',(second var)
                               (make-var-info :kind ',(first var) :val ,(third var))))
                 vars)
       (let ,(forthis-var-helper vars 'get-value)
         ,@body))))

(defun forthis-var-helper (vars fun)
  "Process variable definition VARS according to function FUN."
  (mapcan (lambda (vars-of-kind)
            (remove-if
              #'null
              (mapcar (lambda (var)
                        (if (symbolp var)
                          (case fun
                            (add-value `(add-value ',var (make-var-info :kind ',(car vars-of-kind) :val ',var)))
                            (get-value `(,var (get-value ',var ',(car vars-of-kind)))))))
                      (cdr vars-of-kind))))
          vars))

(defmacro forthis (term vars &body body)
  "Match tree nodes in BODY with TERM and write differences in VARS."
  (let ((trm (gensym))
        (new-context (gensym)))
    `(let* ((,trm ,term)
            (,new-context (make-hash-table :test #'eq))
            (*context* (cons ,new-context *context*)))
       ,@(forthis-var-helper vars 'add-value)
       (cond ,@(mapcar
                 (lambda (cnd)
                   `((aif (match ,(car cnd) ,trm)
                          (progn (mapc (lambda (var)
                                         (add-value (first var)
                                                    (make-var-info :kind (get-value (first var))
                                                                   :val (second var))))
                                       it)
                                 (let ,(forthis-var-helper vars 'get-value)
                                   (declare (ignorable ,@(mapcan (lambda (var) (cdr var)) vars)))
                                   ,@(cdr cnd))))))
                 body)))))

(defmacro /. (term vars &body body)
  "Rewrite tree TERM according to rules in BODY."
  `(traverse ,term
            (lambda (x) (forthis x ,vars ,@body))))
