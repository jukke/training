;; Lisp training, 2011
;; Author: Gurenko Oleg
;;
;; Utilites for syntax tree.
;; Each node has signature: (<node-kind> <op> (<operand1> <operand2)).

;; -----------
;; NODE MAKERS
;; -----------

(defun make-node-op-expr (op operand1 operand2 &key no-errors)
  "Make an OP-EXPR node."
  (if (and (or (eq 'add op)
               (eq 'sub op)
               (eq 'mul op)
               (eq 'div op))
           (or (node-expr-p operand1)
               (node-var-p operand1))
           (or (node-expr-p operand2)
               (node-var-p operand2)))
    (list 'expr op operand1 operand2)
    (throw-error "TREE/MAKE-NODE-OP-EXPR: incorrect values." nil no-errors)))

(defun make-node-num-expr (num &key no-errors)
  "Make a NUM-EXPR node."
  (if (or (node-num-p num)
          (node-var-p num))
    (list 'expr 'num num)
    (throw-error "TREE/MAKE-NODE-NUM-EXPR: incorrect values." nil no-errors)))

(defun make-node-id-expr (id &key no-errors)
  "Make an ID-EXPR node."
  (if (or (node-id-p id)
          (node-var-p id))
    (list 'expr 'id id)
    (throw-error "TREE/MAKE-NODE-ID-EXPR: incorrect values." nil no-errors)))

(defun make-node-expr (op operand1 operand2 &key no-errors)
  "Make an EXPR node."
  (case op
    (num (make-node-num-expr operand1 :no-errors no-errors))
    (id (make-node-id-expr operand1 :no-errors no-errors))
    (add (make-node-op-expr op operand1 operand2 :no-errors no-errors))
    (sub (make-node-op-expr op operand1 operand2 :no-errors no-errors))
    (mul (make-node-op-expr op operand1 operand2 :no-errors no-errors))
    (div (make-node-op-expr op operand1 operand2 :no-errors no-errors))
    (otherwise (throw-error "TREE/MAKE-NODE-EXPR: undefined operation -> ~S" op no-errors))))

(defun make-node-num (num &key no-errors)
  "Make a NUM node."
  (if (and num
           (listp num)
           (eq 'num (car num)))
    num
    (throw-error "TREE/MAKE-NODE-NUM: incorrect values." nil no-errors)))

(defun make-node-var (var &key no-errors)
  "Make a VAR node."
  (if (and var
           (listp var)
           (eq 'var (first var))
           (symbolp (second var))
           (eq #\? (first (coerce (string (second var)) 'list))))
    var
    (throw-error "TREE/MAKE-NODE-VAR: incorrect values." nil no-errors)))

(defun make-node-id (id &key no-errors)
  "Make an ID node."
  (if (and id
           (listp id)
           (eq 'id (car id)))
    id
    (throw-error "TREE/MAKE-NODE-ID: incorrect values." nil no-errors)))

(defun make-node-def (def-type id val &key no-errors)
  "Make a DEF node."
  (if (and (eq 'const def-type)
           (or (node-id-p id)
               (node-var-p id))
           (or (node-expr-p val)
               (node-var-p val)))
    (list 'def def-type id val)
    (throw-error "TREE/MAKE-NODE-DEF: incorrect values." nil no-errors)))

(defun make-node-stmt (stm &key no-errors)
  "Make a STMT node."
  (if (or (node-def-p stm)
          (node-expr-p stm)
          (node-var-p stm))
    (list 'stmt stm)
    (throw-error "TREE/MAKE-NODE-STMT: incorrect values." nil no-errors)))

(defun make-node-stmt* (stm stmts &key no-errors)
  "Make a STMT* node."
  (if (and (or (node-stmt-p stm)
               (node-var-p stm))
           (or (node-stmt*-p stmts)
               (node-var-p stmts)
               (null stmts)))
    (if stmts
      (list 'stmt* stm stmts)
      (list 'stmt* stm (list 'stmt*)))
    (throw-error "TREE/MAKE-NODE-STMT*: incorrect values." nil no-errors)))

(defun make-node (node-kind params &key no-errors)
  "Make a NODE."
  (case node-kind
    (expr (make-node-expr (first params) (second params) (third params) :no-errors no-errors))
    (num (make-node-num (first params) :no-errors no-errors))
    (var (make-node-var (first params) :no-errors no-errors))
    (id (make-node-id (first params) :no-errors no-errors))
    (def (make-node-def (first params) (second params) (third params) :no-errors no-errors))
    (stmt (make-node-stmt (first params) :no-errors no-errors))
    (stmt* (make-node-stmt* (first params) (second params) :no-errors no-errors))
    (otherwise (throw-error "TREE/MAKE-NODE: incorrect values" nil no-errors))))

;; -------------
;; NODE CHECKERS
;; -------------

(defun node-op-expr-p (node)
  "Check if NODE is an OP-EXPR node."
  (and (listp node)
       (let ((node-kind (first node))
             (op (second node))
             (operand1 (third node))
             (operand2 (fourth node)))
         (and (eq 'expr node-kind)
              (or (eq 'add op)
                  (eq 'sub op)
                  (eq 'mul op)
                  (eq 'div op))
              (or (node-expr-p operand1)
                  (node-var-p operand1))
              (or (node-expr-p operand2)
                  (node-var-p operand2))))))

(defun node-num-expr-p (node)
  "Check if NODE is a NUM-EXPR node."
  (and (listp node)
       (let ((node-kind (first node))
             (op (second node))
             (operand1 (third node)))
         (and (eq 'expr node-kind)
              (eq 'num op)
              (or (node-num-p operand1)
                  (node-var-p operand1))))))

(defun node-id-expr-p (node)
  "Check if NODE is an ID-EXPR node."
  (and (listp node)
       (let ((node-kind (first node))
             (op (second node))
             (operand1 (third node)))
         (and (eq 'expr node-kind)
              (eq 'id op)
              (or (node-id-p operand1)
                  (node-var-p operand1))))))

(defun node-expr-p (node)
  "Check if NODE is an EXPR node."
  (and (listp node)
       (or (node-op-expr-p node)
           (node-num-expr-p node)
           (node-id-expr-p node))))

(defun node-num-p (node)
  "Check if NODE is a NUM node."
  (and (listp node)
       (let ((node-kind (first node))
             (num (second node)))
         (and (eq 'num node-kind)
              (numberp num)))))

(defun node-var-p (node)
  "Check if NODE is a VAR node."
  (and (listp node)
       (let ((node-kind (first node))
             (var (second node)))
         (and (eq 'var node-kind)
              (symbolp var)
              (eq #\? (first (coerce (string var) 'list)))))))

(defun node-id-p (node)
  "Check if NODE is an ID node."
  (and (listp node)
       (let ((node-kind (first node))
             (id (second node)))
         (and (eq 'id node-kind)
              (symbolp id)))))

(defun node-def-p (node)
  "Check if NODE is a DEF node."
  (and (listp node)
       (let ((node-kind (first node))
             (def-type (second node))
             (id (third node))
             (val (fourth node)))
         (and (eq 'def node-kind)
              (eq 'const def-type)
              (or (node-id-p id)
                  (node-var-p id))
              (or (node-expr-p val)
                  (node-var-p val))))))

(defun node-stmt-p (node)
  "Check if NODE is a STMT node."
  (and (listp node)
       (let ((node-kind (first node))
             (stm (second node)))
         (and (eq 'stmt node-kind)
              (or (node-expr-p stm)
                  (node-def-p stm)
                  (node-var-p stm))))))

(defun node-stmt*-p (node)
  "Check if NODE is a STMT* node."
  (or (equalp node '(stmt*))
      (let ((node-kind (first node))
            (stm (second node))
            (stmts (third node)))
        (and (eq 'stmt* node-kind)
             (or (node-stmt-p stm)
                 (node-var-p stm))
             (or (node-stmt*-p stmts)
                 (node-var-p stmts))))))

(defun node-p (lst)
  "Check if LST is a NODE of a syntax tree."
  (or (node-expr-p lst)
      (node-num-p lst)
      (node-var-p lst)
      (node-id-p lst)
      (node-def-p lst)
      (node-stmt-p lst)
      (node-stmt*-p lst)))

(defun check-tree (tree)
  "Check TREE."
  (node-p tree))

;; ----------
;; NODE PRINT
;; ----------

(defun print-node-var (node)
  "Print node VAR."
  (and (node-var-p node)
       (concatenate 'string (write-to-string (second node)))))

(defun print-node-expr (node &optional (prev-op nil))
  "Print node OP-EXPR."
  (cond
    ((node-expr-p node)
     (let ((op (second node))
           (operand1 (third node))
           (operand2 (fourth node)))
       (case op
         (num (concatenate 'string (write-to-string (second operand1))))
         (id (concatenate 'string (write-to-string (second operand1))))
         (add (if (or (eq 'mul prev-op)
                      (eq 'div prev-op))
                (concatenate 'string "(" (print-node-expr operand1 op) "+" (print-node-expr operand2 op) ")")
                (concatenate 'string (print-node-expr operand1 op) "+" (print-node-expr operand2 op))))
         (sub (if (or (eq 'mul prev-op)
                      (eq 'div prev-op))
                (concatenate 'string "(" (print-node-expr operand1 op) "-" (print-node-expr operand2 op) ")")
                (concatenate 'string (print-node-expr operand1 op) "-" (print-node-expr operand2 op))))
         (mul (concatenate 'string (print-node-expr operand1 op) "*" (print-node-expr operand2 op)))
         (div (concatenate 'string (print-node-expr operand1 op) "/" (print-node-expr operand2 op))))))
    ((node-var-p node)
     (print-node-var node))))

(defun print-node-num (node)
  "Print node NUM."
  (cond
    ((node-num-p node)
     (concatenate 'string (write-to-string (second node))))
    ((node-var-p node)
     (print-node-var node))))

(defun print-node-id (node)
  "Print node ID."
  (cond
    ((node-id-p node)
     (concatenate 'string (write-to-string (second node))))
    ((node-var-p node)
     (print-node-var node))))

(defun print-node-def (node)
  "Print node DEF."
  (cond
    ((node-def-p node)
     (let ((def-type (second node))
           (id (third node))
           (val (fourth node)))
       (concatenate 'string (write-to-string def-type) " " (print-node-id id) ":=" (print-node-expr val))))
    ((node-var-p node)
     (print-node-var node))))

(defun print-node-stmt (node)
  "Print node STMT."
  (cond
    ((node-stmt-p node)
     (let ((stm (second node)))
       (cond
         ((node-expr-p stm)
          (concatenate 'string (print-node-expr stm) ";"))
         ((node-def-p stm)
          (concatenate 'string (print-node-def stm) ";")))))
    ((node-var-p node)
     (print-node-var node))))

(defun print-node-stmt* (node)
  "Print node STMT*."
  (cond
    ((equalp node '(stmt*))
     (concatenate 'string nil))
    ((node-stmt*-p node)
     (let ((stm (second node))
           (stmts (third node)))
       (if (null stmts)
         (concatenate 'string (print-node-stmt stm))
         (concatenate 'string (print-node-stmt stm) (format nil "~%") (print-node-stmt* stmts)))))
    ((node-var-p node)
     (print-node-var node))))

(defun print-tree (tree)
  "Print TREE."
  (if (check-tree tree)
    (let ((node-type (first tree)))
      (case node-type
        (var (print-node-var tree))
        (num (print-node-num tree))
        (id (print-node-id tree))
        (expr (print-node-expr tree))
        (def (print-node-def tree))
        (stmt (print-node-stmt tree))
        (stmt* (print-node-stmt* tree))))
    (format t "Incorrect tree.")))

;; ------------
;; NODE GETTERS
;; ------------

(defun get-number (node)
  "Get number from NODE."
  (cond
    ((node-num-p node) (second node))
    ((node-num-expr-p node) (get-number (third node)))))

(defun get-id (node)
  "Get id from NODE."
  (cond
    ((node-id-p node) (second node))
    ((node-id-expr-p node) (get-id (third node)))))

(defun get-var (node)
  "Get var from NODE."
  (if (node-var-p node)
    (second node)))

;; -------------
;; TREE TRAVERSE
;; -------------

(defun ntraverse (tree func)
  "Destructive traverse syntax TREE and apply function FUNC to each node."
  (cond
    ((listp tree)
     (let* ((new-node (mapcar (lambda (tree-node)
                                (ntraverse tree-node func))
                              tree))
            (matched-node (funcall func new-node)))
       (or matched-node
           new-node)))
    ((symbolp tree)
     tree)
    ((numberp tree)
     tree)))

(defun traverse (tree func)
  "Traverse syntax TREE and apply function FUNC to each node."
  (cond
    ((listp tree)
     (funcall func (mapcar (lambda (tree-node)
                             (traverse tree-node func))
                           tree))
     tree)
    ((symbolp tree)
     tree)
    ((numberp tree)
     tree)))
