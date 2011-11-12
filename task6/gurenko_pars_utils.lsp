;; Write separate functions for parsing expression and for parsing numbers/variables.
;; Write reader-macro with following signature:
;; #! <grammar-node-name> <string>
;;
;; For example:
;; #! num "10" -> (num 10)
;; #! num "10+5" -> syntax error
;; #! var "?a" -> (var ?a)
;; #! var "10+?a" -> syntax error
;; #! expr "10" -> (expr num (num 10))
;; #! expr "?a" -> (expr var (var ?a))
;; #! expr "10+?a" -> (expr add (expr num (num 10)) (expr var (var ?a)))

(load "gurenko_lexer.lsp")
(load "gurenko_parser.lsp")

(defun parse-expr (str)
  "Parse expression from STR."
  (parser (lexer str)))

(defun parse-num (str)
  "Parse number from STR."
  (let ((lexed-str (lexer str)))
    (multiple-value-bind (fct fct-type tail err)
      (fact-num lexed-str)
      (if (null err)
        (if (and
              (null tail)
              (eq 'num fct-type))
          fct
          `(Not number -> ,lexed-str))
        err))))

(defun parse-var (str)
  "Parse variable from STR."
  (let ((lexed-str (lexer str)))
    (multiple-value-bind (fct fct-type tail err)
      (fact-var lexed-str)
      (if (null err)
        (if (and
              (null tail)
              (eq 'var fct-type))
          fct
          `(Not variable -> ,lexed-str))
        err))))

(set-dispatch-macro-character #\# #\!
                              #'(lambda (stream sub-char arg)
                                  (let ((pars-type (read stream t nil t))
                                        (expression (read stream t nil t)))
                                    (cond
                                      ((eq 'expr pars-type) `(parse-expr ,expression))
                                      ((eq 'num pars-type) `(parse-num ,expression))
                                      ((eq 'var pars-type) `(parse-var ,expression))))))
