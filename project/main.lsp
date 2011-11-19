;; Lisp training, 2011
;; Author: Gurenko Oleg
;;
;; Loader for training project.

(compile-file "lexer.lsp")
(compile-file "error.lsp")
(compile-file "tree.lsp")
(compile-file "parser.lsp")
(compile-file "match.lsp")
(compile-file "parser_utils.lsp")

(load "lexer.fasl")
(load "error.fasl")
(load "tree.fasl")
(load "parser.fasl")
(load "match.fasl")
(load "parser_utils.fasl")
