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

#|
;; tests
(format t "~A~%   -> ~A~%" 
        "#! id 'a'" 
        #! id "a")
(format t "~A~%   -> ~A~%" 
        "#! num '5'" 
        #! num "5")
(format t "~A~%   -> ~A~%" 
        "#! expr '1'" 
        #! expr "1")
(format t "~A~%   -> ~A~%" 
        "#! expr 'a'" 
        #! expr "a")
(format t "~A~%   -> ~A~%" 
        "#! expr '1+a'" 
        #! expr "1+a")
(format t "~A~%   -> ~A~%" 
        "#! def 'const a:= 1'" 
        #! def "const a:= 1")
(format t "~A~%   -> ~A~%" 
        "#! stmt '1+a;'" 
        #! stmt "1+a;")
(format t "~A~%   -> ~A~%" 
        "#! stmt 'const a:= 1;'" 
        #! stmt "const a:= 1;")
(format t "~A~%   -> ~A~%" 
        "#! stmt* '1+a; const a:=1;'" 
        #! stmt* "1+a; const a:=1;")
(format t "~A~%   -> ~A~%" 
        "(print-tree #! stmt* '1*(2+3)-5*6;')" 
        (print-tree #! stmt* "1*(2+3)-5*6;"))
(format t "~A~%   -> ~A~%" 
        "(letvar ((def ?d #! def 'const a:=1+2')) #! stmt '?d;')"
        (letvar ((def ?d #! def "const a:=1+2")) #! stmt "?d;"))
(format t "~A~%   -> ~A~%"
        "(check-tree (letvar ((def ?d #! def 'const a:=1+2')) #! stmt '?d;'))"
        (check-tree (letvar ((def ?d #! def "const a:=1+2")) #! stmt "?d;")))
(format t "~A~%   -> ~A~%" 
        "(/. #! expr '1+2+3' ((expr ?e1 ?e2)) (#! expr '?e1+?e2' #! expr '?e1-?e2'))" 
        (/. #! expr "1+2+3" ((expr ?e1 ?e2)) (#! expr "?e1+?e2" #! expr "?e1-?e2")))
(format t "~A~%   -> ~A~%"
        "(/. #! expr '15*(16*a+16*b)' ((expr ?e) (id ?i1 ?i2)) (#! expr '?e*?i1+?e*?i2' #! expr '?e*(?i1+?i2)'))"
        (/. #! expr "15*(16*a+16*b)" ((expr ?e) (id ?i1 ?i2)) (#! expr "?e*?i1+?e*?i2" #! expr "?e*(?i1+?i2)")))
|#
