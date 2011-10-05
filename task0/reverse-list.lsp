(defun reverse-list (lst &optional res)
  "Reversing list using recursion."
  (if lst
    (reverse-list (cdr lst) (cons (car lst) res))
    res))

;; some list to test
(print 
  (setq lst '(1 2 3)))

;; reversing using function reverse-list
(print
  (reverse-list lst))

;; reversing without recursion
(print
  (cons 
    (car (cdr (cdr lst))) 
    (cons 
      (car (cdr lst))
      (cons (car lst) nil))))
