;; Задано два списка. Сформировать новый список,
;; содержащий только те элементы второго списка,
;; которых нет в первом списке.

(defun remove-elem (elem lst)
  "Remove element ELEM from list LST."
  (if lst
    (if (equal elem (car lst))
      (remove-elem elem (cdr lst))
      (cons (car lst) (remove-elem elem (cdr lst))))
    nil))

(defun subtract (a b)
  "Subtract elements of list A from list B."
  (if a
    (subtract (cdr a) (remove-elem (car a) b))
    b))

;; tests
(format t "~A -> ~A ~%" '(subtract '(1 2) '(2 3)) (subtract '(1 2) '(2 3)))
(format t "~A -> ~A ~%" '(subtract '(1 2) '(1 2)) (subtract '(1 2) '(1 2)))
(format t "~A -> ~A ~%" '(subtract '(1 (1 3)) '((1 3) 3)) (subtract '(1 (1 3)) '((1 3) 3)))
(format t "~A -> ~A ~%" '(subtract nil '(3 nil)) (subtract nil '(3 nil)))
(format t "~A -> ~A ~%" '(subtract '(1 nil) '(2 nil)) (subtract '(1 nil) '(2 nil)))
(format t "~A -> ~A ~%" '(subtract '(1 2) nil) (subtract '(1 2) nil))
(format t "~A -> ~A ~%" '(subtract nil '(1 2)) (subtract nil '(1 2)))
