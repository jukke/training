;; Задан список чисел. Сформировать новый список,
;; в котором будут такие же элементы,
;; как в исходном списке, только в упорядоченном
;; по неубыванию (т.е. нестрогому возрастанию)
;; порядке методом выбора (т.е. приспособить метод
;; выбора для решения такой задачи).

(defun min-elem-down (lst &optional (res (car lst)))
  "Find minimal element RES in list LST. Down-recursion."
  (if lst
    (if (< (car lst) res)
      (min-elem-down (cdr lst) (car lst))
      (min-elem-down (cdr lst) res))
    res))

(defun min-elem-up (lst)
  "Find minimal element in list LST. Up-recursion."
  (if (cdr lst)
    (let ((res (min-elem-up (cdr lst))))
    (if (< (car lst) res)
      (car lst)
      res))
    (car lst)))

(defun remove-first-entry (elem lst)
  "Remove first entry of element ELEM from list LST."
  (if lst
    (if (eq elem (car lst))
      (cdr lst)
      (cons (car lst) (remove-first-entry elem (cdr lst))))
    nil))

(defun reverse-list (lst &optional res)
  "Reverse list LST."
  (if lst
    (reverse-list (cdr lst) (cons (car lst) res))
    res))

(defun selection-sort (lst &optional res)
  "Selection sort of list LST."
  (if lst
    (let ((min-el (min-elem-down lst)))
      (selection-sort (remove-first-entry min-el lst) (cons min-el res)))
    (reverse-list res)))

;; tests
(format t "~A -> ~A ~%" '(selection-sort '(1 3 2 6 3)) (selection-sort '(1 3 2 6 3)))
(format t "~A -> ~A ~%" '(selection-sort nil) (selection-sort nil))
