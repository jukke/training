;; Задан список. Сформировать новый список,
;; который будет содержать самый длинный палиндром
;; исходного списка. Если палиндромов максимальной
;; длины несколько, то выдать любой из них.
;; 
;; Исходный список задан в виде: (as fgd asa gfdg)

(defun reverse-list (lst &optional res)
  "Reverse list LST."
  (if lst
    (reverse-list (cdr lst) (cons (car lst) res))
    res))

(defun is-palindrom (lst)
  "Check if list LST is palindrom."
  (if (equal lst (reverse-list lst))
    t
    nil))

(defun symbol-to-list (sym)
  "Convert list of symbols SYML to list of sublists."
  (coerce (symbol-name sym) 'list))

(defun lst-length (lst &optional (len 0))
  "Length of list LST."
  (if lst
    (lst-length (cdr lst) (1+ len))
    len))

(defun remove-not-palindrom (lst)
  "Remove not-palindroms from list of symbols LST."
  (if lst
    (if (is-palindrom (symbol-to-list (car lst)))
      (cons (car lst) (remove-not-palindrom (cdr lst)))
      (remove-not-palindrom (cdr lst)))
    nil))

(defun max-length-sublist (lst &optional (res (car lst)))
  "Find maximal sublist of list LST."
  (if lst
    (if (> (lst-length (symbol-to-list (car lst))) (lst-length (symbol-to-list res)))
      (max-length-sublist (cdr lst) (car lst))
      (max-length-sublist (cdr lst) res))
    res))

(defun max-symbol-palindrom (lst)
  "Find maximal palindrom sublist of list LST."
  (max-length-sublist (remove-not-palindrom lst)))

;; tests
(format t "~A -> ~A ~%" '(max-symbol-palindrom '(asd fg sasa)) (max-symbol-palindrom '(asd fg sasa)))
(format t "~A -> ~A ~%" '(max-symbol-palindrom '(asd a fg sasa)) (max-symbol-palindrom '(asd a fg sasa)))
(format t "~A -> ~A ~%" '(max-symbol-palindrom '(asd asa kuk fg sasa)) (max-symbol-palindrom '(asd asa kuk fg sasa)))
(format t "~A -> ~A ~%" '(max-symbol-palindrom '(asd nil sasa)) (max-symbol-palindrom '(asd nil sasa)))
(format t "~A -> ~A ~%" '(max-symbol-palindrom nil) (max-symbol-palindrom nil))
