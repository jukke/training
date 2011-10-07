;; Получить из списка '(a b c a a b a a c d d c) результат
;; в виде списка '(c a a b a a c), который является
;; самым большим палиндромом.

(defun is-palindrom (lst)
  (if (equal lst (reverse lst))
    t))

(defun max-tail-palindrom (lst)
  (if lst
    (if (is-palindrom lst)
      lst
      (max-tail-palindrom (cdr lst)))
    nil))

(defun max-palindrom (lst &key (res nil))
  (if lst
    (if (< (length lst) (length res))
      res
      (let ((new-max (max-tail-palindrom lst)))
        (if (> (length new-max) (length res))
          (max-palindrom (butlast lst) :res new-max)
          (max-palindrom (butlast lst) :res res))))
    res))

;; tests
(format t "~A -> ~A ~%" '(max-palindrom '(a b c a a b a a c d d c)) (max-palindrom '(a b c a a b a a c d d c)))
(format t "~A -> ~A ~%" '(max-palindrom '(a b c a a b a a c d d nil c)) (max-palindrom '(a b c a a b a a c d d c)))
(format t "~A -> ~A ~%" '(max-palindrom nil) (max-palindrom nil))
