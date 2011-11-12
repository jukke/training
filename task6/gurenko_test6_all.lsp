;; Lisp training.
;; Gurenko Oleg.
;; Test #6. All tasks.

(defun make-list-interval (num1 num2 &optional res)
  "Make list with numbers of interval from NUM1 to NUM2."
  (do ((i (ceiling num1) (1+ i)))
    ((> i (floor num2))
     (nreverse res))
    (push i res)))

(defun substruct-sets (set1 set2)
  "Substruct sets SET1 and SET2."
  (if set1
    (if (member (car set1) set2)
      (substruct-sets (cdr set1) set2)
      (cons (car set1) (substruct-sets (cdr set1) set2)))
    nil))

(defun multiply-sets (set1 set2 &optional (res nil))
  "Multiply sets SET1 and SET2."
  (if set2
    (if (member (car set2) set1)
      (multiply-sets set1 (cdr set2) (append (list (car set2)) res))
      (multiply-sets set1 (cdr set2) res))
    (reverse res)))

(defun operate-num (lst res)
  "Operate situations: #[num1 num2]."
  (if (and 
        (numberp (car lst))
        (numberp (cadr lst))
        (null res))
    (values
      (append
        res
        (make-list-interval (car lst) (cadr lst)))
      (cddr lst))))

(defun find-sign (sign lst &optional res)
  "Find (and SIGN <number>) if list LST and return number and list without this combination."
  (if (cddr lst)
    (if (and
          (eq 'and (first lst))
          (eq sign (second lst))
          (numberp (third lst)))
      (values 
        (third lst)
        (append res (cdddr lst)))
      (find-sign sign (cdr lst) (append res (list (car lst)))))
    (values nil nil)))

(defun operate-more-less (lst res)
  "Operate situations: #[> num1 and < num 2]."
  (if (and 
        (null res)
        (numberp (second lst)))
    (multiple-value-bind (num tail)
      (find-sign '< (cddr lst))
      (if num
        (values
          (append 
            res
            (make-list-interval (second lst) num))
          tail)))))

(defun operate-less-more (lst res)
  "Operate situations: #[< num1 and > num 2]."
  (if (and
        (null res)
        (numberp (second lst)))
    (multiple-value-bind (num tail)
      (find-sign '> (cddr lst))
      (if num
        (values
          (append
            res
            (make-list-interval num (second lst)))
          tail)))))

(defun operate-or (lst res)
  "Operate situations: #[... or num1 ...] and #[... or num1 num2 ...]."
  (cond
    ((and (numberp (second lst)) (numberp (third lst))) (values
                                                          (append 
                                                            res 
                                                            (substruct-sets (make-list-interval (second lst) (third lst)) res))
                                                          (cdddr lst)))
    ((numberp (second lst)) (values
                              (append
                                res
                                (substruct-sets (list (second lst)) res))
                              (cddr lst)))))

(defun operate-and (lst res)
  "Operate situations: #[... and num1 ...] and #[... and num1 num2 ...] and #[... and > num1 ...] and #[... and < num1 ...]."
  (cond
    ((and (eq '> (second lst)) (numberp (third lst))) (values
                                                        (remove-if #'(lambda (x)
                                                                       (< x (third lst)))
                                                                   res)
                                                        (cdddr lst)))
    ((and (eq '< (second lst)) (numberp (third lst))) (values
                                                        (remove-if #'(lambda (x)
                                                                       (> x (third lst)))
                                                                   res)
                                                        (cdddr lst)))
    ((and (numberp (second lst)) (numberp (third lst))) (values
                                                          (multiply-sets (make-list-interval (second lst) (third lst)) res)
                                                          (cdddr lst)))
    ((numberp (second lst)) (values
                              (multiply-sets (list (second lst)) res)
                              (cddr lst)))))

(defun scan-list (lst &optional res)
  "Scan list."
  (if lst
    (multiple-value-bind (head tail)
      (cond
        ((numberp (car lst)) (operate-num lst res))
        ((eq 'and (car lst)) (operate-and lst res))
        ((eq 'or (car lst)) (operate-or lst res))
        ((eq '< (car lst)) (operate-less-more lst res))
        ((eq '> (car lst)) (operate-more-less lst res)))
      (scan-list tail head))
    res))

(set-dispatch-macro-character #\# #\[
                              #'(lambda (stream char1 char2)
                                  (let ((pair (read-delimited-list #\] stream t)))
                                    (list 'quote (scan-list pair)))))

(set-macro-character #\]
                     (get-macro-character #\)))

;; tests
(format t "~A~% -> ~A~%" "#[2 7]" #[2 7])
(format t "~A~% -> ~A~%" "#[2 7 or 15]" #[2 7 or 15])
(format t "~A~% -> ~A~%" "#[2 7 or 10 15]" #[2 7 or 10 15])
(format t "~A~% -> ~A~%" "#[2 7 or 10 15 or 13 17]" #[2 7 or 10 15 or 13 17])
(format t "~A~% -> ~A~%" "#[10 15 and 13 17]" #[10 15 and 13 17])
(format t "~A~% -> ~A~%" "#[> 10 and < 15]" #[> 10 and < 15])
(format t "~A~% -> ~A~%" "#[2 7 or 10 15 and < 10]" #[2 7 or 10 15 and < 10])
(format t "~A~% -> ~A~%" "#[> 10 and > 12 and < 15]" #[> 10 and > 12 and < 15])
(format t "~A~% -> ~A~%" "#[< 10 and > 6 and < 15]" #[< 10 and > 6 and < 15])
