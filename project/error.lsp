;; Lisp training, 2011
;; Author: Gurenko Oleg
;;
;; Errors handler.

(defun throw-error (error-text lst no-errors)
  "Throw error with text ERR in list LST if flag NO-ERRORS is true."
  (if (null no-errors)
    (error error-text lst)
    (values nil lst)))
