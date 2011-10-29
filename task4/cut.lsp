;; Gurenko Oleg
;; Test 4. Macros CUT.
;; Based on function TRAVERSE from hometask #3.

(defun traverse (func tree fget-node fget-children fmake-node)
  "Tree TREE traversal. Apply function FUNC to each node."
  (funcall fmake-node 
           (funcall func (funcall fget-node tree)) 
           (mapcar #'(lambda (child)
                       (if (null child)
                         nil
                         (traverse func child fget-node fget-children fmake-node)))
                   (funcall fget-children tree))))

(defun fget-node (node)
  "Value of node NODE."
  (and 
    (atom node)
    node))

(defun fget-children (node)
  "Children of node NODE."
  (and
    (listp node)
    node))

(defun fmake-node (val nodes)
  "Make a node with value VAL and list of children NODES."
  (or
    val
    nodes))

(defmacro cut (&rest rest)
  "Macros cut returns lambda with undefined parameters in REST."
  (let* ((params nil)
         (forms (traverse 
                  #'(lambda (val)
                      (if (eq '_ val)
                        (let ((tmp (gensym)))
                          (push tmp params)
                          tmp)
                        val))
                  rest 
                  #'fget-node 
                  #'fget-children 
                  #'fmake-node)))
    `(lambda ,(reverse params) ,forms)))

;; tests
(format t "~A -> ~A~%" '(funcall (cut list 1 2 3)) (funcall (cut list 1 2 3)))
(format t "~A -> ~A~%" '(funcall (cut list 1 _ 3) 2) (funcall (cut list 1 _ 3) 2))
(format t "~A -> ~A~%" '(funcall (cut list 1 (list _ _) 4) 2 3) (funcall (cut list 1 (list _ _) 4) 2 3))
