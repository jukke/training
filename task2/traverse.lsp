;; Write function (traverse func tree fget-node fget-children fmake-node).
;; Visit each node in a tree and apply function func.
;; Functions fget-node, fget-children and fmake-node describe a tree.

(defun traverse (func tree fget-node fget-children fmake-node)
  "Tree TREE traversal. Apply function FUNC to each node."
  (if (null tree)
    nil
    (funcall fmake-node 
             (funcall func (funcall fget-node tree)) 
             (mapcar #'(lambda (child)
                         (if (null child)
                           nil
                           (traverse func child fget-node fget-children fmake-node)))
                     (funcall fget-children tree)))))

(defun fget-node (node)
  "Value of node NODE."
  (car node))

(defun fget-children (node)
  "Children of node NODE."
  (cadr node))

(defun fmake-node (val nodes)
  "Make a node with value VAL and list of children NODES."
  (list val nodes))

;; tests
(format t "~A ~%   -> ~A ~%" 
        '(fmake-node 1 '()) 
        (fmake-node 1 '()))
(format t "~A ~%   -> ~A ~%" 
        '(fmake-node 1 (list (fmake-node 2 '()) (fmake-node 3 '()))) 
        (fmake-node 1 (list (fmake-node 2 '()) (fmake-node 3 '()))))
(format t "~A ~%   -> ~A ~%" 
        '(traverse #'1+ (fmake-node 1 (list (fmake-node 2 '()) (fmake-node 3 '()))) #'fget-node #'fget-children #'fmake-node) 
        (traverse #'1+ (fmake-node 1 (list (fmake-node 2 '()) (fmake-node 3 '()))) #'fget-node #'fget-children #'fmake-node))
