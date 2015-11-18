;;; Borrowed from https://github.com/vseloved/rutils
;;; See for license.

(in-package :gadgets)

(defmacro dotree ((subtree tree &optional result) &body body)
  "Iterate over each SUBTREE of the TREE in depth-first order.
   Optionally return RESULT."
  (with-gensyms (rec child)
    `(labels ((,rec (,subtree)
                ,@body
                (unless (atom ,subtree)
                  (dolist (,child ,subtree)
                    (,rec ,child)))))
       (awhen ,tree
         (,rec it))
       ,result)))

(defmacro doleaves ((node tree &optional result) &body body)
  "Iterate over each leaf NODE of the TREE in depth-first order.
   Optionally return RESULT."
  (with-gensyms (rec child)
    `(labels ((,rec (,node)
                (unless (atom ,node)
                  (dolist (,child ,node)
                    (if (atom ,child)
                        (let ((,node ,child))
                          ,@body)
                        (,rec ,child))))))
       (awhen ,tree
         (,rec it))
       ,result)))


(defun maptree (fn tree)
  "Map a one-argument function FN over subtree of the TREE
   in depth-first order, using the output of FN as the continuing tree. Note
   that FN must continually return its input in order to map over the whole
   tree."
  (labels ((rec (node)
       (let ((res (funcall fn node)))
         (if (atom res)
       res
       (mapcar #'rec res)))))
    (awhen tree
      (rec it))))

(defun mapleaves (fn tree)
  "Map a one-argument function FN over each leaf node of the TREE
   in depth-first order, returning a new tree with the same structure."
  (labels ((rec (node)
             (if (atom node)
                 (funcall fn node)
     (mapcar #'rec node))))
    (awhen tree
      (rec it))))

(defun tree-size (tree)
  "Returns the number of nodes (internal & external) in the indicated TREE."
  (let ((acc 0))
    (dotree (_ tree)
      (incf acc))
    acc))

(defun tree-depth (tree)
  "Returns the length of the largest of nodes from the root tree."
  (cond ((atom tree) 0)
        ((rest tree)
         (1+ (reduce #'max (cons 1 (mapcar #'tree-depth (rest tree))))))
        (t 1)))

;;;End borrowed

(defun leaves-search-replace (tree &key (test #'eql) match value
          valuefunc predicate)
  (let ((val (if valuefunc
     valuefunc
     (lambda (x) (declare (ignore x)) value)))
  (tst (if predicate
     predicate
     (lambda (x) (funcall test x match)))))
    (mapleaves
     (lambda (x)
       (if (funcall tst x)
     (funcall val x)
     x))
     tree)))

(defun tree-search-replace (tree &key (test #'eql)
          (key #'identity)
          match value
          valuefunc predicate)
  (let ((val (if valuefunc
     valuefunc
     (lambda (x) (declare (ignore x)) value)))
  (tst (if predicate
     predicate
     (lambda (x) (funcall test x match))))
  (prunings nil))
    (let ((res
     (maptree
      (lambda (x)
        (if (funcall tst (funcall key x))
      (progn
        (push x prunings)
        (funcall val x))
      x))
      tree)))
      (apply #'values res (nreverse prunings)))))

(defun collect-by-feature (items feature-func)
  "Group the items in a hash table by the key found by feature-func."
  (collecting-hash-table (:mode :append :test 'equal)
    (dolist (itm items)
      (collect (funcall feature-func itm) itm))))

(defun tree-by-feature (items feature-func &key root
                                             (format #'identity) (identity-func #'identity))
  "Feature-func creates a child key, by which items will be grouped under a parent. Itentity func creates a parent key to match the child key, in the case that the parent object itself can't serve as a matchable key for the children."
  (let ((data (collect-by-feature items feature-func)))
    (labels ((construct-tree (node-key)
               (collecting
                 (dolist (item (gethash node-key data))
                   (collect
                       (cons (funcall format item)
                             (construct-tree
                              (funcall identity-func item))))))))
      (construct-tree root))))
