(in-package #:gadgets)

;;; Paul Graham, On Lisp, p191
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

;;; by analogy
(defmacro awhen (test-form &rest then-forms)
  `(let ((it ,test-form))
     (when it ,@then-forms)))

(defmacro awhen2 (test &body clauses)
  (let ((sig (gensym)))
    `(multiple-value-bind (it ,sig) ,test
       (when (or it ,sig) ,@clauses))))

(defmacro awhen2only (test &body clauses)
  (let ((sig (gensym)))
    `(multiple-value-bind (it ,sig) ,test
       (when ,sig ,@clauses))))


#|
Don't think anyone is using this, and it causes grumpiness in sigil/parenscript

(defun sharp-backquote-reader (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
		 collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
 #\# #\` #'sharp-backquote-reader)

(defun sharp-tilde-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  `(lambda (&rest it)
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
 #\# #\~ #'sharp-tilde-reader)
|#
