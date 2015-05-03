
(defvar *definition-keys* '(defun defvar defparameter defmacro defgeneric
			    defmethod))

(defun get-toplevel-symbols (forms)
  (collecting
    (dolist (f forms)
      (when (listp f)
	  (when (member (car f) *definition-keys*)
	    (collect (cons (first f) (second f))))))))



(defun get-file-contents (apath)
  (with-open-file (s apath)
    (loop
	 for thing = (read s nil 'eof)
	 collect s)))
