(in-package :gadgets)

;(defmacro with-gensyms (syms &body body)
;  `(let ,(mapcar #'(lambda (s)
;                     `(,s (gensym)))
;                 syms)
;     ,@body))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))
