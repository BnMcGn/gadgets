(defpattern tail-as (arg)
  (let ((it (gensym)))
    `(guard ,it
            (funcall (lambda (thing)
                       (labels ((proc (thg) (match thg
                                              ((cons _ (guard x (proc x))) t)
                                              (,arg t))))
                         (proc thing)))
                     ,it))))
