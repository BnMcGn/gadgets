;;;; gadgets.lisp

(in-package #:gadgets)

;;; "gadgets" goes here. Hacks and glory await!

(defun symbolize (entity &key (package *package*))
  (if (symbolp entity)
      (intern (mkstr entity) package)
      (intern (string-upcase entity) package)))

(defun keywordize (entity)
  (symbolize entity :package 'keyword))

(defun keywordize-foreign (entity)
  (keywordize (string-upcase (to-lisp-case entity))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun string-unless-number (x)
  (if (numberp x)
      x
      (handler-case
         (parse-integer x)
        #+:sbcl (sb-int:simple-parse-error () x)
        #+:ccl (ccl::parse-integer-not-integer-string () x))))

(defun symbol-unless-number (x)
  (let ((val (string-unless-number x)))
    (if (numberp val)
        val
        (symbolize val))))

;;;FIXME: Inefficient: don't need to know whole length of string
(defun string-true (string)
  (when (< 0 (length string))
    string))

(defun autoquote (itm)
  (if (symbolp itm)
      (list 'quote itm)
      itm))

(defmacro ret (var val &body body)
  `(let ((,var ,val))
     ,@body
     ,var))

(defmacro def-as-func (var func-form)
  `(setf (symbol-function ',var) ,func-form))

(defun funcall-in-macro (func &rest args)
  (if (and (consp func) (eq 'function (car func)))
      (apply (symbol-function (second func)) args)
      (apply func args)))

(defun apply-in-macro (func &rest args)
  (if (and (consp func) (eq 'function (car func)))
      (apply #'apply (symbol-function (second func)) args)
      (apply #'apply func args)))

(defun functionp-in-macro (item)
  (and (consp item) (member (car item) '(function lambda))))

(defun sequence-starts-with (seq seq2)
   (loop for x across seq2
      for y across seq
      do (when (not (eq x y))
     (return nil))
      finally (return t)))

(defun sequence-ends-with (seq testseq)
  ()(subseq seq (max 0 (- (length seq) (length testseq)))))
(defun assoc-cdr (&rest params)
  (cdr (apply #'assoc params)))

(defun assoc-all (item alist &key (test #'eql))
  "Gets all items associated with a key, not just the first. Returns a list"
  (loop for x in alist
       when (funcall test (car x) item)
       collect (cdr x)))

;;Dubious...
(defun assoc-set (key alist-or-symb val)
  (let* ((have-symb (symbolp alist-or-symb))
   (alist (if have-symb
       (symbol-value alist-or-symb)
       alist-or-symb)))
    (cond ((assoc-cdr key alist)
     (setf (assoc-cdr key alist) val))
    (alist
     (rplacd (last alist) (list (cons key val))))
    (have-symb
     (setf alist-or-symb `((,key . ,val))))
    (t
     (error 'unaddable-list)))))

(defun assoc-symbolize (alist &key (package *package*))
  (collecting
    (dolist (var alist)
      (collect (cons (symbolize (car var) :package package)
         (cdr var))))))

(defmacro do-alist ((key value source) &body body)
  (with-gensyms (itm)
    `(dolist (,itm ,source)
       (let ((,key (car ,itm))
             (,value (cdr ,itm)))
         ,@body))))

(defmacro do-hash-table ((key value source) &body body)
  (once-only (source)
    `(dolist (,key (alexandria:hash-table-keys ,source))
       (let ((,value (gethash ,key ,source)))
         ,@body))))

(defun key-in-hash? (key hashtable)
  (nth-value 1 (gethash key hashtable)))

(defun in-plist? (key plist)
  (< 1 (length (member key plist))))

(defun merge-plists (&rest plists)
  (let ((result (copy-list (first plists))))
    (dolist (plist (rest plists))
      (loop for (key value) on plist by #'cddr
         do (setf (getf result key) value)))
    result))

(defun xsubseq (sequence start end &key (type 'sequence))
  "Returns sequence with start->end chopped out of it"
  (concatenate type
   (subseq sequence 0 start)
   (subseq sequence (1+ end))))

(defun sequence->list (seq)
  (loop for x across seq
       collect x))

(defmacro with-slow-collectors ((&rest collectors) &body body)
  "Collect some things into lists forwards. The names in COLLECTORS
are defined as local functions which each collect into a separate
list.  Returns as many values as there are collectors, in the order
they were given."
  (%with-collectors-check-collectors collectors)
  (let ((gensyms-alist
   (%with-collectors-gensyms-alist collectors)))
    `(let ,(loop for collector in collectors
     for tail = (cdr (assoc collector gensyms-alist))
     nconc (list collector tail))
       (labels ((fetch-slow-collectors () (values ,@collectors)))
   (labels ,(loop for collector in collectors
         for tail = (cdr (assoc collector gensyms-alist))
         collect `(,collector
             (thing)
             (if ,collector
           (setf (cdr ,tail)
           (setf ,tail (list thing)))
           (setf ,collector
           (setf ,tail (list thing))))))
     ,@body)))))

(defmacro multiple-value-passthru (vars value-form &body body)
  `(multiple-value-bind ,vars
      ,value-form
     (values ,@body)))

(defmacro multiple-value-apply (function values-form)
  `(apply ,function (multiple-value-list ,values-form)))

(defmacro multiple-valplex (values-form &body form-with-Vn-vars)
  (let* ((maxnum
    (loop for itm in (flatten form-with-Vn-vars)
       maximizing (aif (ignore-errors
             (parse-integer
        (subseq (mkstr itm) 1)))
           it
           0)))
   (symblist
    (loop for i from 0 upto maxnum
         collect (symb 'v i))))
    `(multiple-value-bind ,symblist ,values-form
       (declare (ignorable ,@symblist))
       ,@form-with-Vn-vars)))

(defun apply-compose (&rest functions)
  (lambda (&rest whatever)
    (labels ((func (data funcs)
      (if funcs
          (apply (car funcs) (func data (cdr funcs)))
          whatever)))
      (func whatever functions))))

(defun fetch-keyword (key alist &key (in-list t))
  "Find if a key is in a list, return the next item
  after it. if checklist is true, test the first element of any sublists for the   key and if found return rest of list as parameter."
  (let ((keytest (if in-list
          (lambda (x y)
      (or (eql x y)
          (and (consp y) (eql x (car y)))))
          #'eql)))
    (aif (loop for x on alist
       do (when (funcall keytest key (car x))
      (return (if (atom (car x)) (second x) (cdar x)))))
   (values it t)
   (values nil nil))))

;removes found keywords from list, returning cleaned list as second val
(defun extract-keywords (keywords alist &key in-list)
  (with-collectors (keypairs< rest<)
    (let ((currkey nil))
      (dolist (itm alist)
  (anaphora:acond
    (currkey
     (keypairs< (cons currkey itm))
     (setf currkey nil))
    ((find itm keywords :test #'eq-symb)
     (setf currkey it))
    ((and in-list (consp itm) (find (car itm) keywords :test #'eq-symb))
     (keypairs< itm))
    (t (rest< itm))))
      (when currkey
  (keypairs< (list currkey))))))

(defmacro bind-extracted-keywords ((source remainder &rest keys) &body body)
  "Removes the keywords named in keys, with their accompanying parameters, from
the expression supplied in source. Source, minus the keys, is bound to
remainder. The names of the keys are used for bindings for the accompanying
values. (bind-extracted-keywords ((1 2 :x 3) data :x) <body>) Results in the
body being executed with data bound to (1 2) and x bound to 3."
  (with-gensyms (extracts)
    `(multiple-value-bind (,extracts ,remainder)
         (extract-keywords
          ',(mapcar (lambda (x) (if (symbolp x) x (car x))) keys)
          ,source)
       (declare (ignorable ,remainder))
       (let ,(collecting
               (dolist (k keys)
                 (if (listp k)
                     (collect (list (symb (car k))
                                    (if (member :multiple k)
                                        `(assoc-all ,(car k) ,extracts)
                                        `(and (assoc ,(car k) ,extracts)
                                              (cdr (assoc ,(car k) ,extracts))))))
                     (collect (list (symb k)
                                    `(and (assoc ,k ,extracts)
                                          (cdr (assoc ,k ,extracts))))))))
         ,@body))))

(defmacro autobind-specials ((vars params prefix) &body body)
  "A convenience macro to allow function parameters to override special vars. Given a series of specials named *prefix-thing*, a symbol in vars named thing and prefix set to '*prefix-, then 'thing' will be bound to the value of the keyword thing, if it is found in the params, else *prefix-thing*, that being unbound, by a default value. The default value can be specified by replacing thing in the vars list with (thing <default>), much like in a lambda list."
  (let ((vars (collecting
          (dolist (v vars)
      (collect (if (symbolp v) (list v nil) v)))))
  (pbound (gensym)))
    `(let ((,pbound (extract-keywords ',(mapcar #'car vars) ,params)))
       (let ,(collecting
        (dolist (v vars)
    (let ((specsym (symb prefix (car v) '*)))
      (collect
          (list (car v)
          `(aif (assoc ',(car v) ,pbound :test #'eq-symb)
            (cdr it)
            (if (boundp ',specsym)
                ,specsym
                (second ',v))))))))
   ,@body))))

(defmacro def-wrapper-func ((name callname &rest specialkeys) &body body)
  "Creates a wrapper func that extracts and binds specified keys from a call. The post extraction call will be bound to the name specified by callname. This call is not executed by def-wrapper-func. It must be called manually in the body section, eg: (apply #'funcall callvar)"
  (let ((call (gensym))
  (keylist (gensym))
  (specialkeys-plain
   (mapcar (lambda (x)
       (if (atom x)
           x
           (car x))) specialkeys))
  (defaults
   (loop for itm in specialkeys
        unless (atom itm)
        collect (cons (car itm) (cdr itm))))
        (full-list (gensym)))
    `(defun ,name (&rest ,call)
       (multiple-value-bind (,keylist ,callname)
     (extract-keywords ',specialkeys-plain ,call)
   (let ((,full-list
    (concatenate 'list ,keylist ',defaults)))
     (let ,(concatenate
      'list
      (loop for k in specialkeys-plain
         collect (list
            (intern (symbol-name k))
            `(assoc-cdr ,k ,full-list)))
      (loop for k in specialkeys-plain
         collect (list
            (symb k '-supplied-p)
            `(not (not (assoc ,k ,keylist))))))
       (declare (ignorable ,@(loop for k in specialkeys-plain
              collect `,(symb k '-supplied-p))))
       ,@body))))))


(defun range (start &optional
        (stop start stop-supplied-p)
        (step 1))
  (unless stop-supplied-p (setf start 0))
  (if (> (abs (- (+ start step) stop)) (abs (- start stop)))
      nil
      (if (minusp step)
    (loop for i from start above stop by (abs step) collect i)
    (loop for i from start below stop by step collect i))))

(defmacro loop-window ((varname list size) &body body)
  (let ((list-val (gensym))
  (size-val (gensym))
  (i (gensym))
  (j (gensym)))
    `(let ((,list-val ,list)
     (,size-val ,size))
       (loop for ,i from ,size-val to (list-length ,list-val)
      for ,j from 0 to (- (list-length ,list-val) ,size-val)
        for ,varname = (subseq ,list-val ,j ,i) ,@body))))

(defmacro do-window ((var/s source
                      &key (size 2) (step 1)
                        start-padding) &body body)
  (let ((size (if (listp var/s) (length var/s) size))
        (data (gensym))
        (i (gensym)))
    `(let ((,data (concatenate 'list ,start-padding ,source)))
       (dolist (,i (range 0 (1+ (- (length ,data) ,size)) ,step))
         ,(if (listp var/s)
              `(destructuring-bind ,var/s (subseq ,data ,i (+ ,i ,size))
                 ,@body)
              `(let ((,var/s (subseq ,data ,i (+ ,i ,size))))
                 ,@body))))))

(defparameter *whitespace-characters*
  '(#\Space #\Newline #\Backspace #\Tab
    #\Linefeed #\Page #\Return #\Rubout))

(defun last-car (list)
  (car (last list)))

(defun string-equal-caseless (a b)
  (string-equal (string-upcase a) (string-upcase b)))

(defun boolify (item)
  (typecase item
    (string
     (if (member (string-trim *whitespace-characters* item)
     '("1" "true" "yes" "t") :test #'string-equal-caseless)
   t nil))
    (integer
     (if (< item 1) nil t))
    (t t)
    (otherwise nil)))

(defun make-trycar (spec)
  (labels ((proc (spec)
       (let ((crfunc (case (car spec)
           (#\A #'car)
           (#\D #'cdr))))
         (if (null (cdr spec))
       (lambda (x)
         (if (consp x)
       (values (funcall crfunc x) t)
          (values nil nil)))
       (let ((cmpfunc (proc (cdr spec))))
         (lambda (x)
           (if (consp x)
         (funcall cmpfunc (funcall crfunc x))
         (values nil nil))))))))
    (proc (nreverse (coerce (string-trim "CR" (symbol-name spec)) 'list)))))

(defun trycar (carspec list?)
  (funcall (make-trycar carspec) list?))

(defmacro tryit (&body body)
  `(handler-case
       (values
  (progn ,@body)
  t)
     (t (e) (declare (ignore e)) (values nil nil))))

(defun chunk (n alist)
  (if (> (list-length alist) n)
      (cons (subseq alist 0 n) (chunk n (nthcdr n alist)))
      (list alist)))

(defun flatten-1 (alist)
  (collecting
    (dolist (x alist)
      (if (atom x)
    (collect x)
    (dolist (y x)
      (collect y))))))

(defun flatten-when (predicate items)
  (let ((res nil))
    (dolist (itm items)
      (if (funcall predicate itm)
          (dolist (subitm itm)
            (push subitm res))
          (push itm res)))
    (nreverse res)))

(defun eq-symb-case (a b)
  (or (eq a b) (equal (mkstr a) (mkstr b))))

(defun eq-symb (a b)
  (or (eq-symb-case a b)
      (equal (string-upcase (mkstr a)) (string-upcase (mkstr b)))))

(defun match-a-symbol (item symbols)
  (first-match symbols (lambda (x) (eq-symb x item))))

(defun tree-level (tree level)
  (if (= 0 level)
      (list tree)
      (loop for itm in tree
     unless (atom itm)
     append (tree-level itm (1- level)))))

(defun divide-on-index (seq ind &key fail)
  "Return seq divided into two subsequences at index. If seq is shorter than ind, return seq in the first part and nil in the second."
  (let (stor
        (src seq))
    (dotimes (i ind)
      (unless src
        (if fail (return-from divide-on-index nil) (return)))
      (push (car src) stor)
      (setf src (cdr src)))
    (values (nreverse stor) src)))

(defun divide-sequence (seq test)
  (let ((ind
   (loop for itm across seq
      for i from 0
      until (not (funcall test itm))
      finally (return i))))
    (values
     (subseq seq 0 ind)
     (subseq seq ind))))

(defun divide-list (alist test)
  (labels ((proc (accum alist test)
             (if alist
                 (if (funcall test (car alist))
                     (values (nreverse accum) alist)
                     (proc (cons (car alist) accum)
                           (cdr alist)
                           test))
                 (values (nreverse accum) nil))))
    (proc nil alist test)))

(defun divide-list+ (alist test)
  "Like divide-list, but includes the item that triggered test in the first
  list."
  (labels ((proc (accum alist)
             (if alist
                 (if (funcall test (car alist))
                     (values (nreverse (cons (car alist) accum))
                             (cdr alist))
                     (proc (cons (car alist) accum)
                           (cdr alist)))
                 (values (nreverse accum) nil))))
    (proc nil alist)))

(defun remove-if-member (seq things &key key (test #'eq))
  (let ((keyfunc (or key (lambda (x) x))))
    (remove-if #'(lambda (x)
       (member (funcall keyfunc x) things :test test)) seq)))

(defun splitfilter (alist test)
  (with-collectors (in< out<)
    (dolist (elmt alist)
      (if (funcall test elmt)
    (in< elmt)
    (out< elmt)))))

(defun first-match (list predicate)
  (multiple-value-bind (val sig)
      (dolist (x list)
  (when (funcall predicate x)
    (return (values x t))))
    (if sig
  (values val t)
  (values nil nil))))

(defun keyword-splitter (data &key (flatten t))
  (if data
      (cond ((and (consp (car data)) (keywordp (caar data))
     (if flatten
         (cons
          (car data)
          (keyword-splitter (cdr data) :flatten flatten))
         (cons
          (list (car data))
          (keyword-splitter (cdr data) :flatten flatten)))))
      ((keywordp (car data))
       (multiple-value-bind
       (this next)
     (divide-list
      (cdr data)
      (lambda (x) (or (keywordp x)
          (and (consp x) (keywordp (car x))))))
         (cons (cons (car data) (if (= 1 (length this)) (car this) this))
         (keyword-splitter next :flatten flatten))))
      (t (error "Missing keyword")))))

(defun keyword-value (key alist)
  (if alist
      (if (eq key (car alist))
    (if (> (length alist) 1)
        (values (second alist) t)
        (values nil nil))
    (keyword-value key (cdr alist)))
      (values nil nil)))

(defun set-keyword-value (key alist newval)
  (if alist
      (if (eq key (car alist))
    (if (> (length alist) 1)
        (setf (car (cdr alist)) newval)
        (setf (cdr alist) (cons newval nil)))
    (if (null (cdr alist))
        (setf (cdr alist) (list key newval))
        (set-keyword-value key (cdr alist) newval)))
      (error "Can't set nil")))

(defsetf keyword-value set-keyword-value)

(defun strip-keywords (arglist &keys targets)
  "Remove keyword arguments from an arglist if their values are nil."
  (multiple-value-bind (main key) (keyword-splitter arglist)
    (concatenate
     'list main
     (collecting
       (map-by-2 (lambda (k v) (and targets (member k targets))) )))))

(defun strip-keywords (arglist &key targets (test (lambda (x) (not (null x)))))
  "Remove keyword arguments from an arglist if their values are nil."
  (labels ((proc-outer (arglist)
             (if (null arglist)
                 nil (if (keywordp (car arglist))
                         (proc-keys arglist)
                         (cons (car arglist) (proc-outer (cdr arglist))))))
           (proc-keys (arglist)
             (if (null arglist)
                 nil
                 (if targets
                     (if (member (car arglist) targets)
                         (if (funcall test (second arglist))
                             (list* (car arglist) (second arglist)
                                    (proc-keys (cddr arglist)))
                             (proc-keys (cddr arglist)))
                         (list* (car arglist) (second arglist)
                                (proc-keys (cddr arglist))))
                     (if (funcall test (second arglist))
                         (list* (car arglist) (second arglist)
                                (proc-keys (cddr arglist)))
                         (proc-keys (cddr arglist)))))))
    (proc-outer arglist)))

(defmacro with-any/all/none (&body body)
  (let ((name (gensym)))
    `(block ,name
       (labels ((returner (rval) (return-from ,name rval)))
   (macrolet ((any (test &optional (retval t))
          `(when ,test (returner ,retval)))
        (all (test &optional retval)
          `(when (not ,test) (returner ,retval)))
        (none (test &optional retval)
          `(when ,test (returner ,retval))))
     ,@body)))))

;anaphoric macro: 2nd expr wraps first (which is contained in it) if true,
;else expr run plain.
(defmacro awrap-expr-if (pred expr &body cond-expr-with-var-it)
  `(if ,pred
       (funcall
  (lambda (it)
    ,@cond-expr-with-var-it)
  ,expr)
       ,expr))

(defun aslist (itm)
  (if (listp itm)
      itm
      (list itm)))

(defmacro aif2only (test &optional then else)
     (let ((win (gensym)))
       `(multiple-value-bind (it ,win) ,test
    (if ,win ,then ,else))))

(defun rotating-cache (&optional initial)
  (lambda (newval)
    (prog1
  initial
      (setf initial newval))))

(defun tracker-same (&key initial (test #'equal))
  (lambda (thing)
    (prog1
  (funcall test initial thing)
      (setf initial thing))))

(defun tracker-different (&key initial (test (complement #'equal)))
  (tracker-same :initial initial :test test))

(defmacro do-file-by-line ((line stream-or-path) &body body)
  (with-gensyms (stream fspec)
    `(let ((,fspec ,stream-or-path))
       (cond
   ((or (pathnamep ,fspec) (stringp ,fspec))
    (with-open-file (,stream ,fspec)
      (loop
         for ,line = (read-line ,stream nil 'eof)
         until (eq ,line 'eof)
         do (progn ,@body))))
   ((streamp ,fspec)
    (loop
       for ,line = (read-line ,fspec nil 'eof)
       until (eq ,line 'eof)
       do (progn ,@body)))
   (t (error "Not a stream or path!"))))))

(defun map-file-by-line (function stream-or-path)
  (collecting
      (do-file-by-line (line stream-or-path)
  (collect (funcall function line)))))

(defmacro do-list-with-rest ((head tail source) &body body)
  (once-only (source)
    `(let ((,head nil))
       (loop for ,tail on ,source
    do (prog1
     ,@body
         (push (car ,tail) ,head))))))

;Returns tlist (copy) with ind set to val. If ind is beyond the length of tlist,
;pad out the list with padding
(defun list-set-place (tlist ind val padding)
  (if (<= (length tlist) ind)
      (concatenate
       'list
       tlist
       (loop for i from 1 to (- ind (length tlist))
      collect padding)
       (cons val nil))
      (concatenate
       'list
       (subseq tlist 0 ind)
       (list val)
       (subseq tlist (1+ ind)))))

(defun %togglelist-index-adjuster (mask ind &optional (tally 0))
  (if (consp mask)
      (if (car mask)
    (if (= ind 0)
        tally
        (%togglelist-index-adjuster (cdr mask) (1- ind) (1+ tally)))
    (%togglelist-index-adjuster (cdr mask) ind (1+ tally)))
      (+ ind tally)))

(defun %togglelist-track-toggles (orig-mask)
  (let ((stor (copy-list orig-mask)))
    (lambda (ind &optional val)
      (if (null ind)
    stor
    (let ((newind (%togglelist-index-adjuster orig-mask ind)))
      (setf stor (list-set-place stor newind val t))
      stor)))))

;FIXME togglelist is ugly and complicated. Need better way to do same thing.
(defmacro do-togglelist ((var thelist storevar) &body body)
  (once-only (thelist)
    (with-gensyms (index currstore)
      `(let ((,index 0)
       (,currstore (%togglelist-track-toggles ,storevar)))
   (labels
       ((toggle (&key (index ,index) (value nil))
    (funcall ,currstore index value))
        (get-list (&key (tlist ,thelist) (mask ,storevar))
    (loop for x on tlist
       for y in mask
       when y collect (car x) into stor
       finally (return (concatenate 'list stor x))))
        (save-store ()
    (setf ,storevar (funcall ,currstore nil))))
           (when (null ,storevar)
       (setf ,storevar (cons t nil)))
     (dolist (,var (get-list))
       ,@body
       (incf ,index))
     (save-store))))))

(defmacro preserve-other-values (expression func)
  "Take the values returned by expression, pass the first of them to func,
   returning its first value as the primary value and appending the remaining
   values from expression as unchanged.
  (1+ (values 1 2 3)) => 2
  (preserve-other-values (values 1 2 3)
                         #'1+) => 2 2 3"
  (with-gensyms (vals)
    `(let ((,vals (multiple-value-list ,expression)))
       (apply #'values (funcall ,func (car ,vals)) (cdr ,vals)))))

(defmacro pif (test then &optional else)
  (with-gensyms (tres)
    `(let ((,tres ,test))
       (format t "~%Print-IF expression: ~a  RESULT: ~a~%" (quote ,test) ,tres)
       (if ,test ,then ,else))))

(defmacro print-lambda ((&rest args) &body body)
  (with-gensyms (res)
    `(lambda (,@args)
       (format t "~%Print-lambda Input:")
       (print (list ,@(remove-if (lambda (x)
           (eq (elt (symbol-name x) 0) #\&)) args)))
       (format t "~%Print-lambda Output:")
       (let ((,res (multiple-value-list ,@body)))
   (dolist (x ,res)
     (print x))
   (print "")
   (apply #'values ,res)))))

(defmacro print-cond (&rest clauses)
  (let ((count 0))
    `(cond
       ,@(collecting
          (dolist (cl clauses)
            (incf count)
            (collect `(,(car cl)
              (preserve-other-values
               (progn ,@(cdr cl))
               (lambda (x)
                 (format t "~&Print-cond: Clause ~a: Result: ~a~%"
                         ,count x)
                 x)))))))))

(defmacro print-and (&rest forms)
  (let ((count 0)
        (itm (gensym)))
    `(and
      ,@(collecting
         (dolist (f forms)
           (incf count)
           (collect
               `(let ((,itm ,f))
                  (progn (if ,itm
                             (format t "~&Print-and: Clause ~a: ~a~%" ,count ,itm)
                             (format t "~&Print-and: FAILED at ~a~%" ,count))
                         ,itm))))))))

(defun tree-union (t1 t2 &key
       (cmp (lambda (x y) (eq (car (aslist x)) (car (aslist y)))))
       (node (compose #'car #'aslist))
       (children (compose #'cdr #'aslist))
       (rejoin #'cons))
  (cond ((null t1) t2)
  ((null t2) t1)
  (t
   (let ((store nil))
     (collecting
       (dolist (t1node t1)
         (block top
     (do-togglelist (t2node t2 store)
       (when (funcall cmp t1node t2node)
         (progn
           (toggle)
           (let ((ch1 (funcall children t1node))
           (ch2 (funcall children t2node)))
       (if (or ch1 ch2)
           (collect
         (funcall
          rejoin
          (funcall node t1node)
          (tree-union ch1 ch2
           :cmp cmp :children children :node node
           :rejoin rejoin)))
           (collect t1node)))
           (save-store)
           (return-from top))))
     (collect t1node)))
       (do-togglelist (t2node t2 store)
         (collect t2node)))))))

(defmacro dotree ((node tree &key descend-if traverse-if (nodes-only t))
      &body body)
  (once-only (tree descend-if traverse-if)
    `(tree-tracker
       (labels ((main (,node)
      ,@body))
   (labels
       ((rec (tree)
    (if (atom tree)
        (when tree
          (main tree))
        (progn
          (when (not ,nodes-only)
      (main tree))
          (if (and ,descend-if (not (funcall ,descend-if tree)))
        (main (car tree))
        (if (null (car tree));Handle in-list nil
            (tree-down () (main (car tree)))
            (tree-down (:members (car tree)) (rec (car tree)))))
          (if (and ,traverse-if (not (funcall ,traverse-if tree)))
        (main (cdr tree))
        (tree-along (rec (cdr tree))))))
    (values)))
     (tree-down (:members ,tree) (rec ,tree)))))))

;FIXME - consider building this as class instead of macro
(defmacro tree-tracker (&body body)
  `(let ((*tree-index* nil)
   (*tree-lengths* nil))
     (declare (special *tree-index* *tree-lengths*))
     (labels
   ((%incf-ind ()
      (incf (car (last *tree-index*))))
    (%decf-ind ()
      (decf (car (last *tree-index*))))
    (%trim-ind ()
      (setf *tree-index* (butlast *tree-index*))
      (let ((tindlen (length *tree-index*)))
        (when (< tindlen (length *tree-lengths*))
    (setf *tree-lengths*
          (subseq *tree-lengths* 0 (1+ tindlen))))))
    (%grow-ind ()
      (setf *tree-index* (append *tree-index* '(0))))
    (%store-length (len)
      (setf *tree-lengths*
      (list-set-place *tree-lengths*
          (1- (length *tree-index*)) len nil))))
       (macrolet
     ((tree-along (&body code)
      `(prog2
           (%incf-ind)
           (progn
             ,@code)
         (%decf-ind)))
      (tree-down ((&key members length) &body code)
        (once-only (members length)
    `(prog2
         (progn
           (%grow-ind)
           (awhen (or ,length (and (listp ,members) (length ,members)))
       (%store-length it)))
         (progn
           ,@code)
       (%trim-ind)))))
      ,@body))))

;turns tree-tracker index into a composed car/cdr accessor func.
(defun index->accessor (ilist)
  (if (null ilist)
      (lambda (x) x)
      (apply #'compose
       (nreverse
  (butlast
   (collecting
     (dolist (num ilist)
       (dotimes (i num)
         (collect #'cdr))
       (collect #'car))))))))

(defun %setup-hash-table (data test)
  (if (listp data)
      (aand (make-hash-table :test test)
      (progn
        (dolist (x data)
    (setf (gethash x it) nil))
        it))
      data))

(defmacro collecting-set ((&key union intersection difference (returns 'list)
        (test '(function eql)))
        &body body)
  (with-gensyms (data intersection-d difference-d x)
    `(,(case returns
       (list 'alexandria:hash-table-keys)
       (hash-table 'identity)
       (otherwise (error "Return type not found")))
      (let ((,data (make-hash-table :test ,test))
      ,@(when intersection
        `((,intersection-d
           (%setup-hash-table ,intersection ,test))))
      ,@(when difference
        `((,difference-d
           (%setup-hash-table ,difference ,test)))))
  (labels
      ((collect (item)
         ,@(when intersection
           `((unless (nth-value 1 (gethash item ,intersection-d))
         (return-from collect))))
         ,@(when difference
           `((when (nth-value 1 (gethash item ,difference-d))
         (return-from collect))))
         (setf (gethash item ,data) nil)))
    (dolist (,x ,union)
      (collect ,x))
    ,@body
    ,data)))))

(defmacro collecting-string (&body body)
  `(apply #'strcat
         (collecting ,@body)))

(defun map-tuples (&rest funcs-and-input/inputs)
  "Like mapcar, except that multiple functions are permitted, their output - per input element - being gathered as by list*. Map-tuples can be viewed as a combination of mapcar and pairlis. All parameters are presumed to be functions except the last, which is input:
   (map-tuples func1 func2... input1)
To use multiple input lists (like mapcar) insert the keyword :input between functions and inputs:
   (map-tuples func1 func2... :input input1 input2...)"
  (multiple-value-bind (funcs inputs)
      (multiple-value-bind (part1 part2)
    (divide-list funcs-and-input/inputs (curry #'eq :input))
  (if part2
      (values part1 (cdr part2))
      (values (butlast part1) (last part1))))
    (apply #'mapcar
     (lambda (&rest items)
       (apply #'list*
        (mapcar (lambda (func) (apply func items)) funcs)))
     inputs)))

(defun maplist/step (func step list &rest more-lists)
  (labels ((proc (result lists)
             (multiple-value-bind (curr rest)
                 (with-collectors (curr< rest<)
                   (dolist (list lists)
                     (multiple-value-bind (curr rest)
                         (divide-on-index list step :fail t)
                       (unless curr
                         (return-from proc (nreverse result)))
                       (curr< curr)
                       (rest< rest))))
               (proc (cons (apply func curr) result) rest))))
    (proc nil (cons list more-lists))))

(defun map-by-2 (func list)
  (maplist/step (lambda (x)
                  (funcall func (car x) (second x)))
                2 list))

(defmacro quotef (setf-spec)
  `(setf ,setf-spec `(quote ,,setf-spec)))

(defun use-package-with-shadowing (package &optional (target-package *package*))
  (let ((package (find-package package))
  (target-package (find-package target-package)))
    (when (eq package target-package)
      (error "Can't import package into itself"))
    (if (member package (package-use-list target-package))
  (print "Package already used")
  (progn
    (do-external-symbols (sym package)
      (when (find-symbol (mkstr sym) target-package)
        (shadowing-import sym target-package)))
    (use-package package target-package)))))

(defmacro with-file-lock ((path &key interval) &body body)
  "Get an exclusive lock on a file. If lock cannot be obtained, keep
trying after waiting a while"
  (let ((lock-path (gensym))
  (lock-file (gensym)))
    `(let ((,lock-path (format nil "~a.lock" (namestring ,path))))
       (unwind-protect
      (progn
        (loop
     :for ,lock-file = (open ,lock-path :direction :output
           :if-exists nil
           :if-does-not-exist :create)
     :until ,lock-file
     :do (sleep ,(or interval 0.1))
     :finally (close ,lock-file))
        ,@body)
   (ignore-errors
     (delete-file ,lock-path))))))

(defun encode-time-delta (second minute hour day)
  (+ second (* 60 minute) (* 3600 hour) (* 43200 day)))

(defmacro return-on-true (clause &optional from-target)
  "Executes return/return-from on the result of clause if it is true"
  (with-gensyms (value)
    `(let ((,value ,clause))
       (when ,value
         ,(if from-target
              `(return-from ,from-target ,value)
              `(return ,value))))))

(defun cat (&rest items)
  (apply #'concatenate 'list items))

(defun extend-pathname (path &rest extensions)
  (let ((exts 
          (apply #'concatenate 'list
           (mapcar (lambda (x)
                     (if (stringp x)
                         (list x)
                         (let ((pd (pathname-directory x)))
                           (unless (eq (car pd) :relative)
                             (error "Extension must not be an absolute path"))
                           (cdr pd))))
                   extensions))))
    (make-pathname :defaults path
                   :directory (append (pathname-directory path) exts))))
