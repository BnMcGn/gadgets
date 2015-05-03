;;;; gadgets.lisp

(in-package #:gadgets)

;;; "gadgets" goes here. Hacks and glory await!

(defun symbolize (entity &key (package *package*))
  (if (symbolp entity)
      (intern (mkstr entity) package)
      (intern (string-upcase entity) package)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun autoquote (itm)
  (if (symbolp itm)
      (list 'quote itm)
      itm))

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

(defmacro assoc-cdr (key &rest alist)
  `(cdr (assoc ,key ,@alist)))

(defun assoc-all (item alist)
  "Gets all items associated with a key, not just the first. Returns a list"
  (loop for x in alist
       when (eql (car x) item)
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
      (cl-utilities:collect (cons (symbolize (car var) :package package) 
		     (cdr var))))))

(defun alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
       collect (if (symbolp (car i))
		   (intern (symbol-name (car i)) keyword-package)
		   (intern (string-upcase (car i)) keyword-package))
       collect (cdr i))))

(defun plist->alist (plist)
  (loop for (k v) on plist by #'cddr
	  collect (cons (intern (symbol-name k)) v)))

(defun alist->hash (al &optional (hash (make-hash-table)))
	   (dolist (x (reverse al))
	     (setf (gethash (car x) hash) (cdr x)))
	   hash)

(defun hash->alist (hsh)
  (cl-utilities:collecting
    (maphash 
     (lambda (k v)
       (cl-utilities:collect (cons k v)))
     hsh)))

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
  (cl-utilities::%with-collectors-check-collectors collectors)
  (let ((gensyms-alist 
	 (cl-utilities::%with-collectors-gensyms-alist collectors)))
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
    (loop for func in (reverse functions)
	 for data = whatever then (apply func data)
	 do (print data)
	 finally (return data))))

(defun apply-compose (&rest functions)
  (lambda (&rest whatever)
    (labels ((func (data funcs)
		  (if funcs
		      (apply (car funcs) (func data (cdr funcs)))
		      whatever)))
      (func whatever functions))))

(defun fetch-keyword (key alist &key (parameter t) (in-list t))
  "Find if a key is in a list. If parameter is true, return the next item 
  after it. if checklist is true, test the first element of any sublists for the   key and if found return rest of list as parameter."
  (let ((keytest (if in-list 
		      (lambda (x y)
			(or (eql x y)
			    (and (consp y) (eql x (car y)))))
		      #'eql)))
    (aif (loop for x on alist
       do (when (funcall keytest key (car x))
	    (return (list t (if (atom (car x)) (second x) (cdar x))))))
	 (if parameter (values-list it) t)
	 nil)))

;removes found keywords from list, returning cleaned list as second val
(defun extract-keywords (keywords alist &optional stack)
  (if keywords
      (let ((pos (position (car keywords) alist :test #'eq-symb)))
	(extract-keywords 
	   (cdr keywords)
	   (if pos
	       (xsubseq alist pos (1+ pos) :type 'list)
	       alist)
	   (if pos
	       (acons (car keywords) (elt alist (1+ pos)) stack)
	       stack)))
      (values stack alist)))

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


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;;maybe fix up...
;;maybe the macro should just rewrite the code...
'(defmacro! with-shadow ((fname fun) &body body)
  "shadow the function named fname with fun; any call to fname within body 
   will use fun, instead of the default function for fname"
  `(let ((,g!res)
         (fun-orig))
     (cond ((fboundp ',fname) 
            ;if there is already a function with that name defined, shadow it
            (setf fun-orig (symbol-function ',fname))
            (setf (symbol-function ',fname) ,fun)
            (setf ,g!res (progn ,@body))
            (setf (symbol-function ',fname) fun-orig)
            (values))
           (t 
             ;otherwise, define a new function with that name and then 
             ;undo the operation afterwards by unbinding that function
             (setf fun-orig #'identity)
             (setf (symbol-function ',fname) ,fun)
             (setf ,g!res (progn ,@body))
             (fmakunbound ',fname)
             (values)))
     ,g!res))

(defun last-car (list)
  (car (last list)))

(defun trycar-compiler (spec)
  (let ((crfunc (case (car spec)
		(#\A #'car)
		(#\D #'cdr))))
    (if (null (cdr spec))
	(lambda (x)
	  (if (consp x)
	      (values (funcall crfunc x) t)
	      (values nil nil)))
	(let ((cmpfunc (trycar-compiler (cdr spec))))
	  (lambda (x)
	    (if (consp x)
		(funcall cmpfunc (funcall crfunc x))
		(values nil nil)))))))

(defmacro trycar (carspec val)
  `(funcall 
    (trycar-compiler
     ',(nreverse (coerce (string-trim "CR" (symbol-name carspec)) 'list)))
    ,val))
       
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

(defun eq-symb (a b)
  (or (eq a b) (equal (mkstr a) (mkstr b))))

(defun eq-symb-upcase (a b)
  (or (eq-symb a b) 
      (equal (string-upcase (mkstr a)) (string-upcase (mkstr b)))))

(defun tree-level (tree level)
  (if (= 0 level)
      (list tree)
      (loop for itm in tree
	   unless (atom itm)
	   append (tree-level itm (1- level)))))

(defun divide-on-index (seq ind)
  "Return seq divided into two subsequences at index. If seq is shorter than ind, return seq in the first part and nil in the second."
  (let (stor
	(src seq))
    (dotimes (i ind)
      (unless src
	(return))
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
  (let ((stream (gensym)))
  `(cond 
     ((pathnamep ,stream-or-path)
      (with-open-file (,stream ,stream-or-path)
	(loop 
	   for ,line = (read-line ,stream nil 'eof)
	   until (eq ,line 'eof)
	   do (progn ,@body))))
     ((streamp ,stream-or-path)
      (loop
	   for ,line = (read-line ,stream-or-path nil 'eof)
	   until (eq ,line 'eof)
	   do (progn ,@body)))
     (t (error "Not a stream or path!")))))

(defmacro do-list-with-rest ((head tail source) &body body)
  (once-only (source)
    `(let ((,head nil))
       (loop for ,tail on ,source
	  do (prog1
		 ,@body
	       (push (car ,tail) ,head))))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

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

(defmacro pif (test then &optional else)
  (with-gensyms (tres)
    `(let ((,tres ,test))
       (format t "Print-IF expression: ~a  RESULT: ~a~%" (quote ,test) ,tres)
       (if ,test ,then ,else))))

(defmacro print-lambda ((&rest args) &body body)
  `(lambda (,@args) 
     (format t "Print-lambda Input:")
     (print (list ,@(remove-if (lambda (x)
				 (eq (elt (symbol-name x) 0) #\&)) args)))
     (format t "~%Print-lambda Output:")
     (print ,@body)))

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

(defun %hash-collecting-modes (mode)
  (case mode
    (:replace (list
	       (lambda (e n) (declare (ignore e)) n)
	       (lambda (v) v)))
    (:keep (list
	    (lambda (e n) (declare (ignore n)) e)
	    (lambda (v) v)))
    (:tally (list
	     (lambda (e n) (declare (ignore n)) (1+ e))
	     (lambda (v) (declare (ignore v)) 1)))
    (:sum (list
	   (lambda (e n) (+ e n))
	   (lambda (v) v)))
    (:append (list
	      (lambda (e n) (nconc e (list n)))
	      (lambda (v) (list v))))
    (:push (list
	    (lambda (e n) (cons n e))
	    (lambda (v) (list v))))
    (otherwise (error "Mode not found"))))

(defmacro collecting-hash-table ((&key (test '(function eql) test-set-p) 
				       existing (mode :append)) &body body)
  (and test-set-p existing (error "Can't set test when reusing an existing hash table"))
  (with-gensyms (fill init stor)
    `(let ((,stor (aif ,existing it (make-hash-table :test ,test))))
       (destructuring-bind (,fill ,init) (%hash-collecting-modes ,mode)
	 (labels
	     ((collect (key value &key mode)
	       (destructuring-bind (fill init)
		   (if mode (%hash-collecting-modes mode) (list ,fill ,init))
		 (multiple-value-bind (itm exists) (gethash key ,stor)
		   (if exists 
		       (setf (gethash key ,stor) (funcall fill itm value))
		       (setf (gethash key ,stor) (funcall init value)))))))
	   ,@body
	   ,stor)))))
	       
