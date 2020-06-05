;;; dubious.lisp

(in-package #:gadgets)

;;; Bad gadget dumping ground

;;; For items that are unfinished, poorly thought out, redundant or otherwise
;;; not currently fit for public consumption.

;;; Use alexandria:make-keyword
(defun keywordize (entity)
  (symbolize entity :package 'keyword))

(defun keywordize-foreign (entity)
  (keywordize (string-upcase (to-lisp-case entity))))

;;; use gadgets:not-empty
;;;FIXME: Inefficient: don't need to know whole length of string
(defun string-true (string)
  (when (< 0 (length string))
    string))

(defun autoquote (itm)
  (if (symbolp itm)
      (list 'quote itm)
      itm))

;;; *-in-macro used heavily, but probably wrong way to do things.
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

(defun get-function-name-in-macro (item)
  (and (listp item) (eq 'function (car item)) (second item)))

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


(defun in-plist? (key plist)
  (< 1 (length (member key plist))))

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


(defun tree-level (tree level)
  (if (= 0 level)
      (list tree)
      (loop for itm in tree
         unless (atom itm)
         append (tree-level itm (1- level)))))


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
                 (divide-on-true
                  (lambda (x) (or (keywordp x)
                                  (and (consp x) (keywordp (car x)))))
                  (cdr data))
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

(defun aslist (itm)
  (if (listp itm)
      itm
      (list itm)))


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

;;Use with-output-to-string
(defmacro collecting-string (&body body)
  `(apply #'strcat
          (collecting ,@body)))

;;1. Too specific, never used
;;2. Collecting-hash-table might cover this
(defun merge-plists (&rest plists)
  (let ((result (copy-list (first plists))))
    (dolist (plist (rest plists))
      (loop for (key value) on plist by #'cddr
         do (setf (getf result key) value)))
    result))

;;multiple-value-* are early macros, not thought through

(defmacro multiple-value-passthru (vars value-form &body body)
  `(multiple-value-bind ,vars
       ,value-form
     (values ,@body)))

(defmacro multiple-value-apply (function values-form)
  `(apply ,function (multiple-value-list ,values-form)))

(defmacro multiple-valplex (values-form &body form-with-Vn-vars)
  (let* ((maxnum
          (loop for itm in (flatten form-with-Vn-vars)
                maximizing (alexandria:if-let
                               ((res (ignore-errors
                                      (parse-integer
                                       (subseq (mkstr itm) 1)))))
                             res
                             0)))
         (symblist
          (loop for i from 0 upto maxnum
             collect (symb 'v i))))
    `(multiple-value-bind ,symblist ,values-form
       (declare (ignorable ,@symblist))
       ,@form-with-Vn-vars)))

