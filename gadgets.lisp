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

(defun capitalize-first (item)
  "Returns a string representation of item with the first letter capitalized
and the remaining characters lower-case, where applicable. Item can be a
string or a symbol"
  (format nil "~@(~A~)" (mkstr item)))

(defun string-unless-number (x)
  "Return the input as a string unless it can be parsed into a number."
  (if (numberp x)
      x
      (handler-case
          (parse-integer x)
        #+:sbcl (sb-int:simple-parse-error () x)
        #+:ccl (ccl::parse-integer-not-integer-string () x))))

(defun symbol-unless-number (x)
  "Convert the input string into a symbol unless it can be converted into a
number."
  (let ((val (string-unless-number x)))
    (if (numberp val)
        val
        (symbolize val))))

(defun not-empty (itm)
  "A predicate to detect 0 length sequences."
  (and itm (< 0 (length itm))))

(defmacro ret (var val &body body)
  "A single variable let that returns the variable when the body completes.

(ret x 3 (incf x)) => 4"
  `(let ((,var ,val))
     ,@body
     ,var))

(defmacro def-as-func (var func-form)
  "Set a variable in the function namespace."
  `(setf (symbol-function ',var) ,func-form))

(defun sequences-start-same (seq seq2)
  "Given two sequences, are they the same until one runs out? This function
does not care which sequence contains the other. Use sequence-starts-with if
you need something more specific."
  (loop for x across seq2
     for y across seq
     do (when (not (eq x y))
          (return nil))
     finally (return t)))

(defun sequence-starts-with (seq testseq)
  "Does the sequence begin with the test sequence?"
  (let ((slen (length testseq)))
    (if (< (length seq) slen)
        nil
        (dotimes (i slen t)
          (unless (eq (elt seq i) (elt testseq i))
            (return nil))))))

(defun sequence-ends-with (seq testseq)
  (sequences-start-same
   (subseq seq (max 0 (- (length seq) (length testseq))))
   testseq))

(defun assoc-cdr (&rest params)
  "A shortcut for (cdr (assoc ...)) to give immediate access to an alist
value."
  (cdr (apply #'assoc params)))

(defun assoc-all (item alist &key (test #'eql))
  "Gets all items associated with a key, not just the first. Returns a list

(assoc-all :a '((:a . 1) (:b . 2) (:c . 3) (:a . 4)))
=> (1 4)"
  (loop for x in alist
     when (funcall test (car x) item)
     collect (cdr x)))

(defun alist-p (item)
  "Determine if an item appears to be an assoc list"
  (cond
    ((null item) t)
    ((and (listp item)
          (every #'consp item)) t)
    (t nil)))

(defun plist-p (item)
  "Determine if an item qualifies as a plist"
  (cond
    ((null item) t)
    ((and (listp item)
          (evenp (length item))
          (not (alist-p item))) t)
    (t nil)))

(defun invert-hash-table (hash &key (test #'eql) (mode :replace))
  "Returns a new hash table with keys and values swapped:
(:a 1 :b 3 :c 5) => (1 :a 3 :b 5 :c)

The hash table test can be set with :test. The method of value collection can
be controlled with :mode. Modes are those available for
cl-hash-util:collecting-hash-table."
  (collecting-hash-table (:test test :mode mode)
    (maphash (lambda (k v) (collect v k)) hash)))

(defun rekey (store mapping &key ignore-missing (test #'eql))
  (let
      ((mapping (cond
                  ((hash-table-p mapping) mapping)
                  ((alist-p mapping) (alist->hash mapping))
                  ((plist-p mapping) (plist->hash mapping))
                  (t (error "Valid mapping not found!"))))
       (results (make-hash-table :test test)))
    (funcall
     (cond ((hash-table-p store) #'maphash)
           ((alist-p store) #'map-assoc)
           ((plist-p store) #'map-by-2)
           (t (error "Store is not mappable!")))
     (if ignore-missing
         (lambda (k v)
           (when (key-in-hash? k mapping)
             (setf (gethash (gethash k mapping) results) v)))
         (lambda (k v)
           (if (key-in-hash? k mapping)
               (setf (gethash (gethash k mapping) results) v)
               (setf (gethash k results) v))))
     store)
    results))

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

(defun hash-table->source (ht)
  "Returns a source code representation of a hash table."
  `(alist->hash ',(hash->alist ht)
                :existing (make-hash-table :test #',(hash-table-test ht))))

(defun xsubseq (sequence start end &key (type 'sequence))
  "Returns sequence with start->end chopped out of it"
  (concatenate type
               (subseq sequence 0 start)
               (subseq sequence (1+ end))))

(defun sequence->list (seq)
  (loop for x across seq
     collect x))

(defmacro or2 (&rest clauses)
  "A version of or that bases its decision on the second value of each clause. Forms that return no second value are considered T."
  (with-gensyms (blok)
    `(block ,blok
       ,@(mapcar
          (lambda (clause)
            `(let ((res (multiple-value-list ,clause)))
               (when
                   (or (> 2 (length res))
                       (elt res 1))
                 (return-from ,blok (car res)))))
          clauses))))

(defun apply-compose (&rest functions)
  (lambda (&rest whatever)
    (labels ((func (data funcs)
               (if funcs
                   (apply (car funcs) (func data (cdr funcs)))
                   whatever)))
      (func whatever functions))))

(defun fetch-keyword (key alist &key (in-list t))
  "Find if a key is in a list, return the next item after it. if checklist
 is true, test the first element of any sublists for the key and if found
return rest of list as parameter. A bit coarser in function than getf. Will
tolerate improper plists."
  (let ((keytest (if in-list
                     (lambda (x y)
                       (or (eql x y)
                           (and (consp y) (eql x (car y)))))
                     #'eql))
        (found nil))
    (values (loop for x on alist
               do (when (funcall keytest key (car x))
                    (setf found t)
                    (return (if (atom (car x)) (second x) (cdar x)))))
            found)))

(defun extract-keywords (keywords alist &key in-list (test #'eq-symb))
  "Traverses a plist or lambda list, removing the specified keywords and the
value that immediately follows each. Found key/value pairs are returned as a
plist in the first value. The cleaned list is returned as the second value.

This, or the related macro bind-extracted-keywords, is particularly useful for adding features to macros. It will strip out added keywords from parameter lists, allowing the remainder to be passed to the original macro processing code."
  (with-collectors (keypairs< rest<)
    (let ((currkey nil))
      (dolist (itm alist)
        (anaphora:acond
         (currkey
          (keypairs< (cons currkey itm))
          (setf currkey nil))
         ((find itm keywords :test test)
          (setf currkey it))
         ((and in-list (consp itm) (find (car itm) keywords :test test))
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

(defun range (start &optional (stop start stop-supplied-p) (step 1))
  "Creates a list containing a sequential range of integers. By default the
range runs from 0 to one below the supplied stop value:
(range 3) -> (0 1 2)
If a second parameter is supplied, the first is treated as a starting value, and
the second as a stop:
(range 7 10) -> (7 8 9)
The third parameter specifies a step size:
(range 0 10 2) -> (0 2 4 6 8)
A negative step parameter causes the range to travel down from the start to the
stop:
(range 10 5) -> (10 9 8 7 6)"
  (unless stop-supplied-p (setf start 0))
  (if (> (abs (- (+ start step) stop)) (abs (- start stop)))
      nil
      (if (minusp step)
          (loop for i from start above stop by (abs step) collect i)
          (loop for i from start below stop by step collect i))))

(defmacro do-window ((var/s source
                            &key (size 2) (step 1)
                            start-padding) &body body)
  "Like dolist, iterates over a list, but instead of binding a single list
item per iteration binds a segment of the list as a sliding window.
  (do-window (x '(1 2 3 4 5 6)) ...)
will execute the body 5 times with x bound respectively to:
  (1 2) (2 3) (3 4) (4 5) (5 6)

The step keyword adjusts how far the window slides per iteration. A destructuring spec can be provided in place of the variable. Therefore do-window
can be used to iterate over a plist like so:
  (do-window ((k v) '(:a 1 :b 2 :c 3) :step 2) ...)
Each key and value will be bound to k and v, respectively.

The size keyword allows adjustment of the window size.

Leading padding may be provided to do-window with the start-padding
keyword."
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
  "Are two strings equal when case is ignored?"
  (string-equal (string-upcase a) (string-upcase b)))

(defun boolify (item)
  (typecase item
    (string
     (if (member (string-trim *whitespace-characters* item)
                 '("1" "true" "yes" "t") :test #'string-equal-caseless)
         t nil))
    (integer
     (if (< item 1) nil t))
    (list
     (not-empty item))
    (sequence
     (not-empty item))
    (t t)
    (otherwise nil)))

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
         (when x
           (collect x))
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

;;For things that send multiple items with "[]" appended to the var name.
(defun eq-symb-multiple (a b)
  (or (eq-symb a b)
      (and (= (length (mkstr a)) (+ 2 (length (mkstr b))))
           (eq-symb a (symb b '[])))
      (and (= (+ 2 (length (mkstr a))) (length (mkstr b)))
           (eq-symb (symb a '[]) b))))

(defun match-a-symbol (item symbols)
  (first-match (lambda (x) (eq-symb x item)) symbols))

(defun match-various (matchables)
  "Returns a function to check if an input string - presumably input from a user - is approximately a member of the matchables list. Matchables can contain symbols, numbers or strings. Match-various will not intern the user input before comparing it to the symbols, preventing mischievous users from stuffing the symbol table."
  (multiple-value-bind (symbols others)
      (splitfilter #'symbolp matchables)
    (lambda (test-string)
      (multiple-value-bind (val sig)
          (match-a-symbol test-string symbols)
        (if sig
            (values val sig)
            (let ((res (member (string-unless-number test-string)
                               others :test #'equal)))
              (if res
                  (values (car res) t)
                  (values nil nil))))))))


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
  list rather than the second."
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

(defun splitfilter (predicate alist)
  (with-collectors (in< out<)
    (dolist (elmt alist)
      (if (funcall predicate elmt)
          (in< elmt)
          (out< elmt)))))

(defun split-sequence-on-subseq (search-seq target-seq)
  (let ((len (length search-seq)))
    (labels ((proc (seq stack)
               (let ((pos (search search-seq seq)))
                 (if pos
                     (proc (subseq seq (+ pos len))
                           (cons (subseq seq 0 pos)
                                 stack))
                     (nreverse (cons seq stack))))))
      (proc target-seq nil))))

(defun first-match (predicate list)
  (multiple-value-bind (val sig)
      (dolist (x list)
        (when (funcall predicate x)
          (return (values x t))))
    (if sig
        (values val t)
        (values nil nil))))

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

;;;Returns tlist (copy) with ind set to val. If ind is beyond the length of tlist,
;;;pad out the list with padding
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

(defmacro print-all-values (expr)
  "Like print, but prints - and passes on - all values received. Useful for debugging expressions that return multiple values."
  `(let ((res (multiple-value-list ,expr)))
     (mapc #'print res)
     (apply #'values res)))

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

(defun mapcan-by-2 (func list)
  (apply #'concatenate 'list (map-by-2 func list)))

(defun map-assoc (func alist)
  (mapcar (lambda (x) (funcall func (car x) (cdr x))) alist))

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

(defmacro with-file-lock ((path &key (interval 0.1)) &body body)
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
                 :do (sleep ,interval)
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
  "A short form of (concatenate 'list ...)"
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

(defun call-with-temporary-directory
    (thunk &key (want-pathname-p t))
  (loop
     with donep = nil
     until donep
     do
       (uiop:with-temporary-file (:pathname tfile :type "lck")
         (let ((dirname
                (uiop:ensure-directory-pathname
                 (make-pathname :defaults tfile :type "dir"))))
           (unless (uiop:directory-exists-p dirname)
             (setf donep t)
             (ensure-directories-exist dirname)
             (unwind-protect
                  (if want-pathname-p
                      (funcall thunk dirname)
                      (uiop:with-current-directory (dirname)
                        (funcall thunk)))
               (uiop:delete-directory-tree
                dirname
                :validate t)))))))

(defmacro with-temporary-directory ((&key pathname) &body body)
  `(call-with-temporary-directory
    (lambda (,@pathname)
      ,@body)))

(defun try-awhile (predicate &key (sleep 0.001) (wait 1.0) on-success on-fail)
  "Will continue to call predicate until either it returns success or a given amount of time elapses. Duration can be set with the :wait keyword. It defaults to 1 second. Try-awhile will sleep between predicate calls unless the :sleep keyword is set to nil. Default sleep is 0.001 of a second.

Try-awhile will return the predicate value on success or nil on failure. If a function is supplied to the :on-success argument, it will be executed if the predicate succeeds and its result will be returned instead of the predicate result. The :on-fail keyword may be used to supply a function that will be run if the time elapses without a predicate success. It's result will be returned instead of the default nil."
  (let ((wait-units (* wait internal-time-units-per-second))
        (start (get-internal-real-time))
        (result nil))
    (loop
       do
         (setf result (funcall predicate))
         (when result
           (return
             (if on-success
                 (funcall on-success)
                 result)))
         (when (< (+ start wait-units) (get-internal-real-time))
           (return
             (if on-fail
                 (funcall on-fail)
                 nil)))
         (when sleep
           (sleep sleep)))))

