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

(defmacro ret (var val &body body)
  "A single variable let that returns the variable when the body completes.

(ret x 3 (incf x)) => 4"
  `(let ((,var ,val))
     ,@body
     ,var))

(defmacro def-as-func (var func-form)
  "Set a variable in the function namespace."
  `(setf (symbol-function ',var) ,func-form))

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

(defun extract-keywords (keywords alist &key in-list (test #'string-equal*))
  "Traverses a plist or lambda list, removing the specified keywords and the
value that immediately follows each. Found key/value pairs are returned as a
plist in the first value. The cleaned list is returned as the second value.

This, or the related macro bind-extracted-keywords, is particularly useful for adding features to macros. It will strip out added keywords from parameter lists, allowing the remainder to be passed to the original macro processing code."
  (cl-utilities:with-collectors (keypairs< rest<)
    (let ((currkey nil))
      (dolist (itm alist)
        (cond
          (currkey
           (keypairs< (cons currkey itm))
           (setf currkey nil))
          ((alexandria:when-let
               ((key (find itm keywords :test test)))
             (setf currkey key)
             t))
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
       (let ,(cl-utilities:collecting
              (dolist (k keys)
                (if (listp k)
                    (cl-utilities:collect
                        (list (symb (car k))
                                   (if (member :multiple k)
                                       `(assoc-all ,(car k) ,extracts)
                                       `(and (assoc ,(car k) ,extracts)
                                             (cdr (assoc ,(car k) ,extracts))))))
                    (cl-utilities:collect (list (symb k)
                                   `(and (assoc ,k ,extracts)
                                         (cdr (assoc ,k ,extracts))))))))
         ,@body))))

(defmacro quotef (setf-spec)
  `(setf ,setf-spec `(quote ,,setf-spec)))

(defun quoted-p (item)
  "Macro utility to test if an item has been quoted by the macro user."
  (and (consp item) (eq 'quote (car item))))

(defun quoted-symbol-p (item)
  "Macro utility to test if an item has been passed into a macro as a quoted symbol. If so, returns the symbol without the quote."
  (and (quoted-p item) (symbolp (second item)) (second item)))

(defun quoted-list-p (item)
  "Utility to test if a macro parameter is a quoted list. Returns the list if it is."
  (and (quoted-p item) (listp (second item)) (second item)))

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

;;;
;;; String
;;;

(defun capitalize-first (item)
  "Returns a string representation of item with the first letter capitalized
and the remaining characters lower-case, where applicable. Item can be a
string or a symbol"
  (format nil "~@(~A~)" (mkstr item)))

(defun to-lowercase (item)
  (format nil "~(~a~)" (mkstr item)))

(defun to-uppercase (item)
  (format nil "~:@(~a~)" (mkstr item)))

(defun string-unless-number (x)
  "Return the input as a string unless it can be recognized as an integer."
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

(defun boolify (item)
  "Attempts to guess when a string or number should be interpreted as T. Postive integers and strings like \"true\" and \"yes\" will be interpreted as true. Non-empty lists and sequences are true. Most other things are NIL"
  (typecase item
    (string
     (if (member (string-trim *whitespace-characters* item)
                 '("1" "true" "yes" "t" "y") :test #'string-equal-caseless)
         t nil))
    (integer
     (if (< item 1) nil t))
    (list
     (and (not-empty item) t))
    (sequence
     (and (not-empty item) t))
    (t t)
    (otherwise nil)))

(defun not-empty (itm)
  "A predicate to detect 0 length sequences."
  (and itm (< 0 (length itm)) itm)) ;Return item if it isn't empty.

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
  "Does the sequence end with the test sequence?"
  (sequences-start-same
   (subseq seq (max 0 (- (length seq) (length testseq))))
   testseq))

(defun string-join (filler strings)
  (format nil (format nil "~~{~~a~~^~a~~}" filler) strings))

(defparameter *whitespace-characters*
  '(#\Space #\Newline #\Backspace #\Tab
    #\Linefeed #\Page #\Return #\Rubout))

(defun string-equal* (a b)
  "Broad version of string-equal. Will take input that is not a string or symbol."
  (and
   (or (stringp a) (symbolp a)) (or (stringp b) (symbolp b))
   (string-equal a b)))

(defun string-equal-caseless (a b)
  "Are two strings equal when case is ignored?"
  (string-equal (string-upcase a) (string-upcase b)))

(defun string-equal-case (a b)
  "A case sensitive version of string-equal."
  (or (eq a b) (equal (mkstr a) (mkstr b))))

(defun string-equal-multiple (a b)
  "For things that send multiple items with \"[]\" appended to the var name, a convention started by the PHP people. Mostly useful for web programming."
  (or (string-equal a b)
      (and (= (length (mkstr a)) (+ 2 (length (mkstr b))))
           (string-equal a (symb b '[])))
      (and (= (+ 2 (length (mkstr a))) (length (mkstr b)))
           (string-equal (symb a '[]) b))))

(defun truncate-string (str &key (length 20) (indicator "..."))
  (let ((ln (length str)))
    (if (> ln length)
        (concatenate 'string
                     (subseq str 0 (- length (length indicator)))
                     indicator)
        str)))

;;;
;;; Keyed collection
;;;

(defun assoc-cdr (&rest keys-and-alists)
  "A shortcut for (cdr (assoc ...)) to give immediate access to an alist
value."
  (cdr (apply #'assoc keys-and-alists)))

(defun assoc-all (item alist &key (test #'eql))
  "Gets all items associated with a key, not just the first. Returns a list

(assoc-all :a '((:a . 1) (:b . 2) (:c . 3) (:a . 4)))
=> (1 4)"
  (loop for x in alist
     when (funcall test (car x) item)
     collect (cdr x)))

(defun assoc-or (keys alist)
  "Finds the first key in keys that has a match in alist. Will use equal to match
strings."
  (when keys
    (alexandria:if-let ((res (assoc (car keys) alist
                                    :test (if (stringp (car keys))
                                              #'equal
                                              #'eql))))
      res
      (assoc-or (cdr keys) alist))))

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
  (hu:collecting-hash-table (:test test :mode mode)
    (maphash (lambda (k v) (hu:collect v k)) hash)))

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

(defun %set-up-hash-table (data test)
  (if (listp data)
      (let ((stor (make-hash-table :test test)))
        (dolist (x data)
          (setf (gethash x stor) nil))
        stor)
      data))


;;;
;;; List and sequence
;;;

(defun xsubseq (sequence start end &key (type 'sequence))
  "Returns sequence with start->end chopped out of it"
  (concatenate type
               (subseq sequence 0 start)
               (subseq sequence (1+ end))))

(defun sequence->list (seq)
  (loop for x across seq
     collect x))

(defun last-car (list)
  (car (last list)))

(defun chunk (n alist)
  (if (> (list-length alist) n)
      (cons (subseq alist 0 n) (chunk n (nthcdr n alist)))
      (list alist)))

(defun flatten-1 (alist)
  "Flattens conses found in the top level of a list. Nils in the top level will be removed.

  (flatten-1 '((1 2 3) nil (nil) ((4 5) (6 7))))
  (1 2 3 NIL (4 5) (6 7)) "
  (cl-utilities:collecting
   (dolist (x alist)
     (if (atom x)
         (when x
           (cl-utilities:collect x))
         (mapc-improper #'cl-utilities:collect x)))))

(defun flatten-when (predicate items &key descend-all)
  "Recursively flattens any conses found in items if the predicate returns true on them. Will not flatten NILs unless the predicate indicates it. The predicate will not be called on non-cons items. Flatten-when will not normally descend into lists which it will not flatten, passing unchanged any list or cons item that fails the predicate. To cause it to descend into non-matching portions of the tree, set the :descend-all keyword."
  (cl-utilities:collecting
    (labels ((proc (items)
               (mapc-improper
                (lambda (itm)
                  (if (consp itm)
                      (if (funcall predicate itm)
                          (proc itm)
                          (if descend-all
                              (cl-utilities:collect
                                  (flatten-when
                                   predicate itm
                                   :descend-all descend-all))
                              (cl-utilities:collect itm)))
                      (cl-utilities:collect itm)))
                items)))
      (proc items))))

(defun flatten-1-when (predicate items)
  "Returns a list with any conses in it flattened if predicate returns true when called with that item. Will not flatten NILs unless the predicate indicates it. The predicate will not be called on non-cons items."
  (cl-utilities:collecting
    (dolist (itm items)
      (if (and (consp itm) (funcall predicate itm))
          (mapc-improper #'cl-utilities:collect itm)
          (cl-utilities:collect itm)))))

(defun part-on-index (list/seq index &key fail)
  "Divides a list into two parts at the specified index. The two parts are returned as values. If the index is too large for the sequence, part-on-index will silently return the sequence as the first value. Set the :fail keyword T to raise an error instead."
  (let ((i index))
    (part-after-true
     (lambda (x)
       (declare (ignore x))
       (when (= 0 (decf i))
         t))
     list/seq :fail fail)))

(defgeneric part-on-true (test list/seq &key fail)
  (:documentation
   "Divides a list or sequence into two parts, with the second part starting with the first item to cause test to return true. The two parts of the sequence are returned as values. If a dividing point is not found, part-on-true will return the whole sequence as the first value. If you wish it to raise an error instead, set the :fail parameter to true."))

(defmethod part-on-true ((test function) (list/seq sequence) &key fail)
  (loop
     for itm across list/seq
     for i from 0
     do (when (funcall test itm)
          (return
            (values (subseq list/seq 0 i)
                    (subseq list/seq i))))
     finally (if fail
                 (error "No dividing point found in sequence")
                 (subseq list/seq i))))

(defmethod part-on-true ((test function) (list/seq list) &key fail)
  (labels ((proc (accum alist)
             (if alist
                 (if (funcall test (car alist))
                     (values (nreverse accum) alist)
                     (proc (cons (car alist) accum)
                           (cdr alist)))
                 (if fail
                     (error "No dividing point found in list")
                     (nreverse accum)))))
    (proc nil list/seq)))

(defgeneric part-after-true (test list/seq &key fail)
  (:documentation
   "Like part-on-true, but includes the first matching item in the first list."))

(defmethod part-after-true ((test function) (list/seq sequence) &key fail)
  (loop
     for itm across list/seq
     for i from 0
     do (when (funcall test itm)
          (return
            (values (subseq list/seq 0 (1+ i))
                    (subseq list/seq (1+ i)))))
     finally (if fail
                 (error "No dividing point found in sequence")
                 (subseq list/seq i))))

(defmethod part-after-true ((test function) (list/seq list) &key fail)
  (labels ((proc (accum alist)
             (if alist
                 (if (funcall test (car alist))
                     (values (nreverse (cons (car alist) accum))
                             (cdr alist))
                     (proc (cons (car alist) accum)
                           (cdr alist)))
                 (if fail
                     (error "No dividing point found in list")
                     (nreverse accum)))))
    (proc nil list/seq)))

(defun remove-if-member (seq things &key key (test #'eq))
  (let ((keyfunc (or key (lambda (x) x))))
    (remove-if #'(lambda (x)
                   (member (funcall keyfunc x) things :test test)) seq)))

(defun splitfilter (predicate alist)
  (cl-utilities:with-collectors (in< out<)
    (dolist (elmt alist)
      (if (funcall predicate elmt)
          (in< elmt)
          (out< elmt)))))

(defun split-sequence-on-subseq (subseq/s sequence)
  (let ((searches
         (hu:alist->hash
          (mapcar (lambda (x) (cons (if (characterp x) x (elt x 0)) x))
                  (alexandria:ensure-list subseq/s))
          :mode :append)))
    (dotimes (i (length sequence))
      (dolist (poss (gethash (elt sequence i) searches))
        (when (sequence-starts-with (subseq sequence i) poss)
          (return-from split-sequence-on-subseq
            (values (subseq sequence 0 i) (subseq sequence (+ i (length poss))) poss)))))
    sequence))

(defun first-match (predicate list)
  "See also 'some'"
  (multiple-value-bind (val sig)
      (dolist (x list)
        (when (funcall predicate x)
          (return (values x t))))
    (if sig
        (values val t)
        (values nil nil))))

(defun first-match-index (predicate list)
  "Returns the index of the first item in list that satisfies predicate."
  (loop for itm in list
        for i from 0
        when (funcall predicate itm)
        do (return-from first-match-index i)))

(defun ordered-unique (list &key (test #'eql))
  "Returns a unique list of the items in list in the order in which they first
appear."
  (let* ((max 0)
         (data (hu:collecting-hash-table (:mode :keep :test test)
                 (loop
                    for itm in list
                    for i from 0
                    do (hu:collect itm i)
                    finally (setf max i))))
         (data (invert-hash-table data :test #'eq))
         (res nil))
    (dotimes (i (1+ max))
      (alexandria:when-let ((val (gethash i data)))
        (push val res)))
    (nreverse res)))

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

;;;
;;; Numerical
;;;

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

(defun relative-to-range (start end num)
  "Returns a value indicating where num is positioned relative to start and end. If num lies between start and end, the return value will be between 0.0 and 1.0."
  (/ (- num start) (- end start)))

(defun as-in-range (start end num)
  "Complement of relative-of-range function. Treats num as if it were a fraction of the range specified by start and end. Returns the absolute number that results."
  (+ start (* num (- end start))))

;;;
;;; Execution control
;;;


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


;;Consider ignore-errors
(defmacro tryit (&body body)
  "Execute the code in the body, returning T as the second value if the code executes without error, but returning (NIL NIL) if an exception is thrown. This provides a quick way to turn an error into a boolean value.

WARNING: This isn't always a great idea for production code. Tryit will mask all raised errors, So if your code causes an error aside from the one you expect, you won't be warned of the variance."
  `(handler-case
       (values
        (progn ,@body)
        t)
     (t (e) (declare (ignore e)) (values nil nil))))

(defmacro three-way (test minus-clause zero-clause plus-clause)
  (let ((val (gensym)))
    `(let ((,val ,test))
       (cond ((> 0 ,val) ,minus-clause)
             ((equal 0 ,val) ,zero-clause)
             (t ,plus-clause)))))

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

(defun map-tuples (&rest funcs-and-input/inputs)
  "Like mapcar, except that multiple functions are permitted, their output - per input element - being gathered as by list*. Map-tuples can be viewed as a combination of mapcar and pairlis. All parameters are presumed to be functions except the last, which is input:
   (map-tuples func1 func2... input1)
To use multiple input lists (like mapcar) insert the keyword :input between functions and inputs:
   (map-tuples func1 func2... :input input1 input2...)"
  (multiple-value-bind (funcs inputs)
      (multiple-value-bind (part1 part2)
          (part-on-true (curry #'eq :input) funcs-and-input/inputs)
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
                 (cl-utilities:with-collectors (curr< rest<)
                   (dolist (list lists)
                     (unless list
                       (return-from proc (nreverse result)))
                     (multiple-value-bind (curr rest)
                         (handler-case
                             (part-on-index list step :fail t)
                           (simple-error (e) (declare (ignore e))
                               (error
                                "Length of input lists was not divisible by step.")))
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

(defun map-improper (func list?)
  "Map over a list, proper or not. The return mapping will be a proper list."
  (let ((res nil)
        (curr list?))
    (loop do
         (if (null curr)
             (return-from map-improper (nreverse res))
             (if (atom curr)
                 (progn
                   (push (funcall func curr) res)
                   (setf curr nil))
                 (progn
                   (push (funcall func (car curr)) res)
                   (setf curr (cdr curr))))))))

(defun mapc-improper (func list?)
  "Mapc over a list, proper or not. Original list is returned. Like mapc, mapc-improper is used for side effects only."
  (let ((curr list?))
    (loop do
         (if (null curr)
             (return-from mapc-improper list?)
             (if (atom curr)
                 (progn
                   (funcall func curr)
                   (setf curr nil))
                 (progn
                   (funcall func (car curr))
                   (setf curr (cdr curr))))))))

(defmacro return-on-true (clause &optional from-target)
  "Executes return/return-from on the result of clause if it is true"
  (with-gensyms (value)
    `(let ((,value ,clause))
       (when ,value
         ,(if from-target
              `(return-from ,from-target ,value)
              `(return ,value))))))

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

;;;
;;; Tree
;;;

(defun part-tree (test tree)
  "Divides the s-expression supplied in tree into an inner and an outer portion. The outer portion is returned in the first value as a closure. The inner portion is returned as the second value. The inner portion consists of the first part of the tree that passes test. The tree is traversed breadth-first.

> (part-tree
    (lambda (x) (eq 'deepest (car (ensure-list x))))
    '(deep (deeper (deeperer (deepest (deepester you-are-here))))))
 #<CLOSURE (LAMBDA (GADGETS::X) :IN GADGETS:PART-TREE) {C19C81D}>
 (DEEPEST (DEEPESTER YOU-ARE-HERE))

> (funcall * :xyz)
 (DEEP (DEEPER (DEEPERER :XYZ)))

The returned closure should be called with a single argument. It will return the outer portion with the supplied argument in place of the inner portion."
  (if (funcall test tree)
      (values (lambda (x) x) tree)
      (if (listp tree)
          (dotimes (i (length tree) (values nil tree))
            (let ((res (multiple-value-list
                        (part-tree test (nth i tree)))))
              (when (functionp (car res))
                (return-from part-tree
                  (values
                   (lambda (x)
                     (append (subseq tree 0 i)
                             (list (funcall (car res) x))
                             (subseq tree (1+ i) (length tree))))
                   (second res))))))
          (values nil tree))))

;;;
;;; File and OS
;;;

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
  (cl-utilities:collecting
   (do-file-by-line (line stream-or-path)
     (cl-utilities:collect (funcall function line)))))

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

(defun make-clock (ticks-per-second)
  (unless (< ticks-per-second internal-time-units-per-second)
    (error "Internal clock will not support that many ticks per second"))
  (let ((units (floor internal-time-units-per-second ticks-per-second))
        (start (get-internal-real-time)))
    (lambda ()
      (floor (- (get-internal-real-time) start) units))))

(defmacro defclock (name ticks-per-second)
  "Defines a function that will return an integer number of ticks since it started."
  `(eval-always
     (def-as-func ,name (make-clock ,ticks-per-second))))

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

(defun homedir-relative-pathname (name)
  ;;FIXME: Why is internal symbol necessary here?
  (merge-pathnames name (uiop::user-homedir-pathname)))

(defun call-with-temporary-directory (thunk &key (want-pathname-p t))
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
    (lambda (,@(when pathname
                 (unless (symbolp pathname)
                   (error "Pathname must be a symbol or NIL"))
                 (list pathname))))
      ,@body)
    :want-pathname-p pathname)

;;;
;;; Debugging printers
;;;

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
       ,@(cl-utilities:collecting
          (dolist (cl clauses)
            (incf count)
            (cl-utilities:collect `(,(car cl)
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
      ,@(cl-utilities:collecting
         (dolist (f forms)
           (incf count)
           (cl-utilities:collect
               `(let ((,itm (multiple-value-list ,f)))
                  (progn (if ,itm
                             (format t "~&Print-and: Clause ~a: values:~a~%"
                                     ,count ,itm)
                             (format t "~&Print-and: FAILED at ~a~%" ,count))
                         (apply #'values ,itm)))))))))

(defmacro print-all-values (expr)
  "Like print, but prints - and passes on - all values received. Useful for debugging expressions that return multiple values."
  `(let ((res (multiple-value-list ,expr)))
     (mapc #'print res)
     (apply #'values res)))

(defparameter *dump-stor* nil)

(defun dump (thing)
  (setf *dump-stor* thing))

(defun dive ()
  *dump-stor*)



