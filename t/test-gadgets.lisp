(in-package :cl-user)
(defpackage test-gadgets
  (:use :cl :prove :gadgets))
(in-package :test-gadgets)


(plan 132)

;;symb
(is (symb 'qw 'er) 'qwer)

;;capitalize-first
(is (capitalize-first "asdf") "Asdf")

;;to-lowercase
(is (to-lowercase "AsDF") "asdf")

;;to-uppercase
(is (to-uppercase "aSdf") "ASDF")

;;string-unless-number
(is (string-unless-number "asdf") "asdf")
(is (string-unless-number "1") 1)
(is (string-unless-number "1.0") "1.0")

;;symbol-unless-number
(is (symbol-unless-number "asdf") 'asdf)
(is (symbol-unless-number "5") 5)

;;not-empty
(ok (not-empty "asdf"))
(ok (not (not-empty "")))

;;ret
(is (ret x 3 (incf x)) 4)

;;def-as-func
(def-as-func test-symbol (lambda () 3))
(is (test-symbol) 3)

;;sequences-start-same
(ok (sequences-start-same "asdf" "as"))
(ok (sequences-start-same "as" "asdf"))
(ok (sequences-start-same "as" "as"))
(ok (not (sequences-start-same "as" "AS")))
(ok (not (sequences-start-same "sa" "as")))

;;sequence-starts-with
(ok (sequence-starts-with "asdf" "as"))
(ok (not (sequence-starts-with "as" "asdf")))
(ok (sequence-starts-with "as" "as"))
(ok (not (sequence-starts-with "as" "AS")))
(ok (not (sequence-starts-with "sa" "as")))

;;sequence-ends-with
(ok (sequence-ends-with "asdf" "df"))
(ok (sequence-ends-with "asdf" "asdf"))
(ok (not (sequence-ends-with "asdf" "sd")))
(ok (not (sequence-ends-with "df" "asdf")))

;;assoc-all
(is (assoc-all :a '((:a . 1) (:b . 2) (:c . 3) (:a . 4))) '(1 4))

;;assoc-or
(is (assoc-or '(:e :t :c) '((:a . 1) (:b . 2) (:c . 3) (:a . 4))) '(:c . 3))

;;alist-p
(ok (alist-p nil))
(ok (alist-p '((:a . 1) (:b . 2))))
(ok (not (alist-p '(:a 1 :b 2))))
(ok (not (alist-p 3)))

;;plist-p
(ok (plist-p nil))
(ok (not (plist-p '((:a . 1) (:b . 2)))))
(ok (plist-p '(:a 1 :b 2)))
(ok (not (plist-p 3)))

;;invert-hash-table
(is (sort
     (alexandria:hash-table-keys
      (invert-hash-table (hu:plist->hash '(:a 1 :b 3 :c 5))))
     #'<)
    '(1 3 5))

;;rekey
(is (getf (hu:hash->plist (rekey '(:a 1 :b 2) '(:a :c :b :a))) :a) 2)

;;do-alist
(let ((k nil)
      (v nil))
  (do-alist (key value '((:a . 1) (:b . 2) (:c . 3)))
    (setf k key)
    (setf v value))
  (is k :c)
  (is v 3))

;;do-hash-table
(let ((data (hu:alist->hash '((:a . 1) (:b . 2) (:c . 3)))))
  (let ((v nil))
    (do-hash-table (key value data)
      (when (eq key :c)
        (setf v value)))
    (is v 3))

  ;;key-in-hash?
  (ok (key-in-hash? :b data))
  (ok (not (key-in-hash? :d data))))

;;xsubseq
(is (xsubseq "asdfquerty" 2 7 :type 'string) "asty")

;;sequence->list
(is (car (sequence->list "asdf")) #\a)

;;or2
(is (or2 (values 1 t) 2) 1)
(is (or2 (values 1 nil) 2) 2)
(is (or2 (values 1) 2) 1)

;;fetch-keyword
(is-values (fetch-keyword :x '(1 2 3 :x 4 5)) '(4 t))
(is-values (fetch-keyword :y '(1 2 3 :x 4 5)) '(nil nil))

;;extract-keywords
;;bind-extracted-keywords
(bind-extracted-keywords ('(one two :three 3 :four 5 :eight 7 9) other :three :four)
  (is three 3)
  (is four 5)
  (is (car other) 'one)
  (ok (member :eight other))
  (ok (not (member :three other))))

;;use-package-with-shadowing
(when (find-package 'test1) (delete-package 'test1))
(when (find-package 'test2) (delete-package 'test2))
(defpackage #:test1 (:export :var1))
(defpackage #:test2 (:export :var1 :var2))
(defparameter test1::var1 1)
(defparameter test2::var1 2)
(defparameter test2::var2 3)
(use-package-with-shadowing 'test2 'test1)
(is (symbol-value (read-from-string "test1::var1")) 2)
(is (symbol-value (read-from-string "test1::var2")) 3)

;;range
(is (range 4) '(0 1 2 3))
(is (range 2 4) '(2 3))
(is (range 4 2) nil)
(is (range 0 4 2) '(0 2))
(is (range 3 0 -1) '(3 2 1))
(is (range 0 3 -1) nil)

;;do-window
(is (cl-utilities:collecting
        (do-window (x '(1 2 3 4 5 6))
          (cl-utilities:collect (apply #'+ x))))
    '(3 5 7 9 11))
(is (cl-utilities:collecting
        (do-window (x '(1 2 3 4 5 6) :step 2)
          (cl-utilities:collect (apply #'+ x))))
    '(3 7 11))
(is (cl-utilities:collecting
        (do-window (x '(1 2 3 4 5) :step 2)
          (cl-utilities:collect (apply #'+ x))))
    '(3 7))
(is (cl-utilities:collecting
        (do-window (x '(1 2 3 4 5) :step 2 :start-padding '(0))
          (cl-utilities:collect (apply #'+ x))))
    '(1 5 9))
(is-print
 (do-window ((a b c) '(1 2 3 4 5 6) :size 3)
   (when (eq c 3)
     (princ (+ a b))))
 "3")

;;string-equal*


;;boolify
;;FIXME: temp hack. Something broken in compilation here.
(progn
  (is (boolify 0) nil)
  (is (boolify 1) t)
  (is (boolify 5) t)
  (is (boolify "no") nil)
  (is (boolify "yes") t)
  (is (boolify "y") t)
  (is (boolify "t") t)
  (is (boolify '(1 2 3)) t)
  (is (boolify t) t)
  (is (boolify #(1 2 3)) t)
  (is (boolify #()) nil))

;;tryit
(is-values (tryit (error "Arrgh!")) '(nil nil))
(is-values (tryit 1) '(1 t))

;;chunk
(is (last-car (chunk 2 '(1 2 3 4 5))) '(5))

;;flatten-1
(let ((res (flatten-1 '((1 2 3) nil (nil) ((4 5) (6 7)) (8 . 9)))))
  (is (car res) 1)
  (is (length res) 8)
  (is (listp (fifth res)) t))

;;flatten-when
(let* ((data '((a (:b (c))) (:b 9 (c) (:d))))
       (res1 (flatten-when (alexandria:compose #'keywordp #'car) data))
       (res2 (flatten-when (alexandria:compose #'keywordp #'car)
                           data :descend-all t)))
  (is (length (car res1)) 2)
  (is (length res1) 5)
  (is (fifth res1) :d)
  (is (length (car res2)) 3)
  (is (length res2) 5)
  (is (fifth res2) :d)
  (is (second (car res2)) :b))

;;flatten-1-when
(let ((res (flatten-1-when
            (alexandria:compose #'keywordp #'car)
            '((a (:b (c))) (:b 9 (c) (:d))))))
  (is (length res) 5)
  (is (second res) :b)
  (ok (listp (fifth res))))

;;three-way
(is (mapcar (lambda (x) (three-way x 'a 'b 'c)) '(-1 0 1))
    '(a b c))

;;string-equal-case
(ok (string-equal-case "a" :|a|))
(is (string-equal-case "a" :a) nil)
(is (string-equal-case 'a 'b) nil)
(ok (string-equal-case 'cl-user::x 'x))
(is (string-equal-case '|asdf| "ASDF") nil)

;;string-equal-multiple
(ok (string-equal-multiple :a "a[]"))
(ok (string-equal-multiple :a "A"))
(is (string-equal-multiple "a()" "a[]") nil)

#| Removed from gadgets due to lack of clarity
;;match-a-symbol
(is (match-a-symbol "X" '(:x)) :x)
(ok (null (match-a-symbol "y" '(:x))))

;;match-various
(let ((matcher (match-various '(:a b "c" 4))))
  (is (funcall matcher "a") :a)
  (isnt (funcall matcher "C") "c")
  (is '(funcall matcher "B") b)
  (is (funcall matcher "4") 4)
  (ok (null (funcall matcher "asdf"))))
|#

;;part-on-index
(is (nth-value 1 (part-on-index '(1 2 3 4 5 6) 3)) '(4 5 6))
(is (nth-value 1 (part-on-index '(1 2 3 4 5 6) 6)) nil)
(is (nth-value 1 (part-on-index '(1 2 3 4 5 6) 7)) nil)
(is-error (nth-value 1 (part-on-index '(1 2 3 4 5 6) 7 :fail t)) 'simple-error)

;;part-on-true
(is-values (part-on-true #'evenp '(1 3 5 2 9)) '((1 3 5) (2 9)))
(is (part-on-true #'evenp '(1 3 5 7 9)) '(1 3 5 7 9))
(is-error (part-on-true #'evenp '(1 3 5 7 9) :fail t) 'simple-error)

;;part-after-true
(is-values (part-after-true #'evenp '(1 3 5 2 9)) '((1 3 5 2) (9)))
(is (part-after-true #'evenp '(1 3 5 7 9)) '(1 3 5 7 9))
(is-error (part-after-true #'evenp '(1 3 5 7 9) :fail t) 'simple-error)

;;remove-if-member
(is (remove-if-member '(1 2 3 4 5) '(3 5 9)) '(1 2 4))

;;splitfilter
(is (nth-value 1 (splitfilter #'evenp (range 6))) '(1 3 5))

;;split-sequence-on-subseq
(is-values (split-sequence-on-subseq "sd" "asdf") '("a" "f" "sd"))
(is-values (split-sequence-on-subseq '("af" "er" "ic") "the quick brown fox")
           '("the qu" "k brown fox" "ic"))
(is (multiple-value-list (split-sequence-on-subseq "x" "asdf")) '("asdf"))

;;first-match
(is-values (first-match #'evenp '(1 3 5 4 2)) '(4 t))
(is-values (first-match #'evenp '(1 3 5)) '(nil nil))

;;ordered-unique
(is (ordered-unique '(1 1 2 3 2 1 4 5 1 2 4)) '(1 2 3 4 5))

;;part-tree
(multiple-value-bind (outer inner)
    (part-tree
     (lambda (x) (eq 'deepest (car (alexandria:ensure-list x))))
     '(deep (deeper (deeperer (deepest (deepester you-are-here))))))
  (is (car inner) 'deepest)
  (is (car (funcall outer nil)) 'deep)
  (is (cadr (cadadr (funcall outer :x))) :x))
(multiple-value-bind (outer inner)
    (part-tree #'keywordp nil)
  (is inner nil)
  (is outer nil))

;;aif2only

;;do-file-by-line
;;map-file-by-line
(is (map-file-by-line
     #'length (asdf:system-relative-pathname 'gadgets "t/sample.txt")) '(3 3 5 4 4))

;;do-list-with-rest
;;preserve-other-values
(is-values (preserve-other-values (values 1 2 3) #'1+) '(2 2 3))

;;pif
;;print-lambda
;;print-cond
;;print-and
;;print-all-values

;;maplist/step
;;map-by-2
(is (map-by-2 (lambda (x y) (list x y)) '(1 2 3 4)) '((1 2) (3 4)))

;;mapcan-by-2
(is (mapcan-by-2 (lambda (x y) (list x y)) '(1 2 3 4)) '(1 2 3 4))

;;map-assoc
;;with-file-lock
;;encode-time-delta
;;cat
;;call-with-temporary-directory
;;with-temporary-directory
;;try-awhile

#| Tree utils removed from gadgets pending review
;;dotree
(is '(4 7 1 2 5 3 6)
    (cl-utilities:collecting
        (dotree (x '((1 2 (3)) 4 (5 (6)) 7)
                   :proc-branch nil :proc-leaf t :order :breadth)
          (cl-utilities:collect x))))
(is '(1 2 3 4 5 6 7)
    (cl-utilities:collecting
        (dotree (x '((1 2 (3)) 4 (5 (6)) 7)
                   :proc-branch nil :proc-leaf t :order :depth)
          (cl-utilities:collect x))))
|#
