(in-package :cl-user)
(defpackage test-gadgets
  (:use :cl :prove :gadgets))
(in-package :test-gadgets)


(plan 90)

;;symb
(is 'qwer (symb 'qw 'er))

;;capitalize-first
(is "Asdf" (capitalize-first "asdf"))

;;to-lowercase
(is "asdf" (to-lowercase "AsDF"))

;;to-uppercase
(is "ASDF" (to-uppercase "aSdf"))

;;string-unless-number
(is "asdf" (string-unless-number "asdf"))
(is 1 (string-unless-number "1"))
(is "1.0" (string-unless-number "1.0"))

;;symbol-unless-number
(is 'asdf (symbol-unless-number "asdf"))
(is 5 (symbol-unless-number "5"))

;;not-empty
(ok (not-empty "asdf"))
(ok (not (not-empty "")))

;;ret
(is 4 (ret x 3 (incf x)))

;;def-as-func
(def-as-func test-symbol (lambda () 3))
(is 3 (test-symbol))

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
(is '(1 4) (assoc-all :a '((:a . 1) (:b . 2) (:c . 3) (:a . 4))))

;;assoc-or
(is '(:c . 3) (assoc-or '(:e :t :c) '((:a . 1) (:b . 2) (:c . 3) (:a . 4))))

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
(is '(1 3 5) (sort
              (alexandria:hash-table-keys
               (invert-hash-table (hu:plist->hash '(:a 1 :b 3 :c 5))))
              #'<))

;;rekey
(is 2 (getf (hash->plist (rekey '(:a 1 :b 2) '(:a :c :b :a))) :a))

;;do-alist
(let ((k nil)
      (v nil))
  (do-alist (key value '((:a . 1) (:b . 2) (:c . 3)))
    (setf k key)
    (setf v value))
  (is :c k)
  (is 3 v))

;;do-hash-table
(let ((data (alist->hash '((:a . 1) (:b . 2) (:c . 3)))))
  (let ((k nil)
        (v nil))
    (do-hash-table (key value data)
      (setf k key)
      (setf v value))
    (is :c k)
    (is 3 v))

  ;;key-in-hash?
  (ok (key-in-hash? :b data))
  (ok (not (key-in-hash? :d data))))

;;xsubseq
(is "asty" (xsubseq "asdfquerty" 2 7 :type 'string))

;;sequence->list
(is #\a (car (sequence->list "asdf")))

;;or2
(is 1 (or2 (values 1 t) 2))
(is 2 (or2 (values 1 nil) 2))
(is 1 (or2 (values 1) 2))

;;apply-compose
;;fetch-keyword
(is-values (fetch-keyword :x '(1 2 3 :x 4 5)) '(4 t))
(is-values (fetch-keyword :y '(1 2 3 :x 4 5)) '(nil nil))

;;extract-keywords
;;bind-extracted-keywords
(bind-extracted-keywords ('(one two :three 3 :four 5 :eight 7 9) other :three :four)
  (is 3 three)
  (is 5 four)
  (is 'one (car other))
  (ok (member :eight other))
  (ok (not (member :three other))))

;;range
(is '(0 1 2 3) (range 4))
(is '(2 3) (range 2 4))
(is nil (range 4 2))
(is '(0 2) (range 0 4 2))
(is '(3 2 1) (range 3 0 -1))
(is nil (range 0 3 -1))

;;do-window
(is '(3 5 7 9 11)
    (collecting
        (do-window (x '(1 2 3 4 5 6))
          (collect (apply #'+ x)))))
(is '(3 7 11)
    (collecting
        (do-window (x '(1 2 3 4 5 6) :step 2)
          (collect (apply #'+ x)))))
(is '(3 7)
    (collecting
        (do-window (x '(1 2 3 4 5) :step 2)
          (collect (apply #'+ x)))))
(is '(1 5 9)
    (collecting
        (do-window (x '(1 2 3 4 5) :step 2 :start-padding '(0))
          (collect (apply #'+ x)))))
(is-print
 (do-window ((a b c) '(1 2 3 4 5 6) :size 3)
   (when (eq c 3)
     (princ (+ a b))))
 "3")

;;string-equal*

;;string-equal-caseless
(is nil (string-equal-caseless "asdf" "ASDFX"))
(ok (string-equal-caseless "asdf" "ASDf"))
(ok (string-equal-caseless "123a" "123A"))

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
(is '(5) (last-car (chunk 2 '(1 2 3 4 5))))

;;flatten-1
(let ((res (flatten-1 '((1 2 3) nil (nil) ((4 5) (6 7)) (8 . 9)))))
  (is 1 (car res))
  (is 8 (length res))
  (is t (listp (fifth res))))

;;flatten-when
(let* ((data '((a (:b (c))) (:b 9 (c) (:d))))
       (res1 (flatten-when (alexandria:compose #'keywordp #'car) data))
       (res2 (flatten-when (alexandria:compose #'keywordp #'car)
                           data :descend-all t)))
  (is 2 (length (car res1)))
  (is 5 (length res1))
  (is :d (fifth res1))
  (is 3 (length (car res2)))
  (is 5 (length res2))
  (is :d (fifth res2))
  (is :b (second (car res2))))

;;flatten-1-when
(let ((res (flatten-1-when
            (alexandria:compose #'keywordp #'car)
            '((a (:b (c))) (:b 9 (c) (:d))))))
  (is 5 (length res))
  (is :b (second res))
  (ok (listp (fifth res))))

;;three-way
(is '(a b c)
    (mapcar (lambda (x) (three-way x 'a 'b 'c)) '(-1 0 1)))

;;string-equal-case
(ok (string-equal-case "a" :|a|))
(is nil (string-equal-case "a" :a))
(is nil (string-equal-case 'a 'b))
(ok (string-equal-case 'cl-user::x 'x))
(is nil (string-equal-case '|asdf| "ASDF"))

;;string-equal-multiple
(ok (string-equal-multiple :a "a[]"))
(ok (string-equal-multiple :a "A"))
(is nil (string-equal-multiple "a()" "a[]"))

;;match-a-symbol
(is :x (match-a-symbol "X" '(:x)))
(ok (null (match-a-symbol "y" '(:x))))

;;match-various
(let ((matcher (match-various '(:a b "c" 4))))
  (is :a (funcall matcher "a"))
  (isnt "c" (funcall matcher "C"))
  (is 'b (funcall matcher "B"))
  (is 4 (funcall matcher "4"))
  (ok (null (funcall matcher "asdf"))))

;;divide-on-index
(is '(4 5 6) (nth-value 1 (divide-on-index '(1 2 3 4 5 6) 3)))
(is nil (nth-value 1 (divide-on-index '(1 2 3 4 5 6) 6)))
(is nil (nth-value 1 (divide-on-index '(1 2 3 4 5 6) 7)))
(is-error (nth-value 1 (divide-on-index '(1 2 3 4 5 6) 7 :fail t)) 'simple-error)

;;divide-on-true
(is-values (divide-on-true #'evenp '(1 3 5 2 9)) '((1 3 5) (2 9)))
(is (divide-on-true #'evenp '(1 3 5 7 9)) '(1 3 5 7 9))
(is-error (divide-on-true #'evenp '(1 3 5 7 9) :fail t) 'simple-error)

;;divide-after-true
(is-values (divide-after-true #'evenp '(1 3 5 2 9)) '((1 3 5 2) (9)))
(is (divide-after-true #'evenp '(1 3 5 7 9)) '(1 3 5 7 9))
(is-error (divide-after-true #'evenp '(1 3 5 7 9) :fail t) 'simple-error)

;;remove-if-member
(is '(1 2 4) (remove-if-member '(1 2 3 4 5) '(3 5 9)))

;;splitfilter
(is '(1 3 5) (nth-value 1 (splitfilter #'evenp (range 6))))

;;split-sequence-on-subseq
(is '("a" "f") (split-sequence-on-subseq "sd" "asdf"))
(is-values (split-sequence-on-subseq '("af" "er" "ic") "the quick brown fox")
           '(("the qu" "k brown fox") "ic"))

;;first-match
(is-values (first-match #'evenp '(1 3 5 4 2)) '(4 t))
(is-values (first-match #'evenp '(1 3 5)) '(nil nil))

;;ordered-unique
(is '(1 2 3 4 5) (ordered-unique '(1 1 2 3 2 1 4 5 1 2 4)))

;;divide-tree
(multiple-value-bind (outer inner)
    (divide-tree
     (lambda (x) (eq 'deepest (car (alexandria:ensure-list x))))
     '(deep (deeper (deeperer (deepest (deepester you-are-here))))))
  (is 'deepest (car inner))
  (is 'deep (car (funcall outer nil)))
  (is :x (cadr (cadadr (funcall outer :x)))))
(multiple-value-bind (outer inner)
    (divide-tree #'keywordp nil)
  (is nil inner)
  (is nil outer))

;;aif2only

;;do-file-by-line
;;map-file-by-line
(is '(3 3 5 4 4) (map-file-by-line
                  #'length (asdf:system-relative-pathname 'gadgets "t/sample.txt")))

;;do-list-with-rest
;;preserve-other-values
(is-values (preserve-other-values (values 1 2 3) #'1+) '(2 2 3))

;;pif
;;print-lambda
;;print-cond
;;print-and
;;print-all-values
;;collecting-set
;;map-tuples

;;maplist/step
;;map-by-2
(is '((1 2) (3 4)) (map-by-2 (lambda (x y) (list x y)) '(1 2 3 4)))

;;mapcan-by-2
(is '(1 2 3 4) (mapcan-by-2 (lambda (x y) (list x y)) '(1 2 3 4)))

;;map-assoc
;;with-file-lock
;;encode-time-delta
;;cat
;;extend-pathname
;;call-with-temporary-directory
;;with-temporary-directory
;;try-awhile

;;dotree
(is '(4 7 1 2 5 3 6)
    (collecting
        (dotree (x '((1 2 (3)) 4 (5 (6)) 7)
                   :proc-branch nil :proc-leaf t :order :breadth)
          (collect x))))
(is '(1 2 3 4 5 6 7)
    (collecting
        (dotree (x '((1 2 (3)) 4 (5 (6)) 7)
                   :proc-branch nil :proc-leaf t :order :depth)
          (collect x))))
