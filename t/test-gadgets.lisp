(in-package :cl-user)
(defpackage test-gadgets
  (:use :cl :prove :gadgets)
  (:export #:test-gadgets))
(in-package :test-gadgets)



#|
  (is-values (gethash '(:system :watch-level) udata) '(nil nil))
  (ok (not (gethash '(:email) udata))))

  (is-error (update-from-user *uname* *fields* indata) 'simple-error)

|#


(defun test-gadgets ()
  (plan 59)

  ;;symb
  (is 'cl-user::qwer (symb 'qw 'er))

  ;;string-unless-number
  ;;symbol-unless-number
  ;;not-empty
  ;;ret
  ;;def-as-func

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
  ;;do-hash-table
  ;;key-in-hash?
  ;;xsubseq
  ;;sequence->list
  ;;multiple-value-*
  ;;or2

  ;;extract-keywords
  ;;bind-extracted-keywords

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

  ;;string-equal-caseless
  (is nil (string-equal-caseless "asdf" "ASDFX"))
  (ok (string-equal-caseless "asdf" "ASDf"))
  (ok (string-equal-caseless "123a" "123A"))

  ;;boolify
  (is nil (boolify 0))
  (is t (boolify 1))
  (is t (boolify 5))
  (is nil (boolify "no"))
  (is t (boolify "yes"))
  (is t (boolify "y"))
  (is t (boolify "t"))
  (is t (boolify '(1 2 3)))
  (is t (boolify t))
  (is t (boolify #(1 2 3)))
  (is nil (boolify #()))

  ;;tryit
  ;;chunk
  ;;flatten-1
  (let ((res (flatten-1 '((1 2 3) nil (nil) ((4 5) (6 7))))))
    (is 1 (car res))
    (is 6 (length res))
    (is t (listp (fifth res))))

  ;;flatten-when
  ;;eq-symb-case
  ;;eq-symb
  ;;eq-symb-multiple
  ;;match-a-symbol
  ;;match-various
  ;;divide-on-index
  ;;divide-sequence
  ;;divide-list
  ;;divide-list+
  ;;remove-if-member
  ;;splitfilter
  ;;split-sequence-on-subsequence
  ;;first-match

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
  ;;do-list-with-rest
  ;;preserve-other-values
  ;;pif
  ;;print-lambda
  ;;print-cond
  ;;print-and
  ;;print-all-values
  ;;collecting-set
  ;;map-tuples
  ;;maplist/step
  ;;map-by-2
  ;;mapcan-by-2
  ;;map-assoc
  ;;with-file-lock
  ;;encode-time-delta
  ;;cat
  ;;extend-pathname
  ;;call-with-temporary-directory
  ;;with-temporary-directory
  ;;try-awhile

  (finalize))
