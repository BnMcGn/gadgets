;;;; package.lisp

(defpackage #:gadgets
  (:use #:cl #:cl-hash-util)
  (:import-from #:cl-utilities
                #:once-only
                #:split-sequence
                #:split-sequence-if
                #:split-sequence-if-not)
  (:import-from #:alexandria
                #:flatten
                #:compose
                #:curry
                #:rcurry
                #:with-gensyms)
  (:import-from #:uiop
                #:strcat)
  (:export
           :mkstr
           :symb
           :symbolize
           :def-as-func
           :sequence-starts-with
           :assoc-cdr
           :assoc-all
           :xsubseq
           :fetch-keyword
           :extract-keywords
           :bind-extracted-keywords
           :range
           :loop-window
           :eval-always
           :last-car
           :trycar
           :chunk
           :flatten-1
           :string-equal-case
           :part-on-index
           :remove-if-member
           :splitfilter
           :do-file-by-line
           :do-list-with-rest
           :pif
           :print-lambda
           :key-in-hash?
           :first-match
           :quotef
           :do-alist
           :do-hash-table
           :ret
           :use-package-with-shadowing
           :string-unless-number
           :symbol-unless-number
           :with-file-lock
           :encode-time-delta
           :map-file-by-line
           :strcat
           :*whitespace-characters*
           :boolify
           :tryit
           :maplist/step
           :map-by-2
           :preserve-other-values
           :print-cond
           :return-on-true
           :do-window
           :flatten-when
           :print-and
           :sequences-start-same
           :split-sequence-on-subseq
           :mapcan-by-2
           :call-with-temporary-directory
           :with-temporary-directory
           :try-awhile
           :print-all-values
           :or2
           :not-empty
           :capitalize-first
           :sequence-ends-with
           :alist-p
           :plist-p
           :invert-hash-table
           :map-assoc
           :rekey
           :string-equal-multiple
           :hash-table->source
           :truncate-string
           :part-tree
           :flatten-1-when
           :map-improper
           :mapc-improper
           :to-lowercase
           :to-uppercase
           :ordered-unique
           :dump
           :dive
           :part-on-true
           :part-after-true
           :assoc-or
           :as-in-range
           :relative-to-range
           :three-way
           :string-equal*
           :quoted-symbol-p
           :quoted-p
           :quoted-list-p
           :string-join
           :sequence->list
           :homedir-relative-pathname
           :make-clock
   :list-set-place))
