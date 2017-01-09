;;;; package.lisp

(defpackage #:gadgets
  (:use #:cl #:anaphora #:cl-hash-util)
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
  (:import-from #:kebab
                #:to-lisp-case)
  (:shadowing-import-from
   #:anaphora #:it #:aif #:alet)
  (:export :with-collectors
           :collecting
           :collect
           :mkstr
           :symb
           :symbolize
           :autoquote
           :def-as-func
           :sequence-starts-with
           :split-sequence
           :split-sequence-if
           :split-sequence-if-not
           :assoc-cdr
           :assoc-all
           :assoc-set
           :assoc-symbolize
           :alist->plist
           :plist->alist
           :alist->hash
           :hash->alist
           :in-plist?
           :merge-plists
           :with-slow-collectors
           :fetch-slow-collectors
           :xsubseq
           :apply-compose
           :multiple-value-passthru
           :multiple-value-apply
           :multiple-valplex
           :fetch-keyword
           :extract-keywords
           :bind-extracted-keywords
           :autobind-specials
           :def-wrapper-func
           :range
           :loop-window
           :eval-always
           :last-car
           :trycar
           :chunk
           :flatten-1
           :eq-symb
           :eq-symb-case
           :tree-level
           :divide-on-index
           :divide-sequence
           :divide-list
           :remove-if-member
           :splitfilter
           :keyword-splitter
           :keyword-value
           :set-keyword-value
           :with-any/all/none
           :any
           :all
           :none
           :awrap-expr-if
           :aslist
           :aif2
           :aif2only
           :it
           :awhen
           :tracker-same
           :tracker-different
           :do-file-by-line
           :do-list-with-rest
           :list-set-place
           :tree-union
           :dotree
           :tree-tracker
           :index->accessor
           :pif
           :print-lambda
           :collecting-set
           :collecting-hash-table
           :funcall-in-macro
           :apply-in-macro
           :doleaves
           :maptree
           :mapleaves
           :tree-size
           :tree-depth
           :tree-search-replace
           :functionp-in-macro
           :key-in-hash?
           :first-match
           :awhen2
           :awhen2only
           :quotef
           :make-trycar
           :match-a-symbol
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
           :leaves-search-replace
           :collect-by-feature
           :tree-by-feature
           :*whitespace-characters*
           :boolify
           :tryit
           :keywordize
           :keywordize-foreign
           :with-keys
           :map-tuples
           :string-true
           :maplist/step
           :map-by-2
           :preserve-other-values
           :print-cond
           :hash->plist
           :package-external-symbols
           :ensure-package
           :package-own-symbols
           :package-exported-symbols
           :generic-p
           :class-p
           :make-symbol-flags
           :display-symbol
           :package-internal-symbols
           :package-own-internal-symbols
           :package-not-imported
           :return-when
           :strip-keywords
           :return-on-true
           :cat
           :collecting-string
           :do-window
           :extend-pathname
           :flatten-when
           :print-and
           :divide-list+
           :sequences-start-same
           :split-sequence-on-subseq
           :mapcan-by-2
           :call-with-temporary-directory
           :with-temporary-directory
           :try-awhile
           :ensure-member
           :send-recompiled-signal
           :watch-for-recompile
           :request-watch-on-names
           :get-function-name-in-macro
           :dependency-watcher
           :dependency-auto-watcher
           :watch-for-recompile/auto-watcher
           :print-all-values
           :or2
           :match-various
           :not-empty
           :capitalize-first
           :sequence-ends-with
           :alist-p
           :plist-p
           :invert-hash-table
           :map-assoc
           :rekey
           :eq-symb-multiple
           :hash-table->source
           :truncate-string
           :string-equal-caseless
           :divide-tree))
