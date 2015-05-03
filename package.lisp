;;;; package.lisp

(defpackage #:gadgets
  (:use #:cl #:anaphora) ;#:let-over-lambda)
  (:import-from #:cl-utilities 
		#:with-collectors 
		#:collecting
		#:compose
		#:once-only)
  (:import-from #:alexandria 
		#:flatten)
  (:shadowing-import-from #:cl-utilities #:collect)
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
	   :eq-symb-upcase
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
	   :key-in-hash?))

