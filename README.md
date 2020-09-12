## Gadgets

[![CircleCI](https://circleci.com/gh/BnMcGn/gadgets.svg?style=svg)](https://circleci.com/gh/BnMcGn/gadgets)

## symbolize
**Function**
**Parameters: entity &key package**
Turns a string into an upcased symbol. The symbol will be interned in the current package if a package is not specified

## mkstr
#### Function
#### Parameters: &rest args
Paul Graham's mkstr. Returns a string representation of the parameters.

## ret 
#### Macro
#### Parameters: var val &body body
A single variable let that returns the variable when the body completes.

## def-as-func 
#### Macro 
#### Parameters: var func-form
Set a variable in the function namespace.

## fetch-keyword
#### Function
#### Parameters: key alist &key in-list

Find if a key is in a list, return the next item after it. if checklist
 is true, test the first element of any sublists for the key and if found
return rest of list as parameter. A bit coarser in function than getf. Will
tolerate improper plists.

## extract-keywords
#### Function
#### Parameters: keywords alist &key in-list test
Traverses a plist or lambda list, removing the specified keywords and the
value that immediately follows each. Found key/value pairs are returned as a
plist in the first value. The cleaned list is returned as the second value.

This, or the related macro bind-extracted-keywords, is particularly useful for adding features to macros. It will strip out added keywords from parameter lists, allowing the remainder to be passed to the original macro processing code.

## bind-extracted-keywords
#### Macro
#### Parameters: (source remainder &rest keys) body
Removes the keywords named in keys, with their accompanying parameters, from
the expression supplied in source. Source, minus the keys, is bound to
remainder. The names of the keys are used for bindings for the accompanying
values. (bind-extracted-keywords ((1 2 :x 3) data :x) <body>) Results in the
body being executed with data bound to (1 2) and x bound to 3.

## quotef
#### Macro
#### Parameters: setf-spec
(defmacro quotef (setf-spec)
  `(setf ,setf-spec `(quote ,,setf-spec)))

## quoted-p
#### Function
#### Parameters: item
Macro utility to test if an item has been quoted by the macro user.

## quoted-symbol-p
#### Function
#### Parameters: item 
Macro utility to test if an item has been passed into a macro as a quoted symbol. If so, returns the symbol without the quote.

## quoted-list-p
#### Function
#### Parameters: item 
Utility to test if a macro parameter is a quoted list. Returns the list if it is.

## use-package-with-shadowing
#### Function
#### Parameters: package &optional target-package 
Like use-package, but shadows existing symbols from target-package without asking. Target-package is the current package unless otherwise specified.

# String

## capitalize-first
#### Function
#### Parameters: item
Returns a string representation of item with the first letter capitalized
and the remaining characters lower-case, where applicable. Item can be a
string or a symbol.

## to-lowercase
#### Function
#### Parameters: item
Returns a lowercase copy of the supplied string.

## to-uppercase
#### Function
#### Parameters: item
Returns an uppercase copy of the supplied string.

## string-unless-number
#### Function
#### Parameters: item
For processing user input. Return the input as a string unless it can be recognized as an integer.

## symbol-unless-number
#### Function
#### Parameters: item
Convert the input string into a symbol unless it can be converted into a number.

## boolify
#### Function
#### Parameters: item
Attempts to guess when a string or number should be interpreted as T. Postive integers and strings like "true" and "yes" will be interpreted as true. Non-empty lists and sequences are true. Most other things are NIL. This function is useful for interactions in which a human is expected to answer a true or false question.

## not-empty
#### Function
#### Parameters: sequence
A predicate to detect 0 length sequences.

## sequences-start-same
#### Function
#### Parameters: seq seq2 
Given two sequences, are they the same until one runs out? This function
does not care which sequence contains the other. Use sequence-starts-with if
you need something more specific.

## sequence-starts-with
#### Function
#### Parameters: seq testseq
Does the sequence begin with the test sequence?

## sequence-ends-with
#### Function
#### Parameters: seq testseq
Does the sequence end with the test sequence?

## string-join
#### Function
#### Parameters: filler strings
Returns a string consisting of one each of the items in `strings` with `filler` interspersed between all of the items.

## **whitespace-characters**
#### List
A list of the characters that can represent whitespace in a common lisp string: (#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)

## string-equal*
#### Function
#### Parameters: a b
Broad version of string-equal. Will take input that is not a string or symbol.

## string-equal-caseless
#### Function
#### Parameters: a b
Are two strings equal when case is ignored?

## string-equal-case
#### Function
#### Parameters: 
A strictly case sensitive version of string-equal.

## truncate-string
#### Function
#### Parameters: str &key length indicator
Trim a string to the length specified by `length` - default 20, appending instead the string specified by `indicator` - default "...".

# Keyed collection

## assoc-cdr
#### Function
#### Parameters: &rest keys-and-alists 
A shortcut for (cdr (assoc ...)).

## assoc-all
#### Function
#### Parameters: item alist &key test 
Gets all items associated with a key, not just the first. Returns a list

    (assoc-all :a '((:a . 1) (:b . 2) (:c . 3) (:a . 4)))
    => (1 4)

## assoc-or
#### Function
#### Parameters: keys alist
Finds the first key in keys that has a match in alist. Uses equal to match
strings.

## alist-p
#### Function
#### Parameters: item
Does an item appear to be an assoc-list?

## plist-p
#### Function
#### Parameters: item
Determine if an item qualifies as a plist

## invert-hash-table
#### Function
#### Parameters: hash &key test mode
Returns a new hash table with keys and values swapped:
(:a 1 :b 3 :c 5) => (1 :a 3 :b 5 :c)

The hash table test can be set with :test. The method of value collection can
be controlled with :mode. Modes are those available for
cl-hash-util:collecting-hash-table.

## rekey
#### Function
#### Parameters: store mapping &key ignore-missing test
Takes a store (one of hash, alist or plist) and a mapping (also a hash, alist or plist) and returns a new hash table with the values from store rekeyed according to the oldkey -> newkey pairs found in mapping. Ignore-missing instructs rekey on what to do when a key is found in store but not in mapping. If true, rekey will drop the item that has no match. When false, it will include the item as is.

Test will be passed to the newly created result hash.

## do-alist
#### Macro
#### Parameters: (key value source) &body body
Iterate over an alist.

## do-hash-table
#### Macro
#### Parameters: (key value source) &body body
Iterate over a hash table

## key-in-hash?
#### Function
#### Parameters: key hash
Check if a hash table has a key.

Shortcut for (nth-value 1 (gethash key hash))

## hash-table-source
#### Function
#### Parameters: hash
Returns a source code representation of a hash table.

# List and sequence

## xsubseq
#### Function
#### Parameters: sequence start end &key type
Returns copy of sequence with start->end chopped out of it.

## sequence->list
#### Function
#### Parameters: seq
Returns a list consisting of the contents of the input sequence.

## last-car
#### Function
#### Parameters: list
Returns the last item in a list proper.

## chunk
#### Function
#### Parameters: n list
(defun chunk (n alist)
  (if (> (list-length alist) n)
      (cons (subseq alist 0 n) (chunk n (nthcdr n alist)))
      (list alist)))

## flatten-1
#### Function
#### Parameters: list
Flattens conses found in the top level of a list. Nils in the top level will be removed.

    (flatten-1 '((1 2 3) nil (nil) ((4 5) (6 7))))
    (1 2 3 NIL (4 5) (6 7)) 

## flatten-when
#### Function
#### Parameters: predicate items &key descend-all
Recursively flattens any conses found in items if the predicate returns true on them. Will not flatten NILs unless the predicate indicates it. The predicate will not be called on non-cons items. Flatten-when will not normally descend into lists which it will not flatten, passing unchanged any list or cons item that fails the predicate. To cause it to descend into non-matching portions of the tree, set the :descend-all keyword.

## flatten-1-when 
#### Function
#### Parameters: predicate items
Returns a list with any conses in it flattened if predicate returns true when called with that item. Will not flatten NILs unless the predicate indicates it. The predicate will not be called on non-cons items.

## part-on-index
#### Function
#### Parameters: list/seq index &key fail
Divides a list into two parts at the specified index. The two parts are returned as values. If the index is too large for the sequence, part-on-index will silently return the sequence as the first value. Set the :fail keyword T to raise an error instead.

## part-on-true
#### Function
#### Parameters: predicate list/seq &key fail
Divides a list or sequence into two parts, with the second part starting with the first item to cause test to return true. The two parts of the sequence are returned as values. If a dividing point is not found, part-on-true will return the whole sequence as the first value. If you wish it to raise an error instead, set the :fail parameter to true.

## part-after-true
#### Function
#### Parameters: predicate list/seq &key fail
Like part-on-true, but includes the first matching item in the first list.

## remove-if-member
#### Function
#### Parameters: sequence things &key key test
Returns a copy of `sequence` without items that are a member of `things`. You can modify the test function which is used by member - default eq - with the :test keyword. The :key keyword allows a single parameter function that receives a sequence item and should return a modified item for comparison to the items in `things`.

## splitfilter
#### Function
#### Parameters: predicate list
Performs the remove-if and remove-if-not operations on a list simultaneously, returning each list as the first and second values respectively.

## split-sequence-on-subseq
#### Function
#### Parameters: subseq/s sequence
Searches for a phrase or phrases in a sequence. When it finds one, it returns as values the subsequence up to the match, the subsequence after the match, and the matching subsequence. If no match is found, the whole sequence is returned as the first value.

## first-match
#### Function
#### Parameters: predicate list
Like the built in function `some`, first-match returns the first item in the list that evaluates true in the predicate. First-match uses the second value to indicate success, unlike `some`.

## first-match-index
#### Function
#### Parameters: predicate list
Returns the index of the first item in list that satisfies predicate.

## ordered-unique
#### Function
#### Parameters: list &key test
Returns a unique list of the items in list in the order in which they first
appear.

## do-list-with-rest
#### Macro
#### Parameters: (head tail source) &body body
A dolist for situations where access to the whole list is needed. Do-list-with-rest will iterate through the list supplied in source, initially binding the list to tail. On each iteration an item is removed from the first position of tail and pushed onto head. Note that the contents of head will be in reverse from those in source

## list-set-place
#### Function
#### Parameters: list index value padding
Returns a copy of the list with index set to value. If index is beyond the length of the list, pad out the list with the value in padding.

# Numerical

## range
#### Function
#### Parameters: start &optional stop step
Creates a list containing a sequential range of integers. By default the
range runs from 0 to one below the supplied stop value:

    (range 3) -> (0 1 2)
    
If a second parameter is supplied, the first is treated as a starting value, and
the second as a stop:

    (range 7 10) -> (7 8 9)
    
The third parameter specifies a step size:

    (range 0 10 2) -> (0 2 4 6 8)

A negative step parameter causes the range to travel down from the start to the
stop:

    (range 10 5) -> (10 9 8 7 6)

## relative-to-range
#### Function
#### Parameters: start end num
Returns a value indicating where num is positioned relative to start and end. If num lies between start and end, the return value will be between 0.0 and 1.0.

## as-in-range
#### Function
#### Parameters: start end num
Complement of relative-of-range function. Treats num as if it were a fraction of the range specified by start and end. Returns the absolute number that results.

# Execution control


## or2
#### Macro
#### Parameters: &rest clauses
A version of or that bases its decision on the second value of each clause. Forms that return no second value are considered T.

## apply-compose
#### Function
#### Macro
#### Parameters: 
(defun apply-compose (&rest functions)
  (lambda (&rest whatever)
    (labels ((func (data funcs)
               (if funcs
                   (apply (car funcs) (func data (cdr funcs)))
                   whatever)))
      (func whatever functions))))

## do-window
#### Macro
#### Parameters: (var/s source &key size step start-padding) &body body
Like dolist, iterates over a list, but instead of binding a single list
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
keyword.

## tryit
#### Macro
#### Parameters: &body body
Execute the code in the body, returning T as the second value if the code executes without error, but returning (NIL NIL) if an exception is thrown. This provides a quick way to turn an error into a boolean value.

WARNING: This isn't always a great idea for production code. Tryit will mask all raised errors, So if your code throws an error outside of what you expected, you won't be warned of the variance.

## three-way
#### Macro
#### Parameters: test minus-clause zero-clause plus-clause
If test results in a number that is less than zero, minus-clause will be evaluated. If zero, then zero-clause. Otherwise, plus-clause.

## preserve-other-values
#### Macro
#### Parameters: expression func
Take the values returned by expression, pass the first of them to func,
returning its first value as the primary value and appending the remaining
values from expression as unchanged.

    (1+ (values 1 2 3)) => 2
    (preserve-other-values (values 1 2 3)
                         #'1+) => 2 2 3

## maplist/step
#### Function
#### Parameters: func step list &rest more-lists
Maps through the supplied lists, passing a step-sized chunk from each into func. Returns a list of the results. The lists should all be the same length. That length should be divisible by the value specified in step.

## map-by-2
#### Function
#### Parameters: func list
Maps over a list two items at a time. The items are passed to the function as first and second parameters.

## mapcan-by-2
#### Function
#### Parameters: func list
This function is ideal for processing plists. As with map-by-2, func will receive items 2 at a time as the first and second parameters. It should return a list. For example:

    (mapcan-by-2 (lambda (key value) (list key value)) <some-list>)
will copy a plist with no changes.

## map-assoc
#### Function
#### Parameters: func alist
Map over an assoc list, with the key and value from each pair being sent to func as, respectively, the first and second parameters.

## map-improper
#### Function
#### Parameters: func list?
Map over a list, proper or not. The return mapping will be a proper list.

## mapc-improper
#### Function
#### Parameters: func list?
Mapc over a list, proper or not. Original list is returned. Like mapc, mapc-improper is used for side effects only.

## return-on-true
#### Macro
#### Parameters: clause &optional from-target
Executes return/return-from on the result of clause if it is true

## try-awhile
#### Function
#### Parameters: function &key sleep wait on-success on-fail
Will continue to call function until either it returns success or a given amount of time elapses. Duration can be set with the :wait keyword. It defaults to 1 second. Try-awhile will sleep between function calls unless the :sleep keyword is set to nil. Default sleep is 0.001 of a second.

Try-awhile will return the function value on success or nil on failure. If a function is supplied to the :on-success argument, it will be executed if the function succeeds and its result will be returned instead. The :on-fail keyword may be used to supply a function that will be run if the time elapses without a successful function run. Its result will be returned instead of the default nil.

Try-awhile blocks until completion.

# Tree

## part-tree
#### Function
#### Parameters: test tree
Divides the s-expression supplied in tree into an inner and an outer portion. The outer portion is returned in the first value as a closure. The inner portion is returned as the second value. The inner portion consists of the first part of the tree that passes test. The tree is traversed breadth-first.

    > (part-tree
        (lambda (x) (eq 'deepest (car (ensure-list x))))
        '(deep (deeper (deeperer (deepest (deepester you-are-here))))))
    #<CLOSURE (LAMBDA (GADGETS::X) :IN GADGETS:PART-TREE) {C19C81D}>
    (DEEPEST (DEEPESTER YOU-ARE-HERE))

    > (funcall * :xyz)
    (DEEP (DEEPER (DEEPERER :XYZ)))

The returned closure should be called with a single argument. It will return the outer portion with the supplied argument in place of the inner portion.

# File and OS

## do-file-by-line
#### Macro
#### Parameters: (line stream-or-path) &body body
Steps through a file or stream, one line at a time, binding the contents of the line to the variable specified in `line`. If a pathname is supplied, the file will be closed on completion or other exit. Streams will not be closed.

## map-file-by-line
#### Function
#### Parameters: function stream-or-path
A wrapper around do-file-by-line. The specified file or stream will be passed to the supplied function one line at a time. The function results will be accumulated in a list and returned.

## with-file-lock
#### Macro
#### Parameters: (path &key interval) &body body
Get an exclusive lock on a file. If lock cannot be obtained, keep
trying after waiting a while

## encode-time-delta
#### Function
#### Parameters: second minute hour day
A utility function for creating a time delta. Returns the time delta as an integer representing seconds.

## make-clock
#### Function
#### Parameters: ticks-per-second
Returns a timer closure that, on execution, returns the number of units of time elapsed since make-clock returned it.

## homedir-relative-pathname
#### Function
#### Parameters: name
Merge the supplied pathname onto the path of the current user's home directory.

## call-with-temporary-directory
#### Function
#### Parameters: thunk &key want-pathname-p
Generates a temporary directory in the appropriate location (specified by uiop), calls the supplied function, then cleans up the directory.

By default, the created pathname is passed to the function. If want-pathname-p is set to NIL, the current directory will instead be set to the temporary directory for the duration of the call.

## with-temporary-directory
#### Macro
#### Parameters: (&key pathname) &body body
Generates a temporary directory in the appropriate location (specified by uiop), executes the body of the macro, then cleans up the directory.

When a symbol is supplied to :pathname the temporary name will be bound to it. Otherwise the current directory will be set to the temporary directory for the duration of body execution.

# Debugging printers

Certain forms are awkward to debug using plain print. These tools provide some extra options.

## pif
#### Macro
#### Parameters: test then &optional else
This macro is a drop in replacement for if. It prints the test expression and the result of the test, indicating which way the test has branched.

## print-lambda
#### Macro
#### Parameters: (&rest args) &body body
A drop in replacement for lambda that behaves as trace does for defined functions.

## print-cond
#### Macro
#### Parameters: &rest clauses
A verbose drop in replacement for cond.

## print-and
#### Macro
#### Parameters: &rest forms
A verbose drop in replacement for and.

## print-all-values
#### Macro
#### Parameters: expr
Like print, but prints - and passes on - all values received. Useful for debugging expressions that return multiple values.

## dump
#### Function
#### Parameters: thing
For situations where print can't reach out. Put an item into storage for later retrieval by dive.

## dive
#### Function
Retrieve dumped items.


