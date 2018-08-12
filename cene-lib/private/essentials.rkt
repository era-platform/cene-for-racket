#lang parendown racket/base

; cene/private/essentials
;
; A sufficient set of essential built-in operations for the Cene
; programming language (implementation details).

;   Copyright 2018 The Era Authors
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing,
;   software distributed under the License is distributed on an
;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;   either express or implied. See the License for the specific
;   language governing permissions and limitations under the License.


(require #/for-syntax racket/base)
(require #/for-syntax #/only-in syntax/parse expr id nat syntax-parse)


(require #/only-in racket/contract/base -> ->i any/c list/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define/generic)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-all list-any list-foldl list-map)
(require #/only-in lathe-comforts/maybe
  just maybe-bind maybe/c nothing)
(require #/only-in lathe-comforts/struct struct-easy)

(require #/only-in effection/order
  call-fuse call-merge cline-result? compare-by-cline compare-by-dex
  dex? dex-cline dex-dex dex-fuse dex-give-up dex-merge dex-name
  in-cline? in-dex? get-dex-from-cline name? name-of ordering-eq
  table-empty table-get table-shadow)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe
  autoname-cline autoname-dex autoname-fuse autoname-merge cline dex
  fuse gen:cline-internals gen:dex-internals gen:furge-internals merge
  name table->sorted-list)

(require cene/private)


(provide cene-runtime-essentials)



(define s-nil (core-sink-struct "nil" #/list))
(define s-cons (core-sink-struct "cons" #/list "first" "rest"))

(define s-assoc (core-sink-struct "assoc" #/list "key" "val"))


; TODO: We used this in `effection/order/base`, and we're using it
; again here. See if it should be an export of Effection.
(define-simple-macro (maybe-ordering-or first:expr second:expr)
  (w- result first
  #/expect result (just #/ordering-eq) result
    second))

; TODO: We used this in `effection/order/base`, and we're using it
; again here. See if it should be an export of Effection.
(define (maybe-compare-aligned-lists as bs maybe-compare-elems)
  (expect (list as bs) (list (cons a as) (cons b bs))
    (just #/ordering-eq)
  #/maybe-ordering-or (maybe-compare-elems a b)
  #/maybe-compare-aligned-lists as bs maybe-compare-elems))

(define/contract (sink-list->maybe-racket sink-list)
  (-> sink? #/maybe/c list?)
  ; NOTE: We could call `sink-list->maybe-racket` itself recursively,
  ; but we explicitly accumulate elements using a parameter
  ; (`rev-racket-list`) of a recursive helper function (`next`) so
  ; that we keep the call stack at a constant size throughout the list
  ; traversal.
  (w-loop next sink-list sink-list rev-racket-list (list)
  #/mat (unmake-sink-struct-maybe s-nil sink-list) (just #/list)
    (just #/reverse rev-racket-list)
  #/mat (unmake-sink-struct-maybe s-cons sink-list)
    (just #/list elem sink-list)
    (next sink-list #/cons elem rev-racket-list)
  #/nothing))


; Sorts `proj-tags` and `vals` to put them in a normalized order.
(define/contract (normalize-proj-tags-and-vals proj-tags vals)
  (->i ([proj-tags (listof name?)] [vals list?])
    #:pre (proj-tags vals) (= (length proj-tags) (length vals))
    [_ (list/c (listof name?) list?)])
  (w- entries
    (unsafe:table->sorted-list
    #/list-foldl (table-empty) (map list proj-tags vals)
    #/fn tab entry
      (dissect entry (list proj-tag field-name-rep)
      #/table-shadow proj-tag (just field-name-rep) tab))
  #/list
    (list-map entries #/dissectfn (list proj-tag val) proj-tag)
    (list-map entries #/dissectfn (list proj-tag val) val)))

(define/contract (normalize-tags-and-vals tags vals)
  (->i ([tags (listof name?)] [vals list?])
    #:pre (tags vals) (= (length tags) (add1 #/length vals))
    [_ (list/c (listof name?) list?)])
  (dissect tags (cons main-tag proj-tags)
  #/dissect (normalize-proj-tags-and-vals proj-tags vals)
    (list proj-tags vals)
  #/list (cons main-tag proj-tags) vals))

(define/contract
  (sink-struct-op-autodex? a-tags a-fields b-tags b-fields dex-field)
  (->i
    (
      [a-tags (listof name?)]
      [a-fields list?]
      [b-tags (listof name?)]
      [b-fields list?]
      [dex-field dex?])
    #:pre (a-tags a-fields)
      (= (length a-tags) (add1 #/length a-fields))
    #:pre (b-tags b-fields)
      (= (length b-tags) (add1 #/length b-fields))
    [_ (maybe/c cline-result?)])
  (maybe-ordering-or
    (compare-by-dex (dex-name)
      (unsafe:name
      #/list-map a-tags #/dissectfn (unsafe:name tag) tag)
      (unsafe:name
      #/list-map b-tags #/dissectfn (unsafe:name tag) tag))
  #/maybe-compare-aligned-lists a-fields b-fields #/fn a-field b-field
    (compare-by-dex dex-field a-field b-field)))

(define/contract
  (sink-struct-in? tags comparators in-comparator? x)
  (->i
    (
      [tags (listof name?)]
      [comparators list?]
      [in-comparator? (-> any/c any/c boolean?)]
      [x any/c])
    #:pre (tags comparators)
      (= (length tags) (add1 #/length comparators))
    [_ (maybe/c cline-result?)])
  (expect (unmake-sink-struct-maybe tags x) (just field-vals) #f
  #/w-loop next field-comparators comparators field-vals field-vals
    (expect field-comparators
      (cons comparator-field field-comparators)
      #t
    #/dissect field-vals (cons field-val field-vals)
    
    ; We do a tail call if we can.
    #/mat field-comparators (list)
      (in-comparator? comparator-field field-val)
    
    #/and (in-comparator? comparator-field field-val)
    #/next field-comparators field-vals)))

(define/contract
  (sink-struct-compare
    tags comparators in-comparator? compare-by-comparator a b)
  (->i
    (
      [tags (listof name?)]
      [comparators list?]
      [in-comparator? (-> any/c any/c boolean?)]
      [compare-by-comparator
        (-> any/c any/c any/c #/maybe/c cline-result?)]
      [a any/c]
      [b any/c])
    #:pre (tags comparators)
      (= (length tags) (add1 #/length comparators))
    [_ (maybe/c cline-result?)])
  (maybe-bind (unmake-sink-struct-maybe tags a) #/fn as
  #/maybe-bind (unmake-sink-struct-maybe tags b) #/fn bs
  #/w-loop next as as bs bs comparators comparators
    (expect comparators (cons comparator-field comparators)
      (just #/ordering-eq)
    #/dissect as (cons a as)
    #/dissect bs (cons b bs)
    
    ; We do a tail call if we can.
    #/mat comparators (list)
      (compare-by-comparator comparator-field a b)
    
    #/maybe-bind (compare-by-comparator comparator-field a b)
    #/fn result
    #/expect result (ordering-eq)
      ; We have a potential result to use, but first we check that
      ; the rest of the field values belong to their respective
      ; comparators' domains. If they don't, this structure instance
      ; is not part part of this overall comparison's domain, so the
      ; result is `(nothing)`.
      (w-loop next as as bs bs comparators comparators
        (expect comparators (cons comparator-field comparators)
          (just result)
        #/dissect as (cons a as)
        #/dissect bs (cons b bs)
        #/expect
          (and
            (in-comparator? comparator-field a)
            (in-comparator? comparator-field b))
          #t
          (nothing)
        #/next as bs comparators))
    #/next as bs comparators)))


(struct-easy (dex-internals-sink-struct tags fields)
  (#:guard-easy
    (unless (and (list? tags) (list-all tags #/fn tag #/name? tag))
      (error "Expected tags to be a list of names"))
    (unless
      (and
        (list? fields)
        (list-all fields #/fn dex-field #/dex? dex-field))
      (error "Expected fields to be a list of dexes"))
    (unless (= (length tags) (add1 #/length fields))
      (error "Expected tags to have one more element than fields")))
  
  #:other
  
  #:methods unsafe:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-sink-struct)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-sink-struct tags fields)
      #/dissect (normalize-tags-and-vals tags fields)
        (list tags fields)
      #/list* 'tag:dex-sink-struct tags
      #/list-map fields #/fn dex-field
        (unsafe:autoname-dex dex-field)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-sink-struct a-tags a-fields)
      #/dissect other (dex-internals-sink-struct b-tags b-fields)
      #/sink-struct-op-autodex? a-tags a-fields b-tags b-fields
        (dex-dex)))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-sink-struct tags fields)
      #/sink-struct-in? tags fields in-dex? x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-sink-struct tags fields)
      #/maybe-bind (unmake-sink-struct-maybe tags x) #/fn field-vals
      #/w-loop next
        field-dexes fields
        field-vals field-vals
        rev-result (list)
        
        (expect field-dexes (cons dex-field field-dexes)
          (dissect tags (cons (unsafe:name main-tag-rep) proj-tags)
          #/dissect
            (normalize-proj-tags-and-vals
              proj-tags (reverse rev-result))
            (list proj-tags field-name-reps)
          #/just #/unsafe:name
          #/list* 'name:sink-struct
            (cons main-tag-rep
            #/list-map proj-tags
            #/dissectfn (unsafe:name proj-tag-rep) proj-tag-rep)
            field-name-reps)
        #/dissect field-vals (cons field-val field-vals)
        #/maybe-bind (name-of dex-field field-val)
        #/dissectfn (unsafe:name rep)
        #/next field-dexes field-vals #/cons rep rev-result)))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-sink-struct tags fields)
      #/sink-struct-compare tags fields in-dex? compare-by-dex a b))
  ])

(struct-easy (cexpr-dex-struct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-dex-struct main-tag-name projs)
        (error "Expected this to be a cexpr-dex-struct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cexpr-eval this env)
      (expect this (cexpr-dex-struct main-tag-name projs)
        (error "Expected this to be a cexpr-dex-struct")
      #/sink-dex #/unsafe:dex #/dex-internals-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval proj-cexpr env)))
  ])


(struct-easy (cline-internals-sink-struct tags fields)
  (#:guard-easy
    (unless (and (list? tags) (list-all tags #/fn tag #/name? tag))
      (error "Expected tags to be a list of names"))
    (unless
      (and
        (list? fields)
        (list-all fields #/fn dex-field #/dex? dex-field))
      (error "Expected fields to be a list of dexes"))
    (unless (= (length tags) (add1 #/length fields))
      (error "Expected tags to have one more element than fields")))
  
  #:other
  
  #:methods unsafe:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-sink-struct)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-sink-struct tags fields)
      #/dissect (normalize-tags-and-vals tags fields)
        (list tags fields)
      #/list* 'tag:cline-sink-struct tags
      #/list-map fields #/fn cline-field
        (unsafe:autoname-cline cline-field)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-sink-struct a-tags a-fields)
      #/dissect other (cline-internals-sink-struct b-tags b-fields)
      #/sink-struct-op-autodex? a-tags a-fields b-tags b-fields
        (dex-cline)))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-sink-struct tags fields)
      #/sink-struct-in? tags fields in-cline? x))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-sink-struct tags fields)
      #/dex-internals-sink-struct tags
      #/list-map fields #/fn cline-field
        (get-dex-from-cline cline-field)))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-sink-struct tags fields)
      #/sink-struct-compare
        tags fields in-cline? compare-by-cline a b))
  ])

(struct-easy (cexpr-cline-struct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-cline-struct main-tag-name projs)
        (error "Expected this to be a cexpr-cline-struct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cexpr-eval this env)
      (expect this (cexpr-cline-struct main-tag-name projs)
        (error "Expected this to be a cexpr-cline-struct")
      #/sink-cline #/unsafe:cline #/cline-internals-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval proj-cexpr env)))
  ])


(struct-easy
  (furge-internals-sink-struct
    autoname-furge dex-furge call-furge tags fields)
  #:other
  
  #:methods unsafe:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:furge-sink-struct)
    
    (define (furge-internals-autoname this)
      (dissect this
        (furge-internals-sink-struct autoname-furge _ _ tags fields)
      #/dissect (normalize-tags-and-vals tags fields)
        (list tags fields)
      #/list* 'tag:furge-sink-struct tags
      #/list-map fields #/fn furge-field
        (autoname-furge furge-field)))
    
    (define (furge-internals-autodex this other)
      (dissect this
        (furge-internals-sink-struct _ dex-furge _ a-tags a-fields)
      #/dissect other
        (furge-internals-sink-struct _ _ _ b-tags b-fields)
      #/sink-struct-op-autodex? a-tags a-fields b-tags b-fields
        dex-furge))
    
    (define (furge-internals-call this a b)
      (dissect this
        (furge-internals-sink-struct _ _ call-furge tags fields)
      #/maybe-bind (unmake-sink-struct-maybe tags a) #/fn as
      #/maybe-bind (unmake-sink-struct-maybe tags b) #/fn bs
      #/w- n (length fields)
      #/w-loop next as as bs bs fields fields rev-furged (list)
        (expect fields (cons furge-field fields)
          (make-sink-struct tags #/reverse rev-furged)
        #/dissect as (cons a as)
        #/dissect bs (cons b bs)
        #/next fields as bs
        #/cons (call-furge furge-field a b) rev-furged)))
  ])

(struct-easy (cexpr-merge-struct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-merge-struct main-tag-name projs)
        (error "Expected this to be a cexpr-merge-struct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cexpr-eval this env)
      (expect this (cexpr-merge-struct main-tag-name projs)
        (error "Expected this to be a cexpr-merge-struct")
      #/sink-merge #/unsafe:merge #/furge-internals-sink-struct
        unsafe:autoname-merge (dex-merge) call-merge
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval proj-cexpr env)))
  ])

(struct-easy (cexpr-fuse-struct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-fuse-struct main-tag-name projs)
        (error "Expected this to be a cexpr-fuse-struct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cexpr-eval this env)
      (expect this (cexpr-fuse-struct main-tag-name projs)
        (error "Expected this to be a cexpr-fuse-struct")
      #/sink-fuse #/unsafe:fuse #/furge-internals-sink-struct
        unsafe:autoname-fuse (dex-fuse) call-fuse
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval proj-cexpr env)))
  ])


; TODO: Use this in some kind of CLI entrypoint or something.
(define/contract (cene-runtime-essentials)
  (-> cene-runtime?)
  
  (define defined-dexes (table-empty))
  (define defined-values (table-empty))
  
  (define/contract (def-dexable-value! name dex value)
    (-> sink-name? sink-dex? sink? void?)
    (dissect name (sink-name name)
    #/begin
      (set! defined-dexes
        (table-shadow name (just dex) defined-dexes))
      (set! defined-values
        (table-shadow name (just value) defined-values))
    #/void))
  
  (define/contract (def-value! name value)
    (-> sink-name? sink? void?)
    (def-dexable-value! name (sink-dex #/dex-give-up) value))
  
  (define/contract (macro-impl body)
    (->
      (->
        name? sink? sink-text-input-stream?
        sink-cexpr-sequence-output-stream?
        (->
          name? sink? sink-text-input-stream?
          sink-cexpr-sequence-output-stream?
          sink-effects?)
        sink-effects?)
      sink?)
    (sink-fn-curried 5 #/fn
      unique-name qualify text-input-stream output-stream then
      
      (expect unique-name (sink-name unique-name)
        (cene-err "Expected unique-name to be a name")
      #/expect (sink-text-input-stream? text-input-stream) #t
        (cene-err "Expected text-input-stream to be a text input stream")
      #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
        (cene-err "Expected output-stream to be an expression sequence output stream")
      #/body unique-name qualify text-input-stream output-stream
      #/fn unique-name qualify text-input-stream output-stream
      #/w- effects
        (sink-call then (sink-name unique-name) qualify
          text-input-stream output-stream)
      #/expect (sink-effects? effects) #t
        (cene-err "Expected the return value of a macro's callback to be an effectful computation")
        effects)))
  
  ; This creates a macro implementation function that reads a form
  ; body of precisely `n-args` cexprs, then writes a single cexpr
  ; computed from those using `body`.
  (define/contract (macro-impl-specific-number-of-args n-args body)
    (-> natural? (-> (listof sink-cexpr?) sink-cexpr?) sink?)
    (macro-impl #/fn
      unique-name qualify text-input-stream output-stream then
      
      (sink-effects-read-specific-number-of-cexprs
        unique-name qualify text-input-stream n-args
      #/fn unique-name qualify text-input-stream args
      #/sink-effects-cexpr-write output-stream (body args)
      #/fn output-stream
      #/then unique-name qualify text-input-stream output-stream)))
  
  (define/contract
    (def-func-verbose! main-tag-string n-args racket-func)
    (-> string? exact-positive-integer? procedure? void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify
      #/sink-name-for-struct-main-tag main-tag-name)
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (def-value!
        (sink-name-qualify
        #/sink-name-for-bounded-cexpr-op main-tag-name)
        
        ; Given precisely `n-args` cexprs, we construct a cexpr that
        ; first constructs a nullary struct with tag
        ; `qualified-main-tag-name` and then calls it with the given
        ; cexprs one by one.
        ;
        ; The JavaScript implementation of Cene doesn't verify the
        ; number of arguments to a function; instead it just passes in
        ; all the arguments it gets. But I find it's common for me to
        ; accidentally omit arguments or include extra arguments, so
        ; in `sink-effects-read-specific-number-of-cexprs`, we do some
        ; error-checking as an ad hoc line of defense against that
        ; kind of mistake.
        ;
        (macro-impl-specific-number-of-args n-args #/fn args
          (list-foldl
            (sink-cexpr-struct qualified-main-tag-name #/list)
            args
          #/fn func arg #/sink-cexpr-call func arg)))
      
      ; We define a Cene struct function implementation containing
      ; the function's run time behavior.
      (def-value!
        (sink-name-for-function-implementation qualified-main-tag-name
          (sink-table #/table-empty))
        (sink-cexpr-native #/sink-opaque-fn #/fn struct-value
          (sink-fn-curried n-args racket-func)))
      
      ))
  
  (define-syntax (def-func! stx)
    (syntax-parse stx #/
      (_ main-tag-string:expr param:id ... body:expr)
      #`(def-func-verbose! main-tag-string
          '#,(length (syntax->list #'(param ...)))
          (fn param ...
            body))))
  
  (define/contract (def-nullary-func! main-tag-string result)
    (-> string? sink? void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify
      #/sink-name-for-struct-main-tag main-tag-name)
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (def-value!
        (sink-name-qualify
        #/sink-name-for-bounded-cexpr-op main-tag-name)
        
        ; Given precisely zero cexprs, we construct a cexpr that first
        ; constructs a nullary struct with tag `name` and then calls
        ; it with a trivial value.
        ;
        ; The JavaScript implementation of Cene doesn't have this
        ; special kind of compilation for nullary function calls; it
        ; just has the user pass `(trivial)` explicitly.
        ;
        (macro-impl-specific-number-of-args 0 #/fn args
          (sink-cexpr-call
            (sink-cexpr-struct qualified-main-tag-name #/list)
            (make-sink-cexpr-struct s-trivial #/list))))
      
      ; We define a Cene struct function implementation containing
      ; the function's run time behavior.
      (def-value!
        (sink-name-for-function-implementation qualified-main-tag-name
          (sink-table #/table-empty))
        (sink-cexpr-native #/sink-fn-curried 2 #/fn struct-value arg
          (expect (unmake-sink-struct-maybe s-trivial arg)
            (just #/list)
            (cene-err "Expected the argument to a nullary function to be a trivial")
            result)))
      
      ))
  
  (define/contract (def-data-struct! main-tag-string proj-strings)
    (-> string? (listof string?) void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify
      #/sink-name-for-struct-main-tag main-tag-name)
    #/w- qualified-proj-names
      (list-map proj-strings #/fn proj-string
        (sink-name-qualify #/sink-name-for-struct-proj
          qualified-main-tag-name
        #/sink-name-for-string #/sink-string proj-string))
      
      ; We define a reader macro so that the user can write code that
      ; compiles into an expression that constructs a struct with this
      ; tag.
      (def-value!
        (sink-name-qualify
        #/sink-name-for-bounded-cexpr-op main-tag-name)
        
        (w- n-projs (length qualified-proj-names)
        
        ; Given precisely `n-projs` cexprs, we construct a cexpr that
        ; constructs a struct.
        ;
        ; The JavaScript implementation of Cene doesn't verify that
        ; the number of arguments to a struct constructor is under a
        ; certain amount; instead it just passes all the excess
        ; arguments as function arguments. I find it's common for me
        ; to accidentally omit arguments or include extra arguments,
        ; so in `sink-effects-read-specific-number-of-cexprs`, we do
        ; some error-checking as an ad hoc line of defense against
        ; that kind of mistake.
        ;
        #/macro-impl-specific-number-of-args n-projs #/fn proj-cexprs
          (sink-cexpr-struct qualified-main-tag-name
          #/map list qualified-proj-names proj-cexprs)))
      
      ; We define a Cene struct function implementation which throws
      ; an error. We do this so that we do in fact have a function
      ; implementation for every struct we use, which might be an
      ; invariant that comes in handy. (TODO: See if it does.)
      (def-value!
        (sink-name-for-function-implementation qualified-main-tag-name
          (list-foldl (sink-table #/table-empty) qualified-proj-names
          #/fn table proj-name
            (sink-table-put-maybe table proj-name
            #/just #/make-sink-struct s-trivial #/list)))
        (sink-cexpr-native #/sink-opaque-fn #/fn struct-value
          (cene-err "Called a struct that wasn't intended for calling")))
      
      ; TODO: Also define something we can use to look up an ordered
      ; list of `sink-name-for-string` projection names, given the
      ; `sink-name-for-string` name the main tag name is made from.
      ; Once we have that in place, we'll be able to implement Cene's
      ; destructuring operations.
      
      ))
  
  (define/contract (def-macro! name-string body)
    (->
      string?
      (->
        name? sink? sink-text-input-stream?
        (-> name? sink? sink-text-input-stream? sink-cexpr?
          sink-effects?)
        sink-effects?)
      void?)
    (def-value!
      (sink-name-qualify #/sink-name-for-bounded-cexpr-op
      #/sink-name-for-string #/sink-string name-string)
      (macro-impl #/fn
        unique-name qualify text-input-stream output-stream then
        
        (body unique-name qualify text-input-stream
        #/fn unique-name qualify text-input-stream cexpr
        #/sink-effects-cexpr-write output-stream cexpr
        #/fn output-stream
        #/then unique-name qualify text-input-stream output-stream))))
  
  
  
  ; This binds the nameless bounded expression reader macro. This
  ; implementation proceeds by reading and running a (named) bounded
  ; expression reader macro.
  ;
  ; The Cene code `(foo a b c)` invokes the nameless bounded
  ; expression reader macro, and since that macro typically has this
  ; implementation, it behaves just like `(.foo a b c)`. That common
  ; behavior is that it consumes "foo", looks up an expression reader
  ; macro based on qualifying the string "foo", and runs it.
  ;
  (def-value!
    (sink-name-qualify #/sink-name-for-nameless-bounded-cexpr-op)
    (macro-impl #/fn
      unique-name qualify text-input-stream output-stream then
      
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)))
  
  ; This binds the freestanding expression reader macro for `=`. This
  ; implementation is a line comment syntax: It consumes all the
  ; proceeding non-line-break characters, writes no cexprs at all, and
  ; leaves it at that.
  ;
  ;   \= This is an example comment.
  ;
  (def-value!
    (sink-name-qualify #/sink-name-for-freestanding-cexpr-op
    #/sink-name-for-string #/sink-string "=")
    (macro-impl #/fn
      unique-name qualify text-input-stream output-stream then
      
      (sink-effects-read-non-line-breaks text-input-stream
      #/fn text-input-stream non-line-breaks
      #/then unique-name qualify text-input-stream output-stream)))
  
  
  ; Miscellaneous
  
  (def-data-struct! "trivial" #/list)
  
  (def-data-struct! "nothing" #/list)
  (def-data-struct! "just" #/list "val")
  
  (def-data-struct! "yep" #/list "val")
  (def-data-struct! "nope" #/list "val")
  
  (def-data-struct! "nil" #/list)
  (def-data-struct! "cons" #/list "first" "rest")
  
  (def-data-struct! "assoc" #/list "key" "val")
  
  
  ; Errors and conscience
  
  ; TODO: Test this.
  (def-func! "follow-heart" clamor
    (raise-cene-err (current-continuation-marks) clamor))
  
  (def-data-struct! "clamor-err" #/list "message")
  
  ; TODO: Implement the macro `err`.
  
  
  ; Order
  
  ; TODO: Implement this.
  (def-nullary-func! "dex-cline" (sink-dex 'TODO))
  
  (def-func! "cline-by-dex" dex
    ; TODO: Implement this.
    'TODO)
  
  ; TODO: Implement this.
  (def-nullary-func! "cline-give-up" (sink-cline 'TODO))
  
  ; TODO: Consider implementing the following. This list was taken
  ; from the docs of the JavaScript version of Cene, but Effection has
  ; incorporated some lessons learned since then, so we might want to
  ; work against the list of Effection building blocks instead.
  ;
  ;   cline-default
  ;   cline-by-own-method
  ;   cline-fix
  ;   call-cline
  ;   in-cline
  ;   dexable
  ;   dex-dex
  ;   dex-by-cline
  ;   name-of
  ;   dex-name
  ;   dex-merge
  ;   merge-by-dex
  ;   merge-default
  ;   merge-by-own-method
  ;   merge-fix
  ;   call-merge
  ;   dex-fuse
  ;   fuse-by-merge
  ;   fuse-default
  ;   fuse-by-own-method
  ;   fuse-fix
  ;   call-fuse
  
  
  ; Structs and function calls
  
  (define/contract
    (verify-cexpr-struct-args! main-tag-name projections)
    (-> sink? sink? void?)
    
    (expect main-tag-name (sink-name main-tag-name)
      (cene-err "Expected main-tag-name to be a name")
    #/expect (sink-list->maybe-racket projections) (just projections)
      (cene-err "Expected projections to be a list made up of cons and nil values")
    #/w- projections
      (list-map projections #/fn projection
        (expect (unmake-sink-struct-maybe s-assoc projection)
          (just #/list k v)
          (cene-err "Expected projections to be a list of assoc values")
        #/expect k (sink-name k)
          (cene-err "Expected projections to be an association list with names as keys")
        #/expect v (sink-cexpr v)
          (cene-err "Expected projections to be an association list with cexprs as values")
        #/list k v))
    #/begin
      (list-foldl (table-empty) projections #/fn tab projection
        (dissect projection (list k v)
        #/expect (table-get k tab) (nothing)
          (cene-err "Expected projections to be an association list with mutually unique names as keys")
        #/table-shadow k (just #/trivial) tab))
    #/void))
  
  ; NOTE: The JavaScript version of Cene makes this functionality
  ; possible using a combination of `cexpr-cline-struct`,
  ; `cline-by-dex`, and `dex-by-cline`. We will probably be offering
  ; `get-dex-by-cline` (as provided by Effection) instead of
  ; `dex-by-cline`, but the same circuitous combination would work.
  ; Nevertheless, we provide this operation directly.
  (def-func! "cexpr-dex-struct" main-tag-name projections
    (begin (verify-cexpr-struct-args! main-tag-name projections)
    #/sink-cexpr #/cexpr-dex-struct main-tag-name projections))
  
  ; TODO: Implement a `dex-struct` macro that compiles to a
  ; `cexpr-dex-struct` expression. Perhaps this can be defined on the
  ; Cene side (in a prelude) rather than implemented in Racket, but
  ; there's no harm in implementing it in Racket at first.
  
  (def-func! "cexpr-cline-struct" main-tag-name projections
    (begin (verify-cexpr-struct-args! main-tag-name projections)
    #/sink-cexpr #/cexpr-cline-struct main-tag-name projections))
  
  ; TODO: Implement `cline-struct`.
  
  (def-func! "cexpr-merge-struct" main-tag-name projections
    (begin (verify-cexpr-struct-args! main-tag-name projections)
    #/sink-cexpr #/cexpr-merge-struct main-tag-name projections))
  
  ; TODO: Implement `merge-struct`.
  
  (def-func! "cexpr-fuse-struct" main-tag-name projections
    (begin (verify-cexpr-struct-args! main-tag-name projections)
    #/sink-cexpr #/cexpr-fuse-struct main-tag-name projections))
  
  ; TODO: Implement `fuse-struct`.
  
  ; TODO: The JavaScript version called this `cexpr-construct`. See
  ; which name we prefer.
  (def-func! "cexpr-struct" main-tag-name projections
    (begin (verify-cexpr-struct-args! main-tag-name projections)
    #/sink-cexpr #/cexpr-struct main-tag-name projections))
  
  ; TODO: Consider implementing the following.
  ;
  ;   cexpr-case
  ;   case
  ;   cexpr-call
  ;   c
  ;   constructor-tag
  ;   function-implementation-from-cexpr
  ;   constructor-glossary
  ;   procure-constructor-glossary-getdef
  ;   copy-function-implementations
  ;   committing-to-define-function-implementations
  ;   procure-function-definer
  ;   def-struct
  ;   defn
  ;   caselet
  ;   cast
  
  (def-macro! "fn" #/fn unique-name qualify text-input-stream then
    (sink-effects-read-bounded-ids-and-exprs
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream args
    #/expect (reverse args) (cons body rev-params)
      (cene-err "Expected a fn form to have a body expression")
    #/then unique-name qualify text-input-stream
    #/list-foldl (id-or-expr->cexpr body) rev-params #/fn body param
      (expect param
        (id-or-expr-id param-located-string param-qualified-name)
        (cene-err "Expected every parameter of a fn form to be an identifier")
      #/sink-cexpr-opaque-fn param-qualified-name body)))
  
  
  ; Tables
  
  (def-func! "dex-table" dex-val
    ; TODO: Implement this.
    (sink-dex 'TODO))
  
  (def-func! "merge-table" merge-val
    ; TODO: Implement this.
    'TODO)
  
  (def-func! "fuse-table" fuse-val
    ; TODO: Implement this.
    'TODO)
  
  (def-nullary-func! "table-empty" (sink-table #/table-empty))
  
  (def-func! "table-shadow" key maybe-val table
    (expect (sink-name? key) #t
      (cene-err "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err "Expected table to be a table")
    #/mat (unmake-sink-struct-maybe s-nothing maybe-val) (just #/list)
      (sink-table-put-maybe table key #/nothing)
    #/mat (unmake-sink-struct-maybe s-just maybe-val)
      (just #/list val)
      (sink-table-put-maybe table key #/just val)
    #/cene-err "Expected maybe-val to be a nothing or a just"))
  
  (def-func! "table-get" key table
    (expect (sink-name? key) #t
      (cene-err "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err "Expected table to be a table")
    #/w- result (sink-table-get-maybe table key)
    #/expect result (just result)
      (make-sink-struct s-nothing #/list)
    #/make-sink-struct s-just #/list result))
  
  (def-func! "table-map-fuse" table fuse key-to-operand
    ; TODO: Implement this.
    'TODO)
  
  (def-func! "table-sort" cline table
    ; TODO: Implement this.
    'TODO)
  
  
  ; Effects
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `no-effects`.
  ;
  ; TODO: See which name we prefer.
  ;
  (def-nullary-func! "effects-noop" (sink-effects-noop))
  
  ; TODO: Consider implementing the following.
  ;
  ;   fuse-effects
  ;   get-mode
  ;   assert-current-mode
  ;   later
  ;   make-promise-later
  ;   getdef
  ;   definer-define
  ;   committing-to-define
  
  
  ; Unit tests
  
  ; TODO: Consider implementing the following.
  ;
  ;   test-async
  
  
  ; Namespaces
  
  ; TODO: Consider implementing the following.
  ;
  ;   procure-sub-ns-table
  ;   procure-name
  ;   procure-contributed-element-getdef
  ;   procure-contribute-listener
  ;   procure-contributed-elements
  ;   nsset-empty
  ;   fuse-nsset-by-union
  ;   nsset-not
  ;   nsset-ns-descendants
  ;   contributing-only-to
  
  
  ; Macros
  
  ; TODO: Consider implementing the following. This is the list of
  ; macro-relevant operations from the JavaScript implementation of
  ; Cene, which has an s-expression-based macro system. Now that we're
  ; using a text-stream-based macro system here, several of these will
  ; be unnecessary.
  ;
  ;   istring-nil
  ;   istring-cons
  ;   foreign
  ;   scope
  ;   macro-occurrence
  ;   local-occurrence
  ;   constructor-occurrence
  ;   projection-occurrence
  ;   obtain-by-unqualified-name
  ;   obtain-by-qualified-name
  ;   obtain-directly
  ;   stx
  ;   stx-details-empty
  ;   stx-details-join
  ;   stx-details-macro-call
  ;   procure-claim
  ;   procure-macro-implementation-getdef
  ;   cexpr-var
  ;   cexpr-reified
  ;   cexpr-located
  ;   cexpr-let
  ;   let
  ;   eval-cexpr
  ;   compile-expression-later
  ;   read-all-force
  ;   def-macro
  
  
  ; Integers
  
  ; TODO: Implement this.
  (def-nullary-func! "cline-int" (sink-cline 'TODO))
  
  (def-nullary-func! "int-zero" (sink-int 0))
  
  (def-nullary-func! "int-one" (sink-int 1))
  
  ; TODO: Implement this.
  (def-nullary-func! "fuse-int-by-plus" (sink-fuse 'TODO))
  
  ; TODO: Implement this.
  (def-nullary-func! "fuse-int-by-times" (sink-fuse 'TODO))
  
  (def-func! "int-minus" minuend subtrahend
    (expect minuend (sink-int minuend)
      (cene-err "Expected minuend to be an int")
    #/expect subtrahend (sink-int subtrahend)
      (cene-err "Expected subtrahend to be an int")
    #/sink-int #/- minuend subtrahend))
  
  (def-func! "int-div-rounded-down" dividend divisor
    (expect dividend (sink-int dividend)
      (cene-err "Expected dividend to be an int")
    #/expect divisor (sink-int divisor)
      (cene-err "Expected divisor to be an int")
    #/mat divisor 0 (make-sink-struct s-nothing #/list)
    #/make-sink-struct s-just #/list
    #/let-values ([(q r) (quotient/remainder dividend divisor)])
    #/if (<= 0 r)
      (make-sink-struct s-carried #/list (sink-int q) (sink-int r))
    #/if (<= 0 divisor)
      (make-sink-struct s-carried
      #/list (sink-int #/- q 1) (sink-int #/+ r divisor))
      (make-sink-struct s-carried
      #/list (sink-int #/+ q 1) (sink-int #/- r divisor))))
  
  (def-data-struct! "carried" #/list "main" "carry")
  
  
  ; Strings
  
  ; TODO: Implement this.
  (def-nullary-func! "dex-string" (sink-dex 'TODO))
  
  (def-nullary-func! "string-empty" (sink-string ""))
  
  (def-func! "string-singleton" unicode-scalar
    (expect unicode-scalar (sink-int unicode-scalar)
      (cene-err "Expected unicode-scalar to be an int")
    #/expect
      (and
        (<= 0 unicode-scalar #x10FFFF)
        (not #/<= #xD800 unicode-scalar #xDFFF))
      #t
      (cene-err "Expected unicode-scalar to be in the range of valid Unicode scalars")
    #/sink-string #/list->string #/list #/integer->char
      unicode-scalar))
  
  (def-func! "string-append-later" a b then
    (expect a (sink-string a)
      (cene-err "Expected a to be a string")
    #/expect b (sink-string b)
      (cene-err "Expected b to be a string")
    #/make-sink-effects #/fn
    #/sink-effects-run!
    #/sink-call then #/sink-string #/string-append a b))
  
  ; TODO: Implement the macro `str`.
  
  (def-func! "string-length" string
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/sink-int #/string-length string))
  
  (def-func! "string-get-unicode-scalar" string start
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/expect start (sink-int start)
      (cene-err "Expected start to be an int")
    #/expect (<= 0 start) #t
      (cene-err "Expected start to be a nonnegative int")
    #/expect (< start #/string-length string) #t
      (cene-err "Expected start to be an int less than the length of string")
    #/sink-int #/char->integer #/string-ref string start))
  
  (def-func! "string-cut-later" string start stop then
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/expect start (sink-int start)
      (cene-err "Expected start to be an int")
    #/expect stop (sink-int stop)
      (cene-err "Expected stop to be an int")
    #/expect (<= 0 start) #t
      (cene-err "Expected start to be a nonnegative int")
    #/expect (<= start stop) #t
      (cene-err "Expected start to be an int no greater than stop")
    #/expect (<= stop #/string-length string) #t
      (cene-err "Expected stop to be an int no greater than the length of string")
    #/make-sink-effects #/fn
    #/sink-effects-run!
    #/sink-call then #/sink-string #/substring string start stop))
  
  
  ; Regexes
  
  ; TODO: Consider implementing the following.
  ;
  ;   regex-give-up
  ;   regex-empty
  ;   regex-if
  ;   regex-while
  ;   regex-until
  ;   regex-one-in-range
  ;   regex-one
  ;   regex-from-string
  ;   regex-one-in-string
  ;   optimize-regex-later
  ;   optimized-regex-match-later
  ;   regex-result-matched
  ;   regex-result-failed
  ;   regex-result-passed-end
  
  
  ; File I/O for simple builds
  
  ; TODO: Consider implementing the following.
  ;
  ;   encapsulated-string
  ;   cli-arguments
  ;   cli-input-directory
  ;   cli-output-directory
  ;   input-path-get
  ;   input-path-type
  ;   file-type-directory
  ;   file-type-blob
  ;   file-type-missing
  ;   input-path-directory-list
  ;   input-path-blob-utf-8
  ;   output-path-get
  ;   output-path-directory
  ;   output-path-blob-utf-8
  ;   cli-output-environment-variable-shadow
  
  
  ; FFI
  
  ; TODO: The JavaScript version of Cene has FFI operations for
  ; interacting with JavaScript, naturally. See if we should do
  ; something similar for interacting with Racket.
  
  
  
  (cene-runtime
    (sink-table defined-dexes)
    (sink-table defined-values)))
