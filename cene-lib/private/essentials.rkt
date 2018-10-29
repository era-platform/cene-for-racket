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


(require #/only-in racket/contract/base
  -> ->i any/c cons/c list/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define/generic)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-all list-any list-foldl list-foldr list-kv-map list-map)
(require #/only-in lathe-comforts/maybe
  just maybe-bind maybe/c maybe-map nothing)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in effection/order
  assocs->table-if-mutually-unique cline-exact-rational
  dex-exact-rational dex-immutable-string fuse-exact-rational-by-plus
  fuse-exact-rational-by-times)
(require #/only-in effection/order/base
  call-fuse call-merge cline-by-dex cline-default cline-give-up
  cline-opaque cline-result? cline-struct compare-by-cline
  compare-by-dex dex? dexable dex-cline dex-default dex-dex dex-fix
  dex-fuse dex-give-up dex-merge dex-name dex-opaque dex-struct
  dex-table fusable-function? fuse-by-merge fuse-opaque fuse-struct
  fuse-table in-cline? in-dex? get-dex-from-cline
  make-fusable-function make-ordering-private-gt
  make-ordering-private-lt merge-by-dex merge-opaque merge-struct
  merge-table name? name-of ordering-eq ordering-gt ordering-lt
  ordering-private? table? table-empty table-get table-map-fuse
  table-shadow table-sort)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe
  autoname-cline autoname-dex autoname-fuse autoname-merge cline
  cline-by-own-method-thorough-unchecked
  cline-by-own-method::get-method
  cline-by-own-method::raise-different-methods-error
  cline-fix-unchecked dex dexableof-unchecked
  dex-by-own-method-thorough-unchecked dex-by-own-method::get-method
  dex-by-own-method::raise-different-methods-error dex-fix-unchecked
  fuse fuse-by-own-method-thorough-unchecked
  fuse-by-own-method::get-method
  fuse-by-own-method::raise-cannot-get-output-method-error
  fuse-by-own-method::raise-different-input-methods-error
  fuse-by-own-method::raise-different-output-method-error
  fuse-fix-unchecked fuse-fusable-function-thorough-unchecked
  fuse-fusable-function::raise-cannot-combine-results-error
  fuse-fusable-function::arg-to-method gen:cline-internals
  gen:dex-internals gen:furge-internals merge
  merge-by-own-method-thorough-unchecked
  merge-by-own-method::get-method
  merge-by-own-method::raise-different-input-methods-error
  merge-by-own-method::raise-different-output-method-error
  merge-by-own-method::raise-cannot-get-output-method-error
  merge-fix-unchecked name table->sorted-list)

(require cene/private)
(require #/only-in cene/private/reader-utils
  id-or-expr->cexpr
  id-or-expr-id
  sink-effects-read-bounded-cexprs
  sink-effects-read-bounded-ids-and-exprs
  sink-effects-read-bounded-specific-number-of-cexprs
  sink-effects-read-leading-specific-number-of-cexprs
  sink-effects-read-leading-specific-number-of-identifiers
  sink-name-for-local-variable)
; NOTE: This is all the "essential operations" from
; `cene/private/textpat` except for the struct predicates and
; accessors. This also includes `textpat-one-not-in-string` and
; `textpat-star`.
(require #/only-in cene/private/textpat
  textpat? textpat-empty textpat-from-string textpat-give-up
  textpat-has-empty? textpat-if textpat-one textpat-one-in-range
  textpat-one-in-string textpat-one-not-in-string textpat-result?
  textpat-result-failed textpat-result-matched
  textpat-result-passed-end textpat-star textpat-until textpat-while
  optimized-textpat? optimized-textpat-match optimize-textpat)


(provide cene-runtime-essentials)



(define s-yep (core-sink-struct "yep" #/list "val"))
(define s-nope (core-sink-struct "nope" #/list "val"))

(define s-nil (core-sink-struct "nil" #/list))
(define s-cons (core-sink-struct "cons" #/list "first" "rest"))

(define s-assoc (core-sink-struct "assoc" #/list "key" "val"))

(define s-ordering-lt (core-sink-struct "ordering-lt" #/list))
(define s-ordering-eq (core-sink-struct "ordering-eq" #/list))
(define s-ordering-gt (core-sink-struct "ordering-gt" #/list))

(define s-dexable (core-sink-struct "dexable" #/list "dex" "val"))

(define s-struct-metadata
  (core-sink-struct "struct-metadata"
  #/list "main-tag-name" "projections"))

(define s-textpat-result-matched
  (core-sink-struct "textpat-result-matched" #/list "stop"))
(define s-textpat-result-failed
  (core-sink-struct "textpat-result-failed" #/list))
(define s-textpat-result-passed-end
  (core-sink-struct "textpat-result-passed-end" #/list))


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

; TODO: See if we should put something like this in Effection.
(define-syntax-rule (dexable-struct tag dexable-field ...)
  (dexable
    (dex-struct tag (dissect dexable-field (dexable dex val) dex) ...)
    (tag (dissect dexable-field (dexable dex val) val) ...)))

(define/contract (racket-boolean->sink racket-boolean)
  (-> boolean? sink?)
  (if racket-boolean
    (make-sink-struct s-yep #/list #/make-sink-struct s-nil #/list)
    (make-sink-struct s-nope #/list #/make-sink-struct s-nil #/list)))

(define/contract (sink-maybe->maybe-racket sink-maybe)
  (-> sink? #/maybe/c #/maybe/c sink?)
  (mat (unmake-sink-struct-maybe s-nothing sink-maybe) (just #/list)
    (just #/nothing)
  #/mat (unmake-sink-struct-maybe s-just sink-maybe) (just #/list val)
    (just #/just val)
  #/nothing))

(define/contract (racket-maybe->sink racket-maybe)
  (-> (maybe/c sink?) sink?)
  (mat racket-maybe (just val)
    (make-sink-struct s-just #/list val)
    (make-sink-struct s-nothing #/list)))

(define/contract (sink-list->maybe-racket sink-list)
  (-> sink? #/maybe/c #/listof sink?)
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

(define/contract (racket-list->sink racket-list)
  (-> (listof sink?) sink?)
  (list-foldr racket-list (make-sink-struct s-nil #/list)
  #/fn elem rest
    (make-sink-struct s-cons #/list elem rest)))

(define/contract (sink-valid-dexable->maybe-racket sink-dexable)
  (-> sink? #/unsafe:dexableof-unchecked sink?)
  (expect (unmake-sink-struct-maybe s-dexable sink-dexable)
    (just #/list dex val)
    (nothing)
  #/expect dex (sink-dex dex) (nothing)
  #/expect (in-dex? dex val) #t (nothing)
  #/dexable dex val))


(struct-easy
  (cene-struct-metadata tags proj-string-to-name proj-name-to-string))

(define/contract (verify-sink-struct-metadata! sink-metadata)
  (-> sink? cene-struct-metadata?)
  (expect (unmake-sink-struct-maybe s-struct-metadata sink-metadata)
    (just #/list main-tag-name projs)
    (cene-err "Expected a defined struct metadata entry to be a struct-metadata")
  #/expect main-tag-name (sink-name main-tag-name)
    (cene-err "Expected a defined struct metadata entry to have a main tag name that was a name")
  #/expect (sink-list->maybe-racket projs) (just projs)
    (cene-err "Expected a defined struct metadata entry to have a cons list of projections")
  #/w- projs
    (list-map projs #/fn entry
      (expect (unmake-sink-struct-maybe s-assoc entry)
        (just #/list proj-string proj-name)
        (cene-err "Expected a defined struct metadata entry to have a projection list where each entry was an assoc")
      #/expect proj-string (sink-string proj-string)
        (cene-err "Expected a defined struct metadata entry to have a projection list where each key was a string")
      #/expect proj-name (sink-name proj-name)
        (cene-err "Expected a defined struct metadata entry to have a projection list where each associated value was a name")
      #/w- proj-string-name
        (name-for-sink-string #/sink-string proj-string)
      #/list proj-string proj-string-name proj-name))
  #/expect
    (assocs->table-if-mutually-unique
    #/list-map projs #/dissectfn (list string string-name name)
      (cons string-name name))
    (just proj-string-to-name)
    (cene-err "Expected a defined struct metadata entry to have a projection list with mutually unique strings")
  #/expect
    (assocs->table-if-mutually-unique
    #/list-map projs #/dissectfn (list string string-name name)
      (cons name string))
    (just proj-name-to-string)
    (cene-err "Expected a defined struct metadata entry to have a projection list with mutually unique names")
  #/cene-struct-metadata
    (cons main-tag-name
    #/list-map projs #/dissectfn (list string string-name name) name)
    proj-string-to-name proj-name-to-string))

(define/contract (sink-name-for-struct-metadata inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n
    (list 'name:struct-metadata n)))

(define/contract
  (sink-effects-read-maybe-struct-metadata
    qualify text-input-stream then)
  (->
    sink?
    sink-text-input-stream?
    (->
      sink-text-input-stream?
      (maybe/c #/list/c sink-located-string? sink-name?)
      sink-effects?)
    sink-effects?)
  (sink-effects-read-maybe-identifier
    qualify text-input-stream sink-name-for-struct-metadata
  #/fn text-input-stream maybe-metadata-name
  #/expect maybe-metadata-name
    (just #/list located-string metadata-name)
    (then #/nothing)
  #/sink-effects-get (sink-authorized-name-get-name metadata-name)
  #/fn metadata
  #/then #/just #/verify-sink-struct-metadata! metadata))

(define/contract (struct-metadata-tags metadata)
  (-> cene-struct-metadata? #/listof name?)
  (dissect metadata (cene-struct-metadata tags _ _)
    tags))

(define/contract (struct-metadata-n-projs metadata)
  (-> cene-struct-metadata? #/listof name?)
  (dissect metadata
    (cene-struct-metadata (cons main-tag-name proj-names) _ _)
  #/length proj-names))


(struct-easy (sink-ordering-private racket-ordering-private)
  #:other #:methods gen:sink [])
(struct-easy (sink-cline cline)
  #:other #:methods gen:sink [])
(struct-easy (sink-merge merge)
  #:other #:methods gen:sink [])
(struct-easy (sink-fuse fuse)
  #:other #:methods gen:sink [])
(struct-easy (sink-int racket-int)
  #:other #:methods gen:sink [])
(struct-easy (sink-textpat racket-textpat)
  #:other #:methods gen:sink [])
(struct-easy (sink-optimized-textpat racket-optimized-textpat)
  #:other #:methods gen:sink [])


; Sorts `proj-tags` and `vals` to put them in a normalized order.
(define/contract (normalize-proj-tags-and-vals proj-tags vals)
  (->i ([proj-tags (listof name?)] [vals list?])
    #:pre (proj-tags vals) (= (length proj-tags) (length vals))
    [_ (list/c (listof name?) list?)])
  (expect
    (assocs->table-if-mutually-unique #/map cons proj-tags vals)
    (just projs-table)
    (error "Expected proj-tags to be a list of mutually unique names")
  #/w- entries (unsafe:table->sorted-list projs-table)
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
  (sink-struct-op-autoname tags fields name-tag autoname-field-method)
  (->i
    (
      [tags (listof name?)]
      [fields list?]
      [name-tag symbol?]
      [autoname-field-method (-> any/c any/c)])
    #:pre (tags fields) (= (length tags) (add1 #/length fields))
    [_ any/c])
  (dissect
    (normalize-tags-and-vals tags
    #/list-kv-map fields #/fn i field-method
      (list i #/autoname-field-method field-method))
    (list tags fields)
  #/list* name-tag tags fields))

(define/contract
  (sink-struct-op-autodex a-tags a-fields b-tags b-fields dex-field)
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
      #/sink-struct-op-autoname
        tags fields 'tag:dex-sink-struct unsafe:autoname-dex))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-sink-struct a-tags a-fields)
      #/dissect other (dex-internals-sink-struct b-tags b-fields)
      #/sink-struct-op-autodex a-tags a-fields b-tags b-fields
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

(define/contract (dex-sink-struct tags fields)
  (->i ([tags (listof name?)] [fields (listof dex?)])
    #:pre (tags fields) (= (length tags) (add1 #/length fields))
    [_ dex?])
  (unsafe:dex #/dex-internals-sink-struct tags fields))

(define/contract (sink-dex-struct tags fields)
  (->i ([tags (listof name?)] [fields (listof sink-dex?)])
    #:pre (tags fields) (= (length tags) (add1 #/length fields))
    [_ sink-dex?])
  (sink-dex #/dex-sink-struct tags
  #/list-map fields #/dissectfn (sink-dex dex-field)
    dex-field))

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
      #/sink-struct-op-autoname
        tags fields 'tag:cline-sink-struct unsafe:autoname-cline))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-sink-struct a-tags a-fields)
      #/dissect other (cline-internals-sink-struct b-tags b-fields)
      #/sink-struct-op-autodex a-tags a-fields b-tags b-fields
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
      #/sink-struct-op-autoname
        tags fields 'tag:furge-sink-struct autoname-furge))
    
    (define (furge-internals-autodex this other)
      (dissect this
        (furge-internals-sink-struct _ dex-furge _ a-tags a-fields)
      #/dissect other
        (furge-internals-sink-struct _ _ _ b-tags b-fields)
      #/sink-struct-op-autodex a-tags a-fields b-tags b-fields
        dex-furge))
    
    (define (furge-internals-call this a b)
      (dissect this
        (furge-internals-sink-struct _ _ call-furge tags fields)
      #/maybe-bind (unmake-sink-struct-maybe tags a) #/fn as
      #/maybe-bind (unmake-sink-struct-maybe tags b) #/fn bs
      #/w- n (length fields)
      #/w-loop next as as bs bs fields fields rev-furged (list)
        (expect fields (cons furge-field fields)
          (just #/make-sink-struct tags #/reverse rev-furged)
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


(struct-easy (fuse-internals-effects)
  #:other
  
  #:methods unsafe:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-effects)
    
    (define (furge-internals-autoname this)
      'tag:fuse-effects)
    
    (define (furge-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (furge-internals-call this a b)
      (expect (sink-effects? a) #t (nothing)
      #/expect (sink-effects? b) #t (nothing)
      #/just #/sink-effects-fuse a b))
  ])


(struct-easy (cexpr-case subject-expr tags vars then-expr else-expr)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this
        (cexpr-case subject-expr tags vars then-expr else-expr)
        (error "Expected this to be a cexpr-case")
      #/or
        (-has-free-vars? subject-expr env)
        (-has-free-vars? then-expr #/list-foldl env vars #/fn env var
          (table-shadow var (just #/trivial) env))
        (-has-free-vars? else-expr env)))
    
    (define (cexpr-eval this env)
      (expect this
        (cexpr-case subject-expr tags vars then-expr else-expr)
        (error "Expected this to be a cexpr-case")
      #/w- subject (-eval subject-expr env)
      #/mat (unmake-sink-struct-maybe tags subject) (just vals)
        (-eval then-expr
        #/list-foldl env (map list vars vals) #/fn env entry
          (dissect entry (list var val)
          #/table-shadow var (just val) env))
        (-eval else-expr env)))
  ])

(define/contract
  (sink-cexpr-case subject-expr tags vars then-expr else-expr)
  (->i
    (
      [subject-expr sink-cexpr?]
      [tags (listof name?)]
      [vars (listof sink-name?)]
      [then-expr sink-cexpr?]
      [else-expr sink-cexpr?])
    #:pre (tags vars) (= (length tags) (add1 #/length vars))
    [_ sink-cexpr?])
  (dissect subject-expr (sink-cexpr subject-expr)
  #/dissect then-expr (sink-cexpr then-expr)
  #/dissect else-expr (sink-cexpr else-expr)
  #/w- vars (list-map vars #/dissectfn (sink-name var) var)
  #/sink-cexpr
  #/cexpr-case subject-expr tags vars then-expr else-expr))

(struct-easy (fix-for-sink-dex-list dex-elem)
  #:other
  
  #:property prop:procedure
  (fn this dex
    (dissect this (fix-for-sink-dex-list dex-elem)
    #/dex-default
      (dex-sink-struct s-nil #/list)
      (dex-sink-struct s-cons #/list dex-elem this))))

(define/contract (sink-dex-list dex-elem)
  (-> sink-dex? sink-dex?)
  (dissect dex-elem (sink-dex dex-elem)
  #/sink-dex #/dex-fix #/dexable-struct fix-for-sink-dex-list
  #/dexable (dex-dex) dex-elem))

(define/contract (sink-dex-name)
  (-> sink-dex?)
  (sink-dex #/dex-struct sink-name #/dex-name))

(define/contract (sink-dex-string)
  (-> sink-dex?)
  (sink-dex #/dex-struct sink-string #/dex-immutable-string))

(define/contract (sink-dex-table dex-val)
  (-> sink-dex? sink-dex?)
  (dissect dex-val (sink-dex dex-val)
  #/sink-dex #/dex-struct sink-table #/dex-table dex-val))


(define-syntax-rule
  (define-fix-converter converter constructor error-message)
  (struct-easy (converter unwrap)
    #:other
    
    #:property prop:procedure
    (fn this x
      (dissect this (converter unwrap)
      #/expect (sink-call unwrap x) (constructor result)
        (cene-err error-message)
        result))))

(define-fix-converter converter-for-dex-fix sink-dex
  "Expected the result of a dex-fix body to be a dex")
(define-fix-converter converter-for-cline-fix sink-cline
  "Expected the result of a cline-fix body to be a cline")
(define-fix-converter converter-for-merge-fix sink-merge
  "Expected the result of a merge-fix body to be a merge")
(define-fix-converter converter-for-fuse-fix sink-fuse
  "Expected the result of a fuse-fix body to be a fuse")

(struct-easy (sink-dex-by-own-method-unthorough get-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-dex-by-own-method-unthorough get-method)
    #/mat command
      (unsafe:dex-by-own-method::raise-different-methods-error
        a b a-method b-method)
      (cene-err "Obtained two different methods from the two values being compared")
    #/dissect command (unsafe:dex-by-own-method::get-method source)
    #/expect (sink-call get-method source) (sink-dex method)
      (cene-err "Expected the result of a dex-by-own-method body to be a dex")
      method)))

(struct-easy (sink-cline-by-own-method-unthorough get-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-cline-by-own-method-unthorough get-method)
    #/mat command
      (unsafe:cline-by-own-method::raise-different-methods-error
        a b a-method b-method)
      (cene-err "Obtained two different methods from the two values being compared")
    #/dissect command (unsafe:cline-by-own-method::get-method source)
    #/expect (sink-call get-method source) (sink-cline method)
      (cene-err "Expected the result of a cline-by-own-method body to be a cline")
      method)))

(struct-easy (sink-merge-by-own-method-unthorough get-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-merge-by-own-method-unthorough get-method)
    #/mat command
      (unsafe:merge-by-own-method::raise-different-input-methods-error
        a b a-method b-method)
      (cene-err "Obtained two different methods from the two input values")
    #/mat command
      (unsafe:merge-by-own-method::raise-cannot-get-output-method-error
        a b result input-method)
      (cene-err "Could not obtain a method from the result value")
    #/mat command
      (unsafe:merge-by-own-method::raise-different-output-method-error
        a b result input-method output-method)
      (cene-err "Obtained two different methods from the input and the output")
    #/dissect command (unsafe:merge-by-own-method::get-method source)
    #/expect (sink-call get-method source) (sink-merge method)
      (cene-err "Expected the result of a merge-by-own-method body to be a merge")
      method)))

(struct-easy (sink-fuse-by-own-method-unthorough get-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-fuse-by-own-method-unthorough get-method)
    #/mat command
      (unsafe:fuse-by-own-method::raise-different-input-methods-error
        a b a-method b-method)
      (cene-err "Obtained two different methods from the two input values")
    #/mat command
      (unsafe:fuse-by-own-method::raise-cannot-get-output-method-error
        a b result input-method)
      (cene-err "Could not obtain a method from the result value")
    #/mat command
      (unsafe:fuse-by-own-method::raise-different-output-method-error
        a b result input-method output-method)
      (cene-err "Obtained two different methods from the input and the output")
    #/dissect command (unsafe:fuse-by-own-method::get-method source)
    #/expect (sink-call get-method source) (sink-fuse method)
      (cene-err "Expected the result of a fuse-by-own-method body to be a fuse")
      method)))

(struct-easy (sink-fuse-fusable-fn-unthorough arg-to-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-fuse-fusable-fn-unthorough arg-to-method)
    #/mat command
      (unsafe:fuse-fusable-function::raise-cannot-combine-results-error
        method a b a-result b-result)
      (cene-err "Could not combine the result values")
    #/dissect command
      (unsafe:fuse-fusable-function::arg-to-method arg)
    #/expect (sink-call arg-to-method arg) (sink-fuse method)
      (cene-err "Expected the result of a fuse-fusable-fn body to be a fuse")
      method)))


(define str-prim-pat
  (optimize-textpat
  #/textpat-star #/textpat-one-not-in-string "[]()"))


; TODO: Use this in some kind of CLI entrypoint or something.
;
; TODO BUILTINS: See if we should add something like this as a Cene
; built-in. Yes, a Cene built-in that installs the Cene built-ins.
;
(define/contract (cene-runtime-essentials)
  (-> cene-runtime?)
  
  (define defined-dexes (table-empty))
  (define defined-values (table-empty))
  (define init-package-steps (list))
  
  (define/contract (def-dexable-value-for-lang-impl! name dex value)
    (-> sink-authorized-name? sink-dex? sink? void?)
    (dissect name (sink-authorized-name name)
    #/begin
      (set! defined-dexes
        (table-shadow name (just dex) defined-dexes))
      (set! defined-values
        (table-shadow name (just value) defined-values))
    #/void))
  
  (define/contract (def-dexable-value-for-package! name dex value)
    (-> sink-name? sink-dex? sink? void?)
    (set! init-package-steps
      (cons
        (fn qualify-for-package
          (sink-effects-put (qualify-for-package name) dex value))
        init-package-steps)))
  
  (define/contract (def-value-for-lang-impl! name value)
    (-> sink-authorized-name? sink? void?)
    (def-dexable-value-for-lang-impl!
      name (sink-dex #/dex-give-up) value))
  
  (define/contract (def-value-for-package! name value)
    (-> sink-name? sink? void?)
    (def-dexable-value-for-package!
      name (sink-dex #/dex-give-up) value))
  
  (define/contract (macro-impl body)
    (->
      (->
        sink-authorized-name? sink? sink-text-input-stream?
        sink-cexpr-sequence-output-stream?
        (->
          sink-authorized-name? sink? sink-text-input-stream?
          sink-cexpr-sequence-output-stream?
          sink-effects?)
        sink-effects?)
      sink?)
    (sink-fn-curried 5 #/fn
      unique-name qualify text-input-stream output-stream then
      
      (expect (sink-authorized-name? unique-name) #t
        (cene-err "Expected unique-name to be an authorized name")
      #/expect (sink-text-input-stream? text-input-stream) #t
        (cene-err "Expected text-input-stream to be a text input stream")
      #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
        (cene-err "Expected output-stream to be an expression sequence output stream")
      #/body unique-name qualify text-input-stream output-stream
      #/fn unique-name qualify text-input-stream output-stream
      #/w- effects
        (sink-call then
          unique-name qualify text-input-stream output-stream)
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
      
      (sink-effects-read-bounded-specific-number-of-cexprs
        unique-name qualify text-input-stream n-args
      #/fn unique-name qualify text-input-stream args
      #/sink-effects-cexpr-write output-stream (body args)
      #/fn output-stream
      #/then unique-name qualify text-input-stream output-stream)))
  
  (define/contract
    (def-func-impl-reified!
      qualified-main-tag-name qualified-proj-tag-names impl)
    (-> sink-authorized-name? sink-table? sink? void?)
    (def-value-for-lang-impl!
      (sink-authorized-name-for-function-implementation-code
        qualified-main-tag-name qualified-proj-tag-names)
      (sink-cexpr-reified impl))
    (def-value-for-lang-impl!
      (sink-authorized-name-for-function-implementation-value
        qualified-main-tag-name qualified-proj-tag-names)
      impl))
  
  
  (define/contract
    (def-func-verbose! main-tag-string n-args racket-func)
    (-> immutable-string? exact-positive-integer? procedure? void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify-for-lang-impl
      #/sink-name-for-struct-main-tag main-tag-name)
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (def-value-for-package!
        (sink-name-for-bounded-cexpr-op main-tag-name)
        
        ; Given precisely `n-args` cexprs, we construct a cexpr that
        ; first constructs a nullary struct with tag
        ; `qualified-main-tag-name` and then calls it with the given
        ; cexprs one by one.
        ;
        ; The JavaScript implementation of Cene doesn't verify the
        ; number of arguments to a function; instead it just passes in
        ; all the arguments it gets. But I find it's common for me to
        ; accidentally omit arguments or include extra arguments, so
        ; in `sink-effects-read-bounded-specific-number-of-cexprs`, we
        ; do some error-checking as an ad hoc line of defense against
        ; that kind of mistake.
        ;
        (macro-impl-specific-number-of-args n-args #/fn args
          (list-foldl
            (sink-cexpr-construct qualified-main-tag-name #/list)
            args
          #/fn func arg #/sink-cexpr-call func arg)))
      
      ; We define a Cene struct function implementation containing
      ; the function's run time behavior.
      (def-func-impl-reified!
        qualified-main-tag-name
        (sink-table #/table-empty)
        (sink-opaque-fn #/fn struct-value
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
    (-> immutable-string? sink? void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify-for-lang-impl
      #/sink-name-for-struct-main-tag main-tag-name)
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (def-value-for-package!
        (sink-name-for-bounded-cexpr-op main-tag-name)
        
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
            (sink-cexpr-construct qualified-main-tag-name #/list)
            (make-sink-cexpr-construct s-trivial #/list))))
      
      ; We define a Cene struct function implementation containing
      ; the function's run time behavior.
      (def-func-impl-reified!
        qualified-main-tag-name
        (sink-table #/table-empty)
        (sink-fn-curried 2 #/fn struct-value arg
          (expect (unmake-sink-struct-maybe s-trivial arg)
            (just #/list)
            (cene-err "Expected the argument to a nullary function to be a trivial")
            result)))
      
      ))
  
  (define/contract (def-data-struct! main-tag-string proj-strings)
    (-> immutable-string? (listof immutable-string?) void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify-for-lang-impl
      #/sink-name-for-struct-main-tag main-tag-name)
    #/w- qualified-main-tag-unauthorized-name
      (sink-authorized-name-get-name qualified-main-tag-name)
    #/w- qualified-proj-name-entries
      (list-map proj-strings #/fn proj-string
        (list proj-string
        #/sink-name-qualify-for-lang-impl
        #/sink-name-for-struct-proj
          qualified-main-tag-unauthorized-name
        #/sink-name-for-string #/sink-string proj-string))
    #/w- qualified-proj-names
      (list-map qualified-proj-name-entries
      #/dissectfn (list proj-string qualified-proj-name)
        qualified-proj-name)
    #/expect
      (assocs->table-if-mutually-unique
      #/list-map qualified-proj-names
      #/dissectfn (sink-authorized-name proj-name)
        (cons proj-name #/sink-authorized-name proj-name))
      (just qualified-proj-names-table)
      (error "Expected the projection strings to be mutually unique")
    #/begin
      
      ; We define a reader macro so that the user can write code that
      ; compiles into an expression that constructs a struct with this
      ; tag.
      (def-value-for-package!
        (sink-name-for-bounded-cexpr-op main-tag-name)
        
        (w- n-projs (length qualified-proj-names)
        
        ; Given precisely `n-projs` cexprs, we construct a cexpr that
        ; constructs a struct.
        ;
        ; The JavaScript implementation of Cene doesn't verify that
        ; the number of arguments to a struct constructor is under a
        ; certain amount; instead it just passes all the excess
        ; arguments as function arguments. I find it's common for me
        ; to accidentally omit arguments or include extra arguments,
        ; so in `sink-effects-read-bounded-specific-number-of-cexprs`,
        ; we do some error-checking as an ad hoc line of defense
        ; against that kind of mistake.
        ;
        #/macro-impl-specific-number-of-args n-projs #/fn proj-cexprs
          (sink-cexpr-construct qualified-main-tag-name
          #/map list qualified-proj-names proj-cexprs)))
      
      ; We define a Cene struct function implementation which throws
      ; an error. We do this so that we do in fact have a function
      ; implementation for every struct we use, which might be an
      ; invariant that comes in handy. (TODO: See if it does.)
      ;
      (def-func-impl-reified!
        qualified-main-tag-name
        (sink-table qualified-proj-names-table)
        (sink-opaque-fn #/fn struct-value
          (cene-err "Called a struct that wasn't intended for calling")))
      
      ; We also define something we can use to look up a qualified
      ; main tag name and an ordered list of qualified and unqualified
      ; projection names, given the `sink-name-for-string` name the
      ; main tag name is made from.
      ;
      ; TODO: We haven't even tried to store this in the same format
      ; as the JavaScript version of Cene does. See if we should.
      ;
      (def-dexable-value-for-package!
        (sink-name-for-struct-metadata main-tag-name)
        (sink-dex-struct s-struct-metadata #/list
          (sink-dex-name)
          (sink-dex-list #/sink-dex-struct s-assoc #/list
            (sink-dex-string)
            (sink-dex-name)))
        (make-sink-struct s-struct-metadata #/list
          qualified-main-tag-unauthorized-name
          (racket-list->sink #/list-map qualified-proj-name-entries
          #/dissectfn (list proj-string qualified-proj-name)
            (make-sink-struct s-assoc #/list
              (sink-string proj-string)
              (sink-authorized-name-get-name qualified-proj-name)))))
      
      ))
  
  (define/contract (def-macro! name-string body)
    (->
      immutable-string?
      (->
        sink-authorized-name? sink? sink-text-input-stream?
        (->
          sink-authorized-name? sink? sink-text-input-stream?
          sink-cexpr?
          sink-effects?)
        sink-effects?)
      void?)
    (def-value-for-package!
      (sink-name-for-bounded-cexpr-op
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
  (def-value-for-package! (sink-name-for-nameless-bounded-cexpr-op)
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
  (def-value-for-package!
    (sink-name-for-freestanding-cexpr-op
    #/sink-name-for-string #/sink-string "=")
    (macro-impl #/fn
      unique-name qualify text-input-stream output-stream then
      
      (sink-effects-claim-freshen unique-name #/fn unique-name
      #/sink-effects-read-non-line-breaks text-input-stream
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
  
  ; TODO BUILTINS: Implement the macro `err`, probably in a Cene
  ; prelude. In the prelude, before we define `err`, we can still
  ; report errors using using
  ; `[follow-heart/clamor-err/str-prim ...]`.
  
  
  ; Order
  
  (def-data-struct! "ordering-lt" #/list)
  (def-data-struct! "ordering-eq" #/list)
  (def-data-struct! "ordering-gt" #/list)
  
  (def-func! "is-ordering-private" v
    (racket-boolean->sink #/sink-ordering-private? v))
  
  (def-nullary-func! "make-ordering-private-lt"
    (sink-ordering-private #/make-ordering-private-lt))
  
  (def-nullary-func! "make-ordering-private-gt"
    (sink-ordering-private #/make-ordering-private-gt))
  
  (def-func! "in-dex" dex v
    (expect dex (sink-dex dex)
      (cene-err "Expected dex to be a dex")
    #/racket-boolean->sink #/in-dex? dex v))
  
  (def-func! "name-of" dex v
    (expect dex (sink-dex dex)
      (cene-err "Expected dex to be a dex")
    #/racket-maybe->sink #/maybe-map (name-of dex v) #/fn name
      (sink-name name)))
  
  ; NOTE: In the JavaScript version of Cene, this was called
  ; `call-dex`.
  (def-func! "compare-by-dex" dex a b
    (expect dex (sink-dex dex)
      (cene-err "Expected dex to be a dex")
    #/racket-maybe->sink
    #/maybe-map (compare-by-dex dex a b) #/fn dex-result
      (if (ordering-private? dex-result)
        (sink-ordering-private dex-result)
      #/dissect dex-result (ordering-eq)
      #/make-sink-struct s-ordering-eq #/list)))
  
  (def-data-struct! "dexable" #/list "dex" "val")
  
  (def-nullary-func! "dex-name"
    (sink-dex #/dex-struct sink-name #/dex-name))
  
  (def-nullary-func! "dex-dex"
    (sink-dex #/dex-struct sink-dex #/dex-dex))
  
  (def-nullary-func! "dex-give-up" (sink-dex #/dex-give-up))
  
  (def-func! "dex-default" dex-for-trying-first dex-for-trying-second
    (expect dex-for-trying-first (sink-dex dex-for-trying-first)
      (cene-err "Expected dex-for-trying-first to be a dex")
    #/expect dex-for-trying-second (sink-dex dex-for-trying-second)
      (cene-err "Expected dex-for-trying-second to be a dex")
    #/sink-dex
    #/dex-default dex-for-trying-first dex-for-trying-second))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "dex-opaque" name dex
    (expect name (sink-name name)
      (cene-err "Expected name to be a name")
    #/expect dex (sink-dex dex)
      (cene-err "Expected dex to be a dex")
    #/sink-dex #/dex-opaque name dex))
  
  (def-func! "dex-by-own-method" dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err "Expected dexable-get-method to be a valid dexable")
    #/sink-dex #/unsafe:dex-by-own-method-thorough-unchecked
    #/dexable-struct sink-dex-by-own-method-unthorough
      dexable-get-method))
  
  (def-func! "dex-fix" dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err "Expected dexable-unwrap to be a valid dexable")
    #/sink-dex #/unsafe:dex-fix-unchecked
    #/dexable-struct converter-for-dex-fix dexable-unwrap))
  
  ; NOTE: In the JavaScript version of Cene, there was a similar
  ; operation called `dex-by-cline`.
  (def-func! "get-dex-from-cline" cline
    (expect cline (sink-cline cline)
      (cene-err "Expected cline to be a cline")
    #/sink-dex #/get-dex-from-cline cline))
  
  (def-func! "in-cline" cline v
    (expect cline (sink-cline cline)
      (cene-err "Expected cline to be a cline")
    #/racket-boolean->sink #/in-cline? cline v))
  
  ; NOTE: In the JavaScript version of Cene, this was called
  ; `call-cline`.
  (def-func! "compare-by-cline" cline a b
    (expect cline (sink-cline cline)
      (cene-err "Expected cline to be a cline")
    #/racket-maybe->sink
    #/maybe-map (compare-by-cline cline a b) #/fn cline-result
      (if (ordering-private? cline-result)
        (sink-ordering-private cline-result)
      #/mat cline-result (ordering-lt)
        (make-sink-struct s-ordering-lt #/list)
      #/mat cline-result (ordering-gt)
        (make-sink-struct s-ordering-gt #/list)
      #/dissect cline-result (ordering-eq)
      #/make-sink-struct s-ordering-eq #/list)))
  
  (def-nullary-func! "dex-cline"
    (sink-dex #/dex-struct sink-cline #/dex-cline))
  
  (def-func! "cline-by-dex" dex
    (expect dex (sink-dex dex)
      (cene-err "Expected dex to be a dex")
    #/sink-cline #/cline-by-dex dex))
  
  (def-nullary-func! "cline-give-up" (sink-cline #/cline-give-up))
  
  (def-func! "cline-default"
    cline-for-trying-first cline-for-trying-second
    
    (expect cline-for-trying-first (sink-cline cline-for-trying-first)
      (cene-err "Expected cline-for-trying-first to be a cline")
    #/expect cline-for-trying-second (sink-cline cline-for-trying-second)
      (cene-err "Expected cline-for-trying-second to be a cline")
    #/sink-cline
    #/cline-default cline-for-trying-first cline-for-trying-second))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "cline-opaque" name cline
    (expect name (sink-name name)
      (cene-err "Expected name to be a name")
    #/expect cline (sink-cline cline)
      (cene-err "Expected cline to be a cline")
    #/sink-cline #/cline-opaque name cline))
  
  (def-func! "cline-by-own-method" dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err "Expected dexable-get-method to be a valid dexable")
    #/sink-cline #/unsafe:cline-by-own-method-thorough-unchecked
    #/dexable-struct sink-cline-by-own-method-unthorough
      dexable-get-method))
  
  (def-func! "cline-fix" dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err "Expected dexable-unwrap to be a valid dexable")
    #/sink-cline #/unsafe:cline-fix-unchecked
    #/dexable-struct converter-for-cline-fix dexable-unwrap))
  
  (def-func! "call-merge" merge a b
    (expect merge (sink-merge merge)
      (cene-err "Expected merge to be a merge")
    #/racket-maybe->sink #/call-merge merge a b))
  
  (def-func! "call-fuse" fuse a b
    (expect fuse (sink-fuse fuse)
      (cene-err "Expected fuse to be a fuse")
    #/racket-maybe->sink #/call-fuse fuse a b))
  
  (def-nullary-func! "dex-merge"
    (sink-dex #/dex-struct sink-merge #/dex-merge))
  
  (def-nullary-func! "dex-fuse"
    (sink-dex #/dex-struct sink-fuse #/dex-fuse))
  
  ; NOTE: We do not implement operations like `merge-default` or
  ; `fuse-default` from the JavaScript version of Cene since they are
  ; not well-behaved furges.
  
  (def-func! "merge-by-dex" dex
    (expect dex (sink-dex dex)
      (cene-err "Expected dex to be a dex")
    #/sink-merge #/merge-by-dex dex))
  
  (def-func! "fuse-by-merge" merge
    (expect merge (sink-merge merge)
      (cene-err "Expected merge to be a merge")
    #/sink-fuse #/fuse-by-merge merge))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "merge-opaque" name merge
    (expect name (sink-name name)
      (cene-err "Expected name to be a name")
    #/expect merge (sink-merge merge)
      (cene-err "Expected merge to be a merge")
    #/sink-merge #/merge-opaque name merge))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "fuse-opaque" name fuse
    (expect name (sink-name name)
      (cene-err "Expected name to be a name")
    #/expect fuse (sink-fuse fuse)
      (cene-err "Expected fuse to be a fuse")
    #/sink-fuse #/fuse-opaque name fuse))
  
  (def-func! "merge-by-own-method" dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err "Expected dexable-get-method to be a valid dexable")
    #/sink-merge #/unsafe:merge-by-own-method-thorough-unchecked
    #/dexable-struct sink-merge-by-own-method-unthorough
      dexable-get-method))
  
  (def-func! "fuse-by-own-method" dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err "Expected dexable-get-method to be a valid dexable")
    #/sink-fuse #/unsafe:fuse-by-own-method-thorough-unchecked
    #/dexable-struct sink-fuse-by-own-method-unthorough
      dexable-get-method))
  
  (def-func! "merge-fix" dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err "Expected dexable-unwrap to be a valid dexable")
    #/sink-merge #/unsafe:merge-fix-unchecked
    #/dexable-struct converter-for-merge-fix dexable-unwrap))
  
  (def-func! "fuse-fix" dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err "Expected dexable-unwrap to be a valid dexable")
    #/sink-fuse #/unsafe:fuse-fix-unchecked
    #/dexable-struct converter-for-fuse-fix dexable-unwrap))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-fusable-fn" v
    (racket-boolean->sink
    #/expect v (sink-opaque-fn v) #f
    #/fusable-function? v))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "make-fusable-fn" func
    (sink-opaque-fn #/make-fusable-function #/fn arg
      (sink-call func arg)))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "fuse-fusable-fn" dexable-arg-to-method
    (expect (sink-valid-dexable->maybe-racket dexable-arg-to-method)
      (just dexable-arg-to-method)
      (cene-err "Expected dexable-arg-to-method to be a valid dexable")
    #/sink-fuse #/fuse-struct sink-opaque-fn
    #/unsafe:fuse-fusable-function-thorough-unchecked
    #/dexable-struct sink-fuse-fusable-fn-unthorough
      dexable-arg-to-method))
  
  
  ; Authorized names
  ;
  ; NOTE: The JavaScript version of Cene doesn't have any of these.
  
  ; TODO: See if we need authorized names to be dexable. If we do,
  ; expose a `dex-authorized-name` to Cene instead of this.
  (def-func! "is-authorized-name" v
    (racket-boolean->sink #/sink-authorized-name? v))
  
  (def-func! "authorized-name-get-name" name
    (expect (sink-authorized-name? name) #t
      (cene-err "Expected name to be an authorized name")
    #/sink-authorized-name-get-name name))
  
  (def-func! "name-subname" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-subname name))
  
  (def-func! "authorized-name-subname" name
    (expect (sink-authorized-name? name) #t
      (cene-err "Expected name to be an authorized name")
    #/sink-authorized-name-subname name))
  
  
  ; Structs and function calls
  
  ; NOTE: In the JavaScript version of Cene, this was called
  ; `constructor-glossary` with projections `main-tag` and
  ; `source-to-rep`. The representation was otherwise the same except
  ; that the keys of the `source-to-rep`/`projections` assoc list were
  ; names, and now we store them as strings.
  (def-data-struct! "struct-metadata"
    (list "main-tag-name" "projections"))
  
  (define/contract
    (verify-cexpr-struct-args! main-tag-name projections)
    (-> sink? sink?
      (list/c sink-name? #/listof #/list/c name? cexpr?))
    
    (expect main-tag-name (sink-authorized-name main-tag-name)
      (cene-err "Expected main-tag-name to be an authorized name")
    #/expect (sink-list->maybe-racket projections) (just projections)
      (cene-err "Expected projections to be a list made up of cons and nil values")
    #/w- projections
      (list-map projections #/fn projection
        (expect (unmake-sink-struct-maybe s-assoc projection)
          (just #/list k v)
          (cene-err "Expected projections to be a list of assoc values")
        #/expect k (sink-authorized-name k)
          (cene-err "Expected projections to be an association list with authorized names as keys")
        #/expect v (sink-cexpr v)
          (cene-err "Expected projections to be an association list with expressions as values")
        #/list k v))
    #/if
      (names-have-duplicate?
      #/list-map projections #/dissectfn (list k v) k)
      (cene-err "Expected projections to be an association list with mutually unique names as keys")
    #/list main-tag-name projections))
  
  (define/contract
    (sink-effects-expand-struct-op
      unique-name qualify text-input-stream then)
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (->
        sink-authorized-name?
        sink?
        sink-text-input-stream?
        (maybe/c #/list/c name? #/listof #/list/c name? cexpr?)
        sink-effects?)
      sink-effects?)
    
    (sink-effects-claim-freshen unique-name #/fn unique-name
    #/sink-effects-read-maybe-struct-metadata qualify text-input-stream
    #/fn text-input-stream maybe-metadata
    #/expect maybe-metadata (just metadata)
      (then unique-name qualify text-input-stream #/nothing)
    #/dissect (struct-metadata-tags metadata)
      (cons main-tag-name proj-names)
    
    #/sink-effects-read-bounded-specific-number-of-cexprs
      unique-name qualify text-input-stream (length proj-names)
    #/fn unique-name qualify text-input-stream proj-exprs
    
    #/then unique-name qualify text-input-stream
    #/just #/list main-tag-name #/map list proj-names proj-exprs))
  
  ; NOTE: The JavaScript version of Cene makes this functionality
  ; possible using a combination of `cexpr-cline-struct`,
  ; `cline-by-dex`, and `dex-by-cline`. We will probably be offering
  ; `get-dex-by-cline` (as provided by Effection) instead of
  ; `dex-by-cline`, but the same circuitous combination would work.
  ; Nevertheless, we provide this operation directly.
  ;
  (def-func! "expr-dex-struct" main-tag-name projections
    (dissect (verify-cexpr-struct-args! main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-dex-struct main-tag-name projections))
  
  (def-macro! "dex-struct" #/fn
    unique-name qualify text-input-stream then
    
    (sink-effects-expand-struct-op
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream maybe-pieces
    #/expect maybe-pieces (just #/list main-tag-name projections)
      (cene-err "Expected a dex-struct form to designate a struct metadata name")
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-dex-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-cline-struct`.
  (def-func! "expr-cline-struct" main-tag-name projections
    (dissect (verify-cexpr-struct-args! main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-cline-struct main-tag-name projections))
  
  (def-macro! "cline-struct" #/fn
    unique-name qualify text-input-stream then
    
    (sink-effects-expand-struct-op
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream maybe-pieces
    #/expect maybe-pieces (just #/list main-tag-name projections)
      (cene-err "Expected a cline-struct form to designate a struct metadata name")
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-cline-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-merge-struct`.
  (def-func! "expr-merge-struct" main-tag-name projections
    (dissect (verify-cexpr-struct-args! main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-merge-struct main-tag-name projections))
  
  (def-macro! "merge-struct" #/fn
    unique-name qualify text-input-stream then
    
    (sink-effects-expand-struct-op
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream maybe-pieces
    #/expect maybe-pieces (just #/list main-tag-name projections)
      (cene-err "Expected a merge-struct form to designate a struct metadata name")
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-merge-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-fuse-struct`.
  (def-func! "expr-fuse-struct" main-tag-name projections
    (dissect (verify-cexpr-struct-args! main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-fuse-struct main-tag-name projections))
  
  (def-macro! "fuse-struct" #/fn
    unique-name qualify text-input-stream then
    
    (sink-effects-expand-struct-op
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream maybe-pieces
    #/expect maybe-pieces (just #/list main-tag-name projections)
      (cene-err "Expected a fuse-struct form to designate a struct metadata name")
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-fuse-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-struct`.
  (def-func! "expr-construct" main-tag-name projections
    (dissect (verify-cexpr-struct-args! main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-construct main-tag-name projections))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-macro! "construct" #/fn
    unique-name qualify text-input-stream then
    
    (sink-effects-expand-struct-op
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream maybe-pieces
    #/expect maybe-pieces (just #/list main-tag-name projections)
      (cene-err "Expected a construct form to designate a struct metadata name")
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-construct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-case`.
  (def-func! "expr-case"
    subject-expr main-tag-name projections then-expr else-expr
    (expect subject-expr (sink-cexpr subject-expr)
      (cene-err "Expected subject-expr to be an expression")
    #/expect main-tag-name (sink-authorized-name main-tag-name)
      (cene-err "Expected main-tag-name to be an authorized name")
    #/expect (sink-list->maybe-racket projections) (just projections)
      (cene-err "Expected projections to be a list made up of cons and nil values")
    #/w- projections
      (list-map projections #/fn projection
        (expect (unmake-sink-struct-maybe s-assoc projection)
          (just #/list k v)
          (cene-err "Expected projections to be a list of assoc values")
        #/expect k (sink-authorized-name k)
          (cene-err "Expected projections to be an association list with authorized names as keys")
        #/expect v (sink-name v)
          (cene-err "Expected projections to be an association list with names as values")
        #/list k v))
    #/if
      (names-have-duplicate?
      #/list-map projections #/dissectfn (list k v) k)
      (cene-err "Expected projections to be an association list with mutually unique authorized names as keys")
    #/if
      (names-have-duplicate?
      #/list-map projections #/dissectfn (list k v) v)
      (cene-err "Expected projections to be an association list with mutually unique names as values")
    #/expect then-expr (sink-cexpr then-expr)
      (cene-err "Expected then-expr to be an expression")
    #/expect else-expr (sink-cexpr else-expr)
      (cene-err "Expected else-expr to be an expression")
    #/sink-cexpr #/cexpr-case subject-expr
      (cons main-tag-name
      #/list-map projections #/dissectfn (list proj-name var)
        var)
      (list-map projections #/dissectfn (list proj-name var)
        proj-name)
      then-expr
      else-expr))
  
  (define/contract
    (sink-effects-read-case-pattern qualify text-input-stream then)
    (->
      sink?
      sink-text-input-stream?
      (->i
        (
          [text-input-stream sink-text-input-stream?]
          [tags (listof name?)]
          [vars (listof sink-name?)])
        #:pre (tags vars) (= (length tags) (add1 #/length vars))
        [_ sink-effects?])
      sink-effects?)
    
    (sink-effects-read-maybe-struct-metadata qualify text-input-stream
    #/fn text-input-stream maybe-metadata
    #/expect maybe-metadata (just metadata)
      (cene-err "Expected the first part of a case pattern to designate a struct metadata name")
    #/maybe-bind maybe-metadata #/fn metadata
    #/w- tags (struct-metadata-tags metadata)
    #/w- n-projs (struct-metadata-n-projs metadata)
    
    #/sink-effects-read-leading-specific-number-of-identifiers
      qualify text-input-stream n-projs sink-name-for-local-variable
    #/fn text-input-stream vars
    #/w- vars
      (list-map vars #/dissectfn (list located-string var)
        (sink-authorized-name-get-name var))
    #/if (sink-names-have-duplicate? vars)
      (cene-err "Expected the variables of a case pattern to be mutually unique")
    
    #/then text-input-stream tags vars))
  
  (def-macro! "case" #/fn unique-name qualify text-input-stream then
    
    (sink-effects-read-leading-specific-number-of-cexprs
      unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-subject
    #/dissect args-subject (list subject-expr)
    
    #/sink-effects-read-case-pattern qualify text-input-stream
    #/fn text-input-stream tags vars
    
    #/sink-effects-read-bounded-specific-number-of-cexprs
      unique-name qualify text-input-stream 2
    #/fn unique-name qualify text-input-stream args-branches
    #/dissect args-branches (list then-expr else-expr)
    
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-case subject-expr tags vars then-expr else-expr))
  
  (def-macro! "cast" #/fn unique-name qualify text-input-stream then
    
    (sink-effects-read-leading-specific-number-of-cexprs
      unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-subject
    #/dissect args-subject (list subject-expr)
    
    #/sink-effects-read-case-pattern qualify text-input-stream
    #/fn text-input-stream tags vars
    
    #/sink-effects-read-bounded-specific-number-of-cexprs
      unique-name qualify text-input-stream 2
    #/fn unique-name qualify text-input-stream args-branches
    #/dissect args-branches (list else-expr then-expr)
    
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-case subject-expr tags vars then-expr else-expr))
  
  (def-macro! "caselet" #/fn
    unique-name qualify text-input-stream then
    
    (sink-effects-read-leading-specific-number-of-identifiers
      qualify text-input-stream 1 sink-name-for-local-variable
    #/fn text-input-stream args-subject-var
    #/dissect args-subject-var (list #/list _ subject-var)
    #/w- subject-var (sink-authorized-name-get-name subject-var)
    
    #/sink-effects-read-leading-specific-number-of-cexprs
      unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-subject-expr
    #/dissect args-subject-expr (list subject-expr)
    
    #/sink-effects-read-case-pattern qualify text-input-stream
    #/fn text-input-stream tags vars
    
    #/sink-effects-read-bounded-specific-number-of-cexprs
      unique-name qualify text-input-stream 2
    #/fn unique-name qualify text-input-stream args-branches
    #/dissect args-branches (list then-expr else-expr)
    
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-let (list #/list subject-var subject-expr)
    #/sink-cexpr-case (sink-cexpr-var subject-var) tags vars
      then-expr
      else-expr))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-call`.
  (def-func! "expr-call" func-expr arg-expr
    (expect func-expr (sink-cexpr func-expr)
      (cene-err "Expected func-expr to be an expression")
    #/expect arg-expr (sink-cexpr arg-expr)
      (cene-err "Expected arg-expr to be an expression")
    #/sink-cexpr #/cexpr-call func-expr arg-expr))
  
  (def-macro! "c" #/fn unique-name qualify text-input-stream then
    
    (sink-effects-read-leading-specific-number-of-cexprs
      unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-func
    #/dissect args-func (list func-expr)
    
    #/sink-effects-read-bounded-cexprs
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream args-args
    
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/list-foldl func-expr args-args #/fn func arg
      (cexpr-call func arg)))
  
  ; NOTE BUILTINS: The following built-ins from the JavaScript version
  ; of Cene seem like they're not relevant to the approach we've taken
  ; here, since we're using a single global namespace instead of
  ; first-class namespaces.
  ;
  ;   constructor-tag
  ;   function-implementation-from-cexpr
  ;   procure-constructor-glossary-getdef
  ;   copy-function-implementations
  ;   committing-to-define-function-implementations
  ;   procure-function-definer
  
  ; TODO BUILTINS: Implement `def-struct`, probably in a Cene prelude.
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "expr-opaque-fn" param body
    (expect (sink-name? param) #t
      (cene-err "Expected param to be a name")
    #/expect (sink-cexpr? body) #t
      (cene-err "Expected body to be an expression")
    #/sink-cexpr-opaque-fn param body))
  
  ; TODO BUILTINS: Implement `defn`, probably in a Cene prelude.
  
  (def-macro! "fn" #/fn unique-name qualify text-input-stream then
    (sink-effects-read-bounded-ids-and-exprs
      unique-name qualify text-input-stream
      sink-name-for-local-variable
    #/fn unique-name qualify text-input-stream args
    #/expect (reverse args) (cons body rev-params)
      (cene-err "Expected a fn form to have a body expression")
    #/then unique-name qualify text-input-stream
    #/list-foldl (id-or-expr->cexpr body) rev-params #/fn body param
      (expect param
        (id-or-expr-id param-located-string param-qualified-name)
        (cene-err "Expected every parameter of a fn form to be an identifier")
      #/sink-cexpr-opaque-fn
        (sink-authorized-name-get-name param-qualified-name)
        body)))
  
  
  ; Tables
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-table" v
    (racket-boolean->sink #/sink-table? v))
  
  (def-func! "dex-table" dex-val
    (expect dex-val (sink-dex dex-val)
      (cene-err "Expected dex-val to be a dex")
    #/sink-dex-table dex-val))
  
  (def-func! "merge-table" merge-val
    (expect merge-val (sink-merge merge-val)
      (cene-err "Expected merge-val to be a merge")
    #/sink-merge #/merge-struct sink-table #/merge-table merge-val))
  
  (def-func! "fuse-table" fuse-val
    (expect fuse-val (sink-fuse fuse-val)
      (cene-err "Expected fuse-val to be a fuse")
    #/sink-fuse #/fuse-struct sink-table #/fuse-table fuse-val))
  
  (def-nullary-func! "table-empty" (sink-table #/table-empty))
  
  (def-func! "table-shadow" key maybe-val table
    (expect (sink-name? key) #t
      (cene-err "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err "Expected table to be a table")
    #/expect (sink-maybe->maybe-racket maybe-val) (just maybe-val)
      (cene-err "Expected maybe-val to be a nothing or a just")
    #/sink-table-put-maybe table key maybe-val))
  
  (def-func! "table-get" key table
    (expect (sink-name? key) #t
      (cene-err "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err "Expected table to be a table")
    #/racket-maybe->sink #/sink-table-get-maybe table key))
  
  (def-func! "table-map-fuse" table fuse key-to-operand
    (expect table (sink-table table)
      (cene-err "Expected table to be a table")
    #/expect fuse (sink-fuse fuse)
      (cene-err "Expected fuse to be a fuse")
    #/table-map-fuse table fuse #/fn k
      (sink-call key-to-operand #/sink-name k)))
  
  (def-func! "table-sort" cline table
    (expect cline (sink-cline cline)
      (cene-err "Expected cline to be a cline")
    #/expect table (sink-table table)
      (cene-err "Expected table to be a table")
    #/racket-maybe->sink
    #/maybe-map (table-sort cline table) #/fn ranks
      (racket-list->sink #/list-map ranks #/fn rank
        (sink-table rank))))
  
  
  ; Effects
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `no-effects`.
  ;
  ; TODO: See which name we prefer.
  ;
  (def-nullary-func! "effects-noop" (sink-effects-noop))
  
  (def-nullary-func! "fuse-effects"
    (sink-fuse #/unsafe:fuse #/fuse-internals-effects))
  
  ; NOTE BUILTINS: The following built-ins from the JavaScript version
  ; of Cene seem like they're not relevant to the approach we've taken
  ; here, since we're using a single global namespace instead of
  ; first-class namespaces.
  ;
  ;   get-mode
  ;   assert-current-mode
  
  (define/contract (verify-callback-effects! effects)
    (-> sink? sink-effects?)
    (expect (sink-effects? effects) #t
      (cene-err "Expected the return value of the callback to be an effects value")
      effects))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `later`, and it took an effects value rather than a function that
  ; computed one. When we needed to do what this does, we used
  ; `get-mode` and ignored the mode value.
  (def-func! "effects-later" get-effects
    (sink-effects-later #/fn
    #/verify-callback-effects!
    #/sink-call get-effects #/make-sink-struct s-trivial #/list))
  
  ; TODO BUILTINS: Consider implementing the following.
  ;
  ;   make-promise-later
  ;   getdef
  ;   definer-define
  ;   committing-to-define
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "effects-get" name then
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-effects-get name #/fn result
    #/verify-callback-effects! #/sink-call then result))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "effects-put" name dex value
    (expect (sink-authorized-name? name) #t
      (cene-err "Expected name to be an authorized name")
    #/expect (sink-dex? dex) #t
      (cene-err "Expected dex to be a dex")
    #/sink-effects-put name dex value))
  
  
  ; Unit tests
  
  ; TODO BUILTINS: Consider implementing the following.
  ;
  ;   test-async
  
  
  ; Namespaces
  
  ; TODO BUILTINS: Consider implementing something like the following.
  ; We're taking an approach where we're using a single global
  ; namespace instead of first-class namespaces, but we'll still want
  ; to do something like `contributing-only-to`, and we'll still want
  ; to have an open-world-assumption extensibility framework based on
  ; defining contributed elements and contributed listeners.
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
  
  ; NOTE BUILTINS: The following built-ins from the JavaScript version
  ; of Cene seem like they're not relevant to the approach we've taken
  ; here, since we're using a text-based rather than
  ; s-expression-based macro system.
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
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-var`.
  (def-func! "expr-var" var
    (expect (sink-name? var) #t
      (cene-err "Expected var to be a name")
    #/sink-cexpr-var var))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-reified`.
  (def-func! "expr-reified" val
    (sink-cexpr-reified val))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "expr-located" location-definition-name body
    (expect (sink-name? location-definition-name) #t
      (cene-err "Expected location-definition-name to be a name")
    #/expect body (sink-cexpr body)
      (cene-err "Expected body to be an expression")
    #/sink-cexpr #/cexpr-located location-definition-name body))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-let`.
  (def-func! "expr-let" bindings body
    (expect (sink-list->maybe-racket bindings) (just bindings)
      (cene-err "Expected bindings to be a list")
    #/w- bindings
      (list-map bindings #/fn binding
        (expect (unmake-sink-struct-maybe binding s-assoc)
          (just #/list var val)
          (cene-err "Expected bindings to be an assoc list")
        #/expect (sink-name? var) #t
          (cene-err "Expected bindings to be an assoc list with names as the keys")
        #/expect (sink-cexpr? val) #t
          (cene-err "Expected bindings to be an assoc list with expressions as the values")
        #/list var val))
    #/expect (sink-cexpr? body) #t
      (cene-err "Expected body to be an expression")
    #/sink-cexpr-let bindings body))
  
  (def-macro! "let" #/fn unique-name qualify text-input-stream then
    (sink-effects-read-bounded-ids-and-exprs
      unique-name qualify text-input-stream
      sink-name-for-local-variable
    #/fn unique-name qualify text-input-stream args
    #/expect (reverse args) (cons body rev-bindings)
      (cene-err "Expected a let form to have a body expression")
    #/w- bindings
      (w-loop next rest rev-bindings so-far (list)
        (mat rest (list) so-far
        #/expect rest (list* val var rest)
          (cene-err "Expected a let form to have an odd number of subforms")
        #/next rest (cons (list var val) so-far)))
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-let
      (list-map bindings #/dissectfn (list var val)
        (expect var
          (id-or-expr-id var-located-string var-qualified-name)
          (cene-err "Expected every bound variable of a let form to be an identifier")
        #/list
          (sink-authorized-name-get-name var-qualified-name)
          (id-or-expr->cexpr val)))
      (id-or-expr->cexpr body)))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-expr" v
    (racket-boolean->sink #/sink-cexpr? v))
  
  ; TODO: See if this can be a pure function.
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "effects-expr-can-eval" expr then
    (expect expr (sink-cexpr expr)
      (cene-err "Expected expr to be an expression")
    #/sink-effects-later #/fn
    #/verify-callback-effects! #/sink-call then
    #/racket-boolean->sink #/cexpr-can-eval? expr))
  
  ; TODO: See if this can be a pure function.
  ;
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `eval-cexpr`, and it was a pure function that took a mode
  ; parameter.
  ;
  (def-func! "effects-expr-eval" expr then
    (expect expr (sink-cexpr expr)
      (cene-err "Expected expr to be an expression")
    #/sink-effects-later #/fn
    #/expect (cexpr-can-eval? expr) #t
      (cene-err "Expected expr to be an expression which had all the information it needed for evaluation")
    #/sink-effects-cexpr-eval expr #/fn result
    #/verify-callback-effects! #/sink-call then result))
  
  ; TODO BUILTINS: Consider implementing something like the following
  ; built-ins from the JavaScript version of Cene. We can probably
  ; implement these in a Cene prelude.
  ;
  ;   compile-expression-later
  ;   read-all-force
  ;   def-macro
  
  
  ; Integers
  
  (def-nullary-func! "dex-int"
    (sink-dex #/dex-struct sink-int #/dex-exact-rational))
  
  (def-nullary-func! "cline-int"
    (sink-cline #/cline-struct sink-int #/cline-exact-rational))
  
  (def-nullary-func! "int-zero" (sink-int 0))
  
  (def-nullary-func! "int-one" (sink-int 1))
  
  (def-nullary-func! "fuse-int-by-plus"
    (sink-fuse #/fuse-struct sink-int #/fuse-exact-rational-by-plus))
  
  (def-nullary-func! "fuse-int-by-times"
    (sink-fuse #/fuse-struct sink-int #/fuse-exact-rational-by-times))
  
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
  
  (def-nullary-func! "dex-string" (sink-dex-string))
  
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
    #/sink-string #/string->immutable-string
    #/list->string #/list #/integer->char unicode-scalar))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `string-append-later`.
  (def-func! "effects-string-append" a b then
    (expect a (sink-string a)
      (cene-err "Expected a to be a string")
    #/expect b (sink-string b)
      (cene-err "Expected b to be a string")
    #/sink-effects-later #/fn
    #/verify-callback-effects! #/sink-call then
    #/sink-string #/string->immutable-string #/string-append a b))
  
  ; This is an extremely basic string syntax. It doesn't have any
  ; support for escape sequences. It reads a body that must have zero
  ; occurrences of [ ] ( ) and it treats that body verbatim as the
  ; contents of the string.
  ;
  ; NOTE: The JavaScript version of Cene doesn't have this.
  ;
  (def-macro! "str-prim" #/fn
    unique-name qualify text-input-stream then
    
    (sink-effects-claim-freshen unique-name #/fn unique-name
    #/sink-effects-optimized-textpat-read-located
      str-prim-pat text-input-stream
    #/fn text-input-stream maybe-contents
    #/dissect maybe-contents (just contents)
    #/sink-effects-string-from-located-string contents #/fn contents
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-reified contents))
  
  ; TODO BUILTINS: Implement the macro `str`, probably in a Cene
  ; prelude.
  
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
    #/sink-effects-later #/fn
    #/sink-call then
    #/sink-string #/string->immutable-string
    #/substring string start stop))
  
  
  ; Text patterns
  ;
  ; NOTE: In the JavaScript version of Cene, these were known as
  ; "regexes" instead of "text patterns," and the names used `regex`
  ; instead of `textpat`. These aren't quite regexes, and the fact
  ; they don't backtrack makes them a lot like pattern-matching
  ; clauses in functional programming languages, so the name
  ; "text patterns" is more evocative of what they actually are.
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-textpat" v
    (racket-boolean->sink #/sink-textpat? v))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-optimized-textpat" v
    (racket-boolean->sink #/sink-optimized-textpat? v))
  
  (def-nullary-func! "textpat-give-up"
    (sink-textpat #/textpat-give-up))
  
  (def-nullary-func! "textpat-empty"
    (sink-textpat #/textpat-empty))
  
  (def-func! "textpat-if" condition then else
    (expect condition (sink-textpat condition)
      (cene-err "Expected condition to be a text pattern")
    #/expect then (sink-textpat then)
      (cene-err "Expected then to be a text pattern")
    #/expect else (sink-textpat else)
      (cene-err "Expected else to be a text pattern")
    #/sink-textpat #/textpat-if condition then else))
  
  (def-func! "textpat-while" condition body
    (expect condition (sink-textpat condition)
      (cene-err "Expected condition to be a text pattern")
    #/expect body (sink-textpat body)
      (cene-err "Expected body to be a text pattern")
    #/sink-textpat #/textpat-while condition body))
  
  (def-func! "textpat-until" body condition
    (expect body (sink-textpat body)
      (cene-err "Expected body to be a text pattern")
    #/expect condition (sink-textpat condition)
      (cene-err "Expected condition to be a text pattern")
    #/sink-textpat #/textpat-until body condition))
  
  (def-func! "textpat-one-in-range" start stop
    (expect start (sink-string start)
      (cene-err "Expected start to be a string")
    #/expect (string-length start) 1
      (cene-err "Expected start to be a string containing a single Unicode scalar value")
    #/w- start (string-ref start 0)
    #/expect stop (sink-string stop)
      (cene-err "Expected stop to be a string")
    #/expect (string-length stop) 1
      (cene-err "Expected stop to be a string containing a single Unicode scalar value")
    #/w- stop (string-ref stop 0)
    #/sink-textpat #/textpat-one-in-range start stop))
  
  (def-nullary-func! "textpat-one"
    (sink-textpat #/textpat-one))
  
  (def-func! "textpat-from-string" str
    (expect str (sink-string str)
      (cene-err "Expected str to be a string")
    #/sink-textpat #/textpat-from-string str))
  
  (def-func! "textpat-one-in-string" str
    (expect str (sink-string str)
      (cene-err "Expected str to be a string")
    #/sink-textpat #/textpat-one-in-string str))
  
  (def-func! "textpat-has-empty" t
    (expect t (sink-textpat t)
      (cene-err "Expected t to be a text pattern")
    #/racket-boolean->sink #/textpat-has-empty? t))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `optimize-regex-later`.
  (def-func! "effects-optimize-textpat" t then
    (expect t (sink-textpat t)
      (cene-err "Expected t to be a text pattern")
    #/sink-effects-optimize-textpat t #/fn t
    #/verify-callback-effects! #/sink-call then
    #/sink-optimized-textpat t))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `optimized-regex-match-later`.
  (def-func! "effects-optimized-textpat-match" ot str start stop then
    (expect ot (sink-optimized-textpat ot)
      (cene-err "Expected ot to be a text pattern")
    #/expect str (sink-string str)
      (cene-err "Expected str to be a string")
    #/w- n (string-length str)
    #/expect start (sink-int start)
      (cene-err "Expected start to be an integer")
    #/expect (<= 0 start) #t
      (cene-err "Expected start to be a nonnegative integer")
    #/expect (<= start n) #t
      (cene-err "Expected start to be less than or equal to the length of the string")
    #/expect stop (sink-int stop)
      (cene-err "Expected stop to be an integer")
    #/expect (<= 0 stop) #t
      (cene-err "Expected stop to be a nonnegative integer")
    #/expect (<= stop n) #t
      (cene-err "Expected stop to be less than or equal to the length of the string")
    #/expect (<= start stop) #t
      (cene-err "Expected start to be less than or equal to stop")
    #/sink-effects-later #/fn
    #/verify-callback-effects! #/sink-call then
    #/w- result (optimized-textpat-match ot str start stop)
    #/mat result (textpat-result-matched stop)
      (make-sink-struct s-textpat-result-matched #/list
      #/sink-int stop)
    #/mat result (textpat-result-failed)
      (make-sink-struct s-textpat-result-failed #/list)
    #/dissect result (textpat-result-passed-end)
      (make-sink-struct s-textpat-result-passed-end #/list)))
  
  (def-data-struct! "textpat-result-matched" #/list "stop")
  (def-data-struct! "textpat-result-failed" #/list)
  (def-data-struct! "textpat-result-passed-end" #/list)
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "effects-optimized-textpat-read-located"
    ot input-stream then
    
    (expect ot (sink-optimized-textpat ot)
      (cene-err "Expected ot to be a text pattern")
    #/expect (sink-text-input-stream? input-stream) #t
      (cene-err "Expected input-stream to be a text input stream")
    #/sink-effects-optimized-textpat-read-located ot input-stream
    #/fn input-stream maybe-result
    #/verify-callback-effects!
    #/sink-call then input-stream #/racket-maybe->sink maybe-result))
  
  
  ; File I/O for simple builds
  
  ; TODO BUILTINS: Consider implementing the following.
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
  
  ; TODO BUILTINS: The JavaScript version of Cene has FFI operations
  ; for interacting with JavaScript, naturally. See if we should do
  ; something similar for interacting with Racket.
  
  
  ; Other
  ;
  ; TODO: Figure out what section of operations to put these in.
  ;
  ; NOTE: These have not been designed based on the JavaScript version
  ; of Cene, so it may or may not have operations similar to these.
  
  (def-func! "directive" directive
    (sink-directive directive))
  
  (def-func! "name-for-claim" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-for-claim name))
  
  (def-func! "authorized-name-for-claim" name
    (expect (sink-authorized-name? name) #t
      (cene-err "Expected name to be an authorized name")
    #/sink-authorized-name-for-claim name))
  
  (def-func! "name-for-function-implementation-code"
    main-tag-name proj-tag-names
    
    (expect (sink-name? main-tag-name) #t
      (cene-err "Expected main-tag-name to be a name")
    #/expect
      (in-dex? (sink-dex-table #/sink-dex-struct s-trivial #/list)
        proj-tag-names)
      #t
      (cene-err "Expected proj-tag-names to be a table of trivial values")
    #/sink-name-for-function-implementation-code
      main-tag-name proj-tag-names))
  
  (def-func! "name-for-function-implementation-value"
    main-tag-name proj-tag-names
    
    (expect (sink-name? main-tag-name) #t
      (cene-err "Expected main-tag-name to be a name")
    #/expect
      (in-dex? (sink-dex-table #/sink-dex-struct s-trivial #/list)
        proj-tag-names)
      #t
      (cene-err "Expected proj-tag-names to be a table of trivial values")
    #/sink-name-for-function-implementation-value
      main-tag-name proj-tag-names))
  
  (define (verify-proj-tag-authorized-names! proj-tag-names)
    (expect proj-tag-names (sink-table proj-tag-names)
      (cene-err "Expected proj-tag-names to be a table")
    #/begin
      (table-kv-map proj-tag-names #/fn k v
        (expect v (sink-authorized-name name)
          (cene-err "Expected each value of proj-tag-names to be an authorized name")
        #/expect (eq-by-dex? (dex-name) k name) #t
          (cene-err "Expected each value of proj-tag-names to be an authorized name where the name authorized is the same as the name it's filed under")
          v))
    #/void))
  
  (def-func! "authorized-name-for-function-implementation-code"
    main-tag-name proj-tag-names
    
    (expect (sink-authorized-name? main-tag-name) #t
      (cene-err "Expected main-tag-name to be an authorized name")
    #/begin (verify-proj-tag-authorized-names! proj-tag-names)
    #/sink-authorized-name-for-function-implementation-code
      main-tag-name proj-tag-names))
  
  (def-func! "authorized-name-for-function-implementation-value"
    main-tag-name proj-tag-names
    
    (expect (sink-authorized-name? main-tag-name) #t
      (cene-err "Expected main-tag-name to be an authorized name")
    #/begin (verify-proj-tag-authorized-names! proj-tag-names)
    #/sink-authorized-name-for-function-implementation-value
      main-tag-name proj-tag-names))
  
  (def-func! "name-for-freestanding-expr-op" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-for-freestanding-cexpr-op name))
  
  (def-func! "name-for-bounded-expr-op" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-for-bounded-cexpr-op name))
  
  (def-func! "name-for-nameless-bounded-expr-op" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-for-nameless-bounded-cexpr-op name))
  
  (def-func! "name-for-struct-main-tag" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-for-struct-main-tag name))
  
  (def-func! "name-for-struct-proj" qualified-main-tag-name proj-name
    (expect (sink-name? qualified-main-tag-name) #t
      (cene-err "Expected qualified-main-tag-name to be a name")
    #/expect (sink-name? proj-name) #t
      (cene-err "Expected proj-name to be a name")
    #/sink-name-for-struct-proj qualified-main-tag-name proj-name))
  
  (def-func! "name-for-local-variable" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-for-local-variable name))
  
  (def-func! "name-for-struct-metadata" name
    (expect (sink-name? name) #t
      (cene-err "Expected name to be a name")
    #/sink-name-for-struct-metadata name))
  
  ; TODO: See if Cene code should be able to make the kinds of names
  ; it gets from `name-of` without having a value on hand -- only
  ; having a collection of the names of the values that value would
  ; contain. For instance, see if there should be an operation that
  ; produces the value of
  ; `(name-of (dex-struct yep /dex-struct my-struct) (yep/my-struct))`
  ; based only on the value of
  ; `(name-of (dex-struct my-struct) (my-struct))`, not the actual
  ; `(my-struct)` value itself. If so, this will be a lot of
  ; name-making operations, including one for each dex constructor,
  ; one for each cline constructor, etc.
  
  (def-func! "is-located-string" v
    (racket-boolean->sink #/sink-located-string? v))
  
  (def-func! "effects-string-from-located-string" located-string then
    (expect (sink-located-string? located-string) #t
      (cene-err "Expected located-string to be a located string")
    #/sink-effects-string-from-located-string located-string
    #/fn string
    #/verify-callback-effects! #/sink-call then string))
  
  ; TODO BUILTINS: Make sure we have a sufficient set of operations
  ; for manipulating `sink-located-string` values.
  
  (def-func! "is-expr-sequence-output-stream" v
    (racket-boolean->sink #/sink-cexpr-sequence-output-stream? v))
  
  (def-func! "effects-make-expr-sequence-output-stream"
    unique-name state on-expr then
    
    ; TODO: See if we should differentiate the error messages for
    ; these three occurrences of `verify-callback-effects!`.
    (expect (sink-authorized-name? unique-name) #t
      (cene-err "Expected unique-name to be an authorized name")
    #/sink-effects-make-cexpr-sequence-output-stream
      unique-name
      state
      (fn state cexpr then
        (verify-callback-effects! #/sink-call on-expr
          state
          cexpr
          (sink-fn-curried 1 #/fn state #/then state)))
    #/fn output-stream unwrap
    #/verify-callback-effects! #/sink-call then
      output-stream
      (sink-fn-curried 2 #/fn output-stream then
      #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
        (cene-err "Expected output-stream to be an expression sequence output stream")
      #/unwrap output-stream #/fn state
      #/verify-callback-effects! #/sink-call then state)))
  
  (def-func! "effects-expr-write" output-stream expr then
    (expect (sink-cexpr-sequence-output-stream? output-stream) #t
      (cene-err "Expected output-stream to be an expression sequence output stream")
    #/expect (cexpr? expr) #t
      (cene-err "Expected expr to be an expression")
    #/sink-effects-cexpr-write output-stream expr #/fn output-stream
    #/verify-callback-effects! #/sink-call then output-stream))
  
  (def-func! "is-text-input-stream" v
    (racket-boolean->sink #/sink-text-input-stream? v))
  
  (def-func! "effects-read-eof" input-stream on-eof then
    (expect (sink-text-input-stream? input-stream) #t
      (cene-err "Expected input-stream to be a text input stream")
    #/expect (sink-effects? on-eof) #t
      (cene-err "Expected on-eof to be an effects value")
    #/sink-effects-read-eof input-stream on-eof #/fn input-stream
    #/verify-callback-effects! #/sink-call then input-stream))
  
  ; TODO BUILTINS: Make sure we have a sufficient set of operations
  ; for manipulating `sink-text-input-stream` values. We don't even
  ; have a single way to create them right now, aside from getting
  ; them passed in from the macroexpander. We also don't have much
  ; ability to inspect the text we're getting.
  
  ; NOTE:
  ;
  ; We expose this operation to Cene so that Cene code can install a
  ; definition with a dex that's the same as the dex we use for
  ; built-in struct metadata definitions. Otherwise, Cene programmers
  ; could use `dex-fix` to build their own dexes with functionality
  ; nearly identical to `dex-list`, but `dex-dex` wouldn't consider
  ; them equal. That means users couldn't write struct metadata
  ; definitions that duplicate built-in ones without casing an error,
  ; whereas they could still write duplicates among their own
  ; definitions without a problem, and in this way we would have an
  ; unnecessarily visible distinction between built-in and
  ; user-supplied definitions.
  ;
  ; We could simply expose a `dex-struct-metadata` nullary operation.
  ; However, the format of struct metadata is fully stabilized so
  ; macros can parse it, so it wouldn't give us a future-proofing
  ; path. If we ever have another kind of definition like the struct
  ; metadata one, there's a good chance `dex-list` will come in handy
  ; for that one too, at which point this choice we've made will fit
  ; in better.
  ;
  (def-func! "dex-list" dex-elem
    (expect (sink-dex? dex-elem) #t
      (cene-err "Expected dex-elem to be a dex")
    #/sink-dex-list dex-elem))
  
  
  
  (cene-runtime
    (sink-table defined-dexes)
    (sink-table defined-values)
    (fn qualify-for-package
      (sink-effects-fuse-list #/list-map init-package-steps #/fn step
        (step qualify-for-package)))))
