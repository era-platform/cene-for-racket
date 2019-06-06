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
(require #/for-syntax #/only-in syntax/parse
  expr expr/c id nat syntax-parse)


(require #/only-in racket/contract/base
  -> ->i any/c cons/c list/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define/generic)
(require #/only-in racket/match match-define)
(require #/only-in racket/math natural?)
(require #/only-in racket/runtime-path define-runtime-path)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-all list-any list-foldl list-foldr list-kv-map list-map
  list-zip-map)
(require #/only-in lathe-comforts/maybe
  just just-value maybe-bind maybe/c maybe-map nothing)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in effection/extensibility/base
  authorized-name-get-name)
(require #/only-in effection/order
  assocs->table-if-mutually-unique cline-exact-rational
  dex-exact-rational dex-immutable-string fuse-exact-rational-by-plus
  fuse-exact-rational-by-times)
(require #/only-in effection/order/base
  call-fuse call-merge cline-by-dex cline-default cline-give-up
  cline-opaque cline-result? cline-struct compare-by-cline
  compare-by-dex dex? dexable dexable? dex-cline dex-default dex-dex
  dex-fix dex-fuse dex-give-up dex-merge dex-name dex-opaque
  dex-struct dex-table fusable-function? fuse-by-merge fuse-opaque
  fuse-struct fuse-table in-cline? in-dex? get-dex-from-cline
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
  sink-extfx-read-bounded-cexprs
  sink-extfx-read-bounded-ids-and-exprs
  sink-extfx-read-bounded-specific-number-of-cexprs
  sink-extfx-read-leading-specific-number-of-cexprs
  sink-extfx-read-leading-specific-number-of-identifiers
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

(define-runtime-path prelude-path "prelude.cene")

(provide sink-extfx-init-essentials sink-extfx-init-package)



(define s-yep (core-sink-struct "yep" #/list "val"))
(define s-nope (core-sink-struct "nope" #/list "val"))

(define s-nil (core-sink-struct "nil" #/list))
(define s-cons (core-sink-struct "cons" #/list "first" "rest"))

(define s-nothing (core-sink-struct "nothing" #/list))
(define s-just (core-sink-struct "just" #/list "val"))

(define s-assoc (core-sink-struct "assoc" #/list "key" "val"))

; NOTE: Unlike the other struct tags here, this one is not exposed to
; Cene with `def-data-struct!`. Instead, we expose the related
; functionality as the built-in
; `extfx-put-all-built-in-syntaxes-this-came-with`.
(define s-command-init-package
  (core-sink-struct "command-init-package" #/list "key" "qualify"))

(define s-ordering-lt (core-sink-struct "ordering-lt" #/list))
(define s-ordering-eq (core-sink-struct "ordering-eq" #/list))
(define s-ordering-gt (core-sink-struct "ordering-gt" #/list))

(define s-dexable (core-sink-struct "dexable" #/list "dex" "val"))

(define s-struct-metadata
  (core-sink-struct "struct-metadata"
  #/list "main-tag-name" "projections"))

(define s-carried (core-sink-struct "carried" #/list "main" "carry"))

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
(define-syntax (dexable-struct stx)
  (syntax-parse stx #/ (_ tag:id dexable-field ...)
    
    #:declare dexable-field (expr/c #'dexable? #:name "a field")
    
    #:with (dexable-field-result ...)
    (generate-temporaries #'(dexable-field ...))
    
    #'(let ([dexable-field-result dexable-field.c] ...)
        (dexable
          (dex-struct tag
            (dissect dexable-field-result (dexable dex val) dex)
            ...)
          (tag
            (dissect dexable-field-result (dexable dex val) val)
            ...)))))

(define/contract (racket-boolean->sink racket-boolean)
  (-> boolean? sink?)
  (begin (assert-can-get-cene-definitions!)
  #/if racket-boolean
    (make-sink-struct (s-yep) #/list
      (make-sink-struct (s-nil) #/list))
    (make-sink-struct (s-nope) #/list
      (make-sink-struct (s-nil) #/list))))

(define/contract (sink-maybe->maybe-racket sink-maybe)
  (-> sink? #/maybe/c #/maybe/c sink?)
  (begin (assert-can-get-cene-definitions!)
  #/mat (unmake-sink-struct-maybe (s-nothing) sink-maybe)
    (just #/list)
    (just #/nothing)
  #/mat (unmake-sink-struct-maybe (s-just) sink-maybe)
    (just #/list val)
    (just #/just val)
  #/nothing))

(define/contract (racket-maybe->sink racket-maybe)
  (-> (maybe/c sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/mat racket-maybe (just val)
    (make-sink-struct (s-just) #/list val)
    (make-sink-struct (s-nothing) #/list)))

(define/contract (sink-list->maybe-racket sink-list)
  (-> sink? #/maybe/c #/listof sink?)
  ; NOTE: We could call `sink-list->maybe-racket` itself recursively,
  ; but we explicitly accumulate elements using a parameter
  ; (`rev-racket-list`) of a recursive helper function (`next`) so
  ; that we keep the call stack at a constant size throughout the list
  ; traversal.
  (begin (assert-can-get-cene-definitions!)
  #/w-loop next sink-list sink-list rev-racket-list (list)
  #/mat (unmake-sink-struct-maybe (s-nil) sink-list) (just #/list)
    (just #/reverse rev-racket-list)
  #/mat (unmake-sink-struct-maybe (s-cons) sink-list)
    (just #/list elem sink-list)
    (next sink-list #/cons elem rev-racket-list)
  #/nothing))

(define/contract (racket-list->sink racket-list)
  (-> (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/list-foldr racket-list (make-sink-struct (s-nil) #/list)
  #/fn elem rest
    (make-sink-struct (s-cons) #/list elem rest)))

(define/contract (sink-valid-dexable->maybe-racket sink-dexable)
  (-> sink? #/unsafe:dexableof-unchecked sink?)
  (begin (assert-can-get-cene-definitions!)
  #/expect (unmake-sink-struct-maybe (s-dexable) sink-dexable)
    (just #/list dex val)
    (nothing)
  #/expect dex (sink-dex dex) (nothing)
  #/expect (in-dex? dex val) #t (nothing)
  #/dexable dex val))


(struct-easy
  (cene-struct-metadata tags proj-string-to-name proj-name-to-string))

(define/contract (verify-sink-struct-metadata! fault sink-metadata)
  (-> sink-fault? sink? cene-struct-metadata?)
  (begin (assert-can-get-cene-definitions!)
  #/expect
    (unmake-sink-struct-maybe (s-struct-metadata) sink-metadata)
    (just #/list main-tag-name projs)
    (cene-err fault "Expected a defined struct metadata entry to be a struct-metadata")
  #/expect main-tag-name (sink-name main-tag-name)
    (cene-err fault "Expected a defined struct metadata entry to have a main tag name that was a name")
  #/expect (sink-list->maybe-racket projs) (just projs)
    (cene-err fault "Expected a defined struct metadata entry to have a cons list of projections")
  #/w- projs
    (list-map projs #/fn entry
      (expect (unmake-sink-struct-maybe (s-assoc) entry)
        (just #/list proj-string proj-name)
        (cene-err fault "Expected a defined struct metadata entry to have a projection list where each entry was an assoc")
      #/expect proj-string (sink-string proj-string)
        (cene-err fault "Expected a defined struct metadata entry to have a projection list where each key was a string")
      #/expect proj-name (sink-name proj-name)
        (cene-err fault "Expected a defined struct metadata entry to have a projection list where each associated value was a name")
      #/w- proj-string-name
        (name-for-sink-string #/sink-string proj-string)
      #/list proj-string proj-string-name proj-name))
  #/expect
    (assocs->table-if-mutually-unique
    #/list-map projs #/dissectfn (list string string-name name)
      (cons string-name name))
    (just proj-string-to-name)
    (cene-err fault "Expected a defined struct metadata entry to have a projection list with mutually unique strings")
  #/expect
    (assocs->table-if-mutually-unique
    #/list-map projs #/dissectfn (list string string-name name)
      (cons name string))
    (just proj-name-to-string)
    (cene-err fault "Expected a defined struct metadata entry to have a projection list with mutually unique names")
  #/cene-struct-metadata
    (cons main-tag-name
    #/list-map projs #/dissectfn (list string string-name name) name)
    proj-string-to-name proj-name-to-string))

(define/contract (sink-name-for-struct-metadata inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n
    (list 'name:struct-metadata n)))

(define/contract
  (sink-extfx-read-struct-metadata
    fault unique-name qualify text-input-stream then)
  (->
    sink-fault?
    sink-authorized-name? sink? sink-text-input-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      cene-struct-metadata?
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-read-whitespace fault text-input-stream
  #/fn text-input-stream whitespace
  #/sink-extfx-read-leading-specific-number-of-identifiers
    fault unique-name qualify text-input-stream 1
    sink-name-for-struct-metadata
  #/fn unique-name qualify text-input-stream metadata-names
  #/dissect metadata-names (list #/list located-string metadata-name)
  #/sink-extfx-get (sink-authorized-name-get-name metadata-name)
  #/fn metadata
  ; TODO FAULT: Make this `fault` more specific.
  #/then unique-name qualify text-input-stream
    (verify-sink-struct-metadata! fault metadata)))

(define/contract (struct-metadata-tags metadata)
  (-> cene-struct-metadata? #/listof name?)
  (dissect metadata (cene-struct-metadata tags _ _)
    tags))

(define/contract (struct-metadata-n-projs metadata)
  (-> cene-struct-metadata? natural?)
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
    [_ boolean?])
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
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-dex-struct main-tag-name projs)
        (error "Expected this to be a cexpr-dex-struct")
      #/sink-dex #/unsafe:dex #/dex-internals-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval fault proj-cexpr env)))
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
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-cline-struct main-tag-name projs)
        (error "Expected this to be a cexpr-cline-struct")
      #/sink-cline #/unsafe:cline #/cline-internals-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval fault proj-cexpr env)))
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
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-merge-struct main-tag-name projs)
        (error "Expected this to be a cexpr-merge-struct")
      #/sink-merge #/unsafe:merge #/furge-internals-sink-struct
        unsafe:autoname-merge (dex-merge) call-merge
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval fault proj-cexpr env)))
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
    
    (define (cexpr-eval fault this env)
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


(struct-easy (fuse-internals-extfx)
  #:other
  
  #:methods unsafe:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-extfx)
    
    (define (furge-internals-autoname this)
      'tag:fuse-extfx)
    
    (define (furge-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (furge-internals-call this a b)
      (expect (sink-extfx? a) #t (nothing)
      #/expect (sink-extfx? b) #t (nothing)
      #/just #/sink-extfx-fuse a b))
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
    
    (define (cexpr-eval fault this env)
      (expect this
        (cexpr-case subject-expr tags vars then-expr else-expr)
        (error "Expected this to be a cexpr-case")
      #/w- subject (-eval fault subject-expr env)
      #/mat (unmake-sink-struct-maybe tags subject) (just vals)
        (-eval fault then-expr
        #/list-foldl env (map list vars vals) #/fn env entry
          (dissect entry (list var val)
          #/table-shadow var (just val) env))
        (-eval fault else-expr env)))
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
      (dex-sink-struct (s-nil) #/list)
      (dex-sink-struct (s-cons) #/list dex-elem dex))))

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


; TODO: See if there's a way to eliminate this state.
(define current-fault (make-parameter #/nothing))

(define (get-current-fault)
  (expect (current-fault) (just fault)
    (error "Expected the current fault to be determined dynamically")
    fault))

(define (with-current-fault fault body)
  (expect (current-fault) (nothing)
    (error "Did not expect the current fault to be determined already dynamically")
  #/parameterize ([current-fault (just fault)])
    (body)))


(define-syntax-rule
  (define-fix-converter converter constructor error-message)
  (struct-easy (converter unwrap)
    #:other
    
    #:property prop:procedure
    (fn this x
      (dissect this (converter unwrap)
      #/w- fault (get-current-fault)
      #/expect (sink-call fault unwrap x) (constructor result)
        (cene-err fault error-message)
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
    #/w- fault (get-current-fault)
    #/mat command
      (unsafe:dex-by-own-method::raise-different-methods-error
        a b a-method b-method)
      (cene-err fault "Obtained two different methods from the two values being compared")
    #/dissect command (unsafe:dex-by-own-method::get-method source)
    #/expect (sink-call fault get-method source) (sink-dex method)
      (cene-err fault "Expected the result of a dex-by-own-method body to be a dex")
      method)))

(struct-easy (sink-cline-by-own-method-unthorough get-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-cline-by-own-method-unthorough get-method)
    #/w- fault (get-current-fault)
    #/mat command
      (unsafe:cline-by-own-method::raise-different-methods-error
        a b a-method b-method)
      (cene-err fault "Obtained two different methods from the two values being compared")
    #/dissect command (unsafe:cline-by-own-method::get-method source)
    #/expect (sink-call fault get-method source) (sink-cline method)
      (cene-err fault "Expected the result of a cline-by-own-method body to be a cline")
      method)))

(struct-easy (sink-merge-by-own-method-unthorough get-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-merge-by-own-method-unthorough get-method)
    #/w- fault (get-current-fault)
    #/mat command
      (unsafe:merge-by-own-method::raise-different-input-methods-error
        a b a-method b-method)
      (cene-err fault "Obtained two different methods from the two input values")
    #/mat command
      (unsafe:merge-by-own-method::raise-cannot-get-output-method-error
        a b result input-method)
      (cene-err fault "Could not obtain a method from the result value")
    #/mat command
      (unsafe:merge-by-own-method::raise-different-output-method-error
        a b result input-method output-method)
      (cene-err fault "Obtained two different methods from the input and the output")
    #/dissect command (unsafe:merge-by-own-method::get-method source)
    #/expect (sink-call fault get-method source) (sink-merge method)
      (cene-err fault "Expected the result of a merge-by-own-method body to be a merge")
      method)))

(struct-easy (sink-fuse-by-own-method-unthorough get-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-fuse-by-own-method-unthorough get-method)
    #/w- fault (get-current-fault)
    #/mat command
      (unsafe:fuse-by-own-method::raise-different-input-methods-error
        a b a-method b-method)
      (cene-err fault "Obtained two different methods from the two input values")
    #/mat command
      (unsafe:fuse-by-own-method::raise-cannot-get-output-method-error
        a b result input-method)
      (cene-err fault "Could not obtain a method from the result value")
    #/mat command
      (unsafe:fuse-by-own-method::raise-different-output-method-error
        a b result input-method output-method)
      (cene-err fault "Obtained two different methods from the input and the output")
    #/dissect command (unsafe:fuse-by-own-method::get-method source)
    #/expect (sink-call fault get-method source) (sink-fuse method)
      (cene-err fault "Expected the result of a fuse-by-own-method body to be a fuse")
      method)))

(struct-easy (sink-fuse-fusable-fn-unthorough arg-to-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (sink-fuse-fusable-fn-unthorough arg-to-method)
    #/w- fault (get-current-fault)
    #/mat command
      (unsafe:fuse-fusable-function::raise-cannot-combine-results-error
        method a b a-result b-result)
      (cene-err fault "Could not combine the result values")
    #/dissect command
      (unsafe:fuse-fusable-function::arg-to-method arg)
    #/expect (sink-call fault arg-to-method arg) (sink-fuse method)
      (cene-err fault "Expected the result of a fuse-fusable-fn body to be a fuse")
      method)))


(define str-prim-pat
  (optimize-textpat
  #/textpat-star #/textpat-one-not-in-string "[]()"))


(define/contract
  (sink-extfx-add-init-package-step fault unique-name step)
  (->
    sink-fault?
    sink-authorized-name?
    (-> sink-authorized-name? (-> sink-name? sink-authorized-name?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-and-split unique-name 2
  #/dissectfn (list unique-name-for-sub-write unique-name-for-step)
  #/dissect (cene-definition-establish-init-package-pubsub) (list p s)
  #/sink-extfx-sub-write s unique-name-for-sub-write #/fn entry
    (expect (unmake-sink-struct-maybe (s-command-init-package) entry)
      (just #/list key qualify)
      (cene-err fault "Expected each package initialization command to be a command-init-package")
    #/expect (sink-name? key) #t
      (cene-err fault "Expected each package initialization command to have a name as its key")
    #/step (sink-authorized-name-subname key unique-name-for-step)
      (fn name #/sink-call-qualify fault qualify name))))

; TODO: Use `sink-extfx-init-package` and `sink-extfx-init-essentials`
; in some kind of CLI entrypoint or something.

(define/contract
  (sink-extfx-init-package fault unique-name qualify-for-package)
  (->
    sink-fault?
    sink-authorized-name?
    (-> sink-name? sink-authorized-name?)
    sink-extfx?)
  (sink-extfx-claim-and-split unique-name 2
  #/dissectfn (list unique-name-for-pub-write unique-name-for-step)
  #/dissect (cene-definition-establish-init-package-pubsub) (list p s)
  #/sink-extfx-pub-write p unique-name-for-pub-write
    (make-sink-struct (s-command-init-package) #/list
      (sink-authorized-name-get-name unique-name-for-step)
      (sink-fn-curried-fault 1 #/fn fault name
        (expect (sink-name? name) #t
          (cene-err fault "Expected the input to an extfx-put-all-built-in-syntaxes-this-came-with qualify function to be a name")
        #/qualify-for-package name)))))

(define/contract
  (sink-extfx-init-essentials root-fault root-unique-name)
  (-> sink-fault? sink-authorized-name? sink-extfx?)
  
  (define init-essentials-steps (list))
  
  (define/contract (sink-extfx-run-init-essentials-steps unique-name)
    (-> sink-authorized-name? sink-extfx?)
    (sink-extfx-claim-and-split unique-name
      (length init-essentials-steps)
    #/fn unique-names
    #/sink-extfx-fuse-list
    #/list-zip-map init-essentials-steps unique-names
    #/fn step unique-name
      (step unique-name)))
  
  (define/contract (add-init-essentials-step! step)
    (-> (-> sink-authorized-name? sink-extfx?) void?)
    (set! init-essentials-steps (cons step init-essentials-steps)))
  
  (define/contract
    (sink-extfx-def-dexable-value-for-lang-impl
      unique-name target-name dex value)
    (-> sink-authorized-name? sink-authorized-name? sink-dex? sink?
      sink-extfx?)
    (sink-extfx-claim unique-name #/fn
    #/sink-extfx-put target-name dex value))
  
  (define/contract
    (sink-extfx-def-dexable-value-for-package
      unique-name target-name dex value)
    (-> sink-authorized-name? sink-name? sink-dex? sink? sink-extfx?)
    (sink-extfx-claim-freshen unique-name #/fn unique-name
    #/sink-extfx-add-init-package-step root-fault unique-name
    #/fn unique-name qualify-for-package
      (sink-extfx-claim unique-name #/fn
      #/sink-extfx-put (qualify-for-package target-name) dex value)))
  
  (define/contract
    (sink-extfx-def-value-for-lang-impl unique-name target-name value)
    (-> sink-authorized-name? sink-authorized-name? sink? sink-extfx?)
    (sink-extfx-claim-freshen unique-name #/fn unique-name
    #/sink-extfx-def-dexable-value-for-lang-impl
      unique-name target-name (sink-dex #/dex-give-up) value))
  
  (define/contract
    (sink-extfx-def-value-for-package unique-name target-name value)
    (-> sink-authorized-name? sink-name? sink? sink-extfx?)
    (sink-extfx-claim-freshen unique-name #/fn unique-name
    #/sink-extfx-def-dexable-value-for-package
      unique-name target-name (sink-dex #/dex-give-up) value))
  
  (define/contract (def-value-for-package! name value)
    (-> sink-name? sink? void?)
    (add-init-essentials-step! #/fn unique-name
      (sink-extfx-def-value-for-package unique-name name value)))
  
  (define/contract (macro-impl body)
    (->
      (->
        sink-fault?
        sink-authorized-name? sink? sink-text-input-stream?
        sink-cexpr-sequence-output-stream?
        (->
          sink-authorized-name? sink? sink-text-input-stream?
          sink-cexpr-sequence-output-stream?
          sink-extfx?)
        sink-extfx?)
      sink?)
    (sink-fn-curried-fault 5 #/fn
      fault unique-name qualify text-input-stream output-stream then
      
      (expect (sink-authorized-name? unique-name) #t
        (cene-err fault "Expected unique-name to be an authorized name")
      #/expect (sink-text-input-stream? text-input-stream) #t
        (cene-err fault "Expected text-input-stream to be a text input stream")
      #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
        (cene-err fault "Expected output-stream to be an expression sequence output stream")
      #/body fault unique-name qualify text-input-stream output-stream
      #/fn unique-name qualify text-input-stream output-stream
      #/w- effects
        (sink-call fault then
          unique-name qualify text-input-stream output-stream)
      #/expect (sink-extfx? effects) #t
        (cene-err fault "Expected the return value of a macro's callback to be an extfx effectful computation")
        effects)))
  
  ; This creates a macro implementation function that reads a form
  ; body of precisely `n-args` cexprs, then writes a single cexpr
  ; computed from those using `body`.
  (define/contract (macro-impl-specific-number-of-args n-args body)
    (-> natural? (-> (listof sink-cexpr?) sink-cexpr?) sink?)
    (macro-impl #/fn
      fault unique-name qualify text-input-stream output-stream then
      
      (sink-extfx-read-bounded-specific-number-of-cexprs
        fault unique-name qualify text-input-stream n-args
      #/fn unique-name qualify text-input-stream args
      #/sink-extfx-cexpr-write fault output-stream (body args)
      #/fn output-stream
      #/then unique-name qualify text-input-stream output-stream)))
  
  (define/contract
    (sink-extfx-def-func-impl-reified
      unique-name qualified-main-tag-name qualified-proj-tag-names
      impl)
    (-> sink-authorized-name? sink-authorized-name? sink-table? sink?
      sink-extfx?)
    (sink-extfx-claim-and-split unique-name 2
    #/dissectfn (list unique-name-for-code unique-name-for-value)
    #/sink-extfx-fuse
      (sink-extfx-def-value-for-lang-impl unique-name-for-code
        (sink-authorized-name-for-function-implementation-code
          qualified-main-tag-name qualified-proj-tag-names)
        (sink-cexpr-reified impl))
      (sink-extfx-def-value-for-lang-impl unique-name-for-value
        (sink-authorized-name-for-function-implementation-value
          qualified-main-tag-name qualified-proj-tag-names)
        impl)))
  
  
  (define/contract
    (sink-extfx-def-func-verbose
      unique-name sink-extfx-def-value-custom main-tag-string n-args
      racket-func)
    (->
      sink-authorized-name?
      (-> sink-authorized-name? sink-name? sink? sink-extfx?)
      immutable-string?
      exact-positive-integer?
      procedure?
      sink-extfx?)
    (sink-extfx-claim-and-split unique-name 2
    #/dissectfn (list unique-name-for-macro unique-name-for-impl)
    #/w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-authorized-name
      (sink-name-qualify-for-lang-impl
      #/sink-name-for-struct-main-tag main-tag-name)
    #/w- qualified-main-tag-name
      (sink-authorized-name-get-name
        qualified-main-tag-authorized-name)
    #/sink-extfx-fuse
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (sink-extfx-def-value-custom unique-name-for-macro
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
        ; in `sink-extfx-read-bounded-specific-number-of-cexprs`, we
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
      (sink-extfx-def-func-impl-reified unique-name-for-impl
        qualified-main-tag-authorized-name
        (sink-table #/table-empty)
        (sink-opaque-fn #/fn struct-value
          (sink-fn-curried-fault n-args racket-func)))
      
      ))
  
  (define/contract
    (def-func-verbose!
      sink-extfx-def-value-custom main-tag-string n-args racket-func)
    (->
      (-> sink-authorized-name? sink-name? sink? sink-extfx?)
      immutable-string?
      exact-positive-integer?
      procedure?
      void?)
    (add-init-essentials-step! #/fn unique-name
      (sink-extfx-def-func-verbose
        unique-name sink-extfx-def-value-custom main-tag-string
        n-args racket-func)))
  
  (define-syntax (def-func-fault! stx)
    (syntax-parse stx #/
      (_ main-tag-string:expr fault:id param:id ... body:expr)
      #`(def-func-verbose! sink-extfx-def-value-for-package
          main-tag-string
          '#,(length (syntax->list #'(param ...)))
          (fn fault param ...
            body))))
  
  (define-syntax (def-func! stx)
    (syntax-parse stx #/
      (_ main-tag-string:expr param:id ... body:expr)
      #`(def-func-fault! main-tag-string fault param ... body)))
  
  (define/contract
    (sink-extfx-def-nullary-func unique-name main-tag-string result)
    (-> sink-authorized-name? immutable-string? sink? sink-extfx?)
    (sink-extfx-claim-and-split unique-name 2
    #/dissectfn (list unique-name-for-macro unique-name-for-impl)
    #/w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-authorized-name
      (sink-name-qualify-for-lang-impl
      #/sink-name-for-struct-main-tag main-tag-name)
    #/w- qualified-main-tag-name
      (sink-authorized-name-get-name
        qualified-main-tag-authorized-name)
    #/sink-extfx-fuse
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (sink-extfx-def-value-for-package unique-name-for-macro
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
            (make-sink-cexpr-construct (s-trivial) #/list))))
      
      ; We define a Cene struct function implementation containing
      ; the function's run time behavior.
      (sink-extfx-def-func-impl-reified unique-name-for-impl
        qualified-main-tag-authorized-name
        (sink-table #/table-empty)
        (sink-fn-curried-fault 2 #/fn fault struct-value arg
          (expect (unmake-sink-struct-maybe (s-trivial) arg)
            (just #/list)
            (cene-err fault "Expected the argument to a nullary function to be a trivial")
            result)))
      
      ))
  
  (define/contract (def-nullary-func! main-tag-string result)
    (-> immutable-string? sink? void?)
    (add-init-essentials-step! #/fn unique-name
      (sink-extfx-def-nullary-func
        unique-name main-tag-string result)))
  
  (define/contract
    (sink-extfx-def-data-struct
      unique-name main-tag-string proj-strings)
    (->
      sink-authorized-name?
      immutable-string?
      (listof immutable-string?)
      sink-extfx?)
    (sink-extfx-claim-and-split unique-name 3
    #/dissectfn
      (list
        unique-name-for-macro
        unique-name-for-impl
        unique-name-for-metadata)
    #/w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-authorized-name
      (sink-name-qualify-for-lang-impl
      #/sink-name-for-struct-main-tag main-tag-name)
    #/w- qualified-main-tag-name
      (sink-authorized-name-get-name
        qualified-main-tag-authorized-name)
    #/w- qualified-proj-name-entries
      (list-map proj-strings #/fn proj-string
        (list proj-string
        #/sink-name-qualify-for-lang-impl
        #/sink-name-for-struct-proj qualified-main-tag-name
        #/sink-name-for-string #/sink-string proj-string))
    #/w- qualified-proj-authorized-names
      (list-map qualified-proj-name-entries
      #/dissectfn (list proj-string qualified-proj-name)
        qualified-proj-name)
    #/w- qualified-proj-names
      (list-map qualified-proj-authorized-names
      #/fn proj-authorized-name
        (sink-authorized-name-get-name proj-authorized-name))
    #/expect
      (assocs->table-if-mutually-unique
        (list-map qualified-proj-authorized-names
        #/fn proj-authorized-name
          (dissect
            (sink-authorized-name-get-name proj-authorized-name)
            (sink-name proj-name)
          #/cons proj-name proj-authorized-name)))
      (just qualified-proj-names-table)
      (error "Expected the projection strings to be mutually unique")
    #/sink-extfx-fuse
      
      ; We define a reader macro so that the user can write code that
      ; compiles into an expression that constructs a struct with this
      ; tag.
      (sink-extfx-def-value-for-package unique-name-for-macro
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
        ; so in `sink-extfx-read-bounded-specific-number-of-cexprs`,
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
      (sink-extfx-def-func-impl-reified unique-name-for-impl
        qualified-main-tag-authorized-name
        (sink-table qualified-proj-names-table)
        (sink-opaque-fn-fault #/dissectfn (list fault struct-value)
          (cene-err fault "Called a struct that wasn't intended for calling")))
      
      ; We also define something we can use to look up a qualified
      ; main tag name and an ordered list of qualified and unqualified
      ; projection names, given the `sink-name-for-string` name the
      ; main tag name is made from.
      ;
      ; TODO: We haven't even tried to store this in the same format
      ; as the JavaScript version of Cene does. See if we should.
      ;
      (sink-extfx-def-dexable-value-for-package
        unique-name-for-metadata
        (sink-name-for-struct-metadata main-tag-name)
        (sink-dex-struct (s-struct-metadata) #/list
          (sink-dex-name)
          (sink-dex-list #/sink-dex-struct (s-assoc) #/list
            (sink-dex-string)
            (sink-dex-name)))
        (make-sink-struct (s-struct-metadata) #/list
          qualified-main-tag-name
          (racket-list->sink #/list-map qualified-proj-name-entries
          #/dissectfn (list proj-string qualified-proj-name)
            (make-sink-struct (s-assoc) #/list
              (sink-string proj-string)
              (sink-authorized-name-get-name qualified-proj-name)))))
      
      ))
  
  (define/contract (def-data-struct! main-tag-string proj-strings)
    (-> immutable-string? (listof immutable-string?) void?)
    (add-init-essentials-step! #/fn unique-name
      (sink-extfx-def-data-struct
        unique-name main-tag-string proj-strings)))
  
  (define/contract
    (sink-extfx-def-macro unique-name name-string body)
    (->
      sink-authorized-name?
      immutable-string?
      (->
        sink-fault?
        sink-authorized-name? sink? sink-text-input-stream?
        (->
          sink-authorized-name? sink? sink-text-input-stream?
          sink-cexpr?
          sink-extfx?)
        sink-extfx?)
      sink-extfx?)
    (sink-extfx-def-value-for-package unique-name
      (sink-name-for-bounded-cexpr-op
      #/sink-name-for-string #/sink-string name-string)
      (macro-impl #/fn
        fault unique-name qualify text-input-stream output-stream then
        
        (body fault unique-name qualify text-input-stream
        #/fn unique-name qualify text-input-stream cexpr
        #/sink-extfx-cexpr-write fault output-stream cexpr
        #/fn output-stream
        #/then unique-name qualify text-input-stream output-stream))))
  
  (define/contract (def-macro! name-string body)
    (->
      immutable-string?
      (->
        sink-fault?
        sink-authorized-name? sink? sink-text-input-stream?
        (->
          sink-authorized-name? sink? sink-text-input-stream?
          sink-cexpr?
          sink-extfx?)
        sink-extfx?)
      void?)
    (add-init-essentials-step! #/fn unique-name
      (sink-extfx-def-macro unique-name name-string body)))
  
  
  
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
      fault unique-name qualify text-input-stream output-stream then
      
      (sink-extfx-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)))
  
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
      fault unique-name qualify text-input-stream output-stream then
      
      (sink-extfx-claim-freshen unique-name #/fn unique-name
      #/sink-extfx-read-non-line-breaks fault text-input-stream
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
    (raise-cene-err clamor))
  
  (def-data-struct! "clamor-err" #/list "blame" "message")
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-blame" v
    (racket-boolean->sink #/sink-fault? v))
  
  ; TODO BUILTINS: Implement the macro `err`, probably in a Cene
  ; prelude. In the prelude, before we define `err`, we can still
  ; report errors using `[follow-heart/clamor-err bl /str-prim ...]`.
  
  
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
  
  (def-func-fault! "in-dex" fault dex v
    (expect dex (sink-dex dex)
      (cene-err fault "Expected dex to be a dex")
    #/racket-boolean->sink
    #/with-current-fault fault #/fn
    #/in-dex? dex v))
  
  (def-func-fault! "name-of" fault dex v
    (expect dex (sink-dex dex)
      (cene-err fault "Expected dex to be a dex")
    #/racket-maybe->sink
      (maybe-map
        (with-current-fault fault #/fn
        #/name-of dex v)
      #/fn name
        (sink-name name))))
  
  ; NOTE: In the JavaScript version of Cene, this was called
  ; `call-dex`.
  (def-func-fault! "compare-by-dex" fault dex a b
    (expect dex (sink-dex dex)
      (cene-err fault "Expected dex to be a dex")
    #/racket-maybe->sink
    #/maybe-map
      (with-current-fault fault #/fn
      #/compare-by-dex dex a b)
    #/fn dex-result
      (if (ordering-private? dex-result)
        (sink-ordering-private dex-result)
      #/dissect dex-result (ordering-eq)
      #/make-sink-struct (s-ordering-eq) #/list)))
  
  (def-data-struct! "dexable" #/list "dex" "val")
  
  (def-nullary-func! "dex-name"
    (sink-dex #/dex-struct sink-name #/dex-name))
  
  (def-nullary-func! "dex-dex"
    (sink-dex #/dex-struct sink-dex #/dex-dex))
  
  (def-nullary-func! "dex-give-up" (sink-dex #/dex-give-up))
  
  (def-func-fault! "dex-default"
    fault dex-for-trying-first dex-for-trying-second
    (expect dex-for-trying-first (sink-dex dex-for-trying-first)
      (cene-err fault "Expected dex-for-trying-first to be a dex")
    #/expect dex-for-trying-second (sink-dex dex-for-trying-second)
      (cene-err fault "Expected dex-for-trying-second to be a dex")
    #/sink-dex
    #/dex-default dex-for-trying-first dex-for-trying-second))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "dex-opaque" fault name dex
    (expect name (sink-name name)
      (cene-err fault "Expected name to be a name")
    #/expect dex (sink-dex dex)
      (cene-err fault "Expected dex to be a dex")
    #/sink-dex #/dex-opaque name dex))
  
  (def-func-fault! "dex-by-own-method" fault dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err fault "Expected dexable-get-method to be a valid dexable")
    #/sink-dex #/unsafe:dex-by-own-method-thorough-unchecked
    #/dexable-struct sink-dex-by-own-method-unthorough
      dexable-get-method))
  
  (def-func-fault! "dex-fix" fault dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err fault "Expected dexable-unwrap to be a valid dexable")
    #/sink-dex #/unsafe:dex-fix-unchecked
    #/dexable-struct converter-for-dex-fix dexable-unwrap))
  
  ; NOTE: In the JavaScript version of Cene, there was a similar
  ; operation called `dex-by-cline`.
  (def-func-fault! "get-dex-from-cline" fault cline
    (expect cline (sink-cline cline)
      (cene-err fault "Expected cline to be a cline")
    #/sink-dex
    ; TODO FAULT: See if we really need this `with-current-fault`
    ; call.
    #/with-current-fault fault #/fn
    #/get-dex-from-cline cline))
  
  (def-func-fault! "in-cline" fault cline v
    (expect cline (sink-cline cline)
      (cene-err fault "Expected cline to be a cline")
    #/racket-boolean->sink
    #/with-current-fault fault #/fn
    #/in-cline? cline v))
  
  ; NOTE: In the JavaScript version of Cene, this was called
  ; `call-cline`.
  (def-func-fault! "compare-by-cline" fault cline a b
    (expect cline (sink-cline cline)
      (cene-err fault "Expected cline to be a cline")
    #/racket-maybe->sink
    #/maybe-map
      (with-current-fault fault #/fn
      #/compare-by-cline cline a b)
    #/fn cline-result
      (if (ordering-private? cline-result)
        (sink-ordering-private cline-result)
      #/mat cline-result (ordering-lt)
        (make-sink-struct (s-ordering-lt) #/list)
      #/mat cline-result (ordering-gt)
        (make-sink-struct (s-ordering-gt) #/list)
      #/dissect cline-result (ordering-eq)
      #/make-sink-struct (s-ordering-eq) #/list)))
  
  (def-nullary-func! "dex-cline"
    (sink-dex #/dex-struct sink-cline #/dex-cline))
  
  (def-func-fault! "cline-by-dex" fault dex
    (expect dex (sink-dex dex)
      (cene-err fault "Expected dex to be a dex")
    #/sink-cline #/cline-by-dex dex))
  
  (def-nullary-func! "cline-give-up" (sink-cline #/cline-give-up))
  
  (def-func-fault! "cline-default"
    fault cline-for-trying-first cline-for-trying-second
    
    (expect cline-for-trying-first (sink-cline cline-for-trying-first)
      (cene-err fault "Expected cline-for-trying-first to be a cline")
    #/expect cline-for-trying-second
      (sink-cline cline-for-trying-second)
      (cene-err fault "Expected cline-for-trying-second to be a cline")
    #/sink-cline
    #/cline-default cline-for-trying-first cline-for-trying-second))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "cline-opaque" fault name cline
    (expect name (sink-name name)
      (cene-err fault "Expected name to be a name")
    #/expect cline (sink-cline cline)
      (cene-err fault "Expected cline to be a cline")
    #/sink-cline #/cline-opaque name cline))
  
  (def-func-fault! "cline-by-own-method" fault dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err fault "Expected dexable-get-method to be a valid dexable")
    #/sink-cline #/unsafe:cline-by-own-method-thorough-unchecked
    #/dexable-struct sink-cline-by-own-method-unthorough
      dexable-get-method))
  
  (def-func-fault! "cline-fix" fault dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err fault "Expected dexable-unwrap to be a valid dexable")
    #/sink-cline #/unsafe:cline-fix-unchecked
    #/dexable-struct converter-for-cline-fix dexable-unwrap))
  
  (def-func-fault! "call-merge" fault merge a b
    (expect merge (sink-merge merge)
      (cene-err fault "Expected merge to be a merge")
    #/racket-maybe->sink
    #/with-current-fault fault #/fn
    #/call-merge merge a b))
  
  (def-func-fault! "call-fuse" fault fuse a b
    (expect fuse (sink-fuse fuse)
      (cene-err fault "Expected fuse to be a fuse")
    #/racket-maybe->sink
    #/with-current-fault fault #/fn
    #/call-fuse fuse a b))
  
  (def-nullary-func! "dex-merge"
    (sink-dex #/dex-struct sink-merge #/dex-merge))
  
  (def-nullary-func! "dex-fuse"
    (sink-dex #/dex-struct sink-fuse #/dex-fuse))
  
  ; NOTE: We do not implement operations like `merge-default` or
  ; `fuse-default` from the JavaScript version of Cene since they are
  ; not well-behaved furges.
  
  (def-func-fault! "merge-by-dex" fault dex
    (expect dex (sink-dex dex)
      (cene-err fault "Expected dex to be a dex")
    #/sink-merge #/merge-by-dex dex))
  
  (def-func-fault! "fuse-by-merge" fault merge
    (expect merge (sink-merge merge)
      (cene-err fault "Expected merge to be a merge")
    #/sink-fuse #/fuse-by-merge merge))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "merge-opaque" fault name merge
    (expect name (sink-name name)
      (cene-err fault "Expected name to be a name")
    #/expect merge (sink-merge merge)
      (cene-err fault "Expected merge to be a merge")
    #/sink-merge #/merge-opaque name merge))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "fuse-opaque" fault name fuse
    (expect name (sink-name name)
      (cene-err fault "Expected name to be a name")
    #/expect fuse (sink-fuse fuse)
      (cene-err fault "Expected fuse to be a fuse")
    #/sink-fuse #/fuse-opaque name fuse))
  
  (def-func-fault! "merge-by-own-method" fault dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err fault "Expected dexable-get-method to be a valid dexable")
    #/sink-merge #/unsafe:merge-by-own-method-thorough-unchecked
    #/dexable-struct sink-merge-by-own-method-unthorough
      dexable-get-method))
  
  (def-func-fault! "fuse-by-own-method" fault dexable-get-method
    (expect (sink-valid-dexable->maybe-racket dexable-get-method)
      (just dexable-get-method)
      (cene-err fault "Expected dexable-get-method to be a valid dexable")
    #/sink-fuse #/unsafe:fuse-by-own-method-thorough-unchecked
    #/dexable-struct sink-fuse-by-own-method-unthorough
      dexable-get-method))
  
  (def-func-fault! "merge-fix" fault dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err fault "Expected dexable-unwrap to be a valid dexable")
    #/sink-merge #/unsafe:merge-fix-unchecked
    #/dexable-struct converter-for-merge-fix dexable-unwrap))
  
  (def-func-fault! "fuse-fix" fault dexable-unwrap
    (expect (sink-valid-dexable->maybe-racket dexable-unwrap)
      (just dexable-unwrap)
      (cene-err fault "Expected dexable-unwrap to be a valid dexable")
    #/sink-fuse #/unsafe:fuse-fix-unchecked
    #/dexable-struct converter-for-fuse-fix dexable-unwrap))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-fusable-fn" v
    (racket-boolean->sink
    #/mat v (sink-opaque-fn v) (fusable-function? v)
    #/mat v (sink-opaque-fn-fault v) (fusable-function? v)
      #f))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "make-fusable-fn" caller-fault func
    (sink-opaque-fn-fault
      (make-fusable-function #/dissectfn (list explicit-fault arg)
        (sink-call-fault caller-fault explicit-fault func arg))))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "fuse-fusable-fn-fault"
    fault dexable-fault-and-arg-to-method
    
    (expect
      (sink-valid-dexable->maybe-racket
        dexable-fault-and-arg-to-method)
      (just dexable-fault-and-arg-to-method)
      (cene-err fault "Expected dexable-fault-and-arg-to-method to be a valid dexable")
    #/sink-fuse #/fuse-struct sink-opaque-fn
    #/unsafe:fuse-fusable-function-thorough-unchecked
    #/dexable-struct sink-fuse-fusable-fn-unthorough
      dexable-fault-and-arg-to-method))
  
  
  ; Authorized names
  ;
  ; NOTE: The JavaScript version of Cene doesn't have any of these.
  
  ; TODO: See if we need authorized names to be dexable. If we do,
  ; expose a `dex-authorized-name` to Cene instead of this.
  (def-func! "is-authorized-name" v
    (racket-boolean->sink #/sink-authorized-name? v))
  
  (def-func-fault! "authorized-name-get-name" fault name
    (expect (sink-authorized-name? name) #t
      (cene-err fault "Expected name to be an authorized name")
    #/sink-authorized-name-get-name name))
  
  (def-func-fault! "name-subname" fault index-name inner-name
    (expect (sink-name? index-name) #t
      (cene-err fault "Expected index-name to be a name")
    #/expect (sink-name? inner-name) #t
      (cene-err fault "Expected inner-name to be a name")
    #/sink-name-subname index-name inner-name))
  
  (def-func-fault! "authorized-name-subname"
    fault index-name inner-name
    
    (expect (sink-name? index-name) #t
      (cene-err fault "Expected index-name to be a name")
    #/expect (sink-authorized-name? inner-name) #t
      (cene-err fault "Expected inner-name to be an authorized name")
    #/sink-authorized-name-subname index-name inner-name))
  
  
  ; Structs and function calls
  
  ; NOTE: In the JavaScript version of Cene, this was called
  ; `constructor-glossary` with projections `main-tag` and
  ; `source-to-rep`. The representation was otherwise the same except
  ; that the keys of the `source-to-rep`/`projections` assoc list were
  ; names, and now we store them as strings.
  (def-data-struct! "struct-metadata"
    (list "main-tag-name" "projections"))
  
  (define/contract
    (verify-cexpr-struct-args! fault main-tag-name projections)
    (-> sink-fault? sink? sink?
      (list/c name? (listof #/list/c name? cexpr?)))
    
    (begin (assert-can-get-cene-definitions!)
    #/expect main-tag-name (sink-authorized-name main-tag-name)
      (cene-err fault "Expected main-tag-name to be an authorized name")
    #/expect (sink-list->maybe-racket projections) (just projections)
      (cene-err fault "Expected projections to be a list made up of cons and nil values")
    #/w- projections
      (list-map projections #/fn projection
        (expect (unmake-sink-struct-maybe (s-assoc) projection)
          (just #/list k v)
          (cene-err fault "Expected projections to be a list of assoc values")
        #/expect k (sink-authorized-name k)
          (cene-err fault "Expected projections to be an association list with authorized names as keys")
        #/expect v (sink-cexpr v)
          (cene-err fault "Expected projections to be an association list with expressions as values")
        #/list (authorized-name-get-name k) v))
    #/if
      (names-have-duplicate?
        (list-map projections #/dissectfn (list k v) k))
      (cene-err fault "Expected projections to be an association list with mutually unique names as keys")
    #/list (authorized-name-get-name main-tag-name) projections))
  
  (define/contract
    (sink-extfx-expand-struct-op
      fault unique-name qualify text-input-stream then)
    (->
      sink-fault?
      sink-authorized-name? sink? sink-text-input-stream?
      (->
        sink-authorized-name? sink? sink-text-input-stream?
        name?
        (listof #/list/c name? cexpr?)
        sink-extfx?)
      sink-extfx?)
    
    (sink-extfx-claim-freshen unique-name #/fn unique-name
    #/sink-extfx-read-struct-metadata
      fault unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream metadata
    #/dissect (struct-metadata-tags metadata)
      (cons main-tag-name proj-names)
    
    #/sink-extfx-read-bounded-specific-number-of-cexprs
      fault unique-name qualify text-input-stream
      (length proj-names)
    #/fn unique-name qualify text-input-stream proj-exprs
    
    #/then unique-name qualify text-input-stream main-tag-name
      (map list proj-names proj-exprs)))
  
  ; NOTE: The JavaScript version of Cene makes this functionality
  ; possible using a combination of `cexpr-cline-struct`,
  ; `cline-by-dex`, and `dex-by-cline`. We will probably be offering
  ; `get-dex-from-cline` (as provided by Effection) instead of
  ; `dex-by-cline`, but the same circuitous combination would work.
  ; Nevertheless, we provide this operation directly.
  ;
  (def-func-fault! "expr-dex-struct" fault main-tag-name projections
    (dissect
      (verify-cexpr-struct-args! fault main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-dex-struct main-tag-name projections))
  
  (def-macro! "dex-struct" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-expand-struct-op
      fault unique-name qualify text-input-stream
    #/fn
      unique-name qualify text-input-stream main-tag-name projections
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-dex-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-cline-struct`.
  (def-func-fault! "expr-cline-struct" fault main-tag-name projections
    (dissect
      (verify-cexpr-struct-args! fault main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-cline-struct main-tag-name projections))
  
  (def-macro! "cline-struct" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-expand-struct-op
      fault unique-name qualify text-input-stream
    #/fn
      unique-name qualify text-input-stream main-tag-name projections
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-cline-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-merge-struct`.
  (def-func-fault! "expr-merge-struct" fault main-tag-name projections
    (dissect
      (verify-cexpr-struct-args! fault main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-merge-struct main-tag-name projections))
  
  (def-macro! "merge-struct" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-expand-struct-op
      fault unique-name qualify text-input-stream
    #/fn
      unique-name qualify text-input-stream main-tag-name projections
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-merge-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-fuse-struct`.
  (def-func-fault! "expr-fuse-struct" fault main-tag-name projections
    (dissect
      (verify-cexpr-struct-args! fault main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-fuse-struct main-tag-name projections))
  
  (def-macro! "fuse-struct" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-expand-struct-op
      fault unique-name qualify text-input-stream
    #/fn
      unique-name qualify text-input-stream main-tag-name projections
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-fuse-struct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-struct`.
  (def-func-fault! "expr-construct" fault main-tag-name projections
    (dissect
      (verify-cexpr-struct-args! fault main-tag-name projections)
      (list main-tag-name projections)
    #/sink-cexpr #/cexpr-construct main-tag-name projections))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-macro! "construct" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-expand-struct-op
      fault unique-name qualify text-input-stream
    #/fn
      unique-name qualify text-input-stream main-tag-name projections
    #/then unique-name qualify text-input-stream
    #/sink-cexpr #/cexpr-construct main-tag-name projections))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-case`.
  (def-func-fault! "expr-case"
    fault subject-expr main-tag-name projections then-expr else-expr
    
    (expect subject-expr (sink-cexpr subject-expr)
      (cene-err fault "Expected subject-expr to be an expression")
    #/expect main-tag-name (sink-authorized-name main-tag-name)
      (cene-err fault "Expected main-tag-name to be an authorized name")
    #/expect (sink-list->maybe-racket projections) (just projections)
      (cene-err fault "Expected projections to be a list made up of cons and nil values")
    #/w- projections
      (list-map projections #/fn projection
        (expect (unmake-sink-struct-maybe (s-assoc) projection)
          (just #/list k v)
          (cene-err fault "Expected projections to be a list of assoc values")
        #/expect k (sink-authorized-name k)
          (cene-err fault "Expected projections to be an association list with authorized names as keys")
        #/expect v (sink-name v)
          (cene-err fault "Expected projections to be an association list with names as values")
        #/list k v))
    #/if
      (names-have-duplicate?
        (list-map projections #/dissectfn (list k v)
          (authorized-name-get-name k)))
      (cene-err fault "Expected projections to be an association list with mutually unique authorized names as keys")
    #/if
      (names-have-duplicate?
        (list-map projections #/dissectfn (list k v) v))
      (cene-err fault "Expected projections to be an association list with mutually unique names as values")
    #/expect then-expr (sink-cexpr then-expr)
      (cene-err fault "Expected then-expr to be an expression")
    #/expect else-expr (sink-cexpr else-expr)
      (cene-err fault "Expected else-expr to be an expression")
    #/sink-cexpr #/cexpr-case subject-expr
      (cons (authorized-name-get-name main-tag-name)
      #/list-map projections #/dissectfn (list proj-name var)
        (authorized-name-get-name proj-name))
      (list-map projections #/dissectfn (list proj-name var)
        var)
      then-expr
      else-expr))
  
  (define/contract
    (sink-extfx-read-case-pattern
      fault unique-name qualify text-input-stream then)
    (->
      sink-fault?
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (->i
        (
          [unique-name sink-authorized-name?]
          [qualify sink?]
          [text-input-stream sink-text-input-stream?]
          [tags (listof name?)]
          [vars (listof sink-name?)])
        #:pre (tags vars) (= (length tags) (add1 #/length vars))
        [_ sink-extfx?])
      sink-extfx?)
    
    (sink-extfx-claim-freshen unique-name #/fn unique-name
    #/sink-extfx-read-struct-metadata
      fault unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream metadata
    #/w- tags (struct-metadata-tags metadata)
    #/w- n-projs (struct-metadata-n-projs metadata)
    
    #/sink-extfx-read-leading-specific-number-of-identifiers
      fault unique-name qualify text-input-stream n-projs
      sink-name-for-local-variable
    #/fn unique-name qualify text-input-stream vars
    #/w- vars
      (list-map vars #/dissectfn (list located-string var)
        (sink-authorized-name-get-name var))
    #/if (sink-names-have-duplicate? vars)
      ; TODO FAULT: Make this `fault` more specific.
      (cene-err fault "Expected the variables of a case pattern to be mutually unique")
    
    #/then unique-name qualify text-input-stream tags vars))
  
  (def-macro! "case" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-leading-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-subject
    #/dissect args-subject (list subject-expr)
    
    #/sink-extfx-read-case-pattern
      fault unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream tags vars
    
    #/sink-extfx-read-bounded-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 2
    #/fn unique-name qualify text-input-stream args-branches
    #/dissect args-branches (list then-expr else-expr)
    
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-case subject-expr tags vars then-expr else-expr))
  
  (def-macro! "cast" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-leading-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-subject
    #/dissect args-subject (list subject-expr)
    
    #/sink-extfx-read-case-pattern
      fault unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream tags vars
    
    #/sink-extfx-read-bounded-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 2
    #/fn unique-name qualify text-input-stream args-branches
    #/dissect args-branches (list else-expr then-expr)
    
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-case subject-expr tags vars then-expr else-expr))
  
  (def-macro! "caselet" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-leading-specific-number-of-identifiers
      fault unique-name qualify text-input-stream 1
      sink-name-for-local-variable
    #/fn unique-name qualify text-input-stream args-subject-var
    #/dissect args-subject-var (list #/list _ subject-var)
    #/w- subject-var (sink-authorized-name-get-name subject-var)
    
    #/sink-extfx-read-leading-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-subject-expr
    #/dissect args-subject-expr (list subject-expr)
    
    #/sink-extfx-read-case-pattern
      fault unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream tags vars
    
    #/sink-extfx-read-bounded-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 2
    #/fn unique-name qualify text-input-stream args-branches
    #/dissect args-branches (list then-expr else-expr)
    
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-let (list #/list subject-var subject-expr)
    #/sink-cexpr-case (sink-cexpr-var subject-var) tags vars
      then-expr
      else-expr))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "expr-call-blame"
    fault fault-arg-expr func-expr arg-expr
    
    (expect fault-arg-expr (sink-cexpr fault-arg-expr)
      (cene-err fault "Expected blame-arg-expr to be an expression")
    #/expect func-expr (sink-cexpr func-expr)
      (cene-err fault "Expected func-expr to be an expression")
    #/expect arg-expr (sink-cexpr arg-expr)
      (cene-err fault "Expected arg-expr to be an expression")
    #/sink-cexpr
      (cexpr-call-fault fault-arg-expr func-expr arg-expr)))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-macro! "c-blame" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-leading-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 2
    #/fn unique-name qualify text-input-stream args-func
    #/dissect args-func
      (list (sink-cexpr fault-arg-expr) (sink-cexpr func-expr))
    
    #/sink-extfx-read-bounded-cexprs
      fault unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream args-args
    #/w- args-args
      (list-map args-args #/dissectfn (sink-cexpr arg-arg) arg-arg)
    
    #/expect (reverse args-args)
      (cons last-arg-arg rev-past-args-args)
      ; TODO FAULT: Make this `fault` more specific.
      (cene-err fault "Expected a c-blame form to have at least one argument aside from the blame argument and the function to call")
    
    #/then unique-name qualify text-input-stream
      (sink-cexpr #/cexpr-call-fault fault-arg-expr
        (list-foldl func-expr (reverse rev-past-args-args)
          (fn func arg
            (cexpr-call func arg)))
        last-arg-arg)))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-call`.
  (def-func-fault! "expr-call" fault func-expr arg-expr
    (expect func-expr (sink-cexpr func-expr)
      (cene-err fault "Expected func-expr to be an expression")
    #/expect arg-expr (sink-cexpr arg-expr)
      (cene-err fault "Expected arg-expr to be an expression")
    #/sink-cexpr #/cexpr-call func-expr arg-expr))
  
  (def-macro! "c" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-leading-specific-number-of-cexprs
      fault unique-name qualify text-input-stream 1
    #/fn unique-name qualify text-input-stream args-func
    #/dissect args-func (list (sink-cexpr func-expr))
    
    #/sink-extfx-read-bounded-cexprs
      fault unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream args-args
    #/w- args-args
      (list-map args-args #/dissectfn (sink-cexpr arg-arg) arg-arg)
    
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
  (def-func-fault! "expr-opaque-fn-blame" fault fault-param param body
    (expect (sink-name? fault-param) #t
      (cene-err fault "Expected blame-param to be a name")
    #/expect (sink-name? param) #t
      (cene-err fault "Expected param to be a name")
    #/expect (sink-cexpr? body) #t
      (cene-err fault "Expected body to be an expression")
    #/if (sink-names-have-duplicate? #/list fault-param param)
      (cene-err fault "Expected blame-param and param to be mutually unique")
    #/sink-cexpr-opaque-fn-fault fault-param param body))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "expr-opaque-fn" fault param body
    (expect (sink-name? param) #t
      (cene-err fault "Expected param to be a name")
    #/expect (sink-cexpr? body) #t
      (cene-err fault "Expected body to be an expression")
    #/sink-cexpr-opaque-fn param body))
  
  ; TODO BUILTINS: Implement `defn`, probably in a Cene prelude.
  
  (define (parse-param param)
    (expect param
      (id-or-expr-id param-located-string param-qualified-name)
      (nothing)
    #/just #/sink-authorized-name-get-name param-qualified-name))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-macro! "fn-blame" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-bounded-ids-and-exprs
      fault unique-name qualify text-input-stream
      sink-name-for-local-variable
    #/fn unique-name qualify text-input-stream args
    #/expect args (cons fault-param args)
      (cene-err fault "Expected a fn-blame form to have a blame parameter")
    #/expect (parse-param fault-param) (just fault-param)
      (cene-err fault "Expected the blame parameter of a fn-blame form to be an identifier")
    #/expect (reverse args) (cons body rev-params)
      (cene-err fault "Expected a fn-blame form to have a body expression")
    #/w- rev-params
      (list-map rev-params #/fn param
        (expect (parse-param param) (just param)
          (cene-err fault "Expected every parameter of a fn form to be an identifier")
          param))
    #/if (sink-names-have-duplicate? #/cons fault-param rev-params)
      (cene-err fault "Expected every parameter of a fn form to be unique, including the blame parameter")
    #/expect rev-params (cons last-param rev-past-params)
      (cene-err fault "Expected a fn-blame form to have at least one parameter aside from the blame parameter")
    #/then unique-name qualify text-input-stream
      (list-foldl
        (sink-cexpr-opaque-fn-fault fault-param last-param
          (id-or-expr->cexpr body))
        rev-past-params
      #/fn body param
        (sink-cexpr-opaque-fn param body))))
  
  (def-macro! "fn" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-bounded-ids-and-exprs
      fault unique-name qualify text-input-stream
      sink-name-for-local-variable
    #/fn unique-name qualify text-input-stream args
    #/expect (reverse args) (cons body rev-params)
      (cene-err fault "Expected a fn form to have a body expression")
    #/w- rev-params
      (list-map rev-params #/fn param
        (expect (parse-param param) (just param)
          (cene-err fault "Expected every parameter of a fn form to be an identifier")
          param))
    #/if (sink-names-have-duplicate? rev-params)
      (cene-err fault "Expected every parameter of a fn form to be mutually unique")
    #/then unique-name qualify text-input-stream
      (list-foldl (id-or-expr->cexpr body) rev-params #/fn body param
        (sink-cexpr-opaque-fn param body))))
  
  
  ; Tables
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-table" v
    (racket-boolean->sink #/sink-table? v))
  
  (def-func-fault! "dex-table" fault dex-val
    (expect dex-val (sink-dex dex-val)
      (cene-err fault "Expected dex-val to be a dex")
    #/sink-dex-table dex-val))
  
  (def-func-fault! "merge-table" fault merge-val
    (expect merge-val (sink-merge merge-val)
      (cene-err fault "Expected merge-val to be a merge")
    #/sink-merge #/merge-struct sink-table #/merge-table merge-val))
  
  (def-func-fault! "fuse-table" fault fuse-val
    (expect fuse-val (sink-fuse fuse-val)
      (cene-err fault "Expected fuse-val to be a fuse")
    #/sink-fuse #/fuse-struct sink-table #/fuse-table fuse-val))
  
  (def-nullary-func! "table-empty" (sink-table #/table-empty))
  
  (def-func-fault! "table-shadow" fault key maybe-val table
    (expect (sink-name? key) #t
      (cene-err fault "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err fault "Expected table to be a table")
    #/expect (sink-maybe->maybe-racket maybe-val) (just maybe-val)
      (cene-err fault "Expected maybe-val to be a nothing or a just")
    #/sink-table-put-maybe table key maybe-val))
  
  (def-func-fault! "table-get" fault key table
    (expect (sink-name? key) #t
      (cene-err fault "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err fault "Expected table to be a table")
    #/racket-maybe->sink #/sink-table-get-maybe table key))
  
  (def-func-fault! "table-map-fuse" fault table fuse key-to-operand
    (expect table (sink-table table)
      (cene-err fault "Expected table to be a table")
    #/expect fuse (sink-fuse fuse)
      (cene-err fault "Expected fuse to be a fuse")
    #/table-map-fuse table fuse #/fn k
      (sink-call fault key-to-operand #/sink-name k)))
  
  (def-func-fault! "table-sort" fault cline table
    (expect cline (sink-cline cline)
      (cene-err fault "Expected cline to be a cline")
    #/expect table (sink-table table)
      (cene-err fault "Expected table to be a table")
    #/racket-maybe->sink
    #/maybe-map (table-sort cline table) #/fn ranks
      (racket-list->sink #/list-map ranks #/fn rank
        (sink-table rank))))
  
  
  ; Effects
  
  ; NOTE: In the JavaScript version of Cene, the `...extfx...`
  ; built-ins (or at least the ones that existed) were known as
  ; `...effects...`.
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `no-effects`.
  ;
  ; TODO: See which name we prefer.
  ;
  (def-nullary-func! "extfx-noop" (sink-extfx-noop))
  
  (def-nullary-func! "fuse-extfx"
    (sink-fuse #/unsafe:fuse #/fuse-internals-extfx))
  
  ; NOTE BUILTINS: The following built-ins from the JavaScript version
  ; of Cene seem like they're not relevant to the approach we've taken
  ; here, since we're using a single global namespace instead of
  ; first-class namespaces.
  ;
  ;   get-mode
  ;   assert-current-mode
  
  (define/contract (verify-callback-extfx! fault effects)
    (-> sink-fault? sink? sink-extfx?)
    (expect (sink-extfx? effects) #t
      (cene-err fault "Expected the return value of the callback to be an extfx effects value")
      effects))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `later`, and it took an extfx effects value rather than a function
  ; that computed one. When we needed to do what this does, we used
  ; `get-mode` and ignored the mode value.
  (def-func-fault! "extfx-later" fault get-effects
    (sink-extfx-later #/fn
    #/verify-callback-extfx! fault
    #/sink-call fault get-effects
      (make-sink-struct (s-trivial) #/list)))
  
  ; TODO BUILTINS: Consider implementing the following.
  ;
  ;   make-promise-later
  ;   getdef
  ;   definer-define
  ;   committing-to-define
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "extfx-get" fault name then
    (expect (sink-name? name) #t
      (cene-err fault "Expected name to be a name")
    #/sink-extfx-get name #/fn result
    #/verify-callback-extfx! fault #/sink-call fault then result))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "extfx-put" fault name dex value
    (expect (sink-authorized-name? name) #t
      (cene-err fault "Expected name to be an authorized name")
    #/expect (sink-dex? dex) #t
      (cene-err fault "Expected dex to be a dex")
    #/sink-extfx-put name dex value))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  ;
  ; TODO: It's a little more cumbersome to destructure lists in Cene
  ; rather than Racket. See if, instead, we should have this return a
  ; struct or replace this with separate `establish-pub` and
  ; `establish-sub` functions.
  ;
  (def-func-fault! "establish-pubsub" fault name
    (expect (sink-authorized-name? name) #t
      (cene-err fault "Expected name to be an authorized name")
    #/racket-list->sink #/cene-definition-establish-pubsub name))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "extfx-pub-write" fault p unique-name arg
    (expect (sink-pub? p) #t
      (cene-err fault "Expected p to be a pub")
    #/expect (sink-authorized-name? unique-name) #t
      (cene-err fault "Expected unique-name to be an authorized name")
    #/sink-extfx-pub-write p unique-name arg))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "extfx-sub-write" fault s unique-name func
    (expect (sink-pub? s) #t
      (cene-err fault "Expected s to be a sub")
    #/expect (sink-authorized-name? unique-name) #t
      (cene-err fault "Expected unique-name to be an authorized name")
    #/sink-extfx-sub-write s unique-name #/fn arg
      (verify-callback-extfx! fault #/sink-call fault func arg)))
  
  
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
  (def-func-fault! "expr-var" fault var
    (expect (sink-name? var) #t
      (cene-err fault "Expected var to be a name")
    #/sink-cexpr-var var))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-reified`.
  (def-func! "expr-reified" val
    (sink-cexpr-reified val))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "expr-located" fault location-definition-name body
    (expect (sink-name? location-definition-name) #t
      (cene-err fault "Expected location-definition-name to be a name")
    #/expect body (sink-cexpr body)
      (cene-err fault "Expected body to be an expression")
    #/sink-cexpr #/cexpr-located location-definition-name body))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `cexpr-let`.
  (def-func-fault! "expr-let" fault bindings body
    (expect (sink-list->maybe-racket bindings) (just bindings)
      (cene-err fault "Expected bindings to be a list")
    #/w- bindings
      (list-map bindings #/fn binding
        (expect (unmake-sink-struct-maybe (s-assoc) binding)
          (just #/list var val)
          (cene-err fault "Expected bindings to be an assoc list")
        #/expect (sink-name? var) #t
          (cene-err fault "Expected bindings to be an assoc list with names as the keys")
        #/expect (sink-cexpr? val) #t
          (cene-err fault "Expected bindings to be an assoc list with expressions as the values")
        #/list var val))
    #/expect (sink-cexpr? body) #t
      (cene-err fault "Expected body to be an expression")
    #/sink-cexpr-let bindings body))
  
  (def-macro! "let" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-read-bounded-ids-and-exprs
      fault unique-name qualify text-input-stream
      sink-name-for-local-variable
    #/fn unique-name qualify text-input-stream args
    #/expect (reverse args) (cons body rev-bindings)
      (cene-err fault "Expected a let form to have a body expression")
    #/w- bindings
      (w-loop next rest rev-bindings so-far (list)
        (mat rest (list) so-far
        #/expect rest (list* val var rest)
          (cene-err fault "Expected a let form to have an odd number of subforms")
        #/next rest (cons (list var val) so-far)))
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-let
      (list-map bindings #/dissectfn (list var val)
        (expect var
          (id-or-expr-id var-located-string var-qualified-name)
          (cene-err fault "Expected every bound variable of a let form to be an identifier")
        #/list
          (sink-authorized-name-get-name var-qualified-name)
          (id-or-expr->cexpr val)))
      (id-or-expr->cexpr body)))
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func! "is-expr" v
    (racket-boolean->sink #/sink-cexpr? v))
  
  ; TODO: See if this can be a pure function.
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "extfx-expr-can-eval" fault expr then
    (expect expr (sink-cexpr expr)
      (cene-err fault "Expected expr to be an expression")
    #/sink-extfx-later #/fn
    #/verify-callback-extfx! fault #/sink-call fault then
    #/racket-boolean->sink #/cexpr-can-eval? expr))
  
  ; TODO: See if this can be a pure function.
  ;
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `eval-cexpr`, and it was a pure function that took a mode
  ; parameter.
  ;
  (def-func-fault! "extfx-expr-eval-blame"
    caller-fault explicit-fault expr then
    
    (expect expr (sink-cexpr expr)
      (cene-err caller-fault "Expected expr to be an expression")
    #/sink-extfx-later #/fn
    #/expect (cexpr-can-eval? expr) #t
      (cene-err caller-fault "Expected expr to be an expression which had all the information it needed for evaluation")
    #/sink-extfx-cexpr-eval explicit-fault expr #/fn result
    #/verify-callback-extfx! caller-fault
      (sink-call caller-fault then result)))
  
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
  
  (def-func-fault! "int-minus" fault minuend subtrahend
    (expect minuend (sink-int minuend)
      (cene-err fault "Expected minuend to be an int")
    #/expect subtrahend (sink-int subtrahend)
      (cene-err fault "Expected subtrahend to be an int")
    #/sink-int #/- minuend subtrahend))
  
  (def-func-fault! "int-div-rounded-down" fault dividend divisor
    (expect dividend (sink-int dividend)
      (cene-err fault "Expected dividend to be an int")
    #/expect divisor (sink-int divisor)
      (cene-err fault "Expected divisor to be an int")
    #/mat divisor 0 (make-sink-struct (s-nothing) #/list)
    #/make-sink-struct (s-just) #/list
    #/let-values ([(q r) (quotient/remainder dividend divisor)])
    #/if (<= 0 r)
      (make-sink-struct (s-carried) #/list (sink-int q) (sink-int r))
    #/if (<= 0 divisor)
      (make-sink-struct (s-carried)
      #/list (sink-int #/- q 1) (sink-int #/+ r divisor))
      (make-sink-struct (s-carried)
      #/list (sink-int #/+ q 1) (sink-int #/- r divisor))))
  
  (def-data-struct! "carried" #/list "main" "carry")
  
  
  ; Strings
  
  (def-nullary-func! "dex-string" (sink-dex-string))
  
  (def-nullary-func! "string-empty" (sink-string ""))
  
  (def-func-fault! "string-singleton" fault unicode-scalar
    (expect unicode-scalar (sink-int unicode-scalar)
      (cene-err fault "Expected unicode-scalar to be an int")
    #/expect
      (and
        (<= 0 unicode-scalar #x10FFFF)
        (not #/<= #xD800 unicode-scalar #xDFFF))
      #t
      (cene-err fault "Expected unicode-scalar to be in the range of valid Unicode scalars")
    #/sink-string #/string->immutable-string
    #/list->string #/list #/integer->char unicode-scalar))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `string-append-later`.
  (def-func-fault! "extfx-string-append" fault a b then
    (expect a (sink-string a)
      (cene-err fault "Expected a to be a string")
    #/expect b (sink-string b)
      (cene-err fault "Expected b to be a string")
    #/sink-extfx-later #/fn
    #/verify-callback-extfx! fault #/sink-call fault then
    #/sink-string #/string->immutable-string #/string-append a b))
  
  ; This is an extremely basic string syntax. It doesn't have any
  ; support for escape sequences. It reads a body that must have zero
  ; occurrences of [ ] ( ) and it treats that body verbatim as the
  ; contents of the string.
  ;
  ; NOTE: The JavaScript version of Cene doesn't have this.
  ;
  (def-macro! "str-prim" #/fn
    fault unique-name qualify text-input-stream then
    
    (sink-extfx-claim-freshen unique-name #/fn unique-name
    #/sink-extfx-optimized-textpat-read-located
      fault str-prim-pat text-input-stream
    #/fn text-input-stream maybe-contents
    #/dissect maybe-contents (just contents)
    #/sink-extfx-string-from-located-string contents #/fn contents
    #/then unique-name qualify text-input-stream
    #/sink-cexpr-reified contents))
  
  ; TODO BUILTINS: Implement the macro `str`, probably in a Cene
  ; prelude.
  
  (def-func-fault! "string-length" fault string
    (expect string (sink-string string)
      (cene-err fault "Expected string to be a string")
    #/sink-int #/string-length string))
  
  (def-func-fault! "string-get-unicode-scalar" fault string start
    (expect string (sink-string string)
      (cene-err fault "Expected string to be a string")
    #/expect start (sink-int start)
      (cene-err fault "Expected start to be an int")
    #/expect (<= 0 start) #t
      (cene-err fault "Expected start to be a nonnegative int")
    #/expect (< start #/string-length string) #t
      (cene-err fault "Expected start to be an int less than the length of string")
    #/sink-int #/char->integer #/string-ref string start))
  
  (def-func-fault! "string-cut-later" fault string start stop then
    (expect string (sink-string string)
      (cene-err fault "Expected string to be a string")
    #/expect start (sink-int start)
      (cene-err fault "Expected start to be an int")
    #/expect stop (sink-int stop)
      (cene-err fault "Expected stop to be an int")
    #/expect (<= 0 start) #t
      (cene-err fault "Expected start to be a nonnegative int")
    #/expect (<= start stop) #t
      (cene-err fault "Expected start to be an int no greater than stop")
    #/expect (<= stop #/string-length string) #t
      (cene-err fault "Expected stop to be an int no greater than the length of string")
    #/sink-extfx-later #/fn
    #/sink-call fault then
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
  
  (def-func-fault! "textpat-if" fault condition then else
    (expect condition (sink-textpat condition)
      (cene-err fault "Expected condition to be a text pattern")
    #/expect then (sink-textpat then)
      (cene-err fault "Expected then to be a text pattern")
    #/expect else (sink-textpat else)
      (cene-err fault "Expected else to be a text pattern")
    #/sink-textpat #/textpat-if condition then else))
  
  (def-func-fault! "textpat-while" fault condition body
    (expect condition (sink-textpat condition)
      (cene-err fault "Expected condition to be a text pattern")
    #/expect body (sink-textpat body)
      (cene-err fault "Expected body to be a text pattern")
    #/sink-textpat #/textpat-while condition body))
  
  (def-func-fault! "textpat-until" fault body condition
    (expect body (sink-textpat body)
      (cene-err fault "Expected body to be a text pattern")
    #/expect condition (sink-textpat condition)
      (cene-err fault "Expected condition to be a text pattern")
    #/sink-textpat #/textpat-until body condition))
  
  (def-func-fault! "textpat-one-in-range" fault start stop
    (expect start (sink-string start)
      (cene-err fault "Expected start to be a string")
    #/expect (string-length start) 1
      (cene-err fault "Expected start to be a string containing a single Unicode scalar value")
    #/w- start (string-ref start 0)
    #/expect stop (sink-string stop)
      (cene-err fault "Expected stop to be a string")
    #/expect (string-length stop) 1
      (cene-err fault "Expected stop to be a string containing a single Unicode scalar value")
    #/w- stop (string-ref stop 0)
    #/sink-textpat #/textpat-one-in-range start stop))
  
  (def-nullary-func! "textpat-one"
    (sink-textpat #/textpat-one))
  
  (def-func-fault! "textpat-from-string" fault str
    (expect str (sink-string str)
      (cene-err fault "Expected str to be a string")
    #/sink-textpat #/textpat-from-string str))
  
  (def-func-fault! "textpat-one-in-string" fault str
    (expect str (sink-string str)
      (cene-err fault "Expected str to be a string")
    #/sink-textpat #/textpat-one-in-string str))
  
  (def-func-fault! "textpat-has-empty" fault t
    (expect t (sink-textpat t)
      (cene-err fault "Expected t to be a text pattern")
    #/racket-boolean->sink #/textpat-has-empty? t))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `optimize-regex-later`.
  (def-func-fault! "extfx-optimize-textpat" fault t then
    (expect t (sink-textpat t)
      (cene-err fault "Expected t to be a text pattern")
    #/sink-extfx-optimize-textpat t #/fn t
    #/verify-callback-extfx! fault #/sink-call fault then
    #/sink-optimized-textpat t))
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `optimized-regex-match-later`.
  (def-func-fault! "extfx-optimized-textpat-match"
    fault ot str start stop then
    
    (expect ot (sink-optimized-textpat ot)
      (cene-err fault "Expected ot to be a text pattern")
    #/expect str (sink-string str)
      (cene-err fault "Expected str to be a string")
    #/w- n (string-length str)
    #/expect start (sink-int start)
      (cene-err fault "Expected start to be an integer")
    #/expect (<= 0 start) #t
      (cene-err fault "Expected start to be a nonnegative integer")
    #/expect (<= start n) #t
      (cene-err fault "Expected start to be less than or equal to the length of the string")
    #/expect stop (sink-int stop)
      (cene-err fault "Expected stop to be an integer")
    #/expect (<= 0 stop) #t
      (cene-err fault "Expected stop to be a nonnegative integer")
    #/expect (<= stop n) #t
      (cene-err fault "Expected stop to be less than or equal to the length of the string")
    #/expect (<= start stop) #t
      (cene-err fault "Expected start to be less than or equal to stop")
    #/sink-extfx-later #/fn
    #/verify-callback-extfx! fault #/sink-call fault then
    #/w- result (optimized-textpat-match ot str start stop)
    #/mat result (textpat-result-matched stop)
      (make-sink-struct (s-textpat-result-matched) #/list
      #/sink-int stop)
    #/mat result (textpat-result-failed)
      (make-sink-struct (s-textpat-result-failed) #/list)
    #/dissect result (textpat-result-passed-end)
      (make-sink-struct (s-textpat-result-passed-end) #/list)))
  
  (def-data-struct! "textpat-result-matched" #/list "stop")
  (def-data-struct! "textpat-result-failed" #/list)
  (def-data-struct! "textpat-result-passed-end" #/list)
  
  ; NOTE: The JavaScript version of Cene doesn't have this.
  (def-func-fault! "extfx-optimized-textpat-read-located"
    fault ot input-stream then
    
    (expect ot (sink-optimized-textpat ot)
      (cene-err fault "Expected ot to be a text pattern")
    #/expect (sink-text-input-stream? input-stream) #t
      (cene-err fault "Expected input-stream to be a text input stream")
    #/sink-extfx-optimized-textpat-read-located fault ot input-stream
    #/fn input-stream maybe-result
    #/verify-callback-extfx! fault #/sink-call fault then
      input-stream
      (racket-maybe->sink maybe-result)))
  
  
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
  
  (def-func-fault! "extfx-claim" fault name on-success
    (expect (sink-authorized-name? name) #t
      (cene-err fault "Expected name to be an authorized name")
    #/sink-extfx-claim name #/fn
    #/verify-callback-extfx! fault
      (sink-call fault on-success
        (make-sink-struct (s-trivial) #/list))))
  
  (def-func-fault! "name-for-function-implementation-code"
    fault main-tag-name proj-tag-names
    
    (expect (sink-name? main-tag-name) #t
      (cene-err fault "Expected main-tag-name to be a name")
    #/expect
      (in-dex? (sink-dex-table #/sink-dex-struct (s-trivial) #/list)
        proj-tag-names)
      #t
      (cene-err fault "Expected proj-tag-names to be a table of trivial values")
    #/dissect proj-tag-names (sink-table proj-tag-names)
    #/w- proj-tag-names (table-v-map proj-tag-names #/fn v #/trivial)
    #/sink-name-for-function-implementation-code
      main-tag-name proj-tag-names))
  
  (def-func-fault! "name-for-function-implementation-value"
    fault main-tag-name proj-tag-names
    
    (expect (sink-name? main-tag-name) #t
      (cene-err fault "Expected main-tag-name to be a name")
    #/expect
      (in-dex? (sink-dex-table #/sink-dex-struct (s-trivial) #/list)
        proj-tag-names)
      #t
      (cene-err fault "Expected proj-tag-names to be a table of trivial values")
    #/dissect proj-tag-names (sink-table proj-tag-names)
    #/w- proj-tag-names (table-v-map proj-tag-names #/fn v #/trivial)
    #/sink-name-for-function-implementation-value
      main-tag-name proj-tag-names))
  
  (define (verify-proj-tag-authorized-names! fault proj-tag-names)
    (expect proj-tag-names (sink-table proj-tag-names)
      (cene-err fault "Expected proj-tag-names to be a table")
    #/table-kv-map proj-tag-names #/fn k v
      (expect v (sink-authorized-name name)
        (cene-err fault "Expected each value of proj-tag-names to be an authorized name")
      #/expect
        (eq-by-dex? (dex-name) k (authorized-name-get-name name))
        #t
        (cene-err fault "Expected each value of proj-tag-names to be an authorized name where the name authorized is the same as the name it's filed under")
        name)))
  
  (def-func-fault! "authorized-name-for-function-implementation-code"
    fault main-tag-name proj-tag-names
    
    (expect (sink-authorized-name? main-tag-name) #t
      (cene-err fault "Expected main-tag-name to be an authorized name")
    #/w- proj-tag-names
      (verify-proj-tag-authorized-names! fault proj-tag-names)
    #/sink-authorized-name-for-function-implementation-code
      main-tag-name proj-tag-names))
  
  (def-func-fault! "authorized-name-for-function-implementation-value"
    fault main-tag-name proj-tag-names
    
    (expect (sink-authorized-name? main-tag-name) #t
      (cene-err fault "Expected main-tag-name to be an authorized name")
    #/w- proj-tag-names
      (verify-proj-tag-authorized-names! fault proj-tag-names)
    #/sink-authorized-name-for-function-implementation-value
      main-tag-name proj-tag-names))
  
  (def-func-fault! "name-for-freestanding-expr-op" fault name
    (expect (sink-name? name) #t
      (cene-err fault "Expected name to be a name")
    #/sink-name-for-freestanding-cexpr-op name))
  
  (def-func-fault! "name-for-bounded-expr-op" fault name
    (expect (sink-name? name) #t
      (cene-err fault "Expected name to be a name")
    #/sink-name-for-bounded-cexpr-op name))
  
  (def-func-fault! "name-for-nameless-bounded-expr-op" fault name
    (expect (sink-name? name) #t
      (cene-err fault "Expected name to be a name")
    #/sink-name-for-nameless-bounded-cexpr-op name))
  
  (def-func-fault! "name-for-struct-main-tag" fault name
    (expect (sink-name? name) #t
      (cene-err fault "Expected name to be a name")
    #/sink-name-for-struct-main-tag name))
  
  (def-func-fault! "name-for-struct-proj"
    fault qualified-main-tag-name proj-name
    
    (expect (sink-name? qualified-main-tag-name) #t
      (cene-err fault "Expected qualified-main-tag-name to be a name")
    #/expect (sink-name? proj-name) #t
      (cene-err fault "Expected proj-name to be a name")
    #/sink-name-for-struct-proj qualified-main-tag-name proj-name))
  
  (def-func-fault! "name-for-local-variable" fault name
    (expect (sink-name? name) #t
      (cene-err fault "Expected name to be a name")
    #/sink-name-for-local-variable name))
  
  (def-func-fault! "name-for-struct-metadata" fault name
    (expect (sink-name? name) #t
      (cene-err fault "Expected name to be a name")
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
  
  (def-func-fault! "extfx-string-from-located-string"
    fault located-string then
    
    (expect (sink-located-string? located-string) #t
      (cene-err fault "Expected located-string to be a located string")
    #/sink-extfx-string-from-located-string located-string #/fn string
    #/verify-callback-extfx! fault #/sink-call fault then string))
  
  ; TODO BUILTINS: Make sure we have a sufficient set of operations
  ; for manipulating `sink-located-string` values.
  
  (def-func! "is-expr-sequence-output-stream" v
    (racket-boolean->sink #/sink-cexpr-sequence-output-stream? v))
  
  (def-func-fault! "extfx-make-expr-sequence-output-stream"
    fault unique-name state on-expr then
    
    ; TODO: See if we should differentiate the error messages for
    ; these three occurrences of `verify-callback-extfx!`.
    (expect (sink-authorized-name? unique-name) #t
      (cene-err fault "Expected unique-name to be an authorized name")
    #/sink-extfx-make-cexpr-sequence-output-stream
      fault
      unique-name
      state
      (fn state cexpr then
        (verify-callback-extfx! fault #/sink-call fault on-expr
          state
          cexpr
          (sink-fn-curried 1 #/fn state #/then state)))
    #/fn output-stream unwrap
    #/verify-callback-extfx! fault #/sink-call fault then
      output-stream
      (sink-fn-curried-fault 2 #/fn fault output-stream then
      #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
        (cene-err fault "Expected output-stream to be an expression sequence output stream")
      #/unwrap output-stream #/fn state
      #/verify-callback-extfx! fault #/sink-call fault then state)))
  
  (def-func-fault! "extfx-expr-write" fault output-stream expr then
    (expect (sink-cexpr-sequence-output-stream? output-stream) #t
      (cene-err fault "Expected output-stream to be an expression sequence output stream")
    #/expect (cexpr? expr) #t
      (cene-err fault "Expected expr to be an expression")
    #/sink-extfx-cexpr-write fault output-stream expr
    #/fn output-stream
    #/verify-callback-extfx! fault #/sink-call fault then
      output-stream))
  
  (def-func! "is-text-input-stream" v
    (racket-boolean->sink #/sink-text-input-stream? v))
  
  (def-func-fault! "extfx-read-eof" fault input-stream on-eof then
    (expect (sink-text-input-stream? input-stream) #t
      (cene-err fault "Expected input-stream to be a text input stream")
    #/expect (sink-extfx? on-eof) #t
      (cene-err fault "Expected on-eof to be an extfx value")
    #/sink-extfx-read-eof fault input-stream on-eof
    #/fn input-stream
    #/verify-callback-extfx! fault #/sink-call fault then
      input-stream))
  
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
  (def-func-fault! "dex-list" fault dex-elem
    (expect (sink-dex? dex-elem) #t
      (cene-err fault "Expected dex-elem to be a dex")
    #/sink-dex-list dex-elem))
  
  ; This installs all the built-ins so they're in the appropriate
  ; places for loading code under the given `qualify` function.
  ;
  ; NOTE:
  ;
  ; New releases of this Cene implementation are likely to introduce
  ; more built-ins. This means this behavior will potentially install
  ; definitions it didn't install before. In the big picture, this
  ; could break the way some people's programs use Cene.
  ;
  ; However, this is nothing special to
  ; `extfx-put-all-built-in-syntaxes-this-came-with`. A program that's
  ; executed in a newer version of Cene than it was written for could
  ; already have new definition conflicts at its top level. To avoid
  ; this, Cene packages will already need to be published with a
  ; particular UUID to identify what version of Cene they use. And
  ; since we can assume Cene packages will be pinned to their
  ; respective Cene language UUIDs, there's no problem offering
  ; `extfx-put-all-built-in-syntaxes-this-came-with` as another
  ; built-in for that UUID.
  ;
  ; In order for this Cene implementation to accommodate Cene programs
  ; that were written for earlier versions and slight forks of Cene,
  ; we'll eventually want to implement multiple sets of built-ins to
  ; use to simulate multiple Cene language UUIDs. (TODO: Do that.)
  ; Once we do, we'll probably end up with a much more sophisticated
  ; variant of `extfx-put-all-built-in-syntaxes-this-came-with` which
  ; takes the desired UUID as an argument.
  ;
  (def-func-fault! "extfx-put-all-built-in-syntaxes-this-came-with"
    fault unique-name qualify
    
    (expect (sink-authorized-name? unique-name) #t
      (cene-err fault "Expected unique-name to be an authorized name")
    #/sink-extfx-claim-freshen unique-name #/fn unique-name
    #/sink-extfx-init-package fault unique-name #/fn name
      (w- qualified-name (sink-call fault qualify name)
      #/expect (sink-authorized-name? qualified-name) #t
        (cene-err fault "Expected the result of an extfx-put-all-built-in-syntaxes-this-came-with qualify function to be an authorized name")
        qualified-name)))
  
  
  
  (define prelude-unique-name
    (sink-authorized-name-subname
      (sink-name #/just-value #/name-of (dex-immutable-string)
        "prelude-unique-name")
      (cene-definition-lang-impl-qualify-root)))
  (define/contract (qualify-for-prelude name)
    (-> sink-name? sink-authorized-name?)
    (sink-authorized-name-subname name
    #/sink-authorized-name-subname
      (sink-name #/just-value #/name-of (dex-immutable-string)
        "prelude-qualify-root")
      (cene-definition-lang-impl-qualify-root)))
  
  (define/contract
    (sink-extfx-def-value-for-prelude unique-name target-name value)
    (-> sink-authorized-name? sink-name? sink? sink-extfx?)
    (sink-extfx-claim unique-name #/fn
    #/w- target-name (qualify-for-prelude target-name)
    #/sink-extfx-put target-name (sink-dex #/dex-give-up) value))
  
  
  ; We define these built-ins only in the scope of prelude.cene.
  
  (def-func-verbose! sink-extfx-def-value-for-prelude
    "qualify-for-lang-impl"
    1
    (fn fault name
      (expect (sink-name? name) #t
        (cene-err fault "Expected name to be a name")
      #/sink-name-qualify-for-lang-impl name)))
  
  (def-func-verbose! sink-extfx-def-value-for-prelude
    "extfx-add-init-package-step"
    2
    (fn fault unique-name step
      (expect (sink-authorized-name? unique-name) #t
        (cene-err fault "Expected unique-name to be an authorized name")
      #/sink-extfx-add-init-package-step fault unique-name
        (fn unique-name qualify
          (verify-callback-extfx! fault
            (sink-call fault step unique-name
              (sink-fn-curried-fault 1 #/fn fault name
                (expect (sink-name? name) #t
                  (cene-err fault "Expected the input to an extfx-add-init-package-step qualify function to be a name")
                #/qualify name))))))))
  
  
  ; Define the other built-ins where the prelude code can see them.
  ; Note that if the prelude uses `extfx-add-init-package-step`,
  ; this will automatically cause that step to run for the prelude
  ; itself, so the prelude can use it right away.
  (add-init-essentials-step! #/fn unique-name
    (sink-extfx-init-package root-fault unique-name
      (fn name #/qualify-for-prelude name)))
  
  ; Run the prelude code.
  (add-init-essentials-step! #/fn unique-name
    (sink-extfx-read-top-level root-fault unique-name
      (sink-fn-curried-fault 1 #/fn fault name
        (expect (sink-name? name) #t
          (cene-err fault "Expected the input to the prelude qualify function to be a name")
        #/qualify-for-prelude name))
      (sink-text-input-stream #/box #/just
        (open-input-file prelude-path))))
  
  
  (sink-extfx-run-init-essentials-steps root-unique-name))
