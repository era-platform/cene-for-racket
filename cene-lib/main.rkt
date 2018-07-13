#lang parendown racket/base

; cene
;
; A Racket library with entrypoints to the Cene programming language.

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


(require #/only-in racket/contract/base
  -> ->* and/c any/c listof not/c or/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list list-foldl list-map nat->maybe)
(require #/only-in lathe-comforts/maybe just maybe/c nothing)
(require #/only-in lathe-comforts/struct struct-easy)

(require #/only-in effection/order
  compare-by-dex dex-give-up dex-name name? ordering-eq?)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe name)


; TODO: Implement the Cene programming language.


; NOTE: The "sink" part of the name "sink-struct" refers to the fact
; that this is only one case of Cene's kitchen sink type. Cene is an
; untyped language, but if it ever becomes a typed language, all the
; existing vales can be considered to be of a single type named
; "sink."

; NOTE: Although it is not very strictly enforced, there is an
; intended format to the data here: The value of `tags` should be a
; nonempty list of Effection name values, beginning with the main tag
; name of the struct and then listing the names of the projections.
; The projections' names should have no duplicates. The value of
; `projs` should be a list of Cene values which are the values of the
; projections.
(struct-easy (sink-struct tags projs))

(define/contract (make-sink-struct tags projs)
  (-> pair? (or/c (list) pair?) sink-struct?)
  ; NOTE: For efficiency, we don't do any checking here. The value of
  ; `tags` should be a nonempty list of Effection name values,
  ; beginning with the main tag name of the struct and then listing
  ; the names of the projections. The projections' names should have
  ; no duplicates. The value of `projs` should be a list of Cene
  ; values which are the values of the projections.
  (sink-struct tags projs))

(define/contract (unmake-sink-struct-maybe tags s)
  (-> pair? sink-struct? #/maybe/c #/or/c (list) pair?)
  (dissect s (sink-struct s-tags s-projs)
  
  ; NOTE: This is the happy path. Our struct representation is the way
  ; it is so that we can usually use an object identity comparison
  ; like this instead of traversing the projections.
  #/if (eq? tags s-tags) (just s-projs)
  
  #/dissect tags (cons main-tag proj-tags)
  #/dissect s-tags (cons s-main-tag s-proj-tags)
  #/expect
    (ordering-eq? #/compare-by-dex dex-name main-tag s-main-tag)
    #t
    (nothing)
  #/expect
    (w-loop next proj-tags proj-tags s-projs s-projs proj-hash (hash)
      (expect proj-tags (cons proj-tag proj-tags)
        (expect s-projs (list)
          (error "Encountered a sink-struct with more projection values than projection tags")
        #/just proj-hash)
      #/expect s-projs (list s-proj s-projs)
        (error "Encountered a sink-struct with more projection tags than projection values")
      #/if (hash-has-key? proj-hash proj-tag) (nothing)
      #/next proj-tags s-projs #/hash-set proj-hash proj-tag s-proj))
    (just proj-hash)
    (nothing)
  #/w-loop next
    proj-hash proj-hash
    proj-tags proj-tags
    rev-projs (list)
    
    (expect proj-tags (cons proj-tag proj-tags)
      (expect (hash-empty? proj-hash) #t (nothing)
      #/just #/reverse rev-projs)
    #/if (not #/hash-has-key? proj-hash proj-tag) (nothing)
    #/next (hash-remove proj-hash proj-tag) proj-tags
      (cons (hash-ref proj-hash proj-tag) rev-projs))))


(define/contract (name-rep-map name func)
  (-> name? (-> any/c any/c) name?)
  (dissect name (unsafe:name name)
  #/unsafe:name #/func name))

; TODO: See if we'll use this.
(define/contract (name-qualify unqualified-name)
  (-> name? name?)
  (name-rep-map unqualified-name #/fn n #/list 'name:qualified n))

(define/contract (name-claimed name)
  (-> name? name?)
  (name-rep-map name #/fn n #/list 'name:claimed n))

(define/contract (core-sink-struct syms)
  (-> (and/c pair? #/listof symbol?) (and/c pair? #/listof name?))
  (dissect syms (cons main-sym proj-syms)
  #/w- main-name (unsafe:name #/list 'name:main-core main-sym)
  #/cons main-name #/list-map proj-syms #/fn proj-sym
    (unsafe:name #/list 'name:proj-core proj-sym main-sym)))

(define s-nil (core-sink-struct '#/nil))

(struct-easy (sink-dex dex))
(struct-easy (sink-name name))
(struct-easy (sink-effects todo))
(struct-easy (sink-text-input-stream todo))
(struct-easy (sink-located-string todo))

; NOTE: The term "cexpr" is short for "compiled expression." It's the
; kind of expression that macros generate in order to use as function
; definitions.
(struct-easy (sink-cexpr todo))

(define/contract (sink? v)
  (-> any/c boolean?)
  (or
    (sink-struct? v)
    (sink-dex? v)
    (sink-name? v)
    (sink-effects? v)
    (sink-text-input-stream? v)))

(define/contract (sink-effects-get name then)
  (-> sink-name? (-> sink? sink-effects?) sink-effects?)
  (dissect name (sink-name name)
  #/sink-effects 'TODO))

(define/contract (sink-effects-put name dex value)
  (-> sink-name? sink-dex? sink? sink-effects?)
  (dissect name (sink-name name)
  #/dissect dex (sink-dex dex)
  #/sink-effects 'TODO))

(define/contract (sink-effects-claim name)
  (-> name? sink-effects?)
  (sink-effects-put
    (sink-name #/name-claimed name)
    (sink-dex #/dex-give-up)
    (make-sink-struct s-nil #/list)))

(define/contract (sink-effects-noop)
  (-> sink-effects?)
  (sink-effects 'TODO))

(define/contract (sink-effects-merge-binary a b)
  (-> sink-effects? sink-effects? sink-effects?)
  (dissect a (sink-effects a)
  #/dissect b (sink-effects b)
  #/sink-effects 'TODO))

(define/contract (sink-effects-merge-list effects)
  (-> (listof sink-effects?) sink-effects?)
  (list-foldl (sink-effects-noop) effects #/fn a b
    (sink-effects-merge-binary a b)))

(define/contract (sink-effects-merge . effects)
  (->* () #:rest (listof sink-effects?) sink-effects?)
  (sink-effects-merge-list effects))

(define/contract (cexpr-has-free-vars? cexpr)
  (-> sink-cexpr? boolean?)
  'TODO)

(define/contract (cexpr-eval cexpr)
  (-> (and/c sink-cexpr? #/not/c cexpr-has-free-vars?) sink?)
  'TODO)

(define/contract (sink-call-binary func args)
  (-> sink? sink? sink?)
  'TODO)

(define/contract (sink-call-list func args)
  (-> sink? (listof sink?) sink?)
  (list-foldl func args #/fn func arg #/sink-call-binary func arg))

(define/contract (sink-call func . args)
  (->* (sink?) #:rest (listof sink?) sink?)
  (sink-call-list func args))

(define/contract
  (sink-effects-read-eof text-input-stream on-eof else)
  (->
    sink-text-input-stream?
    sink-effects?
    (-> sink-text-input-stream? sink-effects?)
    sink-effects?)
  (sink-effects 'TODO))

(define/contract
  (sink-effects-read-whether-at-eof text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (sink-effects 'TODO))

(struct exn:fail:cene exn:fail ())

(define/contract
  (sink-effects-read-whitespace text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-effects?)
    sink-effects?)
  (sink-effects 'TODO))

(define/contract
  (sink-effects-read-maybe-identifier text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects 'TODO))

(define/contract
  (sink-effects-read-maybe-given-racket text-input-stream str then)
  (->
    sink-text-input-stream?
    string?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects 'TODO))

(define/contract
  (sink-effects-peek-maybe-given-racket text-input-stream str then)
  (->
    sink-text-input-stream?
    string?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects 'TODO))

(define/contract
  (sink-effects-read-cexprs
    unique-name qualify text-input-stream state on-cexpr then)
  (->
    name?
    procedure?
    sink-text-input-stream?
    any/c
    (->
      name?
      procedure?
      any/c
      sink-cexpr?
      (-> name? procedure? any/c sink-effects?)
      sink-effects?)
    (-> name? procedure? sink-text-input-stream? any/c sink-effects?)
    sink-effects?)
  
  ; NOTE: These are the cases we should handle.
  ;
  ;  <eof>
  ;  <whitespace>
  ;  abc
  ;  \<op>...
  ;  
  ;  (.<op>...)
  ;  [.<op>...]
  ;  
  ;  /.<op>...
  ;  
  ;  (...)
  ;  [...]
  ;  /
  ;  
  ;  )
  ;  ]
  
  ; TODO: Just about every case here will invoke `next` in order to
  ; read further expressions from the input.
  (w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    state state
  #/sink-effects-read-whitespace text-input-stream
  #/fn text-input-stream whitespace
  #/sink-effects-read-whether-at-eof text-input-stream
  #/fn text-input-stream is-at-eof
  #/if is-at-eof
    (then unique-name qualify text-input-stream state)
  
  #/sink-effects-read-maybe-given-racket text-input-stream ")"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (raise #/exn:fail:cene
      "Encountered an unmatched )"
    #/current-continuation-marks)
  #/sink-effects-read-maybe-given-racket text-input-stream "]"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (raise #/exn:fail:cene
      "Encountered an unmatched ]"
    #/current-continuation-marks)
  
  #/w- sink-effects-read-op
    (fn text-input-stream qualify then
      (sink-effects 'TODO))
  
  #/w- sink-effects-read-and-run-op
    (fn unique-name qualify text-input-stream state then
      (sink-effects-read-op text-input-stream qualify
      #/fn text-input-stream op-name
      #/sink-effects-get op-name #/fn op-impl
      
      ; TODO: Convert `qualify`, `on-cexpr`, and the `fn` to sinks
      ; somehow.
      ;
      ; TODO: Raise an error explicitly if the result of the call isn't
      ; a `sink-effects?` value.
      ;
      #/sink-call
        op-impl unique-name qualify text-input-stream state on-cexpr
      #/fn unique-name qualify text-input-stream state
      #/then unique-name qualify text-input-stream state))
  
  #/sink-effects-read-maybe-given-racket text-input-stream "\\"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-and-run-op
      unique-name qualify text-input-stream state
    #/fn unique-name qualify text-input-stream state
    #/next unique-name qualify text-input-stream state)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "("
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-op
        unique-name qualify text-input-stream state
      #/fn unique-name qualify text-input-stream state
      #/sink-effects-read-maybe-given-racket text-input-stream ")"
      #/fn text-input-stream maybe-str
      #/expect maybe-str (just _)
        (raise #/exn:fail:cene
          "Encountered a syntax that began with (. and did not end with )"
        #/current-continuation-marks)
      #/next unique-name qualify text-input-stream state)
    
    #/sink-effects 'TODO)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "["
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-op
        unique-name qualify text-input-stream state
      #/fn unique-name qualify text-input-stream state
      #/sink-effects-read-maybe-given-racket text-input-stream "]"
      #/fn text-input-stream maybe-str
      #/expect maybe-str (just _)
        (raise #/exn:fail:cene
          "Encountered a syntax that began with [. and did not end with ]"
        #/current-continuation-marks)
      #/next unique-name qualify text-input-stream state)
    
    #/sink-effects 'TODO)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "/"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-op
        unique-name qualify text-input-stream state
      #/fn unique-name qualify text-input-stream state
      #/sink-effects-peek-maybe-given-racket text-input-stream ")"
      #/fn text-input-stream maybe-str1
      #/sink-effects-peek-maybe-given-racket text-input-stream "]"
      #/fn text-input-stream maybe-str2
      #/mat (list maybe-str1 maybe-str2) (list (nothing) (nothing))
        (raise #/exn:fail:cene
          "Encountered a syntax that began with /. and did not end with ) or ]"
        #/current-continuation-marks)
      #/next unique-name qualify text-input-stream state)
    
    #/sink-effects 'TODO)
  
  #/sink-effects-read-maybe-identifier text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (sink-effects 'TODO)
  
  #/raise #/exn:fail:cene
    "Encountered an unrecognized case of the expression syntax"
  #/current-continuation-marks))

(struct-easy (trivial))

(define/contract (sink-effects-claim-and-split unique-name n then)
  (-> name? natural? (-> (listof name?) sink-effects?) sink-effects?)
  (mat n 1 (then #/list unique-name)
  #/sink-effects-merge (sink-effects-claim unique-name)
  #/expect (nat->maybe n) (just n) (then #/list)
  #/w-loop next n n next-name unique-name names (list)
    (expect (nat->maybe n) (just n) (then #/cons next-name names)
    #/w- first (name-rep-map unique-name #/fn n #/list 'name:first n)
    #/w- rest (name-rep-map unique-name #/fn n #/list 'name:rest n)
    #/next n rest #/cons first names)))

; This returns a computation that reads all the content of the given
; text input stream and runs the reader macros it encounters. Unlike
; typical Lisp readers, this does not read first-class values; it only
; reads and performs side effects.
(define/contract
  (sink-effects-read-top-level unique-name qualify text-input-stream)
  (-> name? procedure? sink-text-input-stream? sink-effects?)
  (sink-effects-read-eof text-input-stream
    ; If we're at the end of the file, we're done. We claim the
    ; `unique-name` to be sure no one else is using it.
    (sink-effects-claim unique-name)
  #/fn text-input-stream
  #/sink-effects-read-cexprs unique-name qualify text-input-stream
    (trivial)
    (fn unique-name qualify state cexpr then
      ; If we encounter an expression, we evaluate it and call the
      ; result, passing in the current scope information.
      (sink-effects-claim-and-split unique-name 2
      #/dissectfn (list unique-name-first unique-name-rest)
      #/expect (cexpr-has-free-vars? cexpr) #f
        (raise #/exn:fail:cene
          "Encountered a top-level expression with at least one free variable"
        #/current-continuation-marks)
      #/w- effects
        (sink-call (cexpr-eval cexpr) unique-name-first qualify)
      #/expect (sink-effects? effects) #t
        (raise #/exn:fail:cene
          "Expected every top-level expression to evaluate to a callable value that takes two arguments and returns side effects"
        #/current-continuation-marks)
      #/sink-effects-merge effects
      #/then unique-name-rest qualify state))
  #/fn unique-name qualify text-input-stream
  #/sink-effects-read-top-level
    unique-name qualify text-input-stream))

