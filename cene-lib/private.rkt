#lang parendown racket/base

; cene/private
;
; A Racket library with entrypoints to the Cene programming language
; (implementation details).

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
(require #/for-syntax #/only-in syntax/parse expr)

(require #/only-in racket/contract/base
  -> ->* and/c any/c contract? list/c listof none/c or/c parameter/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/control reset-at shift-at)
(require #/only-in racket/generic define/generic define-generics)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-any list-foldl list-foldr list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/maybe
  just just-value maybe/c maybe-map nothing nothing?)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct
  auto-write define-imitation-simple-struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in effection/extensibility/base
  authorized-name? authorized-name-get-name authorized-name-subname
  dspace? error-definer-from-message error-definer-uninformative
  extfx? extfx-claim-unique extfx-ct-continue extfx-get extfx-noop
  extfx-put extfx-split-list fuse-extfx optionally-dexable-dexable
  optionally-dexable-once success-or-error-definer)
(require #/only-in effection/order
  assocs->table-if-mutually-unique dex-immutable-string dex-trivial)
(require #/only-in effection/order/base
  call-fuse compare-by-dex dex? dexable dex-give-up dex-dex dex-name
  dex-struct dex-table fuse-by-merge in-dex? merge-by-dex merge-table
  name? name-of ordering-eq? table? table-empty table-get
  table-map-fuse table-shadow)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe name)

(require #/only-in cene/private/textpat
  textpat? textpat-from-string textpat-lookahead
  textpat-once-or-more textpat-one textpat-one-in-range
  textpat-one-in-string textpat-one-not textpat-one-not-in-string
  textpat-or textpat-star optimized-textpat? optimized-textpat-read!
  optimize-textpat)


(provide #/all-defined-out)



; TODO: Put this in the Effection library or something.
(define/contract (eq-by-dex? dex a b)
  (-> dex? any/c any/c boolean?)
  (expect (compare-by-dex dex a b) (just comparison)
    (error "Expected a and b to be members of the domain of dex")
  #/ordering-eq? comparison))

; TODO: Put this into the `effection/order` module or something (maybe
; even `effection/order/base`).
(define/contract (table-kv-map table kv-to-v)
  (-> table? (-> name? any/c any/c) table?)
  (mat
    (table-map-fuse table
      (fuse-by-merge #/merge-table #/merge-by-dex #/dex-give-up)
    #/fn k
      (dissect (table-get k table) (just v)
      #/table-shadow k (just #/kv-to-v k v) #/table-empty))
    (just result)
    result
  #/table-empty))

; TODO: Put this into the `effection/order` module or something (maybe
; even `effection/order/base`).
(define/contract (table-v-map table v-to-v)
  (-> table? (-> any/c any/c) table?)
  (table-kv-map table #/fn k v #/v-to-v v))


(define-generics sink)


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
(struct-easy (sink-struct tags projs)
  #:other #:methods gen:sink [])

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
  #/expect (eq-by-dex? (dex-name) main-tag s-main-tag) #t (nothing)
  #/expect
    (w-loop next
      s-proj-tags s-proj-tags
      s-projs s-projs
      proj-hash (hash)
      
      (expect s-proj-tags (cons s-proj-tag s-proj-tags)
        (expect s-projs (list)
          (error "Encountered a sink-struct with more projection values than projection tags")
        #/just proj-hash)
      #/expect s-projs (cons s-proj s-projs)
        (error "Encountered a sink-struct with more projection tags than projection values")
      #/if (hash-has-key? proj-hash s-proj-tag) (nothing)
      #/next s-proj-tags s-projs
        (hash-set proj-hash s-proj-tag s-proj)))
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


(struct-easy (sink-fault maybe-continuation-marks)
  #:other #:methods gen:sink [])
(struct-easy (sink-directive directive)
  #:other #:methods gen:sink [])
(struct-easy (sink-dex dex)
  #:other #:methods gen:sink [])
(struct-easy (sink-name name)
  #:other #:methods gen:sink [])
(struct-easy (sink-authorized-name authorized-name)
  #:other #:methods gen:sink [])
(struct-easy (sink-effects go!)
  #:other #:methods gen:sink [])
(struct-easy
  (sink-cexpr-sequence-output-stream
    id box-of-maybe-state-and-handler)
  #:other #:methods gen:sink [])
(struct-easy (sink-text-input-stream box-of-maybe-input)
  #:other #:methods gen:sink [])

; The `parts` representation of a `sink-located-string` is a list of
; entries. Each entry is a three-element list where the first element
; is a starting position, the second element is a nonempty Racket
; string, and the third element is a stopping position.
;
; Each position is a three-element list of a line number (or `#f`), a
; column number (or `#f`), and an overall position number (or `#f`),
; exactly as returned by `port-next-location`.
;
; By ensuring that each part of `parts` contains a nonempty string, we
; can pretend the position information is actually attached to each
; character individually, which makes it clear which information will
; be retained in a substring and which information will not.
;
(struct-easy (sink-located-string parts)
  #:other #:methods gen:sink [])

(struct-easy (sink-string racket-string)
  (#:guard-easy
    ; Racket's basic string operations make it easy to end up with a
    ; mutable string by accident, so we go out of our way to check
    ; that all `sink-string` values are immutable.
    (unless (immutable-string? racket-string)
      (error "Expected racket-string to be an immutable string")))
  #:other #:methods gen:sink [])
; NOTE: Some `sink-opaque-fn-fault` and `sink-opaque-fn` satisfy
; Cene's `is-fusable-fn` predicate, and some do not, so they're not
; entirely opaque.
(struct-easy (sink-opaque-fn-fault racket-fn)
  #:other #:methods gen:sink [])
(struct-easy (sink-opaque-fn racket-fn)
  #:other #:methods gen:sink [])
(struct-easy (sink-table racket-table)
  #:other #:methods gen:sink [])

; NOTE: The term "cexpr" is short for "compiled expression." It's the
; kind of expression that macros generate in order to use as function
; definitions.
(struct-easy (sink-cexpr cexpr)
  #:other #:methods gen:sink [])

(define/contract (sink-name-rep-map name func)
  (-> sink-name? (-> any/c any/c) sink-name?)
  (dissect name (sink-name #/unsafe:name name)
  #/sink-name #/unsafe:name #/func name))

; TODO: Once we have the ability to import names, we should make sure
; this produces an authorized name whose name is importable from a
; UUID-identified import. Modules identified by UUID can only be
; implemented by the language implementation. And since that's the
; case, there should be no way for Cene code to obtain these
; authorized names; just the unauthorized equivalents.
;
(define/contract (sink-name-qualify-for-lang-impl unqualified-name)
  (-> sink-name? sink-authorized-name?)
  (sink-authorized-name-subname unqualified-name
  #/sink-authorized-name-subname
    (sink-name #/just-value #/name-of (dex-immutable-string)
      "qualify")
    (cene-definition-lang-impl-qualify-root)))

(define/contract (sink-table-get-maybe table name)
  (-> sink-table? sink-name? #/maybe/c sink?)
  (dissect table (sink-table table)
  #/dissect name (sink-name name)
  #/table-get name table))

(define/contract (sink-table-put-maybe table name maybe-value)
  (-> sink-table? sink-name? (maybe/c sink?) sink-table?)
  (dissect table (sink-table table)
  #/dissect name (sink-name name)
  #/sink-table #/table-shadow name maybe-value table))

(define/contract cene-definition-get-param
  (parameter/c
    (maybe/c
      (list/c dspace? sink-authorized-name? (-> sink-name? sink?))))
  (make-parameter #/nothing))

(define/contract (assert-can-get-cene-definitions!)
  (-> void?)
  (expect (cene-definition-get-param)
    (just #/list ds lang-impl-qualify-root get)
    (error "Expected an implementation of `cene-definition-get` to be available in the dynamic scope")
  #/void))

(define/contract (assert-cannot-get-cene-definitions!)
  (-> void?)
  (mat (cene-definition-get-param)
    (just #/list ds lang-impl-qualify-root get)
    ; TODO: Make this error message more visually distinct from the
    ; `assert-can-get-cene-definitions!` error message.
    (error "Expected no implementation of `cene-definition-get` to be available in the dynamic scope")
  #/void))

(define/contract (cene-definition-dspace)
  (-> dspace?)
  (expect (cene-definition-get-param)
    (just #/list ds lang-impl-qualify-root get)
    (error "Expected every call to `cene-definition-dspace` to occur with an implementation in the dynamic scope")
    ds))

(define/contract (cene-definition-lang-impl-qualify-root)
  (-> sink-authorized-name?)
  (expect (cene-definition-get-param)
    (just #/list ds lang-impl-qualify-root get)
    (error "Expected every call to `cene-definition-lang-impl-qualify-root` to occur with an implementation in the dynamic scope")
    lang-impl-qualify-root))

(define/contract (cene-definition-get name)
  (-> sink-name? sink?)
  (expect (cene-definition-get-param)
    (just #/list ds lang-impl-qualify-root get)
    (error "Expected every call to `cene-definition-get` to occur with an implementation in the dynamic scope")
  #/get name))

(define cene-definition-get-prompt-tag (make-continuation-prompt-tag))

(define/contract (extfx-with-cene-definition-restorer body)
  (-> (-> (-> (-> extfx?) extfx?) extfx?) extfx?)
  (begin (assert-can-get-cene-definitions!)
  #/w- val (cene-definition-get-param)
  #/body #/fn body
    (parameterize ([cene-definition-get-param val])
    #/reset-at cene-definition-get-prompt-tag
      (body))))

(define/contract (extfx-with-gets-from ds unique-name body)
  (-> dspace? authorized-name? (-> authorized-name? extfx?) extfx?)
  (begin (assert-cannot-get-cene-definitions!)
  #/extfx-claim-and-split unique-name 2
  #/dissectfn (list unique-name lang-impl-qualify-root)
  #/parameterize
    (
      [
        cene-definition-get-param
        (just
          (list ds (sink-authorized-name lang-impl-qualify-root)
            (dissectfn (sink-name name)
              (shift-at cene-definition-get-prompt-tag k
              #/extfx-with-cene-definition-restorer #/fn restore
              #/extfx-get ds name
                #;on-stall (error-definer-uninformative)
              #/fn value
              #/restore #/fn
              #/k value))))])
  #/reset-at cene-definition-get-prompt-tag
    (body unique-name)))

(define/contract (sink-effects-run! effects)
  (-> sink-effects? extfx?)
  (begin (assert-can-get-cene-definitions!)
  #/dissect effects (sink-effects go!)
  #/go!))

(define-generics cexpr
  (cexpr-has-free-vars? cexpr env)
  (cexpr-eval fault cexpr env))

(struct-easy (cexpr-var name)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-var name)
        (error "Expected this to be a cexpr-var")
      #/expect (table-get name env) (just _)
        #t
        #f))
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-var name)
        (error "Expected this to be a cexpr-var")
      #/expect (table-get name env) (just value)
        (error "Tried to eval a cexpr that had a free variable")
        value))
  ])

(struct-easy (cexpr-reified result)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-reified result)
        (error "Expected this to be a cexpr-reified")
        #f))
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-reified result)
        (error "Expected this to be a cexpr-reified")
        result))
  ])

(struct-easy (cexpr-construct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-construct main-tag-name projs)
        (error "Expected this to be a cexpr-construct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-construct main-tag-name projs)
        (error "Expected this to be a cexpr-construct")
      #/make-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval fault proj-cexpr env)))
  ])

(struct-easy (cexpr-call-fault fault-arg func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/or
        (-has-free-vars? fault-arg env)
        (-has-free-vars? func env)
        (-has-free-vars? arg env)))
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-call-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/w- fault-arg (-eval fault fault-arg env)
      #/w- func (-eval fault func env)
      #/w- arg (-eval fault arg env)
      #/expect (sink-fault? fault-arg) #t
        (cene-err fault "Expected the blame argument to be a blame value")
      #/sink-call-fault fault fault-arg func arg))
  ])

(struct-easy (cexpr-call func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call func arg)
        (error "Expected this to be a cexpr-call")
      #/or (-has-free-vars? func env) (-has-free-vars? arg env)))
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-call func arg)
        (error "Expected this to be a cexpr-call")
      #/sink-call fault (-eval fault func env) (-eval fault arg env)))
  ])

(struct-easy (cexpr-opaque-fn-fault fault-param param body)
  (#:guard-easy
    (when (names-have-duplicate? #/list fault-param param)
      (error "Expected fault-param and param to be mutually unique")))
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-opaque-fn-fault fault-param param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/-has-free-vars? body
      #/table-shadow fault-param (just #/trivial)
      #/table-shadow param (just #/trivial)
        env))
    
    (define (cexpr-eval caller-fault this env)
      (expect this (cexpr-opaque-fn-fault fault-param param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/sink-opaque-fn-fault #/dissectfn (list explicit-fault arg)
        (-eval caller-fault body
          (table-shadow fault-param (just explicit-fault)
          #/table-shadow param (just arg)
            env))))
  ])

(struct-easy (cexpr-opaque-fn param body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-opaque-fn param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/-has-free-vars? body
      #/table-shadow param (just #/trivial) env))
    
    (define (cexpr-eval caller-fault this env)
      (expect this (cexpr-opaque-fn param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/sink-opaque-fn #/fn arg
        (-eval caller-fault body
          (table-shadow param (just arg) env))))
  ])

(struct-easy (cexpr-let bindings body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-let bindings body)
        (error "Expected this to be a cexpr-let")
      #/or
        (list-any bindings #/dissectfn (list var val)
          (-has-free-vars? val env))
      #/-has-free-vars? body
      #/list-foldl env bindings #/fn env binding
        (dissect binding (list var val)
        #/table-shadow var (just #/trivial) env)))
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-let bindings body)
        (error "Expected this to be a cexpr-let")
      #/-eval fault body
      #/list-foldl env
        (list-map bindings #/dissectfn (list var val)
          (list var #/-eval fault val env))
      #/fn env binding
        (dissect binding (list var val)
        #/table-shadow var (just val) env)))
  ])

(struct-easy (cexpr-located location-definition-name body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-located location-definition-name body)
        (error "Expected this to be a cexpr-located")
      #/-has-free-vars? body))
    
    (define (cexpr-eval fault this env)
      (expect this (cexpr-located location-definition-name body)
        (error "Expected this to be a cexpr-located")
      ; TODO CEXPR-LOCATED / TODO FAULT: Replace `fault` here with
      ; something related to `location-definition-name` or
      ; `(cene-definition-get location-definition-name)`.
      #/-eval fault body))
  ])

; TODO: Put this in Effection.
(define/contract (extfx-fuse-binary a b)
  (-> extfx? extfx? extfx?)
  (dissect (call-fuse (fuse-extfx) a b) (just result)
    result))

; TODO: Put this in Effection.
(define/contract (extfx-fuse-list lst)
  (-> (listof extfx?) extfx?)
  (list-foldl (extfx-noop) lst #/fn a b
    (extfx-fuse-binary a b)))

; TODO: Put this in Effection.
(define/contract (extfx-fuse . lst)
  (->* () #:rest (listof extfx?) extfx?)
  (extfx-fuse-list lst))

; NOTE: The only purpose of this is to help track down a common kind
; of error where the result of `go!` is mistakenly a `sink-effects?`
; instead of an `extfx?`.
(define/contract (make-sink-effects go!)
  (-> (-> extfx?) sink-effects?)
  (sink-effects go!))

(define/contract (sink-effects-get name then)
  (-> sink-name? (-> sink? sink-effects?) sink-effects?)
  (dissect name (sink-name name)
  #/make-sink-effects #/fn
  #/extfx-with-cene-definition-restorer #/fn restore
  #/extfx-get (cene-definition-dspace) name
    #;on-stall (error-definer-uninformative)
  #/fn result
  #/restore #/fn
  #/sink-effects-run!
  #/then result))

(define/contract (sink-effects-put name dex value)
  (-> sink-authorized-name? sink-dex? sink? sink-effects?)
  (dissect name (sink-authorized-name name)
  #/dissect dex (sink-dex dex)
  #/make-sink-effects #/fn
  #/extfx-put (cene-definition-dspace) name
    (error-definer-from-message
      "Internal error: Expected the sink-effects-put continuation ticket to be written to")
    (fn then
      (extfx-ct-continue then
        (error-definer-from-message
          "Internal error: Expected the sink-effects-put continuation ticket to be written to only once")
        (list
          #;on-conflict
          (success-or-error-definer
            (error-definer-uninformative)
            (extfx-noop))
          (if (in-dex? dex value)
            (optionally-dexable-dexable #/dexable dex value)
            (optionally-dexable-once value)))))))

(define/contract (sink-effects-noop)
  (-> sink-effects?)
  (make-sink-effects #/fn #/extfx-noop))

(define/contract (sink-effects-fuse-binary a b)
  (-> sink-effects? sink-effects? sink-effects?)
  (dissect a (sink-effects a-go!)
  #/dissect b (sink-effects b-go!)
  #/make-sink-effects #/fn #/extfx-fuse (a-go!) (b-go!)))

(define/contract (sink-effects-fuse-list effects)
  (-> (listof sink-effects?) sink-effects?)
  (list-foldl (sink-effects-noop) effects #/fn a b
    (sink-effects-fuse-binary a b)))

(define/contract (sink-effects-fuse . effects)
  (->* () #:rest (listof sink-effects?) sink-effects?)
  (sink-effects-fuse-list effects))

; This performs some computation during the side effect runner, rather
; than performing it right away. The computation doesn't have to be
; pure.
(define/contract (sink-effects-later then)
  (-> (-> sink-effects?) sink-effects?)
  (make-sink-effects #/fn #/sink-effects-run! #/then))

(struct exn:fail:cene exn:fail (clamor))

(define/contract (sink-cexpr-var name)
  (-> sink-name? sink-cexpr?)
  (dissect name (sink-name name)
  #/sink-cexpr #/cexpr-var name))

(define/contract (sink-cexpr-reified result)
  (-> sink? sink-cexpr?)
  (sink-cexpr #/cexpr-reified result))

; TODO: See if this should be an export of Effection.
(define/contract (names-have-duplicate? names)
  (-> (listof name?) boolean?)
  (nothing?
  #/assocs->table-if-mutually-unique #/list-map names #/fn name
    (cons name #/trivial)))

(define/contract (sink-names-have-duplicate? names)
  (-> (listof sink-name?) boolean?)
  (names-have-duplicate?
  #/list-map names #/dissectfn (sink-name name) name))

(define/contract (sink-cexpr-construct main-tag-name projs)
  (-> sink-name? (listof #/list/c sink-name? sink-cexpr?) sink-cexpr?)
  (dissect main-tag-name (sink-name main-tag-name)
  #/if
    (sink-names-have-duplicate?
      (list-map projs #/dissectfn (list proj-name proj-cexpr)
        proj-name))
    (error "Encountered a duplicate projection name")
  #/sink-cexpr #/cexpr-construct main-tag-name #/list-map projs
  #/dissectfn (list (sink-name proj-name) (sink-cexpr proj-cexpr))
    (list proj-name proj-cexpr)))

; NOTE: Since this creates authorized names out of unauthorized names,
; we shouldn't expose it as a Cene built-in. We only use this for
; convenient construction of built-in struct tags within this Cene
; implementation.
(define/contract (make-sink-cexpr-construct tags proj-cexprs)
  (-> (and/c pair? #/listof name?) (listof sink-cexpr?) sink-cexpr?)
  (dissect tags (cons main-tag-name proj-names)
  #/expect (= (length proj-names) (length proj-cexprs)) #t
    (error "Expected tags to have one more entry than proj-cexprs")
  #/sink-cexpr-construct (sink-name main-tag-name)
  #/list-zip-map proj-names proj-cexprs #/fn proj-name proj-cexpr
    (list (sink-name proj-name) proj-cexpr)))

(define/contract (sink-cexpr-call func arg)
  (-> sink-cexpr? sink-cexpr? sink-cexpr?)
  (dissect func (sink-cexpr func)
  #/dissect arg (sink-cexpr arg)
  #/sink-cexpr #/cexpr-call func arg))

(define/contract (sink-cexpr-opaque-fn-fault fault-param param body)
  (-> sink-name? sink-name? sink-cexpr? sink-cexpr?)
  (dissect fault-param (sink-name fault-param)
  #/dissect param (sink-name param)
  #/dissect body (sink-cexpr body)
  #/sink-cexpr #/cexpr-opaque-fn-fault fault-param param body))

(define/contract (sink-cexpr-opaque-fn param body)
  (-> sink-name? sink-cexpr? sink-cexpr?)
  (dissect param (sink-name param)
  #/dissect body (sink-cexpr body)
  #/sink-cexpr #/cexpr-opaque-fn param body))

; TODO: This is only used in `cene/private/essentials`. See if this
; should be moved over there. It seems like it should be so core to
; the language semantics that this file, `cene/private`, is the place
; for it, but maybe not.
(define/contract (sink-cexpr-let bindings body)
  (-> (listof #/list/c sink-name? sink-cexpr?) sink-cexpr?
    sink-cexpr?)
  (dissect body (sink-cexpr body)
  #/if
    (sink-names-have-duplicate? #/list-map bindings
    #/dissectfn (list var val) var)
    (error "Encountered a duplicate let binding variable name")
  #/sink-cexpr #/cexpr-let
    (list-map bindings
    #/dissectfn (list (sink-name var) (sink-cexpr val))
      (list var val))
    body))

(define/contract
  (sink-name-for-function-implementation
    result-tag main-tag-name proj-tag-names)
  (-> immutable-string? sink-name? sink-table? sink-name?)
  (dissect proj-tag-names (sink-table proj-tag-names)
  #/sink-authorized-name-get-name
  #/sink-authorized-name-subname
    (sink-name #/just-value #/name-of (dex-immutable-string)
      result-tag)
  #/sink-authorized-name-subname
    (sink-name #/just-value #/name-of (dex-table #/dex-trivial)
      proj-tag-names)
  #/sink-authorized-name-subname main-tag-name
  #/sink-authorized-name-subname
    (sink-name #/just-value #/name-of (dex-immutable-string)
      "function-implementation")
    (cene-definition-lang-impl-qualify-root)))

(define/contract
  (sink-name-for-function-implementation-code
    main-tag-name proj-tag-names)
  (-> sink-name? sink-table? sink-name?)
  (sink-name-for-function-implementation
    "code" main-tag-name proj-tag-names))

(define/contract
  (sink-name-for-function-implementation-value
    main-tag-name proj-tag-names)
  (-> sink-name? sink-table? sink-name?)
  (sink-name-for-function-implementation
    "value" main-tag-name proj-tag-names))

(define/contract (sink-proj-tag-authorized-names->trivial proj-tag-names)
  (-> sink-table? sink-table?)
  (dissect proj-tag-names (sink-table proj-tag-names)
  #/sink-table #/table-kv-map proj-tag-names #/fn k v
    (expect v (sink-authorized-name authorized-name)
      (error "Expected each value of proj-tag-names to be an authorized name")
    #/expect
      (eq-by-dex? (dex-name)
        k
        (authorized-name-get-name authorized-name))
      #t
      (error "Expected each value of proj-tag-names to be an authorized name where the name authorized is the same as the name it's filed under")
    #/trivial)))

(define/contract
  (sink-authorized-name-for-function-implementation
    result-tag main-tag-name proj-tag-names)
  (-> immutable-string? sink-authorized-name? sink-table?
    sink-authorized-name?)
  (dissect
    (sink-proj-tag-authorized-names->trivial proj-tag-names)
    (sink-table proj-tag-names)
  #/sink-authorized-name-subname
    (sink-name #/just-value #/name-of (dex-immutable-string)
      result-tag)
  #/sink-authorized-name-subname
    (sink-name #/just-value #/name-of (dex-table #/dex-trivial)
      proj-tag-names)
  #/sink-authorized-name-subname
    (sink-authorized-name-get-name main-tag-name)
  #/sink-authorized-name-subname
    (sink-name #/just-value #/name-of (dex-immutable-string)
      "function-implementation")
    (cene-definition-lang-impl-qualify-root)))

(define/contract
  (sink-authorized-name-for-function-implementation-code
    main-tag-name proj-tag-names)
  (-> sink-authorized-name? sink-table? sink-authorized-name?)
  (sink-authorized-name-for-function-implementation
    "code" main-tag-name proj-tag-names))

(define/contract
  (sink-authorized-name-for-function-implementation-value
    main-tag-name proj-tag-names)
  (-> sink-authorized-name? sink-table? sink-authorized-name?)
  (sink-authorized-name-for-function-implementation
    "value" main-tag-name proj-tag-names))

(define/contract (sink-fn-curried-fault n-args racket-func)
  (-> exact-positive-integer? procedure? sink?)
  (dissect (nat->maybe n-args) (just n-args-after-next)
  #/w-loop next n-args-after-next n-args-after-next rev-args (list)
    (expect (nat->maybe n-args-after-next) (just n-args-after-next)
      (sink-opaque-fn-fault #/dissectfn (list fault arg)
        (apply racket-func fault #/reverse #/cons arg rev-args))
    #/sink-opaque-fn #/fn arg
      (next n-args-after-next #/cons arg rev-args))))

(define/contract (sink-fn-curried n-args racket-func)
  (-> exact-positive-integer? procedure? sink-opaque-fn?)
  (dissect (nat->maybe n-args) (just n-args-after-next)
  #/w-loop next n-args-after-next n-args-after-next rev-args (list)
    (expect (nat->maybe n-args-after-next) (just n-args-after-next)
      (sink-opaque-fn #/fn arg
        (apply racket-func #/reverse #/cons arg rev-args))
    #/sink-opaque-fn #/fn arg
      (next n-args-after-next #/cons arg rev-args))))

; TODO: See if we have to use this as often as we do. Maybe some of
; those places should be abstracted over a fault value instead.
(define/contract (make-fault-internal)
  (-> sink-fault?)
  (sink-fault #/just #/current-continuation-marks))

(define/contract (raise-cene-err fault clamor)
  (-> sink-fault? sink? none/c)
  (begin (assert-can-get-cene-definitions!)
  #/w- message
    (mat (unmake-sink-struct-maybe (s-clamor-err) clamor)
      (just #/list #/sink-string message)
      message
    #/format "~s" clamor)
  #/w- marks
    (dissect fault (sink-fault maybe-marks)
    #/mat maybe-marks (just marks) marks
    #/current-continuation-marks)
  #/raise #/exn:fail:cene message marks clamor))

(define-simple-macro (cene-err fault:expr message:string)
  ; TODO: See if there's a way we can either stop depending on
  ; `s-clamor-err` here or move this code after the place where
  ; `s-clamor-err` is defined.
  (begin (assert-can-get-cene-definitions!)
  #/raise-cene-err fault
    (make-sink-struct (s-clamor-err) #/list #/sink-string message)))

(define/contract
  (sink-call-fault-binary caller-fault explicit-fault func arg)
  (-> sink-fault? sink-fault? sink? sink? sink?)
  (begin (assert-can-get-cene-definitions!)
  #/mat func (sink-opaque-fn racket-func)
    (racket-func arg)
  #/mat func (sink-opaque-fn-fault racket-func)
    (racket-func #/list explicit-fault arg)
  #/mat func (sink-struct tags projs)
    (dissect (list-map tags #/fn tag #/sink-name tag)
      (cons main-tag proj-tags)
    
    ; TODO: This lookup might be expensive. See if we should memoize
    ; it.
    #/w- impl
      (cene-definition-get
      #/sink-name-for-function-implementation-value
        main-tag
        (list-foldr proj-tags (sink-table #/table-empty)
        #/fn proj-tag rest
          (sink-table-put-maybe rest proj-tag
          ; TODO: See if there's a way we can either stop depending on
          ; `s-trivial` here or move this code after the place where
          ; `s-trivial` is defined.
          #/just #/make-sink-struct (s-trivial) #/list)))
    
    #/sink-call-fault-binary caller-fault explicit-fault
      (sink-call-fault-binary caller-fault caller-fault impl func)
      arg)
  #/cene-err caller-fault "Tried to call a value that wasn't an opaque function or a struct"))

(define/contract (sink-call-binary caller-fault func arg)
  (-> sink-fault? sink? sink? sink?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-call-fault-binary caller-fault caller-fault func arg))

(define/contract
  (sink-call-fault-list
    caller-fault explicit-fault func first-arg args)
  (-> sink-fault? sink-fault? sink? sink? (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/dissect (reverse #/cons first-arg args) (cons last-arg rev-args)
  #/w- func
    (list-foldl func (reverse rev-args) #/fn func arg
      (sink-call-binary caller-fault func arg))
  #/sink-call-fault-binary caller-fault explicit-fault func last-arg))

(define/contract (sink-call-list caller-fault func args)
  (-> sink-fault? sink? (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/list-foldl func args #/fn func arg
    (sink-call-binary caller-fault func arg)))

(define/contract
  (sink-call-fault caller-fault explicit-fault func first-arg . args)
  (->* (sink-fault? sink-fault? sink? sink?) #:rest (listof sink?)
    sink?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-call-fault-list
    caller-fault explicit-fault func first-arg args))

(define/contract (sink-call caller-fault func . args)
  (->* (sink-fault? sink?) #:rest (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-call-list caller-fault func args))

(define/contract
  (sink-effects-string-from-located-string located-string then)
  (-> sink-located-string? (-> sink-string? sink-effects?)
    sink-effects?)
  (dissect located-string (sink-located-string parts)
  #/sink-effects-later #/fn
  ; TODO: See if this is a painter's algorithm.
  #/then #/sink-string #/string->immutable-string
  #/list-foldl "" parts #/fn state part
    (dissect part (list start-loc string stop-loc)
    #/string-append state string)))

(define/contract (name-for-sink-string string)
  (-> sink-string? name?)
  (just-value #/name-of
    (dex-struct sink-string #/dex-immutable-string)
    string))

(define/contract (sink-name-for-string string)
  (-> sink-string? sink-name?)
  (sink-name #/name-for-sink-string string))

(define/contract (sink-authorized-name-get-name name)
  (-> sink-authorized-name? sink-name?)
  (dissect name (sink-authorized-name effection-authorized-name)
  #/sink-name #/authorized-name-get-name effection-authorized-name))

(define/contract (sink-name-subname index-name inner-name)
  (-> sink-name? sink-name? sink-name?)
  (dissect index-name (sink-name #/unsafe:name index-name)
  #/sink-name-rep-map inner-name #/fn n
    (list 'name:subname index-name n)))

(define/contract (sink-authorized-name-subname index-name inner-name)
  (-> sink-name? sink-authorized-name? sink-authorized-name?)
  (dissect index-name (sink-name index-name)
  #/dissect inner-name (sink-authorized-name inner-name)
  #/sink-authorized-name
    (authorized-name-subname index-name inner-name)))

(define/contract (sink-name-for-freestanding-cexpr-op inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n
    (list 'name:freestanding-cexpr-op n)))

(define/contract (sink-name-for-bounded-cexpr-op inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n
    (list 'name:bounded-cexpr-op n)))

(define/contract (sink-name-for-nameless-bounded-cexpr-op)
  (-> sink-name?)
  (sink-name #/unsafe:name #/list 'name:nameless-bounded-cexpr-op))

(define/contract (sink-name-for-struct-main-tag inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n
    (list 'name:struct-main-tag n)))

(define/contract
  (sink-name-for-struct-proj qualified-main-tag-name proj-name)
  (-> sink-name? sink-name? sink-name?)
  (dissect qualified-main-tag-name
    (sink-name #/unsafe:name qualified-main-tag-name)
  #/sink-name-rep-map proj-name #/fn n
    (list 'name:struct-proj qualified-main-tag-name n)))

(define/contract
  (sink-cexpr-sequence-output-stream-spend! fault stream)
  (-> sink-fault? sink-cexpr-sequence-output-stream?
    (list/c
      any/c
      (-> any/c sink-cexpr? (-> any/c sink-effects?) sink-effects?)))
  (begin (assert-can-get-cene-definitions!)
  #/dissect stream (sink-cexpr-sequence-output-stream id b)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just state-and-handler)
    (cene-err fault "Tried to spend an expression output stream that was already spent")
  #/begin
    (set-box! b (nothing))
    state-and-handler))

(define/contract
  (sink-effects-make-cexpr-sequence-output-stream
    fault unique-name state on-cexpr then)
  (->
    sink-fault?
    sink-authorized-name?
    any/c
    (-> any/c sink-cexpr (-> any/c sink-effects?) sink-effects?)
    (-> sink-cexpr-sequence-output-stream?
      (-> sink-cexpr-sequence-output-stream?
        (-> any/c sink-effects?)
        sink-effects?)
      sink-effects?)
    sink-effects?)
  (sink-effects-claim-and-split unique-name 0 #/dissectfn (list)
  #/w- identity (box #/trivial)
  #/then
    (sink-cexpr-sequence-output-stream identity #/box #/just
    #/list state on-cexpr)
    (fn output-stream then
      (dissect output-stream
        (sink-cexpr-sequence-output-stream found-id _)
      #/expect (eq? identity found-id) #t
        ; TODO: See if we can tweak the design of
        ; `sink-effects-make-cexpr-sequence-output-stream` in such a
        ; way that its clients can specify their own error messages to
        ; take the place of this one. Since we don't currently support
        ; that, we're reporting this error message in such a way that
        ; it makes sense for Cene's
        ; `effects-make-expr-sequence-output-stream` built-in.
        (cene-err fault "Expected the expression sequence output stream given to an effects-make-expr-sequence-output-stream unwrapper to be a descendant of the same one created by that call")
      #/dissect
        (sink-cexpr-sequence-output-stream-spend! fault output-stream)
        (list state on-cexpr)
      #/then state))))

(define/contract
  (sink-effects-cexpr-write fault output-stream cexpr then)
  (->
    sink-fault? sink-cexpr-sequence-output-stream? sink-cexpr?
    (-> sink-cexpr-sequence-output-stream? sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn
  #/dissect output-stream (sink-cexpr-sequence-output-stream id _)
  #/dissect
    (sink-cexpr-sequence-output-stream-spend! fault output-stream)
    (list state on-cexpr)
  #/on-cexpr state cexpr #/fn state
  #/then #/sink-cexpr-sequence-output-stream id #/box #/just #/list
    state on-cexpr))

(define/contract
  (sink-text-input-stream-spend! fault text-input-stream)
  (-> sink-fault? sink-text-input-stream? input-port?)
  (begin (assert-can-get-cene-definitions!)
  #/dissect text-input-stream (sink-text-input-stream b)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just input-port)
    (cene-err fault "Tried to spend a text input stream that was already spent")
  #/begin
    (set-box! b (nothing))
    input-port))

(define/contract
  (sink-effects-read-eof fault text-input-stream on-eof else)
  (->
    sink-fault?
    sink-text-input-stream?
    sink-effects?
    (-> sink-text-input-stream? sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn
  #/w- in (sink-text-input-stream-spend! fault text-input-stream)
  #/if (eof-object? #/peek-byte in)
    (begin (close-input-port in)
      on-eof)
  #/else #/sink-text-input-stream #/box #/just in))

(define/contract
  (sink-effects-optimized-textpat-read-located
    fault pattern text-input-stream then)
  (->
    sink-fault?
    optimized-textpat?
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn
  #/w- in (sink-text-input-stream-spend! fault text-input-stream)
  #/let-values
    (
      [
        (start-line start-column start-position)
        (port-next-location in)])
  #/expect (optimized-textpat-read! pattern in) (just text)
    (then (sink-text-input-stream #/box #/just in) (nothing))
  #/let-values
    (
      [
        (stop-line stop-column stop-position)
        (port-next-location in)])
  #/then (sink-text-input-stream #/box #/just in)
    (just
    #/mat (string-length text) 0
      (sink-located-string #/list)
      (sink-located-string #/list
        (list
          (list start-line start-column start-position)
          text
          (list stop-line stop-column stop-position))))))

(define sink-effects-peek-whether-eof-pat
  (optimize-textpat #/textpat-lookahead #/textpat-one))

(define/contract
  (sink-effects-peek-whether-eof fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    fault sink-effects-peek-whether-eof-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/mat maybe-located-string (just located-string)
    (then text-input-stream #f)
    (then text-input-stream #t)))

(define sink-effects-read-whitespace-pat
  ; TODO: Support a more Unicode-aware notion of whitespace.
  (optimize-textpat #/textpat-star #/textpat-one-in-string " \t\r\n"))

(define/contract
  (sink-effects-read-whitespace fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    fault sink-effects-read-whitespace-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define sink-effects-read-non-line-breaks-pat
  (optimize-textpat
  ; TODO: Support a more Unicode-aware notion of line break.
  #/textpat-star #/textpat-one-not-in-string "\r\n"))

(define/contract
  (sink-effects-read-non-line-breaks fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    fault sink-effects-read-non-line-breaks-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define sink-effects-read-maybe-identifier-pat
  ; TODO: Support a more Unicode-aware notion of identifier. Not only
  ; should `sink-effects-read-maybe-identifier` recognize an
  ; identifier according to one of the Unicode algorithms, it should
  ; normalize it according to a Unicode algorithm as well.
  (optimize-textpat #/textpat-once-or-more #/textpat-or
    (textpat-one-in-string "-0")
    (textpat-one-in-range #\1 #\9)
    (textpat-one-in-range #\a #\z)
    (textpat-one-in-range #\A #\Z)))

; TODO: In each case where it's actually possible for this error to
; appear, use a more specific error message.
(define/contract (sink-call-qualify fault qualify pre-qualified-name)
  (-> sink-fault? sink? sink-name? sink-authorized-name?)
  (begin (assert-can-get-cene-definitions!)
  #/w- qualified-name (sink-call fault qualify pre-qualified-name)
  #/expect (sink-authorized-name? qualified-name) #t
    (cene-err fault
      "Expected the result of a qualify function to be an authorized name")
    qualified-name))

(define/contract
  (sink-effects-read-maybe-identifier
    fault qualify text-input-stream pre-qualify then)
  (->
    sink-fault?
    sink?
    sink-text-input-stream?
    (-> sink-name? sink-name?)
    (->
      sink-text-input-stream?
      (maybe/c #/list/c sink-located-string? sink-authorized-name?)
      sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    fault sink-effects-read-maybe-identifier-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/expect maybe-located-string (just located-string)
    (then text-input-stream #/nothing)
  #/sink-effects-string-from-located-string located-string #/fn string
  #/then text-input-stream
    (just #/list located-string
      (sink-call-qualify fault qualify
        (pre-qualify #/sink-name-for-string string)))))

(define sink-effects-read-maybe-op-character-pat
  ; TODO: Support a more Unicode-aware notion here, maybe the
  ; "pattern" symbols described in the Unicode identifier rules.
  (optimize-textpat #/textpat-one-not #/textpat-or
    (textpat-one-in-string "-0 \t\r\n[]()\\.:")
    (textpat-one-in-range #\1 #\9)
    (textpat-one-in-range #\a #\z)
    (textpat-one-in-range #\A #\Z)))

(define/contract
  (sink-effects-read-maybe-op-character fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    fault sink-effects-read-maybe-op-character-pat text-input-stream
    then))

(define/contract (sink-effects-optimize-textpat t then)
  (->
    textpat?
    (-> optimized-textpat? sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn #/then #/optimize-textpat t))

(define |pat "["| (optimize-textpat #/textpat-from-string "["))
(define |pat "]"| (optimize-textpat #/textpat-from-string "]"))
(define |pat "("| (optimize-textpat #/textpat-from-string "("))
(define |pat ")"| (optimize-textpat #/textpat-from-string ")"))
(define |pat "\\"| (optimize-textpat #/textpat-from-string "\\"))
(define |pat "."| (optimize-textpat #/textpat-from-string "."))
(define |pat ":"| (optimize-textpat #/textpat-from-string ":"))
(define |pat "/"| (optimize-textpat #/textpat-from-string "/"))

(define sink-effects-peek-whether-closing-bracket-pat
  (optimize-textpat #/textpat-lookahead #/textpat-one-in-string "])"))

(define/contract
  (sink-effects-peek-whether-closing-bracket
    fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    fault sink-effects-peek-whether-closing-bracket-pat
    text-input-stream
  #/fn text-input-stream maybe-located-empty-string
  #/mat maybe-located-empty-string (just located-empty-string)
    (then text-input-stream #t)
    (then text-input-stream #f)))

(define/contract
  (sink-effects-read-op
    fault text-input-stream qualify pre-qualify then)
  (->
    sink-fault?
    sink-text-input-stream?
    sink?
    (-> sink-name? sink-name?)
    (-> sink-text-input-stream? sink-authorized-name? sink-effects?)
    sink-effects?)
  
  ; NOTE: These are the cases we should handle here.
  ;
  ;   #
  ;   abc:
  ;   abc
  ;   (markup):
  ;   (markup)
  ;   [markup]:
  ;   [markup]
  
  (sink-effects-read-maybe-op-character fault text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (sink-effects-string-from-located-string identifier
    #/fn identifier
    #/then text-input-stream
      (sink-call-qualify fault qualify
        (pre-qualify #/sink-name-for-string identifier)))
  
  #/w- then
    (fn text-input-stream op-name
      (sink-effects-optimized-textpat-read-located
        fault |pat ":"| text-input-stream
      #/fn text-input-stream maybe-str
      #/then text-input-stream op-name))
  
  ; TODO: Support the use of ( and [ as delimiters for macro
  ; names.
  #/sink-effects-optimized-textpat-read-located
    fault |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "The use of ( to delimit a macro name is not yet supported")
  #/sink-effects-optimized-textpat-read-located
    fault |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "The use of [ to delimit a macro name is not yet supported")
  
  #/sink-effects-read-maybe-identifier
    fault qualify text-input-stream pre-qualify
  #/fn text-input-stream maybe-name
  #/mat maybe-name (just #/list located-string name)
    (then text-input-stream name)
  
  ; TODO FAULT: Make this `fault` more specific.
  #/cene-err fault "Encountered an unrecognized case of the expression operator syntax"))

(define/contract
  (sink-effects-run-op
    fault op-impl unique-name qualify text-input-stream output-stream
    then)
  (->
    sink-fault? sink? sink-authorized-name? sink?
    sink-text-input-stream? sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/w- result
    (sink-call fault op-impl
      unique-name qualify text-input-stream output-stream
    #/sink-fn-curried-fault 4
    #/fn fault unique-name qualify text-input-stream output-stream
    #/expect (sink-authorized-name? unique-name) #t
      (cene-err fault "Expected the unique name of a macro's callback results to be an authorized name")
    #/expect (sink-text-input-stream? text-input-stream) #t
      (cene-err fault "Expected the text input stream of a macro's callback results to be a text input stream")
    #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
      (cene-err fault "Expected the expression sequence output stream of a macro's callback results to be an expression sequence output stream")
    #/sink-effects-claim-freshen unique-name #/fn unique-name
    #/then unique-name qualify text-input-stream output-stream)
  #/expect (sink-effects? result) #t
    (cene-err fault "Expected the return value of a macro to be an effectful computation")
    result))

(define/contract
  (sink-effects-read-and-run-op
    fault unique-name qualify text-input-stream output-stream
    pre-qualify then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/sink-effects-read-op fault text-input-stream qualify pre-qualify
  #/fn text-input-stream op-name
  #/sink-effects-get (sink-authorized-name-get-name op-name)
  #/fn op-impl
  #/sink-effects-run-op
    fault op-impl unique-name qualify text-input-stream output-stream
    then))

(define/contract
  (sink-effects-read-and-run-freestanding-cexpr-op
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-read-and-run-op
    fault unique-name qualify text-input-stream output-stream
    sink-name-for-freestanding-cexpr-op
    then))

(define/contract
  (sink-effects-read-and-run-bounded-cexpr-op
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-read-and-run-op
    fault unique-name qualify text-input-stream output-stream
    sink-name-for-bounded-cexpr-op
    then))

(define/contract
  (sink-effects-run-nameless-op
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/sink-effects-get
    (sink-authorized-name-get-name
    #/sink-call-qualify fault qualify
      (sink-name-for-nameless-bounded-cexpr-op))
  #/fn op-impl
  #/sink-effects-run-op
    fault op-impl unique-name qualify text-input-stream output-stream
    then))

; TODO: See if we should keep this around. We just use it for
; debugging.
(define/contract (sink-text-input-stream-summary text-input-stream)
  (-> sink-text-input-stream? #/or/c eof-object? any/c)
  (dissect text-input-stream (sink-text-input-stream b)
  #/dissect (unbox b) (just in)
  #/peek-string 1000 0 in))

; TODO CEXPR-LOCATED: For every cexpr read this way, wrap that cexpr
; in a `cexpr-located`.
(define/contract
  (sink-effects-read-cexprs
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  
  ; NOTE: These are the cases we should handle.
  ;
  ;   <eof>
  ;   <whitespace>
  ;   \<op>...
  ;   
  ;   (.<op>...)
  ;   [.<op>...]
  ;   
  ;   /.<op>...
  ;   
  ;   (...)
  ;   [...]
  ;   /
  ;   
  ;   )
  ;   ]
  ;
  ; We do not handle identifiers here. Clients who anticipate
  ; identifiers should try to read them with
  ; `sink-effects-read-maybe-identifier` before calling this.
  
  
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/sink-effects-read-whitespace fault text-input-stream
  #/fn text-input-stream whitespace
  #/sink-effects-peek-whether-eof fault text-input-stream
  #/fn text-input-stream is-eof
  #/if is-eof
    (then unique-name qualify text-input-stream output-stream)
  
  #/sink-effects-optimized-textpat-read-located
    fault |pat ")"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "Encountered an unmatched )")
  #/sink-effects-optimized-textpat-read-located
    fault |pat "]"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "Encountered an unmatched ]")
  
  #/sink-effects-optimized-textpat-read-located
    fault |pat "\\"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-and-run-freestanding-cexpr-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-optimized-textpat-read-located
    fault |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-optimized-textpat-read-located
          fault |pat ")"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          ; TODO FAULT: Make this `fault` more specific.
          (cene-err fault "Encountered a syntax that began with ( or (. and did not end with )")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-optimized-textpat-read-located
      fault |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-effects-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-optimized-textpat-read-located
    fault |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-optimized-textpat-read-located
          fault |pat "]"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          ; TODO FAULT: Make this `fault` more specific.
          (cene-err fault "Encountered a syntax that began with [ or [. and did not end with ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-optimized-textpat-read-located
      fault |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-effects-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-optimized-textpat-read-located
    fault |pat "/"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-peek-whether-closing-bracket
          fault text-input-stream
        #/fn text-input-stream is-closing-bracket
        #/if (not is-closing-bracket)
          ; TODO FAULT: Make this `fault` more specific.
          (cene-err fault "Encountered a syntax that began with /. and did not end at ) or ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-optimized-textpat-read-located
      fault |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-effects-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  ; TODO FAULT: Make this `fault` more specific.
  #/cene-err fault "Encountered an unrecognized case of the expression syntax"))

(define/contract (core-sink-struct main-tag-string proj-strings)
  (-> immutable-string? (listof immutable-string?)
    (-> #/and/c pair? #/listof name?))
  
  ; TODO: Currently, we return a function that computes the tags each
  ; and every time. Memoize this. The computation can be perfrmed in
  ; `extfx-with-gets-from` once, and then the function returned here
  ; can look it up from the dynamically scoped binding that
  ; `extfx-with-gets-from` sets up.
  ;
  (fn
    (begin (assert-can-get-cene-definitions!)
    #/w- main-tag-authorized-name
      (sink-name-qualify-for-lang-impl #/sink-name-for-struct-main-tag
      #/sink-name-for-string #/sink-string main-tag-string)
    #/w- main-tag-name
      (sink-authorized-name-get-name main-tag-authorized-name)
    #/list-map
      (cons main-tag-name
      #/list-map proj-strings #/fn proj-string
        (sink-authorized-name-get-name
        #/sink-name-qualify-for-lang-impl
        #/sink-name-for-struct-proj main-tag-name
        #/sink-name-for-string #/sink-string proj-string))
    #/dissectfn (sink-name name)
      name)))

(define s-trivial (core-sink-struct "trivial" #/list))
(define s-clamor-err (core-sink-struct "clamor-err" #/list "message"))

(define/contract (extfx-claim name on-success)
  (-> authorized-name? (-> extfx?) extfx?)
  (extfx-claim-unique name
    (error-definer-from-message
      "Tried to claim a name unique more than once")
    (error-definer-from-message
      "Internal error: Expected the sink-effects-claim familiarity ticket to be spent")
  #/fn fresh-authorized-name familiarity-ticket
  #/extfx-split-list familiarity-ticket 0
    (error-definer-from-message
      "Internal error: Expected the sink-effects-claim familiarity ticket to be spent only once")
  #/dissectfn (list)
  #/on-success))

(define/contract (extfx-claim-and-split unique-name n then)
  (-> authorized-name? natural? (-> (listof authorized-name?) extfx?)
    extfx?)
  (extfx-claim unique-name #/fn
  #/w-loop next n n next-name unique-name names (list)
    (expect (nat->maybe n) (just n) (then names)
    
    ; NOTE: We do not want these to be names that Cene code can
    ; recreate. If we were implementing `extfx-claim-and-split` in a
    ; Cene package, we could do this by using
    ; `authorized-name-subname` with a key name made out of a
    ; `dex-struct` of a struct tag that was made out of unique names
    ; known only to that package. In this implementation, we don't
    ; have to go to all that trouble.
    ;
    #/w- first
      (authorized-name-subname (unsafe:name #/list 'name:first)
        next-name)
    #/w- rest
      (authorized-name-subname (unsafe:name #/list 'name:rest)
        next-name)
    
    #/next n rest #/cons first names)))

(define/contract (sink-effects-claim name on-success)
  (-> sink-authorized-name? (-> sink-effects?) sink-effects?)
  (dissect name (sink-authorized-name name)
  #/make-sink-effects #/fn
    (extfx-with-cene-definition-restorer #/fn restore
    #/extfx-claim name #/fn
    #/restore #/fn
    #/sink-effects-run!
    #/on-success)))

(define/contract (sink-effects-claim-and-split unique-name n then)
  (->
    sink-authorized-name?
    natural?
    (-> (listof sink-authorized-name?) sink-effects?)
    sink-effects?)
  (dissect unique-name (sink-authorized-name unique-name)
  #/make-sink-effects #/fn
    (extfx-with-cene-definition-restorer #/fn restore
    #/extfx-claim-and-split unique-name n #/fn names
    #/restore #/fn
    #/sink-effects-run! #/then #/list-map names #/fn name
      (sink-authorized-name name))))

(define/contract (sink-effects-claim-freshen unique-name then)
  (-> sink-authorized-name? (-> sink-authorized-name? sink-effects?)
    sink-effects?)
  (sink-effects-claim-and-split unique-name 1
  #/dissectfn (list unique-name)
  #/then unique-name))

(define/contract (cexpr-can-eval? cexpr)
  (-> cexpr? boolean?)
  (not #/cexpr-has-free-vars? cexpr #/table-empty))

; This evaluates the given cexpr. If the current side effects runner
; doesn't support evaluating cexprs, this causes an error instead.
;
; The macroexpander's side effects runner supports the operation, and
; so far we've only written code for the macroexpander's side effects
; runner (TODO), so this particular version always succeeds.
;
(define/contract (sink-effects-cexpr-eval fault cexpr then)
  (->
    sink-fault?
    (and/c cexpr? cexpr-can-eval?)
    (-> sink? sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn
  #/then #/cexpr-eval fault cexpr #/table-empty))

; This returns a computation that reads all the content of the given
; text input stream and runs the reader macros it encounters. Unlike
; typical Lisp readers, this does not read first-class values; it only
; reads and performs side effects.
(define/contract
  (sink-effects-read-top-level
    fault unique-name qualify text-input-stream)
  (-> sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-effects?)
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/sink-effects-read-eof fault text-input-stream
    ; If we're at the end of the file, we're done. We claim the
    ; `unique-name` to stay in the habit, even though it's clear no
    ; one else can be using it.
    (sink-effects-claim unique-name #/fn #/sink-effects-noop)
  #/fn text-input-stream
  #/sink-effects-claim-and-split unique-name 3
  #/dissectfn
    (list unique-name-stream unique-name-writer unique-name-main)
  #/sink-effects-make-cexpr-sequence-output-stream
    fault
    unique-name-stream
    unique-name-writer
    (fn unique-name-writer cexpr then
      ; If we encounter an expression, we evaluate it and call the
      ; result, passing in the current scope information.
      (sink-effects-claim-and-split unique-name-writer 2
      #/dissectfn (list unique-name-first unique-name-rest)
      #/expect cexpr (sink-cexpr cexpr)
        ; TODO: Test that we can actually get this error. We might
        ; already be checking for this condition elsewhere.
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Encountered a top-level expression that compiled to a non-expression value")
      #/expect (cexpr-has-free-vars? cexpr #/table-empty) #f
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Encountered a top-level expression with at least one free variable")
      #/sink-effects-fuse (then unique-name-rest)
      #/sink-effects-cexpr-eval fault cexpr #/fn directive
      #/expect directive (sink-directive directive)
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Expected every top-level expression to evaluate to a directive")
      #/w- effects
        (sink-call fault directive unique-name-first qualify)
      #/expect (sink-effects? effects) #t
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Expected every top-level expression to evaluate to a directive made from a callable value that takes two arguments and returns side effects")
        effects))
  #/fn output-stream unwrap
  #/sink-effects-read-cexprs
    fault unique-name-main qualify text-input-stream output-stream
  #/fn unique-name-main qualify text-input-stream output-stream
  #/unwrap output-stream #/fn unique-name-writer
  #/sink-effects-claim-and-split unique-name-writer 0
  #/dissectfn (list)
  #/sink-effects-read-top-level
    fault unique-name-main qualify text-input-stream))

(define/contract (sink-effects-run-string unique-name qualify string)
  (->
    sink-authorized-name?
    (-> sink-name? sink-authorized-name?)
    string?
    sink-effects?)
  (sink-effects-read-top-level
    (make-fault-internal)
    unique-name
    (sink-fn-curried-fault 1 #/fn fault name
      (expect (sink-name? name) #t
        (cene-err fault "Expected the input to the root qualify function to be a name")
      #/qualify name))
    (sink-text-input-stream #/box #/just #/open-input-string string)))
