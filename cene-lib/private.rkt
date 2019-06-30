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
  -> ->* and/c any any/c contract? list/c listof none/c or/c
  parameter/c struct/c)
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
  just just-value maybe-bind maybe/c maybe-map nothing nothing?)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in effection/extensibility/base
  authorized-name? authorized-name-get-name authorized-name-subname
  dex-authorized-name dex-dspace dspace? error-definer?
  error-definer-from-exn error-definer-from-message
  error-definer-uninformative extfx? extfx-claim-unique
  extfx-ct-continue extfx-noop extfx-pub-write extfx-run-getfx
  extfx-put extfx-split-list extfx-sub-write fuse-extfx getfx?
  getfx-bind getfx/c getfx-done getfx-err getfx-get make-pub make-sub
  optionally-dexed-dexed optionally-dexed-once pure-run-getfx
  success-or-error-definer)
(require #/only-in effection/order
  assocs->table-if-mutually-unique dex-immutable-string dex-trivial
  getfx-is-eq-by-dex)
(require #/only-in effection/order/base
  dex? dexed-first-order/c dex-give-up dex-dex dex-name dex-struct
  dex-table fuse-by-merge getfx-call-fuse getfx-dexed-of getfx-name-of
  getfx-table-map-fuse merge-by-dex merge-table name? ordering-eq?
  table? table-empty? table-empty table-get table-shadow)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe name)

(require #/only-in cene/private/textpat
  textpat? textpat-from-string textpat-lookahead
  textpat-once-or-more textpat-one textpat-one-in-range
  textpat-one-in-string textpat-one-not textpat-one-not-in-string
  textpat-or textpat-star optimized-textpat? optimized-textpat-read!
  optimize-textpat)


(provide #/all-defined-out)



; TODO: Put this into the `effection/order` module or something (maybe
; even `effection/order/base`).
(define/contract (table-kv-map table kv-to-v)
  (-> table? (-> name? any/c any/c) table?)
  (mat
    (pure-run-getfx #/getfx-table-map-fuse table
      (fuse-by-merge #/merge-table #/merge-by-dex #/dex-give-up)
    #/fn k
      (dissect (table-get k table) (just v)
      #/getfx-done
        (table-shadow k (just #/kv-to-v k v) #/table-empty)))
    (just result)
    result
  #/table-empty))

; TODO: Put this into the `effection/order` module or something (maybe
; even `effection/order/base`).
(define/contract (table-v-map table v-to-v)
  (-> table? (-> any/c any/c) table?)
  (table-kv-map table #/fn k v #/v-to-v v))

; TODO: See if we should add this to `effection/extensibility`.
(define/contract (extfx-err on-execute)
  (-> error-definer? extfx?)
  (extfx-run-getfx (getfx-err on-execute) #/fn impossible-result
    (error "Internal error: Did not expect `getfx-err` to complete with a result value")))


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
  #/expect
    (pure-run-getfx
      (getfx-is-eq-by-dex (dex-name) main-tag s-main-tag))
    #t
    (nothing)
  #/expect
    (w-loop next
      s-proj-tags s-proj-tags
      s-projs s-projs
      proj-table (table-empty)
      
      (expect s-proj-tags (cons s-proj-tag s-proj-tags)
        (expect s-projs (list)
          (error "Encountered a sink-struct with more projection values than projection tags")
        #/just proj-table)
      #/expect s-projs (cons s-proj s-projs)
        (error "Encountered a sink-struct with more projection tags than projection values")
      #/expect (table-get s-proj-tag proj-table) (nothing) (nothing)
      #/next s-proj-tags s-projs
        (table-shadow s-proj-tag (just s-proj) proj-table)))
    (just proj-table)
    (nothing)
  #/w-loop next
    proj-table proj-table
    proj-tags proj-tags
    rev-projs (list)
    
    (expect proj-tags (cons proj-tag proj-tags)
      (expect (table-empty? proj-table) #t (nothing)
      #/just #/reverse rev-projs)
    #/maybe-bind (table-get proj-tag proj-table) #/fn s-proj
    #/next (table-shadow proj-tag (nothing) proj-table) proj-tags
      (cons s-proj rev-projs))))


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
(struct-easy (sink-getfx go)
  #:other #:methods gen:sink [])
(struct-easy (sink-extfx go!)
  #:other #:methods gen:sink [])
(struct-easy (sink-pub pub)
  #:other #:methods gen:sink [])
(struct-easy (sink-sub sub)
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

; TODO: See if we should do `(sink-name-for-string #/sink-string ...)`
; instead.
(define/contract (sink-name-of-racket-string string)
  (-> immutable-string? sink-name?)
  (sink-name #/just-value #/pure-run-getfx #/getfx-name-of
    (dex-immutable-string)
    string))

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
    (sink-name-of-racket-string "qualify")
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

(struct-easy #/cene-root-info dspace lang-impl-qualify-root)

(define/contract cene-definition-get-param
  (parameter/c
    (maybe/c
      (list/c
        (struct/c cene-root-info dspace? sink-authorized-name?)
        (maybe/c #/-> getfx? any/c))))
  (make-parameter #/nothing))

(define/contract (assert-can-get-cene-definition-globals!)
  (-> void?)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected implementations of `cene-definition-dspace` and `cene-definition-lang-impl-qualify-root` to be available in the dynamic scope")
  #/void))

(define/contract (assert-cannot-get-cene-definition-globals!)
  (-> void?)
  (mat (cene-definition-get-param) (just #/list rinfo maybe-run-getfx)
    ; TODO: Make this error message more visually distinct from the
    ; `assert-can-get-cene-definition-globals!` error message.
    (error "Expected no implementations of `cene-definition-dspace` and `cene-definition-lang-impl-qualify-root` to be available in the dynamic scope")
  #/void))

(define/contract (assert-can-get-cene-definitions!)
  (-> void?)
  (expect (cene-definition-get-param)
    (just #/list rinfo #/just run-getfx)
    (error "Expected an implementation of `cene-definition-get` to be available in the dynamic scope")
  #/void))

(define/contract (assert-cannot-get-cene-definitions!)
  (-> void?)
  (mat (cene-definition-get-param)
    (just #/list rinfo #/just run-getfx)
    ; TODO: Make this error message more visually distinct from the
    ; `assert-can-get-cene-definitions!` error message.
    (error "Expected no implementation of `cene-definition-get` to be available in the dynamic scope")
  #/void))

(define/contract (cene-definition-dexed-root-info)
  (->
    (dexed-first-order/c
      (struct/c cene-root-info dspace? sink-authorized-name?)))
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected every call to `cene-definition-dexed-root-info` to occur with an implementation in the dynamic scope")
  #/just-value #/pure-run-getfx #/getfx-dexed-of
    (dex-struct cene-root-info
      (dex-dspace)
      (dex-struct sink-authorized-name #/dex-authorized-name))
    rinfo))

(define/contract (cene-definition-dspace)
  (-> dspace?)
  (expect (cene-definition-get-param)
    (just #/list
      (cene-root-info ds lang-impl-qualify-root)
      maybe-run-getfx)
    (error "Expected every call to `cene-definition-dspace` to occur with an implementation in the dynamic scope")
    ds))

(define/contract (cene-definition-lang-impl-qualify-root)
  (-> sink-authorized-name?)
  (expect (cene-definition-get-param)
    (just #/list
      (cene-root-info ds lang-impl-qualify-root)
      maybe-run-getfx)
    (error "Expected every call to `cene-definition-lang-impl-qualify-root` to occur with an implementation in the dynamic scope")
    lang-impl-qualify-root))

(define/contract (getfx-err-from-clamor clamor)
  (-> sink? getfx?)
  (dissect
    (expect (unmake-sink-struct-maybe (s-clamor-err) clamor)
      (just #/list (sink-fault maybe-marks) (sink-string message))
      (list (nothing) (format "~s" clamor))
      (list maybe-marks message))
    (list maybe-marks message)
  #/w- marks
    (mat maybe-marks (just marks) marks
    #/current-continuation-marks)
  #/getfx-err #/error-definer-from-exn #/exn:fail message marks))

(define/contract (sink-getfx-get name)
  (-> sink-name? sink-getfx?)
  (dissect name (sink-name name)
  #/sink-getfx #/fn
    (getfx-get (cene-definition-dspace) name
      #;on-stall (error-definer-uninformative)
      )))

(define/contract (cene-run-getfx effects)
  (-> getfx? sink?)
  (expect (cene-definition-get-param)
    (just #/list rinfo #/just run-getfx)
    (error "Expected every call to `cene-run-getfx` to occur with an implementation in the dynamic scope")
  #/run-getfx effects))

(define/contract (make-sink-pub pubsub-name)
  (-> sink-authorized-name? sink-pub?)
  (begin (assert-can-get-cene-definition-globals!)
  #/dissect pubsub-name (sink-authorized-name pubsub-name)
  #/sink-pub #/make-pub (cene-definition-dspace) pubsub-name))

(define/contract (make-sink-sub pubsub-name)
  (-> sink-authorized-name? sink-sub?)
  (begin (assert-can-get-cene-definition-globals!)
  #/dissect pubsub-name (sink-authorized-name pubsub-name)
  #/sink-sub #/make-sub (cene-definition-dspace) pubsub-name))

(define/contract (sink-authorized-name-for-init-package-pubsub)
  (-> sink-authorized-name?)
  (sink-authorized-name-subname
    (sink-name-of-racket-string "init-package-pubsub")
    (cene-definition-lang-impl-qualify-root)))

(define cene-definition-get-prompt-tag (make-continuation-prompt-tag))

(define/contract (getfx-with-cene-definition-restorer body)
  (-> (-> (-> (-> getfx?) getfx?) getfx?) getfx?)
  (begin (assert-can-get-cene-definitions!)
  #/w- val (cene-definition-get-param)
  #/body #/fn body
    (parameterize ([cene-definition-get-param val])
    #/with-getfx-run-getfx #/fn
    #/reset-at cene-definition-get-prompt-tag
      (body))))

(define/contract (extfx-with-cene-definition-restorer body)
  (-> (-> (-> (-> extfx?) extfx?) extfx?) extfx?)
  (begin (assert-can-get-cene-definitions!)
  #/w- val (cene-definition-get-param)
  #/body #/fn body
    (parameterize ([cene-definition-get-param val])
    #/with-extfx-run-getfx #/fn
    #/reset-at cene-definition-get-prompt-tag
      (body))))

(define/contract (with-getfx-run-getfx body)
  (-> (-> any/c) any/c)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected every call to `with-getfx-run-getfx` to occur with an implementation in the dynamic scope")
  #/parameterize
    (
      [
        cene-definition-get-param
        (just #/list rinfo #/just #/fn effects
          (shift-at cene-definition-get-prompt-tag k
          #/getfx-with-cene-definition-restorer #/fn restore
          #/getfx-bind effects #/fn value
          #/restore #/fn
          #/k value))])
    (body)))

(define/contract (with-extfx-run-getfx body)
  (-> (-> any/c) any/c)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected every call to `with-extfx-run-getfx` to occur with an implementation in the dynamic scope")
  #/parameterize
    (
      [
        cene-definition-get-param
        (just #/list rinfo #/just #/fn effects
          (shift-at cene-definition-get-prompt-tag k
          #/extfx-with-cene-definition-restorer #/fn restore
          #/extfx-run-getfx effects #/fn value
          #/restore #/fn
          #/k value))])
    (body)))

(define/contract (with-gets-from rinfo body)
  (-> (struct/c cene-root-info dspace? sink-authorized-name?) (-> any)
    any)
  (begin (assert-cannot-get-cene-definition-globals!)
  #/parameterize
    ([cene-definition-get-param (just #/list rinfo #/nothing)])
    (body)))

(define/contract (getfx-run-sink-getfx effects)
  (-> sink-getfx? getfx?)
  (begin (assert-can-get-cene-definition-globals!)
  #/dissect effects (sink-getfx go)
  #/with-getfx-run-getfx #/fn
  #/go))

(define/contract (extfx-run-sink-extfx effects)
  (-> sink-extfx? extfx?)
  (begin (assert-can-get-cene-definition-globals!)
  #/dissect effects (sink-extfx go)
  #/with-extfx-run-getfx #/fn
  #/go))

(define/contract (cene-run-sink-getfx effects)
  (-> sink-getfx? sink?)
  (begin (assert-can-get-cene-definitions!)
  #/cene-run-getfx #/getfx-run-sink-getfx effects))

(define/contract (getfx-bind-restoring effects then)
  (-> getfx? (-> any/c getfx?) getfx?)
  (getfx-with-cene-definition-restorer #/fn restore
  #/getfx-bind effects #/fn intermediate
  #/restore #/fn
  #/then intermediate))

(define/contract (getfx-map-restoring effects then)
  (-> getfx? (-> any/c any/c) getfx?)
  (getfx-bind-restoring effects #/fn intermediate
  #/getfx-done #/then intermediate))

(define-generics cexpr
  (cexpr-has-free-vars? cexpr env)
  (cexpr-eval-in-env fault cexpr env))

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
    
    (define (cexpr-eval-in-env fault this env)
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
    
    (define (cexpr-eval-in-env fault this env)
      (expect this (cexpr-reified result)
        (error "Expected this to be a cexpr-reified")
        result))
  ])

(struct-easy (cexpr-construct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-construct main-tag-name projs)
        (error "Expected this to be a cexpr-construct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cexpr-eval-in-env fault this env)
      (expect this (cexpr-construct main-tag-name projs)
        (error "Expected this to be a cexpr-construct")
      #/make-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval-in-env fault proj-cexpr env)))
  ])

(struct-easy (cexpr-call-fault fault-arg func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/or
        (-has-free-vars? fault-arg env)
        (-has-free-vars? func env)
        (-has-free-vars? arg env)))
    
    (define (cexpr-eval-in-env fault this env)
      (expect this (cexpr-call-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/w- fault-arg (-eval-in-env fault fault-arg env)
      #/w- func (-eval-in-env fault func env)
      #/w- arg (-eval-in-env fault arg env)
      #/expect (sink-fault? fault-arg) #t
        (cene-err fault "Expected the blame argument to be a blame value")
      #/cene-run-getfx
        (getfx-sink-call-fault fault fault-arg func arg)))
  ])

(struct-easy (cexpr-call func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call func arg)
        (error "Expected this to be a cexpr-call")
      #/or (-has-free-vars? func env) (-has-free-vars? arg env)))
    
    (define (cexpr-eval-in-env fault this env)
      (expect this (cexpr-call func arg)
        (error "Expected this to be a cexpr-call")
      #/sink-call fault
        (-eval-in-env fault func env)
        (-eval-in-env fault arg env)))
  ])

(struct-easy (cexpr-opaque-fn-fault fault-param param body)
  (#:guard-easy
    (when (names-have-duplicate? #/list fault-param param)
      (error "Expected fault-param and param to be mutually unique")))
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-opaque-fn-fault fault-param param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/-has-free-vars? body
      #/table-shadow fault-param (just #/trivial)
      #/table-shadow param (just #/trivial)
        env))
    
    (define (cexpr-eval-in-env caller-fault this env)
      (expect this (cexpr-opaque-fn-fault fault-param param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/sink-opaque-fn-fault #/dissectfn (list explicit-fault arg)
        (-eval-in-env caller-fault body
          (table-shadow fault-param (just explicit-fault)
          #/table-shadow param (just arg)
            env))))
  ])

(struct-easy (cexpr-opaque-fn param body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-opaque-fn param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/-has-free-vars? body
      #/table-shadow param (just #/trivial) env))
    
    (define (cexpr-eval-in-env caller-fault this env)
      (expect this (cexpr-opaque-fn param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/sink-opaque-fn #/fn arg
        (-eval-in-env caller-fault body
          (table-shadow param (just arg) env))))
  ])

(struct-easy (cexpr-let bindings body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cexpr-eval-in-env)
    
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
    
    (define (cexpr-eval-in-env fault this env)
      (expect this (cexpr-let bindings body)
        (error "Expected this to be a cexpr-let")
      #/-eval-in-env fault body
      #/list-foldl env
        (list-map bindings #/dissectfn (list var val)
          (list var #/-eval-in-env fault val env))
      #/fn env binding
        (dissect binding (list var val)
        #/table-shadow var (just val) env)))
  ])

(struct-easy (cexpr-located location-definition-name body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-located location-definition-name body)
        (error "Expected this to be a cexpr-located")
      #/-has-free-vars? body))
    
    (define (cexpr-eval-in-env fault this env)
      (expect this (cexpr-located location-definition-name body)
        (error "Expected this to be a cexpr-located")
      ; TODO CEXPR-LOCATED / TODO FAULT: Replace `fault` here with
      ; something related to `location-definition-name` or
      ; `(sink-getfx-get location-definition-name)`.
      #/-eval-in-env fault body))
  ])

; TODO: Put this in Effection.
(define/contract (extfx-fuse-binary a b)
  (-> extfx? extfx? extfx?)
  (dissect (pure-run-getfx #/getfx-call-fuse (fuse-extfx) a b)
    (just result)
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
; of error where the result of `go!` is mistakenly a `sink-extfx?`
; instead of an `extfx?`.
(define/contract (make-sink-extfx go!)
  (-> (-> extfx?) sink-extfx?)
  (sink-extfx go!))

(define/contract (sink-extfx-run-getfx effects then)
  (-> sink-getfx? (-> any/c sink-extfx?) sink-extfx?)
  (dissect effects (sink-getfx go)
  #/sink-extfx #/fn
    (extfx-with-cene-definition-restorer #/fn restore
    #/extfx-run-getfx (go) #/fn intermediate
    #/restore #/fn
    #/extfx-run-sink-extfx #/then intermediate)))

(define/contract (sink-extfx-put name dex value)
  (-> sink-authorized-name? sink-dex? sink? sink-extfx?)
  (dissect name (sink-authorized-name name)
  #/dissect dex (sink-dex dex)
  #/make-sink-extfx #/fn
    (extfx-put (cene-definition-dspace) name
      (error-definer-from-message
        "Internal error: Expected the sink-extfx-put continuation ticket to be written to")
      (fn then
        
        ; NOTE: There is no dex that should rely on
        ; `cene-definition-get-param`, since even the dex combinators
        ; in Effection do nothing to help us restore that parameter as
        ; they go along. Instead of relying on
        ; `cene-definition-get-param`, dexes like `sink-dex-list`
        ; store the values of `cene-definition-get-param` when they're
        ; constructed and then restore them again using their own
        ; `with-gets-from` call. Because of that, it's fine for us to
        ; call `getfx-dexed-of` here in an
        ; `assert-cannot-get-cene-definition-globals!` zone.
        ;
        (extfx-run-getfx (getfx-dexed-of dex value) #/fn maybe-dexed
        #/extfx-ct-continue then
          (error-definer-from-message
            "Internal error: Expected the sink-extfx-put continuation ticket to be written to only once")
          (list
            #;on-conflict
            (success-or-error-definer
              (error-definer-uninformative)
              (extfx-noop))
            (mat maybe-dexed (just d)
              (optionally-dexed-dexed d)
              (optionally-dexed-once value))))))))

(define/contract (sink-extfx-noop)
  (-> sink-extfx?)
  (make-sink-extfx #/fn #/extfx-noop))

(define/contract (sink-extfx-fuse-binary a b)
  (-> sink-extfx? sink-extfx? sink-extfx?)
  (make-sink-extfx #/fn
    (extfx-fuse (extfx-run-sink-extfx a) (extfx-run-sink-extfx b))))

(define/contract (sink-extfx-fuse-list effects)
  (-> (listof sink-extfx?) sink-extfx?)
  (list-foldl (sink-extfx-noop) effects #/fn a b
    (sink-extfx-fuse-binary a b)))

(define/contract (sink-extfx-fuse . effects)
  (->* () #:rest (listof sink-extfx?) sink-extfx?)
  (sink-extfx-fuse-list effects))

; This performs some computation during the side effect runner, rather
; than performing it right away. The computation doesn't have to be
; pure.
(define/contract (sink-extfx-later then)
  (-> (-> sink-extfx?) sink-extfx?)
  (make-sink-extfx #/fn #/extfx-run-sink-extfx #/then))

(define/contract (sink-extfx-pub-write p unique-name arg)
  (-> sink-pub? sink-authorized-name? sink? sink-extfx?)
  (dissect p (sink-pub p)
  #/dissect unique-name (sink-authorized-name unique-name)
  #/make-sink-extfx #/fn
    (extfx-pub-write (cene-definition-dspace) p unique-name
      #;on-conflict
      (success-or-error-definer
        (error-definer-uninformative)
        (extfx-noop))
      arg)))

(define/contract (sink-extfx-sub-write s unique-name func)
  (-> sink-sub? sink-authorized-name? (-> sink? sink-extfx?)
    sink-extfx?)
  (dissect s (sink-sub s)
  #/dissect unique-name (sink-authorized-name unique-name)
  #/make-sink-extfx #/fn
    (extfx-with-cene-definition-restorer #/fn restore
    #/extfx-sub-write (cene-definition-dspace) s unique-name
      #;on-conflict
      (success-or-error-definer
        (error-definer-uninformative)
        (extfx-noop))
      (fn arg
        (restore #/fn
        #/extfx-run-sink-extfx #/func arg)))))

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
    (sink-name-of-racket-string result-tag)
  #/sink-authorized-name-subname
    (sink-name #/just-value #/pure-run-getfx #/getfx-name-of
      (dex-table #/dex-trivial)
      proj-tag-names)
  #/sink-authorized-name-subname main-tag-name
  #/sink-authorized-name-subname
    (sink-name-of-racket-string "function-implementation")
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

(define/contract
  (sink-proj-tag-authorized-names->trivial proj-tag-names)
  (-> sink-table? sink-table?)
  (dissect proj-tag-names (sink-table proj-tag-names)
  #/sink-table #/table-kv-map proj-tag-names #/fn k v
    (expect v (sink-authorized-name authorized-name)
      (error "Expected each value of proj-tag-names to be an authorized name")
    #/expect
      (pure-run-getfx #/getfx-is-eq-by-dex (dex-name)
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
    (sink-name-of-racket-string result-tag)
  #/sink-authorized-name-subname
    (sink-name #/just-value #/pure-run-getfx #/getfx-name-of
      (dex-table #/dex-trivial)
      proj-tag-names)
  #/sink-authorized-name-subname
    (sink-authorized-name-get-name main-tag-name)
  #/sink-authorized-name-subname
    (sink-name-of-racket-string "function-implementation")
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

(define/contract (getfx-cene-err fault message)
  (-> sink-fault? immutable-string? getfx?)
  ; TODO: See if there's a way we can either stop depending on
  ; `s-clamor-err` here or move this code after the place where
  ; `s-clamor-err` is defined.
  (begin (assert-can-get-cene-definition-globals!)
  #/getfx-err-from-clamor #/make-sink-struct (s-clamor-err) #/list
    fault
    (sink-string message)))

(define-simple-macro (cene-err fault:expr message:string)
  (begin (assert-can-get-cene-definitions!)
  #/cene-run-getfx #/getfx-cene-err fault message))

(define/contract
  (getfx-sink-call-fault-binary caller-fault explicit-fault func arg)
  (-> sink-fault? sink-fault? sink? sink? #/getfx/c sink?)
  (begin (assert-can-get-cene-definitions!)
  #/mat func (sink-opaque-fn racket-func)
    (getfx-done #/racket-func arg)
  #/mat func (sink-opaque-fn-fault racket-func)
    (getfx-done #/racket-func #/list explicit-fault arg)
  #/mat func (sink-struct tags projs)
    (dissect (list-map tags #/fn tag #/sink-name tag)
      (cons main-tag proj-tags)
    
    ; TODO: This lookup might be expensive. See if we should memoize
    ; it.
    #/getfx-bind-restoring
      (getfx-run-sink-getfx #/sink-getfx-get
      #/sink-name-for-function-implementation-value
        main-tag
        (list-foldr proj-tags (sink-table #/table-empty)
        #/fn proj-tag rest
          (sink-table-put-maybe rest proj-tag
          ; TODO: See if there's a way we can either stop depending on
          ; `s-trivial` here or move this code after the place where
          ; `s-trivial` is defined.
          #/just #/make-sink-struct (s-trivial) #/list)))
    #/fn impl
    
    #/getfx-bind-restoring
      (getfx-sink-call-fault-binary
        caller-fault caller-fault impl func)
    #/fn func
    #/getfx-sink-call-fault-binary
      caller-fault explicit-fault func arg)
  #/cene-err caller-fault "Tried to call a value that wasn't an opaque function or a struct"))

(define/contract (getfx-sink-call-binary caller-fault func arg)
  (-> sink-fault? sink? sink? #/getfx/c sink?)
  (begin (assert-can-get-cene-definitions!)
  #/getfx-sink-call-fault-binary caller-fault caller-fault func arg))

(define/contract
  (getfx-sink-call-fault-list
    caller-fault explicit-fault func first-arg args)
  (-> sink-fault? sink-fault? sink? sink? (listof sink?)
    (getfx/c sink?))
  (begin (assert-can-get-cene-definitions!)
  #/dissect (reverse #/cons first-arg args) (cons last-arg rev-args)
  #/w- func
    (list-foldl func (reverse rev-args) #/fn func arg
      (cene-run-getfx #/getfx-sink-call-binary caller-fault func arg))
  #/getfx-sink-call-fault-binary
    caller-fault explicit-fault func last-arg))

(define/contract (sink-call-list caller-fault func args)
  (-> sink-fault? sink? (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/list-foldl func args #/fn func arg
    (cene-run-getfx #/getfx-sink-call-binary caller-fault func arg)))

(define/contract
  (getfx-sink-call-fault
    caller-fault explicit-fault func first-arg . args)
  (->* (sink-fault? sink-fault? sink? sink?) #:rest (listof sink?)
    (getfx/c sink?))
  (begin (assert-can-get-cene-definitions!)
  #/getfx-sink-call-fault-list
    caller-fault explicit-fault func first-arg args))

(define/contract (sink-call caller-fault func . args)
  (->* (sink-fault? sink?) #:rest (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-call-list caller-fault func args))

; NOTE: We only use this from places where we're allowed to perform
; perffx (such as places where we're allowed to perform extfx).
(define/contract (sink-string-from-located-string located-string)
  (-> sink-located-string? sink-string?)
  (dissect located-string (sink-located-string parts)
  ; TODO: See if this is a painter's algorithm.
  #/sink-string #/string->immutable-string
    (list-foldl "" parts #/fn state part
      (dissect part (list start-loc string stop-loc)
      #/string-append state string))))

(define/contract (name-for-sink-string string)
  (-> sink-string? name?)
  (just-value #/pure-run-getfx #/getfx-name-of
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
      (-> any/c sink-cexpr? (-> any/c sink-extfx?) sink-extfx?)))
  (begin (assert-can-get-cene-definitions!)
  #/dissect stream (sink-cexpr-sequence-output-stream id b)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just state-and-handler)
    (cene-err fault "Tried to spend an expression output stream that was already spent")
  #/begin
    (set-box! b (nothing))
    state-and-handler))

(define/contract
  (sink-extfx-make-cexpr-sequence-output-stream
    fault unique-name state on-cexpr then)
  (->
    sink-fault?
    sink-authorized-name?
    any/c
    (-> any/c sink-cexpr (-> any/c sink-extfx?) sink-extfx?)
    (-> sink-cexpr-sequence-output-stream?
      (-> sink-cexpr-sequence-output-stream? (-> any/c sink-extfx?)
        sink-extfx?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-and-split unique-name 0 #/dissectfn (list)
  #/w- identity (box #/trivial)
  #/then
    (sink-cexpr-sequence-output-stream identity #/box #/just
    #/list state on-cexpr)
    (fn output-stream then
      (dissect output-stream
        (sink-cexpr-sequence-output-stream found-id _)
      #/expect (eq? identity found-id) #t
        ; TODO: See if we can tweak the design of
        ; `sink-extfx-make-cexpr-sequence-output-stream` in such a way
        ; that its clients can specify their own error messages to
        ; take the place of this one. Since we don't currently support
        ; that, we're reporting this error message in such a way that
        ; it makes sense for Cene's
        ; `extfx-make-expr-sequence-output-stream` built-in.
        (cene-err fault "Expected the expression sequence output stream given to an extfx-make-expr-sequence-output-stream unwrapper to be a descendant of the same one created by that call")
      #/dissect
        (sink-cexpr-sequence-output-stream-spend! fault output-stream)
        (list state on-cexpr)
      #/then state))))

(define/contract
  (sink-extfx-cexpr-write fault output-stream cexpr then)
  (->
    sink-fault? sink-cexpr-sequence-output-stream? sink-cexpr?
    (-> sink-cexpr-sequence-output-stream? sink-extfx?)
    sink-extfx?)
  (sink-extfx-later #/fn
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
  (sink-extfx-read-eof fault text-input-stream on-eof else)
  (->
    sink-fault?
    sink-text-input-stream?
    sink-extfx?
    (-> sink-text-input-stream? sink-extfx?)
    sink-extfx?)
  (sink-extfx-later #/fn
  #/w- in (sink-text-input-stream-spend! fault text-input-stream)
  #/if (eof-object? #/peek-byte in)
    (begin (close-input-port in)
      on-eof)
  #/else #/sink-text-input-stream #/box #/just in))

(define/contract
  (sink-extfx-optimized-textpat-read-located
    fault pattern text-input-stream then)
  (->
    sink-fault?
    optimized-textpat?
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-later #/fn
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

(define sink-extfx-peek-whether-eof-pat
  (optimize-textpat #/textpat-lookahead #/textpat-one))

(define/contract
  (sink-extfx-peek-whether-eof fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-extfx?)
    sink-extfx?)
  (sink-extfx-optimized-textpat-read-located
    fault sink-extfx-peek-whether-eof-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/mat maybe-located-string (just located-string)
    (then text-input-stream #f)
    (then text-input-stream #t)))

(define sink-extfx-read-whitespace-pat
  ; TODO: Support a more Unicode-aware notion of whitespace.
  (optimize-textpat #/textpat-star #/textpat-one-in-string " \t\r\n"))

(define/contract
  (sink-extfx-read-whitespace fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-extfx?)
    sink-extfx?)
  (sink-extfx-optimized-textpat-read-located
    fault sink-extfx-read-whitespace-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define sink-extfx-read-non-line-breaks-pat
  (optimize-textpat
  ; TODO: Support a more Unicode-aware notion of line break.
  #/textpat-star #/textpat-one-not-in-string "\r\n"))

(define/contract
  (sink-extfx-read-non-line-breaks fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-extfx?)
    sink-extfx?)
  (sink-extfx-optimized-textpat-read-located
    fault sink-extfx-read-non-line-breaks-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define sink-extfx-read-maybe-identifier-pat
  ; TODO: Support a more Unicode-aware notion of identifier. Not only
  ; should `sink-extfx-read-maybe-identifier` recognize an identifier
  ; according to one of the Unicode algorithms, it should normalize it
  ; according to a Unicode algorithm as well.
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
  (sink-extfx-read-maybe-identifier
    fault qualify text-input-stream pre-qualify then)
  (->
    sink-fault?
    sink?
    sink-text-input-stream?
    (-> sink-name? sink-name?)
    (->
      sink-text-input-stream?
      (maybe/c #/list/c sink-located-string? sink-authorized-name?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-optimized-textpat-read-located
    fault sink-extfx-read-maybe-identifier-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/expect maybe-located-string (just located-string)
    (then text-input-stream #/nothing)
  #/then text-input-stream
    (just #/list located-string
      (sink-call-qualify fault qualify
        (pre-qualify #/sink-name-for-string
          (sink-string-from-located-string located-string))))))

(define sink-extfx-read-maybe-op-character-pat
  ; TODO: Support a more Unicode-aware notion here, maybe the
  ; "pattern" symbols described in the Unicode identifier rules.
  (optimize-textpat #/textpat-one-not #/textpat-or
    (textpat-one-in-string "-0 \t\r\n[]()\\.:")
    (textpat-one-in-range #\1 #\9)
    (textpat-one-in-range #\a #\z)
    (textpat-one-in-range #\A #\Z)))

(define/contract
  (sink-extfx-read-maybe-op-character fault text-input-stream then)
  (->
    sink-fault?
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-optimized-textpat-read-located
    fault sink-extfx-read-maybe-op-character-pat text-input-stream
    then))

(define |pat "["| (optimize-textpat #/textpat-from-string "["))
(define |pat "]"| (optimize-textpat #/textpat-from-string "]"))
(define |pat "("| (optimize-textpat #/textpat-from-string "("))
(define |pat ")"| (optimize-textpat #/textpat-from-string ")"))
(define |pat "\\"| (optimize-textpat #/textpat-from-string "\\"))
(define |pat "."| (optimize-textpat #/textpat-from-string "."))
(define |pat ":"| (optimize-textpat #/textpat-from-string ":"))
(define |pat "/"| (optimize-textpat #/textpat-from-string "/"))

(define sink-extfx-peek-whether-closing-bracket-pat
  (optimize-textpat #/textpat-lookahead #/textpat-one-in-string "])"))

(define/contract
  (sink-extfx-peek-whether-closing-bracket
    fault text-input-stream then)
  (-> sink-fault? sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-extfx?)
    sink-extfx?)
  (sink-extfx-optimized-textpat-read-located
    fault sink-extfx-peek-whether-closing-bracket-pat
    text-input-stream
  #/fn text-input-stream maybe-located-empty-string
  #/mat maybe-located-empty-string (just located-empty-string)
    (then text-input-stream #t)
    (then text-input-stream #f)))

(define/contract
  (sink-extfx-read-op
    fault text-input-stream qualify pre-qualify then)
  (->
    sink-fault?
    sink-text-input-stream?
    sink?
    (-> sink-name? sink-name?)
    (-> sink-text-input-stream? sink-authorized-name? sink-extfx?)
    sink-extfx?)
  
  ; NOTE: These are the cases we should handle here.
  ;
  ;   #
  ;   abc:
  ;   abc
  ;   (markup):
  ;   (markup)
  ;   [markup]:
  ;   [markup]
  
  (sink-extfx-read-maybe-op-character fault text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (then text-input-stream
      (sink-call-qualify fault qualify
        (pre-qualify #/sink-name-for-string
          (sink-string-from-located-string identifier))))
  
  #/w- then
    (fn text-input-stream op-name
      (sink-extfx-optimized-textpat-read-located
        fault |pat ":"| text-input-stream
      #/fn text-input-stream maybe-str
      #/then text-input-stream op-name))
  
  ; TODO: Support the use of ( and [ as delimiters for macro
  ; names.
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "The use of ( to delimit a macro name is not yet supported")
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "The use of [ to delimit a macro name is not yet supported")
  
  #/sink-extfx-read-maybe-identifier
    fault qualify text-input-stream pre-qualify
  #/fn text-input-stream maybe-name
  #/mat maybe-name (just #/list located-string name)
    (then text-input-stream name)
  
  ; TODO FAULT: Make this `fault` more specific.
  #/cene-err fault "Encountered an unrecognized case of the expression operator syntax"))

(define/contract
  (sink-extfx-run-op
    fault op-impl unique-name qualify text-input-stream output-stream
    then)
  (->
    sink-fault? sink? sink-authorized-name? sink?
    sink-text-input-stream? sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
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
    #/sink-extfx-claim-freshen unique-name #/fn unique-name
    #/then unique-name qualify text-input-stream output-stream)
  #/expect (sink-extfx? result) #t
    (cene-err fault "Expected the return value of a macro to be an effectful computation")
    result))

(define/contract
  (sink-extfx-read-and-run-op
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
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-read-op fault text-input-stream qualify pre-qualify
  #/fn text-input-stream op-name
  #/sink-extfx-run-getfx
    (sink-getfx-get #/sink-authorized-name-get-name op-name)
  #/fn op-impl
  #/sink-extfx-run-op
    fault op-impl unique-name qualify text-input-stream output-stream
    then))

(define/contract
  (sink-extfx-read-and-run-freestanding-cexpr-op
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-read-and-run-op
    fault unique-name qualify text-input-stream output-stream
    sink-name-for-freestanding-cexpr-op
    then))

(define/contract
  (sink-extfx-read-and-run-bounded-cexpr-op
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-read-and-run-op
    fault unique-name qualify text-input-stream output-stream
    sink-name-for-bounded-cexpr-op
    then))

(define/contract
  (sink-extfx-run-nameless-op
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-run-getfx
    (sink-getfx-get #/sink-authorized-name-get-name
      (sink-call-qualify fault qualify
        (sink-name-for-nameless-bounded-cexpr-op)))
  #/fn op-impl
  #/sink-extfx-run-op
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
  (sink-extfx-read-cexprs
    fault unique-name qualify text-input-stream output-stream then)
  (->
    sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-extfx?)
    sink-extfx?)
  
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
  ; `sink-extfx-read-maybe-identifier` before calling this.
  
  
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-read-whitespace fault text-input-stream
  #/fn text-input-stream whitespace
  #/sink-extfx-peek-whether-eof fault text-input-stream
  #/fn text-input-stream is-eof
  #/if is-eof
    (then unique-name qualify text-input-stream output-stream)
  
  #/sink-extfx-optimized-textpat-read-located
    fault |pat ")"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "Encountered an unmatched )")
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "]"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (cene-err fault "Encountered an unmatched ]")
  
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "\\"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-extfx-read-and-run-freestanding-cexpr-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-extfx-optimized-textpat-read-located
          fault |pat ")"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          ; TODO FAULT: Make this `fault` more specific.
          (cene-err fault "Encountered a syntax that began with ( or (. and did not end with )")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-extfx-optimized-textpat-read-located
      fault |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-extfx-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-extfx-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-extfx-optimized-textpat-read-located
          fault |pat "]"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          ; TODO FAULT: Make this `fault` more specific.
          (cene-err fault "Encountered a syntax that began with [ or [. and did not end with ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-extfx-optimized-textpat-read-located
      fault |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-extfx-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-extfx-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "/"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-extfx-peek-whether-closing-bracket
          fault text-input-stream
        #/fn text-input-stream is-closing-bracket
        #/if (not is-closing-bracket)
          ; TODO FAULT: Make this `fault` more specific.
          (cene-err fault "Encountered a syntax that began with /. and did not end at ) or ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-extfx-optimized-textpat-read-located
      fault |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-extfx-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-extfx-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  ; TODO FAULT: Make this `fault` more specific.
  #/cene-err fault "Encountered an unrecognized case of the expression syntax"))

(define/contract (core-sink-struct main-tag-string proj-strings)
  (-> immutable-string? (listof immutable-string?)
    (-> #/and/c pair? #/listof name?))
  
  ; TODO: Currently, we return a function that computes the tags each
  ; and every time. Memoize this. The computation can be perfrmed in
  ; `with-gets-from` once, and then the function returned here can
  ; look it up from the dynamically scoped binding that
  ; `with-gets-from` sets up.
  ;
  (fn
    (begin (assert-can-get-cene-definition-globals!)
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

(define s-nil (core-sink-struct "nil" #/list))
(define s-cons (core-sink-struct "cons" #/list "first" "rest"))

(define s-clamor-err
  (core-sink-struct "clamor-err" #/list "blame" "message"))

(define/contract (sink-list->maybe-racket sink-list)
  (-> sink? #/maybe/c #/listof sink?)
  ; NOTE: We could call `sink-list->maybe-racket` itself recursively,
  ; but we explicitly accumulate elements using a parameter
  ; (`rev-racket-list`) of a recursive helper function (`next`) so
  ; that we keep the call stack at a constant size throughout the list
  ; traversal.
  (begin (assert-can-get-cene-definition-globals!)
  #/w-loop next sink-list sink-list rev-racket-list (list)
  #/mat (unmake-sink-struct-maybe (s-nil) sink-list) (just #/list)
    (just #/reverse rev-racket-list)
  #/mat (unmake-sink-struct-maybe (s-cons) sink-list)
    (just #/list elem sink-list)
    (next sink-list #/cons elem rev-racket-list)
  #/nothing))

(define/contract (racket-list->sink racket-list)
  (-> (listof sink?) sink?)
  (begin (assert-can-get-cene-definition-globals!)
  #/list-foldr racket-list (make-sink-struct (s-nil) #/list)
  #/fn elem rest
    (make-sink-struct (s-cons) #/list elem rest)))

(define/contract (extfx-claim name on-success)
  (-> authorized-name? (-> extfx?) extfx?)
  (extfx-claim-unique name
    (error-definer-from-message
      "Tried to claim a name unique more than once")
    (error-definer-from-message
      "Internal error: Expected the sink-extfx-claim familiarity ticket to be spent")
  #/fn fresh-authorized-name familiarity-ticket
  #/extfx-split-list familiarity-ticket 0
    (error-definer-from-message
      "Internal error: Expected the sink-extfx-claim familiarity ticket to be spent only once")
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

(define/contract (sink-extfx-claim name on-success)
  (-> sink-authorized-name? (-> sink-extfx?) sink-extfx?)
  (dissect name (sink-authorized-name name)
  #/make-sink-extfx #/fn
    (extfx-with-cene-definition-restorer #/fn restore
    #/extfx-claim name #/fn
    #/restore #/fn
    #/extfx-run-sink-extfx #/on-success)))

(define/contract (sink-extfx-claim-and-split unique-name n then)
  (->
    sink-authorized-name? natural?
    (-> (listof sink-authorized-name?) sink-extfx?)
    sink-extfx?)
  (dissect unique-name (sink-authorized-name unique-name)
  #/make-sink-extfx #/fn
    (extfx-with-cene-definition-restorer #/fn restore
    #/extfx-claim-and-split unique-name n #/fn names
    #/restore #/fn
    #/extfx-run-sink-extfx #/then #/list-map names #/fn name
      (sink-authorized-name name))))

(define/contract (sink-extfx-claim-freshen unique-name then)
  (-> sink-authorized-name? (-> sink-authorized-name? sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-and-split unique-name 1
  #/dissectfn (list unique-name)
  #/then unique-name))

(define/contract (cexpr-is-closed? cexpr)
  (-> cexpr? boolean?)
  (not #/cexpr-has-free-vars? cexpr #/table-empty))

; This evaluates the given cexpr. This causes an error instead if the
; current getfx side effects runner doesn't support evaluating cexprs
; or doesn't have/allow access to all the modules that would be needed
; to know the function call behaviors of the structs this expression
; can construct. Some Cene compilation targets may even reject a
; dependency on this operation at compile time. Compilation targets
; where the struct tag names have been compiled to meaningless tokens
; will usually not support this operation, since there's no way to
; compare the expresson's first-class names with the tokens the rest
; of the program interacts with.
;
; The macroexpander's getfx side effects runner fully supports the
; operation, and so far the only getfx side effects runner we've
; written is the macrexpander's (TODO), so this particular version
; always succeeds.
;
(define/contract (cexpr-eval fault cexpr)
  (-> sink-fault? (and/c cexpr? cexpr-is-closed?) sink?)
  (cexpr-eval-in-env fault cexpr (table-empty)))

; This returns a computation that reads all the content of the given
; text input stream and runs the reader macros it encounters. Unlike
; typical Lisp readers, this does not read first-class values; it only
; reads and performs side effects.
(define/contract
  (sink-extfx-read-top-level
    fault unique-name qualify text-input-stream)
  (-> sink-fault? sink-authorized-name? sink? sink-text-input-stream?
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-read-eof fault text-input-stream
    ; If we're at the end of the file, we're done. We claim the
    ; `unique-name` to stay in the habit, even though it's clear no
    ; one else can be using it.
    (sink-extfx-claim unique-name #/fn #/sink-extfx-noop)
  #/fn text-input-stream
  #/sink-extfx-claim-and-split unique-name 3
  #/dissectfn
    (list unique-name-stream unique-name-writer unique-name-main)
  #/sink-extfx-make-cexpr-sequence-output-stream
    fault
    unique-name-stream
    unique-name-writer
    (fn unique-name-writer cexpr then
      ; If we encounter an expression, we evaluate it and call the
      ; result, passing in the current scope information.
      (sink-extfx-claim-and-split unique-name-writer 2
      #/dissectfn (list unique-name-first unique-name-rest)
      #/expect cexpr (sink-cexpr cexpr)
        ; TODO: Test that we can actually get this error. We might
        ; already be checking for this condition elsewhere.
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Encountered a top-level expression that compiled to a non-expression value")
      #/expect (cexpr-has-free-vars? cexpr #/table-empty) #f
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Encountered a top-level expression with at least one free variable")
      #/sink-extfx-fuse (then unique-name-rest)
      #/expect (cexpr-eval fault cexpr) (sink-directive directive)
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Expected every top-level expression to evaluate to a directive")
      #/w- effects
        (sink-call fault directive unique-name-first qualify)
      #/expect (sink-extfx? effects) #t
        ; TODO FAULT: Make this `fault` more specific.
        (cene-err fault "Expected every top-level expression to evaluate to a directive made from a callable value that takes two arguments and returns extfx side effects")
        effects))
  #/fn output-stream unwrap
  #/sink-extfx-read-cexprs
    fault unique-name-main qualify text-input-stream output-stream
  #/fn unique-name-main qualify text-input-stream output-stream
  #/unwrap output-stream #/fn unique-name-writer
  #/sink-extfx-claim-and-split unique-name-writer 0 #/dissectfn (list)
  #/sink-extfx-read-top-level
    fault unique-name-main qualify text-input-stream))

(define/contract
  (sink-extfx-run-string fault unique-name qualify string)
  (->
    sink-fault?
    sink-authorized-name?
    (-> sink-name? sink-authorized-name?)
    string?
    sink-extfx?)
  (sink-extfx-read-top-level
    fault
    unique-name
    (sink-fn-curried-fault 1 #/fn fault name
      (expect (sink-name? name) #t
        (cene-err fault "Expected the input to the root qualify function to be a name")
      #/qualify name))
    (sink-text-input-stream #/box #/just #/open-input-string string)))
