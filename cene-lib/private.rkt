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

(require #/only-in racket/contract/base
  -> ->* and/c any/c contract? list/c listof or/c parameter/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define/generic define-generics)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-any list-foldl list-foldr list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/maybe
  just maybe/c maybe-map nothing nothing?)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct
  auto-write define-imitation-simple-struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in effection/order
  assocs->table-if-mutually-unique dex-immutable-string)
(require #/only-in effection/order/base
  compare-by-dex dex? dex-give-up dex-dex dex-name dex-struct
  dex-table fuse-by-merge merge-by-dex merge-table name? name-of
  ordering-eq? table? table-empty table-get table-map-fuse
  table-shadow)
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

; TODO: Since Lathe Comforts isn't specifically designed around
; Effection, its `trivial?` values doesn't expose struct info
; Effection's `dex-struct` can use. We work around that here by
; defining another structure type to use in place of `trivial?`. If
; there ever gets to be a better way to do this, let's use that.
(struct-easy (effection-trivial) #:equal)


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


(struct-easy (sink-directive directive)
  #:other #:methods gen:sink [])
(struct-easy (sink-dex dex)
  #:other #:methods gen:sink [])
(struct-easy (sink-name name)
  #:other #:methods gen:sink [])
(struct-easy (sink-authorized-name name)
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
; NOTE: We use `sink-opaque-fn` to represent the values recognized by
; Cene's `is-fusable-fn`, so they're not always fully opaque.
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

; NOTE: Once we have the ability to import names, we should make sure
; this produces an authorized name whose name is importable from a
; UUID-identified import. Modules identified by UUID can only be
; implemented by the language implementation. And since that's the
; case, there should be no way for Cene code to obtain these
; authorized names; just the unauthorized equivalents.
;
(define/contract (sink-name-qualify-for-lang-impl unqualified-name)
  (-> sink-name? sink-authorized-name?)
  (dissect unqualified-name (sink-name #/unsafe:name n)
  #/sink-authorized-name #/unsafe:name
    (list 'name:qualified-for-lang-impl n)))

(define/contract (sink-name-for-claim inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n #/list 'name:claim n))

(define/contract (sink-authorized-name-for-claim inner-name)
  (-> sink-authorized-name? sink-authorized-name?)
  (dissect inner-name (sink-authorized-name #/unsafe:name inner-name)
  #/sink-authorized-name #/unsafe:name #/list 'name:claim inner-name))

(struct-easy (cene-process-error message))
(struct-easy (cene-process-get name then))
(struct-easy (cene-process-put name dex value))
(struct-easy (cene-process-noop))
(struct-easy (cene-process-fuse a b))

(define/contract (cene-process? v)
  (-> any/c boolean?)
  (or
    (cene-process-error? v)
    (cene-process-get? v)
    (cene-process-put? v)
    (cene-process-noop? v)
    (cene-process-fuse? v)))

(define-imitation-simple-struct
  (cene-runtime?
    cene-runtime-defined-dexes
    cene-runtime-defined-values
    cene-runtime-init-package)
  cene-runtime 'cene-runtime (current-inspector) (auto-write))

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
  (parameter/c #/maybe/c #/-> sink-name? sink?)
  (make-parameter #/nothing))

(define/contract (assert-can-get-cene-definitions!)
  (-> void?)
  (expect (cene-definition-get-param) (just get)
    (error "Expected an implementation of `cene-definition-get` to be available in the dynamic scope")
  #/void))

(define/contract (assert-cannot-get-cene-definitions!)
  (-> void?)
  (mat (cene-definition-get-param) (just get)
    ; TODO: Make this error message more visually distinct from the
    ; `assert-can-get-cene-definitions!` error message.
    (error "Expected no implementation of `cene-definition-get` to be available in the dynamic scope")
  #/void))

(define/contract (cene-definition-get name)
  (-> sink-name? sink?)
  (expect (cene-definition-get-param) (just get)
    (error "Expected every call to `cene-definition-get` to occur with an implementation in the dynamic scope")
  #/get name))

(struct-easy (with-gets-suspended name then))
(struct-easy (with-gets-finished result))

(define/contract (with-gets-from table thunk)
  (-> sink-table? (-> any/c)
    (or/c with-gets-suspended? with-gets-finished?))
  (begin (assert-cannot-get-cene-definitions!)
  #/call-with-current-continuation #/fn suspend-k
  #/with-gets-finished
  #/parameterize
    (
      [
        cene-definition-get-param
        (just #/fn name
          (mat (sink-table-get-maybe table name) (just value) value
          #/call-with-current-continuation #/fn resume-k
          #/suspend-k #/with-gets-suspended name resume-k))])
    (thunk)))

(define/contract
  (with-gets-from-as-process table body body-result-to-process)
  (-> sink-table? (-> any/c) (-> any/c cene-process?) cene-process?)
  (begin (assert-cannot-get-cene-definitions!)
  #/w- with-gets-result (with-gets-from table body)
  #/mat with-gets-result (with-gets-suspended name then)
    (cene-process-get name #/fn value
    #/with-gets-from-as-process
      table (fn #/then value) body-result-to-process)
  #/dissect with-gets-result (with-gets-finished body-result)
  #/body-result-to-process body-result))

(define/contract (sink-effects-run! effects)
  (-> sink-effects? cene-process?)
  (begin (assert-can-get-cene-definitions!)
  #/dissect effects (sink-effects go!)
  #/go!))

(define/contract (cene-process-run rt process)
  (-> cene-runtime? cene-process?
    (list/c cene-runtime? #/listof string?))
  (begin (assert-cannot-get-cene-definitions!)
  #/dissect rt
    (cene-runtime defined-dexes defined-values init-package)
  #/w-loop next-full
    processes (list process)
    rev-next-processes (list)
    defined-dexes defined-dexes
    defined-values defined-values
    rev-errors (list)
    did-something #f
  #/expect processes (cons process processes)
    (mat rev-next-processes (list)
      ; If there are no processes left, we're done. We return the
      ; updated Cene runtime and the list of errors.
      (list
        (cene-runtime defined-dexes defined-values init-package)
        (reverse rev-errors))
    #/if (not did-something)
      ; The processes are stalled. We log errors corresponding to all
      ; the processes.
      (next-full (list) (list) defined-dexes defined-values
        (append
          (list-map rev-next-processes #/fn process
            "Read from a name that was never defined")
          rev-errors)
        #t)
    #/next-full (reverse rev-next-processes) (list)
      defined-dexes defined-values rev-errors #f)
  #/w- next-simple
    (fn rev-next-processes
      (next-full
        processes rev-next-processes defined-dexes defined-values
        rev-errors #t))
  #/w- next-with-error
    (fn error
      (next-full
        processes rev-next-processes defined-dexes defined-values
        (cons error rev-errors)
        #t))
  #/mat process (cene-process-error message)
    (next-with-error message)
  #/mat process (cene-process-noop)
    (next-simple rev-next-processes)
  #/mat process (cene-process-fuse a b)
    (next-simple #/list* b a rev-next-processes)
  #/mat process (cene-process-put name cene-dex value)
    ; If there has already been a definition installed at this name,
    ; this checks that the proposed `dex` matches the stored dex and
    ; that the proposed `value` matches the stored value according to
    ; that dex. Otherwise, it stores the proposed `dex` and `value`
    ; without question.
    (w- name (sink-authorized-name-get-name name)
    #/mat (sink-table-get-maybe defined-dexes name)
      (just existing-cene-dex)
      (dissect cene-dex (sink-dex dex)
      #/dissect existing-cene-dex (sink-dex existing-dex)
      #/expect (eq-by-dex? (dex-dex) dex existing-dex) #t
        (next-with-error "Wrote to the same name with inequal dexes")
      #/dissect (sink-table-get-maybe defined-values name)
        (just existing-value)
      #/next-simple #/cons
        ; NOTE: Since Cene dexes can potentially invoke
        ; `cene-definition-get` on a not-yet-defined name, we use this
        ; this `with-gets-...` operation here so that it can properly
        ; suspend the dex comparison computation as a Cene process.
        (with-gets-from-as-process defined-values
          (fn #/eq-by-dex? existing-dex value existing-value)
        #/fn is-eq
          (expect is-eq #t
            (cene-process-error
              "Wrote to the same name with inequal values")
          #/cene-process-noop))
        rev-next-processes)
    #/next-full
      processes
      rev-next-processes
      (sink-table-put-maybe defined-dexes name #/just cene-dex)
      (sink-table-put-maybe defined-values name #/just value)
      rev-errors
      #t)
  #/mat process (cene-process-get name then)
    ; If there has not yet been a definition installed at this name,
    ; we set this process aside and come back to it later. If there
    ; has, we call `then` with that defined value and set aside its
    ; result as a process to come back to later.
    (expect (sink-table-get-maybe defined-values name) (just value)
      (next-full
        processes
        (cons process rev-next-processes)
        defined-dexes
        defined-values
        rev-errors
        did-something)
    #/next-simple #/cons
      (with-gets-from-as-process defined-values
        (fn #/sink-effects-run! #/then value)
      #/fn process process)
      rev-next-processes)
  #/error "Encountered an unrecognized kind of Cene process"))

(define-generics cexpr
  (cexpr-has-free-vars? cexpr env)
  (cexpr-eval cexpr env))

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
    
    (define (cexpr-eval this env)
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
    
    (define (cexpr-eval this env)
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
    
    (define (cexpr-eval this env)
      (expect this (cexpr-construct main-tag-name projs)
        (error "Expected this to be a cexpr-construct")
      #/make-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        (-eval proj-cexpr env)))
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
    
    (define (cexpr-eval this env)
      (expect this (cexpr-call func arg)
        (error "Expected this to be a cexpr-call")
      #/sink-call (-eval func env) (-eval arg env)))
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
    
    (define (cexpr-eval this env)
      (expect this (cexpr-opaque-fn param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/sink-opaque-fn #/fn arg
        (-eval body #/table-shadow param (just arg) env)))
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
    
    (define (cexpr-eval this env)
      (expect this (cexpr-let bindings body)
        (error "Expected this to be a cexpr-let")
      #/-eval body
      #/list-foldl env
        (list-map bindings #/dissectfn (list var val)
          (list var #/-eval val env))
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
    
    (define (cexpr-eval this env)
      (expect this (cexpr-located location-definition-name body)
        (error "Expected this to be a cexpr-located")
      ; TODO CEXPR-LOCATED: Add `location-definition-name` or
      ; `(cene-definition-get location-definition-name)` to some kind
      ; of call stack.
      #/-eval body))
  ])

; NOTE: The only purpose of this is to help track down a common kind
; of error where the result of `go!` is mistakenly a `sink-effects?`
; instead of a `cene-process?`.
(define/contract (make-sink-effects go!)
  (-> (-> cene-process?) sink-effects?)
  (sink-effects go!))

(define/contract (sink-effects-get name then)
  (-> sink-name? (-> sink? sink-effects?) sink-effects?)
  (make-sink-effects #/fn #/cene-process-get name then))

(define/contract (sink-effects-put name dex value)
  (-> sink-authorized-name? sink-dex? sink? sink-effects?)
  (make-sink-effects #/fn #/cene-process-put name dex value))

(define/contract (sink-effects-noop)
  (-> sink-effects?)
  (make-sink-effects #/fn #/cene-process-noop))

(define/contract (sink-effects-fuse-binary a b)
  (-> sink-effects? sink-effects? sink-effects?)
  (dissect a (sink-effects a-go!)
  #/dissect b (sink-effects b-go!)
  #/make-sink-effects #/fn #/cene-process-fuse (a-go!) (b-go!)))

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
  (->
    sink-authorized-name?
    (listof #/list/c sink-authorized-name? sink-cexpr?)
    sink-cexpr?)
  (dissect main-tag-name (sink-authorized-name main-tag-name)
  #/if
    (sink-names-have-duplicate? #/list-map projs
    #/dissectfn (list proj-name proj-cexpr)
      (sink-authorized-name-get-name proj-name))
    (error "Encountered a duplicate projection name")
  #/sink-cexpr #/cexpr-construct main-tag-name #/list-map projs
  #/dissectfn
    (list (sink-authorized-name proj-name) (sink-cexpr proj-cexpr))
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
  #/sink-cexpr-construct (sink-authorized-name main-tag-name)
  #/list-zip-map proj-names proj-cexprs #/fn proj-name proj-cexpr
    (list (sink-authorized-name proj-name) proj-cexpr)))

(define/contract (sink-cexpr-call func arg)
  (-> sink-cexpr? sink-cexpr? sink-cexpr?)
  (dissect func (sink-cexpr func)
  #/dissect arg (sink-cexpr arg)
  #/sink-cexpr #/cexpr-call func arg))

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
  (-> symbol? sink-name? sink-table? sink-name?)
  (dissect main-tag-name (sink-name #/unsafe:name main-tag)
  #/dissect proj-tag-names (sink-table proj-tag-names)
  #/dissect
    (name-of (dex-table #/dex-struct effection-trivial)
    #/table-v-map proj-tag-names #/dissectfn _ #/effection-trivial)
    (just #/unsafe:name proj-table-name)
  #/sink-name #/unsafe:name
  #/list result-tag main-tag proj-table-name))

(define/contract
  (sink-name-for-function-implementation-code
    main-tag-name proj-tag-names)
  (-> sink-name? sink-table? sink-name?)
  (sink-name-for-function-implementation
    'name:function-implementation-code
    main-tag-name
    proj-tag-names))

(define/contract
  (sink-name-for-function-implementation-value
    main-tag-name proj-tag-names)
  (-> sink-name? sink-table? sink-name?)
  (sink-name-for-function-implementation
    'name:function-implementation-value
    main-tag-name
    proj-tag-names))

(define/contract (sink-proj-tag-authorized-names->trivial proj-tag-names)
  (-> sink-table? sink-table?)
  (dissect proj-tag-names (sink-table proj-tag-names)
  #/sink-table #/table-kv-map proj-tag-names #/fn k v
    (expect v (sink-authorized-name name)
      (error "Expected each value of proj-tag-names to be an authorized name")
    #/expect (eq-by-dex? (dex-name) k name) #t
      (error "Expected each value of proj-tag-names to be an authorized name where the name authorized is the same as the name it's filed under")
    #/trivial)))

(define/contract
  (sink-authorized-name-for-function-implementation-code
    main-tag-name proj-tag-names)
  (-> sink-authorized-name? sink-table? sink-authorized-name?)
  (dissect
    (sink-name-for-function-implementation-code
      (sink-authorized-name-get-name main-tag-name)
      (sink-proj-tag-authorized-names->trivial proj-tag-names))
    (sink-name name)
  #/sink-authorized-name name))

(define/contract
  (sink-authorized-name-for-function-implementation-value
    main-tag-name proj-tag-names)
  (-> sink-authorized-name? sink-table? sink-authorized-name?)
  (dissect
    (sink-name-for-function-implementation-value
      (sink-authorized-name-get-name main-tag-name)
      (sink-proj-tag-authorized-names->trivial proj-tag-names))
    (sink-name name)
  #/sink-authorized-name name))

(define/contract (sink-fn-curried n-args racket-func)
  (-> exact-positive-integer? procedure? sink-opaque-fn?)
  (dissect (nat->maybe n-args) (just n-args-after-next)
  #/w-loop next n-args-after-next n-args-after-next rev-args (list)
  #/sink-opaque-fn #/fn arg
    (w- rev-args (cons arg rev-args)
    #/expect (nat->maybe n-args-after-next) (just n-args-after-next)
      (apply racket-func #/reverse rev-args)
    #/next n-args-after-next rev-args)))

(define/contract (raise-cene-err continuation-marks clamor)
  (-> continuation-mark-set? sink? #/or/c)
  (w- message
    (mat (unmake-sink-struct-maybe s-clamor-err clamor)
      (just #/list #/sink-string message)
      message
    #/format "~s" clamor)
  #/raise
  #/exn:fail:cene message (current-continuation-marks) clamor))

(define-simple-macro (cene-err message:string)
  (raise-cene-err (current-continuation-marks)
  ; TODO: See if there's a way we can either stop depending on
  ; `s-clamor-err` here or move this code after the place where
  ; `s-clamor-err` is defined.
  #/make-sink-struct s-clamor-err #/list #/sink-string message))

(define/contract (sink-call-binary func arg)
  (-> sink? sink? sink?)
  (begin (assert-can-get-cene-definitions!)
  #/mat func (sink-opaque-fn racket-func)
    (racket-func arg)
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
          #/just #/make-sink-struct s-trivial #/list)))
    
    #/sink-call-binary (sink-call-binary impl func) arg)
  #/cene-err "Tried to call a value that wasn't an opaque function or a struct"))

(define/contract (sink-call-list func args)
  (-> sink? (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/list-foldl func args #/fn func arg #/sink-call-binary func arg))

(define/contract (sink-call func . args)
  (->* (sink?) #:rest (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-call-list func args))

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
  (dissect
    (name-of (dex-struct sink-string #/dex-immutable-string) string)
    (just result)
    result))

(define/contract (sink-name-for-string string)
  (-> sink-string? sink-name?)
  (sink-name #/name-for-sink-string string))

(define/contract (sink-authorized-name-get-name name)
  (-> sink-authorized-name? sink-name?)
  (dissect name (sink-authorized-name effection-name)
  #/sink-name effection-name))

(define/contract (sink-name-subname index-name inner-name)
  (-> sink-name? sink-name? sink-name?)
  (dissect index-name (sink-name #/unsafe:name index-name)
  #/sink-name-rep-map inner-name #/fn n
    (list 'name:subname index-name n)))

(define/contract (sink-authorized-name-subname index-name inner-name)
  (-> sink-name? sink-authorized-name? sink-authorized-name?)
  (dissect index-name (sink-name #/unsafe:name index-name)
  #/dissect inner-name (sink-authorized-name #/unsafe:name inner-name)
  #/sink-authorized-name #/unsafe:name
    (list 'name:subname index-name inner-name)))

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

(define/contract (sink-cexpr-sequence-output-stream-spend! stream)
  (-> sink-cexpr-sequence-output-stream?
    (list/c
      any/c
      (-> any/c sink-cexpr? (-> any/c sink-effects?) sink-effects?)))
  (dissect stream (sink-cexpr-sequence-output-stream id b)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just state-and-handler)
    (cene-err "Tried to spend an expression output stream that was already spent")
  #/begin
    (set-box! b (nothing))
    state-and-handler))

(define/contract
  (sink-effects-make-cexpr-sequence-output-stream
    unique-name state on-cexpr then)
  (->
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
        (cene-err "Expected the expression sequence output stream given to an effects-make-expr-sequence-output-stream unwrapper to be a descendant of the same one created by that call")
      #/dissect
        (sink-cexpr-sequence-output-stream-spend! output-stream)
        (list state on-cexpr)
      #/then state))))

(define/contract (sink-effects-cexpr-write output-stream cexpr then)
  (->
    sink-cexpr-sequence-output-stream? sink-cexpr?
    (-> sink-cexpr-sequence-output-stream? sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn
  #/dissect output-stream (sink-cexpr-sequence-output-stream id _)
  #/dissect (sink-cexpr-sequence-output-stream-spend! output-stream)
    (list state on-cexpr)
  #/on-cexpr state cexpr #/fn state
  #/then #/sink-cexpr-sequence-output-stream id #/box #/just #/list
    state on-cexpr))

(define/contract (sink-text-input-stream-spend! text-input-stream)
  (-> sink-text-input-stream? input-port?)
  (dissect text-input-stream (sink-text-input-stream b)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just input-port)
    (cene-err "Tried to spend a text input stream that was already spent")
  #/begin
    (set-box! b (nothing))
    input-port))

(define/contract
  (sink-effects-read-eof text-input-stream on-eof else)
  (->
    sink-text-input-stream?
    sink-effects?
    (-> sink-text-input-stream? sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
  #/if (eof-object? #/peek-byte in)
    (begin (close-input-port in)
      on-eof)
  #/else #/sink-text-input-stream #/box #/just in))

(define/contract
  (sink-effects-optimized-textpat-read-located
    pattern text-input-stream then)
  (->
    optimized-textpat?
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
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
  (sink-effects-peek-whether-eof text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    sink-effects-peek-whether-eof-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/mat maybe-located-string (just located-string)
    (then text-input-stream #f)
    (then text-input-stream #t)))

(define sink-effects-read-whitespace-pat
  ; TODO: Support a more Unicode-aware notion of whitespace.
  (optimize-textpat #/textpat-star #/textpat-one-in-string " \t\r\n"))

(define/contract
  (sink-effects-read-whitespace text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    sink-effects-read-whitespace-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define sink-effects-read-non-line-breaks-pat
  (optimize-textpat
  ; TODO: Support a more Unicode-aware notion of line break.
  #/textpat-star #/textpat-one-not-in-string "\r\n"))

(define/contract
  (sink-effects-read-non-line-breaks text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    sink-effects-read-non-line-breaks-pat text-input-stream
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

(define/contract
  (sink-effects-read-maybe-identifier
    qualify text-input-stream pre-qualify then)
  (->
    sink?
    sink-text-input-stream?
    (-> sink-name? sink-name?)
    (->
      sink-text-input-stream?
      (maybe/c #/list/c sink-located-string? sink-authorized-name?)
      sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    sink-effects-read-maybe-identifier-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/expect maybe-located-string (just located-string)
    (then text-input-stream #/nothing)
  #/sink-effects-string-from-located-string located-string #/fn string
  #/then text-input-stream
    (just #/list located-string
    #/sink-call qualify #/pre-qualify #/sink-name-for-string string)))

(define sink-effects-read-maybe-op-character-pat
  ; TODO: Support a more Unicode-aware notion here, maybe the
  ; "pattern" symbols described in the Unicode identifier rules.
  (optimize-textpat #/textpat-one-not #/textpat-or
    (textpat-one-in-string "-0 \t\r\n[]()\\.:")
    (textpat-one-in-range #\1 #\9)
    (textpat-one-in-range #\a #\z)
    (textpat-one-in-range #\A #\Z)))

(define/contract
  (sink-effects-read-maybe-op-character text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    sink-effects-read-maybe-op-character-pat text-input-stream then))

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
  (sink-effects-peek-whether-closing-bracket text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (sink-effects-optimized-textpat-read-located
    sink-effects-peek-whether-closing-bracket-pat text-input-stream
  #/fn text-input-stream maybe-located-empty-string
  #/mat maybe-located-empty-string (just located-empty-string)
    (then text-input-stream #t)
    (then text-input-stream #f)))

(define/contract
  (sink-effects-read-op text-input-stream qualify pre-qualify then)
  (->
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
  
  (sink-effects-read-maybe-op-character text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (sink-effects-string-from-located-string identifier
    #/fn identifier
    #/then text-input-stream
      (sink-call qualify
      #/pre-qualify #/sink-name-for-string identifier))
  
  #/w- then
    (fn text-input-stream op-name
      (sink-effects-optimized-textpat-read-located
        |pat ":"| text-input-stream
      #/fn text-input-stream maybe-str
      #/then text-input-stream op-name))
  
  ; TODO: Support the use of ( and [ as delimiters for macro
  ; names.
  #/sink-effects-optimized-textpat-read-located
    |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "The use of ( to delimit a macro name is not yet supported")
  #/sink-effects-optimized-textpat-read-located
    |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "The use of [ to delimit a macro name is not yet supported")
  
  #/sink-effects-read-maybe-identifier
    qualify text-input-stream pre-qualify
  #/fn text-input-stream maybe-name
  #/mat maybe-name (just #/list located-string name)
    (then text-input-stream name)
  
  #/cene-err "Encountered an unrecognized case of the expression operator syntax"))

(define/contract
  (sink-effects-run-op
    op-impl unique-name qualify text-input-stream output-stream then)
  (->
    sink? sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/w- result
    (sink-call op-impl
      unique-name qualify text-input-stream output-stream
    #/sink-fn-curried 4
    #/fn unique-name qualify text-input-stream output-stream
    #/expect (sink-authorized-name? unique-name) #t
      (cene-err "Expected the unique name of a macro's callback results to be an authorized name")
    #/expect (sink-text-input-stream? text-input-stream) #t
      (cene-err "Expected the text input stream of a macro's callback results to be a text input stream")
    #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
      (cene-err "Expected the expression sequence output stream of a macro's callback results to be an expression sequence output stream")
    #/sink-effects-claim-freshen unique-name #/fn unique-name
    #/then unique-name qualify text-input-stream output-stream)
  #/expect (sink-effects? result) #t
    (cene-err "Expected the return value of a macro to be an effectful computation")
    result))

(define/contract
  (sink-effects-read-and-run-op
    unique-name qualify text-input-stream output-stream pre-qualify
    then)
  (->
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
  #/sink-effects-read-op text-input-stream qualify pre-qualify
  #/fn text-input-stream op-name
  #/sink-effects-get (sink-authorized-name-get-name op-name)
  #/fn op-impl
  #/sink-effects-run-op
    op-impl unique-name qualify text-input-stream output-stream
    then))

(define/contract
  (sink-effects-read-and-run-freestanding-cexpr-op
    unique-name qualify text-input-stream output-stream then)
  (->
    sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-read-and-run-op
    unique-name qualify text-input-stream output-stream
    sink-name-for-freestanding-cexpr-op
    then))

(define/contract
  (sink-effects-read-and-run-bounded-cexpr-op
    unique-name qualify text-input-stream output-stream then)
  (->
    sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-read-and-run-op
    unique-name qualify text-input-stream output-stream
    sink-name-for-bounded-cexpr-op
    then))

(define/contract
  (sink-effects-run-nameless-op
    unique-name qualify text-input-stream output-stream then)
  (->
    sink-authorized-name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      sink-authorized-name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/sink-effects-get
    (sink-authorized-name-get-name
    #/sink-call qualify #/sink-name-for-nameless-bounded-cexpr-op)
  #/fn op-impl
  #/sink-effects-run-op
    op-impl unique-name qualify text-input-stream output-stream
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
    unique-name qualify text-input-stream output-stream then)
  (->
    sink-authorized-name? sink? sink-text-input-stream?
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
  #/sink-effects-read-whitespace text-input-stream
  #/fn text-input-stream whitespace
  #/sink-effects-peek-whether-eof text-input-stream
  #/fn text-input-stream is-eof
  #/if is-eof
    (then unique-name qualify text-input-stream output-stream)
  
  #/sink-effects-optimized-textpat-read-located
    |pat ")"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "Encountered an unmatched )")
  #/sink-effects-optimized-textpat-read-located
    |pat "]"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "Encountered an unmatched ]")
  
  #/sink-effects-optimized-textpat-read-located
    |pat "\\"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-and-run-freestanding-cexpr-op
      unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-optimized-textpat-read-located
    |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-optimized-textpat-read-located
          |pat ")"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          (cene-err "Encountered a syntax that began with ( or (. and did not end with )")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-optimized-textpat-read-located
      |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-optimized-textpat-read-located
    |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-optimized-textpat-read-located
          |pat "]"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          (cene-err "Encountered a syntax that began with [ or [. and did not end with ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-optimized-textpat-read-located
      |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-optimized-textpat-read-located
    |pat "/"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-peek-whether-closing-bracket text-input-stream
        #/fn text-input-stream is-closing-bracket
        #/if (not is-closing-bracket)
          (cene-err "Encountered a syntax that began with /. and did not end at ) or ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-optimized-textpat-read-located
      |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream output-stream then)
  
  #/cene-err "Encountered an unrecognized case of the expression syntax"))

(define/contract (core-sink-struct main-tag-string proj-strings)
  (-> immutable-string? (listof immutable-string?)
    (and/c pair? #/listof name?))
  (w- main-tag-authorized-name
    (sink-name-qualify-for-lang-impl #/sink-name-for-struct-main-tag
    #/sink-name-for-string #/sink-string main-tag-string)
  #/w- main-tag-name
    (sink-authorized-name-get-name main-tag-authorized-name)
  #/list-map
    (cons main-tag-authorized-name
    #/list-map proj-strings #/fn proj-string
      (sink-name-qualify-for-lang-impl
      #/sink-name-for-struct-proj main-tag-name
      #/sink-name-for-string #/sink-string proj-string))
  #/dissectfn (sink-authorized-name name)
    name))

(define s-trivial (core-sink-struct "trivial" #/list))

(define s-nothing (core-sink-struct "nothing" #/list))
(define s-just (core-sink-struct "just" #/list "val"))

(define s-carried (core-sink-struct "carried" #/list "main" "carry"))

(define s-clamor-err (core-sink-struct "clamor-err" #/list "message"))

(define/contract (sink-effects-claim name)
  (-> sink-authorized-name? sink-effects?)
  (sink-effects-put
    (sink-authorized-name-for-claim name)
    (sink-dex #/dex-give-up)
    (make-sink-struct s-trivial #/list)))

(define/contract (sink-effects-claim-and-split unique-name n then)
  (->
    sink-authorized-name?
    natural?
    (-> (listof sink-authorized-name?) sink-effects?)
    sink-effects?)
  (sink-effects-fuse (sink-effects-claim unique-name)
  #/w-loop next n n next-name unique-name names (list)
    (expect (nat->maybe n) (just n) (then names)
    
    ; NOTE: We do not want these to be names that Cene code can
    ; recreate. If we were implementing `sink-effects-claim-and-split`
    ; in a Cene package, we could do this by using
    ; `sink-authorized-name-subname` with a key name made out of a
    ; `dex-struct` of a struct tag that was made out of unique names
    ; known only to that package. In this implementation, we don't
    ; have to go to all that trouble.
    ;
    #/dissect next-name (sink-authorized-name #/unsafe:name next-name)
    #/w- first
      (sink-authorized-name #/unsafe:name
        (list 'name:first next-name))
    #/w- rest
      (sink-authorized-name #/unsafe:name
        (list 'name:rest next-name))
    
    #/next n rest #/cons first names)))

(define/contract (sink-effects-claim-freshen unique-name then)
  (-> sink-authorized-name? (-> sink-authorized-name sink-effects?)
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
(define/contract (sink-effects-cexpr-eval cexpr then)
  (-> (and/c cexpr? cexpr-can-eval?) (-> sink? sink-effects?)
    sink-effects?)
  (sink-effects-later #/fn #/then #/cexpr-eval cexpr #/table-empty))

; This returns a computation that reads all the content of the given
; text input stream and runs the reader macros it encounters. Unlike
; typical Lisp readers, this does not read first-class values; it only
; reads and performs side effects.
(define/contract
  (sink-effects-read-top-level unique-name qualify text-input-stream)
  (-> sink-authorized-name? sink? sink-text-input-stream?
    sink-effects?)
  (sink-effects-claim-freshen unique-name #/fn unique-name
  #/sink-effects-read-eof text-input-stream
    ; If we're at the end of the file, we're done. We claim the
    ; `unique-name` to stay in the habit, even though it's clear no
    ; one else can be using it.
    (sink-effects-claim unique-name)
  #/fn text-input-stream
  #/sink-effects-claim-and-split unique-name 3
  #/dissectfn
    (list unique-name-stream unique-name-writer unique-name-main)
  #/sink-effects-make-cexpr-sequence-output-stream unique-name-stream
    unique-name-writer
    (fn unique-name-writer cexpr then
      ; If we encounter an expression, we evaluate it and call the
      ; result, passing in the current scope information.
      (sink-effects-claim-and-split unique-name-writer 2
      #/dissectfn (list unique-name-first unique-name-rest)
      #/expect cexpr (sink-cexpr cexpr)
        ; TODO: Test that we can actually get this error. We might
        ; already be checking for this condition elsewhere.
        (cene-err "Encountered a top-level expression that compiled to a non-expression value")
      #/expect (cexpr-has-free-vars? cexpr #/table-empty) #f
        (cene-err "Encountered a top-level expression with at least one free variable")
      #/sink-effects-fuse (then unique-name-rest)
      #/sink-effects-cexpr-eval cexpr #/fn directive
      #/expect directive (sink-directive directive)
        (cene-err "Expected every top-level expression to evaluate to a directive")
      #/w- effects (sink-call directive unique-name-first qualify)
      #/expect (sink-effects? effects) #t
        (cene-err "Expected every top-level expression to evaluate to a directive made from a callable value that takes two arguments and returns side effects")
        effects))
  #/fn output-stream unwrap
  #/sink-effects-read-cexprs
    unique-name-main qualify text-input-stream output-stream
  #/fn unique-name-main qualify text-input-stream output-stream
  #/unwrap output-stream #/fn unique-name-writer
  #/sink-effects-claim-and-split unique-name-writer 0
  #/dissectfn (list)
  #/sink-effects-read-top-level
    unique-name-main qualify text-input-stream))

(define/contract (cene-runtime-effects-run rt get-effects)
  (-> cene-runtime? (-> sink-effects?)
    (list/c cene-runtime? #/listof string?))
  (begin (assert-cannot-get-cene-definitions!)
  #/dissect rt
    (cene-runtime defined-dexes defined-values init-package)
  #/cene-process-run rt #/with-gets-from-as-process defined-values
    (fn #/sink-effects-run! #/get-effects)
    (fn process process)))

(define/contract (cene-init-package rt unique-name qualify)
  (->
    cene-runtime?
    sink-authorized-name?
    (-> sink-name? sink-authorized-name?)
    (list/c cene-runtime? #/listof string?))
  (begin (assert-cannot-get-cene-definitions!)
  #/dissect rt
    (cene-runtime defined-dexes defined-values init-package)
  #/cene-runtime-effects-run rt #/fn
    (init-package unique-name qualify)))

(define/contract (cene-run-string rt unique-name qualify string)
  (->
    cene-runtime?
    sink-authorized-name?
    (-> sink-name? sink-authorized-name?)
    string?
    (list/c cene-runtime? #/listof string?))
  (begin (assert-cannot-get-cene-definitions!)
  #/cene-runtime-effects-run rt #/fn
    (sink-effects-read-top-level
      unique-name
      (sink-fn-curried 1 #/fn name
        (expect (sink-name? name) #t
          (cene-err "Expected the input to the root qualify function to be a name")
        #/qualify name))
      (sink-text-input-stream #/box #/just
      #/open-input-string string))))

; TODO: See if there's a more elegant approach here than just defining
; two separate roots. Note that these are only used in the `cene-test`
; package. We probably won't actually need these `...-sample-...`
; things in the long run, once we have a module system.
(define/contract (sink-sample-unique-name-root-1)
  (-> sink-authorized-name?)
  (sink-authorized-name #/unsafe:name
    'name:sample-unique-name-root-1))
(define/contract (sink-sample-unique-name-root-2)
  (-> sink-authorized-name?)
  (sink-authorized-name #/unsafe:name
    'name:sample-unique-name-root-2))

(define/contract (sink-sample-qualify-root name)
  (-> sink-name? sink-authorized-name?)
  (dissect name (sink-name #/unsafe:name name)
  #/sink-authorized-name #/unsafe:name
    (list 'name:sample-qualify-root name)))

; TODO: See if we'll use this.
(define/contract (cene-runtime-empty)
  (-> cene-runtime?)
  (cene-runtime
    (sink-table #/table-empty)
    (sink-table #/table-empty)
    (fn unique-name qualify-for-package
      (sink-effects-claim-and-split unique-name 0 #/dissectfn (list)
      #/sink-effects-noop))))
