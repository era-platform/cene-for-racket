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
(require #/only-in lathe-comforts/maybe just maybe/c nothing)
(require #/only-in lathe-comforts/struct struct-easy)

(require #/only-in effection/order
  compare-by-dex dex? dex-give-up dex-dex dex-name dex-struct
  dex-table fuse-by-merge merge-by-dex merge-table name? name-of
  ordering-eq? table-empty table-get table-map-fuse table-shadow)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe name)


(provide #/all-defined-out)



; TODO: Put this into the Lathe Comforts library or something.
(struct-easy (trivial))

; TODO: Put this in the Effection library or something.
(define/contract (eq-by-dex? dex a b)
  (-> dex? any/c any/c boolean?)
  (expect (compare-by-dex dex a b) (just comparison)
    (error "Expected a and b to be members of the domain of dex")
  #/ordering-eq? comparison))


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


(struct-easy (sink-dex dex))
(struct-easy (sink-cline cline))
(struct-easy (sink-merge merge))
(struct-easy (sink-fuse fuse))
(struct-easy (sink-name name))
(struct-easy (sink-effects go!))
(struct-easy
  (sink-cexpr-sequence-output-stream box-of-maybe-state-and-handler))
(struct-easy (sink-text-input-stream box-of-maybe-input))
(struct-easy (sink-located-string parts))
(struct-easy (sink-string racket-string))
(struct-easy (sink-opaque-fn racket-fn))
(struct-easy (sink-table racket-table))
(struct-easy (sink-int racket-int))

; NOTE: The term "cexpr" is short for "compiled expression." It's the
; kind of expression that macros generate in order to use as function
; definitions.
(struct-easy (sink-cexpr cexpr))

(define/contract (sink? v)
  (-> any/c boolean?)
  (or
    (sink-struct? v)
    (sink-dex? v)
    (sink-cline? v)
    (sink-merge? v)
    (sink-fuse? v)
    (sink-name? v)
    (sink-effects? v)
    (sink-cexpr-sequence-output-stream? v)
    (sink-text-input-stream? v)
    (sink-located-string? v)
    (sink-string? v)
    (sink-opaque-fn? v)
    (sink-table? v)
    (sink-int? v)
    (sink-cexpr? v)))

(define/contract (name-rep-map name func)
  (-> name? (-> any/c any/c) name?)
  (dissect name (unsafe:name name)
  #/unsafe:name #/func name))

(define/contract (sink-name-rep-map name func)
  (-> sink-name? (-> any/c any/c) sink-name?)
  (dissect name (sink-name #/unsafe:name name)
  #/sink-name #/unsafe:name #/func name))

(define/contract (name-qualify unqualified-name)
  (-> name? name?)
  (sink-name-rep-map unqualified-name #/fn n #/list 'name:qualified n))

(define/contract (sink-name-qualify unqualified-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map unqualified-name #/fn n
    (list 'name:qualified n)))

(define/contract (name-claimed name)
  (-> name? name?)
  (name-rep-map name #/fn n #/list 'name:claimed n))

(struct-easy (cene-process-error message))
(struct-easy (cene-process-get name then))
(struct-easy (cene-process-put name dex value))
(struct-easy (cene-process-noop))
(struct-easy (cene-process-merge a b))

(define/contract (cene-process? v)
  (-> any/c boolean?)
  (or
    (cene-process-error? v)
    (cene-process-get? v)
    (cene-process-put? v)
    (cene-process-noop? v)
    (cene-process-merge? v)))

(struct-easy (cene-runtime defined-dexes defined-values))

; A version of `cene-runtime?` that does not satisfy
; `struct-predicate-procedure?`.
(define/contract (-cene-runtime? v)
  (-> any/c boolean?)
  (cene-runtime? v))

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
  #/dissect rt (cene-runtime defined-dexes defined-values)
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
        (cene-runtime defined-dexes defined-values)
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
  #/mat process (cene-process-merge a b)
    (next-simple #/list* b a rev-next-processes)
  #/mat process (cene-process-put name cene-dex value)
    ; If there has already been a definition installed at this name,
    ; this checks that the proposed `dex` matches the stored dex and
    ; that the proposed `value` matches the stored value according to
    ; that dex. Otherwise, it stores the proposed `dex` and `value`
    ; without question.
    (mat (sink-table-get-maybe defined-dexes name)
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

(struct-easy (cexpr-native result)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-native result)
        (error "Expected this to be a cexpr-native")
        #f))
    
    (define (cexpr-eval this env)
      (expect this (cexpr-native result)
        (error "Expected this to be a cexpr-native")
        result))
  ])

(struct-easy (cexpr-struct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval cexpr-eval)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-struct main-tag-name projs)
        (error "Expected this to be a cexpr-struct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cexpr-eval this env)
      (expect this (cexpr-struct main-tag-name projs)
        (error "Expected this to be a cexpr-struct")
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

; TODO: Add more cexpr instances. The JavaScript version of Cene has
; five more than we do:
;
;   located mat cline-struct merge-struct fuse-struct err

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
  (-> sink-name? sink-dex? sink? sink-effects?)
  (make-sink-effects #/fn #/cene-process-put name dex value))

(define/contract (sink-effects-noop)
  (-> sink-effects?)
  (make-sink-effects #/fn #/cene-process-noop))

(define/contract (sink-effects-merge-binary a b)
  (-> sink-effects? sink-effects? sink-effects?)
  (dissect a (sink-effects a-go!)
  #/dissect b (sink-effects b-go!)
  #/make-sink-effects #/fn #/cene-process-merge (a-go!) (b-go!)))

(define/contract (sink-effects-merge-list effects)
  (-> (listof sink-effects?) sink-effects?)
  (list-foldl (sink-effects-noop) effects #/fn a b
    (sink-effects-merge-binary a b)))

(define/contract (sink-effects-merge . effects)
  (->* () #:rest (listof sink-effects?) sink-effects?)
  (sink-effects-merge-list effects))

(struct exn:fail:cene exn:fail (clamor))

(define/contract (sink-cexpr-var name)
  (-> sink-name? sink-cexpr?)
  (dissect name (sink-name name)
  #/sink-cexpr #/cexpr-var name))

(define/contract (sink-cexpr-native result)
  (-> sink? sink-cexpr?)
  (sink-cexpr #/cexpr-native result))

(define/contract (names-have-duplicate? names)
  (-> (listof name?) boolean?)
  (w-loop next so-far (table-empty) names names
    (expect names (cons name names) #f
    #/expect (table-get name so-far) (nothing) #t
    #/next (table-shadow name (just #/trivial) so-far) names)))

(define/contract (sink-names-have-duplicate? names)
  (-> (listof sink-name?) boolean?)
  (names-have-duplicate?
  #/list-map names #/dissectfn (sink-name name) name))

(define/contract (sink-cexpr-struct main-tag-name projs)
  (-> sink-name? (listof #/list/c sink-name? sink-cexpr?) sink-cexpr?)
  (dissect main-tag-name (sink-name main-tag-name)
  #/if
    (sink-names-have-duplicate? #/list-map projs
    #/dissectfn (list proj-name proj-cexpr) proj-name)
    (error "Encountered a duplicate projection name")
  #/sink-cexpr #/cexpr-struct main-tag-name #/list-map projs
  #/dissectfn (list (sink-name proj-name) (sink-cexpr proj-cexpr))
    (list proj-name proj-cexpr)))

(define/contract (make-sink-cexpr-struct tags proj-cexprs)
  (-> (and/c pair? #/listof name?) (listof sink-cexpr?) sink-cexpr?)
  (dissect tags (cons main-tag-name proj-names)
  #/expect (= (length proj-names) (length proj-cexprs)) #t
    (error "Expected tags to have one more entry than proj-cexprs")
  #/sink-cexpr-struct (sink-name main-tag-name)
  #/list-zip-map proj-names proj-cexprs #/fn proj-name proj-cexpr
    (list (sink-name proj-name) proj-cexpr)))

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

; TODO: Use this.
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
  (sink-name-for-function-implementation main-tag-name proj-tag-names)
  (-> sink-name? sink-table? sink-name?)
  (dissect main-tag-name (sink-name #/unsafe:name main-tag)
  #/dissect proj-tag-names (sink-table proj-tag-names)
  
  ; TODO: Put these into the `effection/order` module or something
  ; (maybe even `effection/order/base`).
  #/w- table-kv-map
    (fn table kv-to-v
      (mat
        (table-map-fuse table
          (fuse-by-merge #/merge-table #/merge-by-dex #/dex-give-up)
        #/fn k
          (dissect (table-get k table) (just v)
          #/table-shadow k (just #/kv-to-v k v) #/table-empty))
        (just result)
        result
      #/table-empty))
  #/w- table-v-map
    (fn table v-to-v
      (table-kv-map table #/fn k v #/v-to-v v))
  
  #/dissect
    (name-of (dex-table #/dex-struct trivial)
    #/table-v-map proj-tag-names #/fn ignored #/trivial)
    (just #/unsafe:name proj-table-name)
  #/sink-name #/unsafe:name #/list 'name:function-implementation
    main-tag proj-table-name))

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
    
    ; TODO: We should probably optimize this lookup, perhaps by
    ; using memoization. If and when we do, we should profile it to
    ; make sure it's a worthwhile optimization.
    #/w- body
      (cene-definition-get #/sink-name-for-function-implementation
        main-tag
        (list-foldr proj-tags (sink-table #/table-empty)
        #/fn proj-tag rest
          (sink-table-put-maybe rest proj-tag
          ; TODO: See if there's a way we can either stop depending on
          ; `s-trivial` here or move this code after the place where
          ; `s-trivial` is defined.
          #/just #/make-sink-struct s-trivial #/list)))
    #/expect body (sink-cexpr body)
      (cene-err "Tried to call a struct for which the function implementation was not an expression")
    #/expect (cexpr-has-free-vars? body #/table-empty) #f
      (cene-err "Tried to call a struct for which the function implementation had free variables")
    
    #/sink-call-binary
      (sink-call-binary (cexpr-eval body #/table-empty) func)
      arg)
  #/cene-err "Tried to call a value that wasn't an opaque function or a struct"))

(define/contract (sink-call-list func args)
  (-> sink? (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/list-foldl func args #/fn func arg #/sink-call-binary func arg))

(define/contract (sink-call func . args)
  (->* (sink?) #:rest (listof sink?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-call-list func args))

(define/contract (sink-string-from-located-string located-string)
  (-> sink-located-string? sink-string?)
  (dissect located-string (sink-located-string parts)
  ; TODO: See if this is a painter's algorithm.
  #/sink-string #/list-foldl "" parts #/fn state part
    (dissect part (list start-loc string stop-loc)
      (string-append state string))))

(define/contract (sink-name-for-string string)
  (-> sink-string? sink-name?)
  (dissect string (sink-string racket-string)
  #/sink-name #/unsafe:name
  #/list 'name:string #/string->symbol racket-string))

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
  (sink-name-for-struct-proj qualified-main-tag-name inner-name)
  (-> sink-name? sink-name? sink-name?)
  (dissect qualified-main-tag-name
    (sink-name #/unsafe:name qualified-main-tag-name)
  #/sink-name-rep-map inner-name #/fn n
    (list 'name:struct-proj qualified-main-tag-name n)))

(define/contract (sink-cexpr-sequence-output-stream-spend! stream)
  (-> sink-cexpr-sequence-output-stream?
    (list/c
      any/c
      (-> any/c sink-cexpr? (-> any/c sink-effects?) sink-effects?)))
  (dissect stream (sink-cexpr-sequence-output-stream b)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just state-and-handler)
    (cene-err "Tried to spend an expression output stream that was already spent")
  #/begin
    (set-box! b (nothing))
    state-and-handler))

(define/contract (sink-effects-cexpr-write output-stream cexpr then)
  (->
    sink-cexpr-sequence-output-stream? sink-cexpr?
    (-> sink-cexpr-sequence-output-stream? sink-effects?)
    sink-effects?)
  (make-sink-effects #/fn
  #/dissect (sink-cexpr-sequence-output-stream-spend! output-stream)
    (list state on-cexpr)
  #/sink-effects-run! #/on-cexpr state cexpr #/fn state
  #/then #/sink-cexpr-sequence-output-stream #/box #/just #/list
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
  (make-sink-effects #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
  #/if (eof-object? #/peek-byte in)
    (begin (close-input-port in)
    #/sink-effects-run! on-eof)
  #/sink-effects-run!
  #/else #/sink-text-input-stream #/box #/just in))

(define/contract
  (sink-effects-peek-whether-eof text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (make-sink-effects #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
  #/sink-effects-run! #/then (sink-text-input-stream #/box #/just in)
    (eof-object? #/peek-byte in)))

(define/contract
  (sink-effects-read-regexp text-input-stream pattern then)
  (->
    sink-text-input-stream?
    regexp?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (make-sink-effects #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
  #/let-values
    (
      [
        (start-line start-column start-position)
        (port-next-location in)])
  #/expect (regexp-try-match pattern in) (list bytes)
    (sink-effects-run! #/then (sink-text-input-stream #/box #/just in)
      (nothing))
  #/let-values
    (
      [
        (stop-line stop-column stop-position)
        (port-next-location in)])
  #/sink-effects-run! #/then (sink-text-input-stream #/box #/just in)
    (just #/sink-located-string #/list
      (list
        (list start-line start-column start-position)
        (bytes->string/utf-8 bytes)
        (list stop-line stop-column stop-position)))))

(define/contract
  (sink-effects-read-whitespace text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-effects?)
    sink-effects?)
  ; TODO: Support a more Unicode-aware notion of whitespace.
  (sink-effects-read-regexp text-input-stream #px"^[ \t\r\n]*"
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define/contract
  (sink-effects-read-non-line-breaks text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-effects?)
    sink-effects?)
  ; TODO: Support a more Unicode-aware notion of line break.
  (sink-effects-read-regexp text-input-stream #px"^[^\r\n]*"
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define/contract
  (sink-effects-read-maybe-identifier text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  ; TODO: Support a more Unicode-aware notion of identifier. Not only
  ; should this recognize an identifier according to one of the
  ; Unicode algorithms, it should normalize it according to a Unicode
  ; algorithm as well.
  (sink-effects-read-regexp text-input-stream #px"^[-01-9a-zA-Z]+"
    then))

(define/contract
  (sink-effects-read-maybe-op-character text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  ; TODO: Support a more Unicode-aware notion here, maybe the
  ; "pattern" symbols described in the Unicode identifier rules.
  (sink-effects-read-regexp text-input-stream
    #px"^[^-01-9a-zA-Z \t\r\n\\[\\]()\\\\.:]"
    then))

(define/contract
  (sink-effects-read-maybe-given-racket text-input-stream str then)
  (->
    sink-text-input-stream?
    string?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  ; TODO: See if we should compile more of these ahead of time rather
  ; than generating regexp code at run time like this.
  (sink-effects-read-regexp text-input-stream
    (pregexp #/string-append "^" #/regexp-quote str)
    then))

(define/contract
  (sink-effects-peek-whether-given-racket text-input-stream str then)
  (->
    sink-text-input-stream?
    string?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  ; TODO: See if we should compile more of these ahead of time rather
  ; than generating regexp code at run time like this.
  (sink-effects-read-regexp text-input-stream
    (pregexp #/string-append "^(?=" (regexp-quote str) ")")
  #/fn text-input-stream maybe-located-empty-string
  #/mat maybe-located-empty-string (just located-empty-string)
    (then text-input-stream #t)
    (then text-input-stream #f)))

(define/contract
  (sink-effects-peek-whether-closing-bracket text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (sink-effects-peek-whether-given-racket text-input-stream ")"
  #/fn text-input-stream it-is
  #/if it-is
    (then text-input-stream #t)
  #/sink-effects-peek-whether-given-racket text-input-stream "]"
    then))

(define/contract
  (sink-effects-read-op text-input-stream qualify pre-qualify then)
  (->
    sink-text-input-stream?
    sink?
    (-> sink-name? sink-name?)
    (-> sink-text-input-stream? sink-name? sink-effects?)
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
  
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-maybe-op-character text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (then text-input-stream
    #/sink-call qualify #/pre-qualify #/sink-name-for-string
    #/sink-string-from-located-string identifier)
  
  #/w- then
    (fn text-input-stream op-name
      (sink-effects-read-maybe-given-racket text-input-stream ":"
      #/fn text-input-stream maybe-str
      #/then text-input-stream op-name))
  
  ; TODO: Support the use of ( and [ as delimiters for macro
  ; names.
  #/sink-effects-read-maybe-given-racket text-input-stream "("
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "The use of ( to delimit a macro name is not yet supported")
  #/sink-effects-read-maybe-given-racket text-input-stream "["
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "The use of [ to delimit a macro name is not yet supported")
  
  #/sink-effects-read-maybe-identifier text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (then text-input-stream
    #/sink-call qualify #/pre-qualify #/sink-name-for-string
    #/sink-string-from-located-string identifier)
  
  #/cene-err "Encountered an unrecognized case of the expression operator syntax"))

(define/contract
  (sink-effects-run-op
    op-impl unique-name qualify text-input-stream output-stream then)
  (->
    sink? name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/w- result
    (sink-call op-impl (sink-name unique-name) qualify
      text-input-stream output-stream
    #/sink-fn-curried 4
    #/fn unique-name qualify text-input-stream output-stream
    #/expect unique-name (sink-name unique-name)
      (cene-err "Expected the unique name of a macro's callback results to be a name")
    #/expect (sink-text-input-stream? text-input-stream) #t
      (cene-err "Expected the text input stream of a macro's callback results to be a text input stream")
    #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
      (cene-err "Expected the expression sequence output stream of a macro's callback results to be an expression sequence output stream")
    #/then unique-name qualify text-input-stream output-stream)
  #/expect (sink-effects? result) #t
    (cene-err "Expected the return value of a macro to be an effectful computation")
    result))

(define/contract
  (sink-effects-read-and-run-op
    unique-name qualify text-input-stream output-stream pre-qualify
    then)
  (->
    name?
    sink?
    sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (-> sink-name? sink-name?)
    (->
      name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-op text-input-stream qualify pre-qualify
  #/fn text-input-stream op-name
  #/sink-effects-get op-name #/fn op-impl
  #/sink-effects-run-op
    op-impl unique-name qualify text-input-stream output-stream
    then))

(define/contract
  (sink-effects-read-and-run-freestanding-cexpr-op
    unique-name qualify text-input-stream output-stream then)
  (->
    name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-and-run-op
    unique-name qualify text-input-stream output-stream
    sink-name-for-freestanding-cexpr-op
    then))

(define/contract
  (sink-effects-read-and-run-bounded-cexpr-op
    unique-name qualify text-input-stream output-stream then)
  (->
    name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-and-run-op
    unique-name qualify text-input-stream output-stream
    sink-name-for-bounded-cexpr-op
    then))

(define/contract
  (sink-effects-run-nameless-op
    unique-name qualify text-input-stream output-stream then)
  (->
    name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-get
    (sink-call qualify #/sink-name-for-nameless-bounded-cexpr-op)
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

; TODO: For every cexpr read this way, wrap that cexpr in a located
; cexpr.
(define/contract
  (sink-effects-read-cexprs
    unique-name qualify text-input-stream output-stream then)
  (->
    name? sink? sink-text-input-stream?
    sink-cexpr-sequence-output-stream?
    (->
      name? sink? sink-text-input-stream?
      sink-cexpr-sequence-output-stream?
      sink-effects?)
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
  
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-whitespace text-input-stream
  #/fn text-input-stream whitespace
  #/sink-effects-peek-whether-eof text-input-stream
  #/fn text-input-stream is-eof
  #/if is-eof
    (then unique-name qualify text-input-stream output-stream)
  
  #/sink-effects-read-maybe-given-racket text-input-stream ")"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "Encountered an unmatched )")
  #/sink-effects-read-maybe-given-racket text-input-stream "]"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (cene-err "Encountered an unmatched ]")
  
  #/sink-effects-read-maybe-given-racket text-input-stream "\\"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-and-run-freestanding-cexpr-op
      unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "("
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-read-maybe-given-racket text-input-stream ")"
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          (cene-err "Encountered a syntax that began with ( or (. and did not end with )")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "["
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-read-maybe-given-racket text-input-stream "]"
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          (cene-err "Encountered a syntax that began with [ or [. and did not end with ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "/"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-effects-peek-whether-closing-bracket text-input-stream
        #/fn text-input-stream is-closing-bracket
        #/if (not is-closing-bracket)
          (cene-err "Encountered a syntax that began with /. and did not end at ) or ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream output-stream then)
  
  #/sink-effects-read-maybe-identifier text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (sink-effects-cexpr-write output-stream
      (sink-cexpr-var #/sink-call qualify #/sink-name-for-string
      #/sink-string-from-located-string identifier)
    #/fn output-stream
    #/then unique-name text-input-stream output-stream)
  
  #/cene-err "Encountered an unrecognized case of the expression syntax"))

(struct-easy (id-or-expr-id located-string qualified-name))
(struct-easy (id-or-expr-expr expr))

; This reads identifiers and cexprs until it gets to a closing
; bracket.
(define/contract
  (sink-effects-read-bounded-ids-and-exprs
    unique-name qualify text-input-stream then)
  (->
    name?
    sink?
    sink-text-input-stream?
    (->
      name?
      sink?
      sink-text-input-stream?
      (listof #/or/c id-or-expr-id? id-or-expr-expr?)
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    rev-results (list)
    
    (sink-effects-read-whitespace text-input-stream
    #/fn text-input-stream whitespace
    #/sink-effects-peek-whether-eof text-input-stream
    #/fn text-input-stream is-eof
    #/if is-eof
      (cene-err "Encountered end of file while expecting any number of identifiers and expressions preceding a closing bracket")
    #/sink-effects-peek-whether-closing-bracket text-input-stream
    #/fn text-input-stream is-closing-bracket
    #/if is-closing-bracket
      (then unique-name qualify text-input-stream
        (reverse rev-results))
    #/sink-effects-read-maybe-identifier text-input-stream
    #/fn text-input-stream maybe-id
    #/mat maybe-id (just located-string)
      (w- qualified-name
        (sink-call qualify #/sink-name-for-string
        #/sink-string-from-located-string located-string)
      #/next unique-name qualify text-input-stream
        (cons (id-or-expr-id located-string qualified-name)
          rev-results))
    #/w- output-stream
      (sink-cexpr-sequence-output-stream #/box #/just #/list
        rev-results
        (fn rev-results cexpr then
          (then #/cons (id-or-expr-expr cexpr) rev-results)))
    #/sink-effects-read-cexprs
      unique-name qualify text-input-stream output-stream
    #/fn unique-name qualify text-input-stream output-stream
    #/dissect (sink-cexpr-sequence-output-stream-spend! output-stream)
      (list rev-results on-cexpr)
    #/next unique-name qualify text-input-stream rev-results)))

(define/contract (id-or-expr->cexpr id-or-expr)
  (-> (or/c id-or-expr-id? id-or-expr-expr?) sink-cexpr?)
  (mat id-or-expr (id-or-expr-id located-string qualified-name)
    ; TODO: Wrap this in a located cexpr.
    (sink-cexpr-var qualified-name)
  #/dissect id-or-expr (id-or-expr-expr cexpr)
    cexpr))

; This reads cexprs until it gets to a closing bracket.
(define/contract
  (sink-effects-read-bounded-cexprs
    unique-name qualify text-input-stream then)
  (->
    name?
    sink?
    sink-text-input-stream?
    (-> name? sink? sink-text-input-stream? (listof sink-cexpr?)
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-bounded-ids-and-exprs
    unique-name qualify text-input-stream
  #/fn unique-name qualify text-input-stream ids-and-exprs
  #/then unique-name qualify text-input-stream
  #/list-map ids-and-exprs #/fn id-or-expr
    (id-or-expr->cexpr id-or-expr)))

; This reads cexprs until it gets to a closing bracket, and it
; verifies that there are precisely `n` of them.
(define/contract
  (sink-effects-read-specific-number-of-cexprs
    unique-name qualify text-input-stream n then)
  (->
    name?
    sink?
    sink-text-input-stream?
    natural?
    (-> name? sink? sink-text-input-stream? (listof sink-cexpr?)
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-bounded-cexprs
    unique-name qualify text-input-stream
  #/fn unique-name qualify text-input-stream cexprs
  #/w- actual-n (length cexprs)
  #/if (< n actual-n)
    (cene-err "Encountered too many expressions")
  #/if (< actual-n n)
    (cene-err "Expected another expression")
  #/then unique-name qualify text-input-stream cexprs))

(define/contract (core-sink-struct main-tag-string proj-strings)
  (-> string? (listof string?) (and/c pair? #/listof name?))
  (w- main-tag-name
    (sink-name-qualify #/sink-name-for-struct-main-tag
    #/sink-name-for-string #/sink-string main-tag-string)
  #/list-map
    (cons main-tag-name #/list-map proj-strings #/fn proj-string
      (sink-name-qualify #/sink-name-for-struct-proj main-tag-name
      #/sink-name-for-string #/sink-string proj-string))
  #/dissectfn (sink-name name)
    name))

(define s-trivial (core-sink-struct "trivial" #/list))

(define s-nothing (core-sink-struct "nothing" #/list))
(define s-just (core-sink-struct "just" #/list "val"))

(define s-carried (core-sink-struct "carried" #/list "main" "carry"))

(define s-clamor-err (core-sink-struct "clamor-err" #/list "message"))

(define/contract (sink-effects-claim name)
  (-> name? sink-effects?)
  (sink-effects-put
    (sink-name #/name-claimed name)
    (sink-dex #/dex-give-up)
    (make-sink-struct s-trivial #/list)))

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
  (-> name? sink? sink-text-input-stream? sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-eof text-input-stream
    ; If we're at the end of the file, we're done. We claim the
    ; `unique-name` to be sure no one else is using it.
    (sink-effects-claim unique-name)
  #/fn text-input-stream
  #/sink-effects-claim-and-split unique-name 2
  #/dissectfn (list unique-name-writer unique-name-main)
  #/w- output-stream
    (sink-cexpr-sequence-output-stream #/box #/just #/list
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
        #/w- effects
          (sink-call (cexpr-eval cexpr #/table-empty)
            (sink-name unique-name-first)
            qualify)
        #/expect (sink-effects? effects) #t
          (cene-err "Expected every top-level expression to evaluate to a callable value that takes two arguments and returns side effects")
        #/sink-effects-merge effects
        #/then unique-name-rest)))
  #/sink-effects-read-cexprs
    unique-name-main qualify text-input-stream output-stream
  #/fn unique-name-main qualify text-input-stream output-stream
  #/dissect (sink-cexpr-sequence-output-stream-spend! output-stream)
    (list unique-name-writer on-cexpr)
  #/sink-effects-claim-and-split unique-name-writer 0
  #/dissectfn (list)
  #/sink-effects-read-top-level
    unique-name-main qualify text-input-stream))

(define/contract (cene-run-string rt string)
  (-> cene-runtime? string? #/list/c cene-runtime? #/listof string?)
  
  (begin (assert-cannot-get-cene-definitions!)
  #/dissect rt (cene-runtime defined-dexes defined-values)
  #/cene-process-run rt #/with-gets-from-as-process defined-values
    (fn
      (sink-effects-run! #/sink-effects-read-top-level
        (unsafe:name #/list 'name:unique-name-root)
        (sink-fn-curried 1 #/fn name
          (expect (sink-name? name) #t
            (cene-err "Expected the input to the root qualify function to be a name")
          #/sink-name-qualify name))
        (sink-text-input-stream #/box #/just
        #/open-input-string string)))
    (fn process process)))

; TODO: See if we'll use this.
(define/contract (cene-runtime-empty)
  (-> cene-runtime?)
  (cene-runtime
    (sink-table #/table-empty)
    (sink-table #/table-empty)))
