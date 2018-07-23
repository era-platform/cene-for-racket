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


(require #/for-syntax racket/base)

(require #/only-in racket/contract/base
  -> ->* and/c any/c contract? list/c listof not/c or/c parameter/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-any list-foldl list-foldr list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/maybe just maybe/c nothing)
(require #/only-in lathe-comforts/struct struct-easy)

(require #/only-in effection/order
  compare-by-dex dex-give-up dex-dex dex-name dex-struct dex-table
  fuse-by-merge merge-by-dex merge-table name? name-of ordering-eq?
  table-empty table-get table-map-fuse table-shadow)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe name)


; TODO: Document these exports.
(provide #/rename-out [-cene-runtime? cene-runtime?])
(provide cene-run-string)
(provide cene-runtime-essentials)

; TODO: See if we really want to provide these exports at all. If we
; do, document them.
(provide exn:fail:cene)



; TODO: Put this into the Lathe Comforts library or something.
(struct-easy (trivial))


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
    (ordering-eq? #/compare-by-dex (dex-name) main-tag s-main-tag)
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
      #/expect
        (ordering-eq? #/compare-by-dex (dex-dex) dex existing-dex)
        #t
        (next-with-error "Wrote to the same name with inequal dexes")
      #/dissect (sink-table-get-maybe defined-values name)
        (just existing-value)
      #/next-simple #/cons
        ; NOTE: Since Cene dexes can potentially invoke
        ; `cene-definition-get` on a not-yet-defined name, we use this
        ; this `with-gets-...` operation here so that it can properly
        ; suspend the dex comparison computation as a Cene process.
        (with-gets-from-as-process defined-values
          (fn #/compare-by-dex existing-dex value existing-value)
        #/fn comparison
          (expect (ordering-eq? comparison) #t
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

(struct-easy (cexpr-var name))
(struct-easy (cexpr-native result))
(struct-easy (cexpr-struct main-tag-name projs))
(struct-easy (cexpr-call func arg))
(struct-easy (cexpr-opaque-fn param body))
(struct-easy (cexpr-let bindings body))

(define/contract (cexpr? v)
  (-> any/c boolean?)
  
  ; TODO: Add more cexpr constructors. The JavaScript version of Cene
  ; hsa five more than we do:
  ;
  ;   located mat cline-struct merge-struct fuse-struct err
  ;
  (or
    (cexpr-var? v)
    (cexpr-native? v)
    (cexpr-struct? v)
    (cexpr-call? v)
    (cexpr-opaque-fn? v)
    (cexpr-let? v)))

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

(define/contract (cexpr-has-free-vars? cexpr)
  (-> sink-cexpr? boolean?)
  (dissect cexpr (sink-cexpr cexpr)
  #/w-loop recur bound-vars (table-empty) cexpr cexpr
  #/mat cexpr (cexpr-var name)
    (expect (table-get name bound-vars) (just _)
      #t
      #f)
  #/mat cexpr (cexpr-native result)
    #f
  #/mat cexpr (cexpr-struct main-tag-name projs)
    (list-any projs #/dissectfn (list proj-name proj-cexpr)
      (recur bound-vars proj-cexpr))
  #/mat cexpr (cexpr-call func arg)
    (or (recur bound-vars func) (recur bound-vars arg))
  #/mat cexpr (cexpr-opaque-fn param body)
    (recur (table-shadow param (just #/trivial) bound-vars) body)
  #/mat cexpr (cexpr-let bindings body)
    (or
      (list-any bindings #/dissectfn (list var val)
        (recur bound-vars val))
    #/recur
      (list-foldl bound-vars bindings #/fn bound-vars binding
        (dissect binding (list var val)
        #/table-shadow var (just #/trivial) bound-vars))
      body)
  #/error "Encountered an unrecognized kind of cexpr"))

(define/contract (cexpr-eval cexpr)
  (-> (and/c sink-cexpr? #/not/c cexpr-has-free-vars?) sink?)
  (begin (assert-can-get-cene-definitions!)
  #/dissect cexpr (sink-cexpr cexpr)
  #/w-loop recur bound-vars (table-empty) cexpr cexpr
  #/mat cexpr (cexpr-var name)
    (expect (table-get name bound-vars) (just value)
      (error "Encountered a cexpr with free vars after already checking for free vars")
      value)
  #/mat cexpr (cexpr-native result)
    result
  #/mat cexpr (cexpr-struct main-tag-name projs)
    (make-sink-struct
      (cons main-tag-name
      #/list-map projs #/dissectfn (list proj-name proj-cexpr)
        proj-name)
    #/list-map projs #/dissectfn (list proj-name proj-cexpr)
      (recur bound-vars proj-cexpr))
  #/mat cexpr (cexpr-call func arg)
    (sink-call (recur bound-vars func) (recur bound-vars arg))
  #/mat cexpr (cexpr-opaque-fn param body)
    (sink-opaque-fn #/fn arg
      (recur (table-shadow param (just arg) bound-vars) body))
  #/mat cexpr (cexpr-let bindings body)
    (recur
      (list-foldl bound-vars
        (list-map bindings #/dissectfn (list var val)
          (list var #/recur bound-vars val))
      #/fn bound-vars binding
        (dissect binding (list var val)
        #/table-shadow var (just val) bound-vars))
      body)
  #/error "Encountered an unrecognized kind of cexpr"))

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
  (-> (and/c pair? #/listof name?) (listof sink-cexpr?) sink-struct?)
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
    #/sink-call-binary
      (sink-call-binary
        ; TODO: We should probably optimize this lookup, perhaps by
        ; using memoization. If and when we do, we should profile it
        ; to make sure it's a worthwhile optimization.
        (cexpr-eval
        #/cene-definition-get #/sink-name-for-function-implementation
          main-tag
          (list-foldr proj-tags (sink-table #/table-empty)
          #/fn proj-tag rest
            (sink-table-put-maybe rest proj-tag
            ; TODO: See if there's a way we can either stop depending
            ; on `s-trivial` here or move this code after the place
            ; where `s-trivial` is defined.
            #/just #/make-sink-struct s-trivial #/list)))
        func)
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
  ; _#
  ; _abc_:
  ; _abc_
  ; _(markup)_:
  ; _(markup)_
  ; _[markup]_:
  ; _[markup]_
  
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-whitespace text-input-stream
  #/fn text-input-stream whitespace
  
  #/sink-effects-read-maybe-op-character text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (then text-input-stream
    #/sink-call qualify #/pre-qualify #/sink-name-for-string
    #/sink-string-from-located-string identifier)
  
  #/w- then
    (fn text-input-stream op-name
      (sink-effects-read-whitespace text-input-stream
      #/fn text-input-stream whitespace
      #/sink-effects-read-maybe-given-racket text-input-stream ":"
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
      (sink-cexpr-var #/sink-name-for-string
      #/sink-string-from-located-string identifier)
    #/fn output-stream
    #/then unique-name text-input-stream output-stream)
  
  #/cene-err "Encountered an unrecognized case of the expression syntax"))

; This reads cexprs until it reads precisely `n` of them (raising an
; error if it gets more, and raising a different error if it notices a
; closing bracket in between cexpr sequences before it's finished),
; reads whitespace, verifies the next character is a closing bracket,
; and then proceeds by calling `then` with updated values and the list
; of cexprs read this way.
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
  (struct-easy (local-state n-left rev-results))
  (w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    state (local-state n #/list)
    
    (dissect state (local-state n-left rev-results)
    #/sink-effects-read-whitespace text-input-stream
    #/fn text-input-stream whitespace
    #/sink-effects-peek-whether-eof text-input-stream
    #/fn text-input-stream is-eof
    #/if is-eof
      (cene-err "Encountered end of file while expecting a speific number of expressions preceding a closing bracket")
    #/sink-effects-peek-whether-closing-bracket text-input-stream
    #/fn text-input-stream is-closing-bracket
    #/if is-closing-bracket
      (expect n-left 0
        (cene-err "Expected another expression")
      #/then unique-name qualify text-input-stream
        (reverse rev-results))
    #/w- output-stream
      (sink-cexpr-sequence-output-stream #/box #/just #/list state
        (fn state cexpr then
          (dissect state (local-state n-left rev-results)
          #/expect (nat->maybe n-left) (just n-left)
            (cene-err "Encountered too many expressions")
          #/then #/local-state n-left #/cons cexpr rev-results)))
    #/sink-effects-read-cexprs
      unique-name qualify text-input-stream output-stream
    #/fn unique-name qualify text-input-stream output-stream
    #/dissect (sink-cexpr-sequence-output-stream-spend! output-stream)
      (list state on-cexpr)
    #/next unique-name qualify text-input-stream state)))

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
        #/expect (cexpr-has-free-vars? cexpr) #f
          (cene-err "Encountered a top-level expression with at least one free variable")
        #/w- effects
          (sink-call (cexpr-eval cexpr)
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
  
  (define/contract (def-func! main-tag-string n-args racket-func)
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
  (def-func! "follow-heart" 1 #/fn clamor
    (raise-cene-err (current-continuation-marks) clamor))
  
  (def-data-struct! "clamor-err" #/list "message")
  
  ; TODO: Implement the macro `err`.
  
  
  ; Order
  
  ; TODO: Implement this.
  (def-nullary-func! "dex-cline" (sink-dex 'TODO))
  
  (def-func! "cline-by-dex" 1 #/fn dex
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
  
  ; TODO: Consider implementing the following.
  ;
  ;   cexpr-cline-struct
  ;   cline-struct
  ;   cexpr-merge-struct
  ;   merge-struct
  ;   cexpr-fuse-struct
  ;   fuse-struct
  ;   cexpr-construct
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
    (w-loop next
      unique-name unique-name
      qualify qualify
      text-input-stream text-input-stream
      rev-params (list)
      
      (sink-effects-read-whitespace text-input-stream
      #/fn text-input-stream whitespace
      #/sink-effects-read-maybe-identifier text-input-stream
      #/fn text-input-stream maybe-param
      #/expect maybe-param (just param)
        (sink-effects-read-whitespace text-input-stream
        #/fn text-input-stream whitespace
        #/sink-effects-peek-whether-eof text-input-stream
        #/fn text-input-stream is-eof
        #/if is-eof
          (cene-err "Encountered end of file in a fn form")
        #/sink-effects-peek-whether-closing-bracket text-input-stream
        #/fn text-input-stream is-closing-bracket
        #/w- finish
          (fn unique-name qualify text-input-stream rev-params body
            (then unique-name qualify text-input-stream
              (list-foldl body rev-params #/fn body param-entry
                (dissect param-entry
                  (list param-located-string param-qualified-name)
                #/sink-cexpr-opaque-fn param-qualified-name body))))
        #/if is-closing-bracket
          (expect rev-params (cons body-entry rev-params)
            (cene-err "Expected a fn form to have a body expression")
          #/dissect body-entry
            (list body-located-string body-qualified-name)
          #/finish unique-name qualify text-input-stream rev-params
          ; TODO: Wrap this in a located cexpr.
          #/sink-cexpr-var body-qualified-name)
          (sink-effects-read-specific-number-of-cexprs
            unique-name qualify text-input-stream 1
          #/fn unique-name qualify text-input-stream bodies
          #/dissect bodies (list body)
          #/finish
            unique-name qualify text-input-stream rev-params body))
      #/next unique-name qualify text-input-stream
        (cons
          (list param
            (sink-call qualify #/sink-name-for-string
            #/sink-string-from-located-string param))
          rev-params))))
  
  
  ; Tables
  
  (def-func! "dex-table" 1 #/fn dex-val
    ; TODO: Implement this.
    (sink-dex 'TODO))
  
  (def-func! "merge-table" 1 #/fn merge-val
    ; TODO: Implement this.
    'TODO)
  
  (def-func! "fuse-table" 1 #/fn fuse-val
    ; TODO: Implement this.
    'TODO)
  
  (def-nullary-func! "table-empty" (sink-table #/table-empty))
  
  (def-func! "table-shadow" 3 #/fn key maybe-val table
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
  
  (def-func! "table-get" 2 #/fn key table
    (expect (sink-name? key) #t
      (cene-err "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err "Expected table to be a table")
    #/w- result (sink-table-get-maybe table key)
    #/expect result (just result)
      (make-sink-struct s-nothing #/list)
    #/make-sink-struct s-just #/list result))
  
  (def-func! "table-map-fuse" 3 #/fn table fuse key-to-operand
    ; TODO: Implement this.
    'TODO)
  
  (def-func! "table-sort" 2 #/fn cline table
    ; TODO: Implement this.
    'TODO)
  
  
  ; Effects
  
  ; TODO: Consider implementing the following.
  ;
  ;   no-effects
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
  
  (def-func! "int-minus" 2 #/fn minuend subtrahend
    (expect minuend (sink-int minuend)
      (cene-err "Expected minuend to be an int")
    #/expect subtrahend (sink-int subtrahend)
      (cene-err "Expected subtrahend to be an int")
    #/sink-int #/- minuend subtrahend))
  
  (def-func! "int-div-rounded-down" 2 #/fn dividend divisor
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
  
  (def-func! "string-singleton" 1 #/fn unicode-scalar
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
  
  (def-func! "string-append-later" 3 #/fn a b then
    (expect a (sink-string a)
      (cene-err "Expected a to be a string")
    #/expect b (sink-string b)
      (cene-err "Expected b to be a string")
    #/make-sink-effects #/fn
    #/sink-effects-run!
    #/sink-call then #/sink-string #/string-append a b))
  
  ; TODO: Implement the macro `str`.
  
  (def-func! "string-length" 1 #/fn string
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/sink-int #/string-length string))
  
  (def-func! "string-get-unicode-scalar" 2 #/fn string start
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/expect start (sink-int start)
      (cene-err "Expected start to be an int")
    #/expect (<= 0 start) #t
      (cene-err "Expected start to be a nonnegative int")
    #/expect (< start #/string-length string) #t
      (cene-err "Expected start to be an int less than the length of string")
    #/sink-int #/char->integer #/string-ref string start))
  
  (def-func! "string-cut-later" 4 #/fn string start stop then
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
