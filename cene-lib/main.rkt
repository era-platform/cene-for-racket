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
  -> ->* and/c any/c list/c listof not/c or/c parameter/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list list-foldl list-map nat->maybe)
(require #/only-in lathe-comforts/maybe just maybe/c nothing)
(require #/only-in lathe-comforts/struct struct-easy)

(require #/only-in effection/order
  compare-by-dex dex-give-up dex-dex dex-name name? ordering-eq?
  table-get table-shadow)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe name)


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
(struct-easy (sink-effects go!))
(struct-easy (sink-text-input-stream box-of-maybe-input))
(struct-easy (sink-located-string parts))
(struct-easy (sink-string racket-string))
(struct-easy (sink-opaque-fn racket-fn))
(struct-easy (sink-table racket-table))

; NOTE: The term "cexpr" is short for "compiled expression." It's the
; kind of expression that macros generate in order to use as function
; definitions.
(struct-easy (sink-cexpr cexpr))

(define/contract (sink? v)
  (-> any/c boolean?)
  (or
    (sink-struct? v)
    (sink-dex? v)
    (sink-name? v)
    (sink-effects? v)
    (sink-text-input-stream? v)
    (sink-located-string? v)
    (sink-string? v)
    (sink-opaque-fn? v)
    (sink-table? v)
    (sink-cexpr? v)))

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
  (call-with-current-continuation #/fn suspend-k
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
  (-> sink-table? (-> any/c) (-> any/c cene-process?)
    (or/c with-gets-suspended? with-gets-finished?))
  (w- with-gets-result (with-gets-from table body)
  #/mat with-gets-result (with-gets-suspended name then)
    (cene-process-get name #/fn value
    #/with-gets-from-as-process
      table (fn #/then value) body-result-to-process)
  #/dissect with-gets-result (with-gets-finished body-result)
  #/body-result-to-process body-result))

; TODO: Write an entrypoint to the Cene language that uses
; `sink-effects-read-top-level` and this together to run Cene code.
(define/contract (run-cene-process rt process)
  (-> cene-runtime? cene-process?
    (list/c cene-runtime? #/listof string?))
  (dissect rt (cene-runtime defined-dexes defined-values)
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
      (sink-table-put-maybe defined-dexes #/just cene-dex)
      (sink-table-put-maybe defined-values #/just value)
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
      (with-gets-from-as-process defined-values (fn #/then value)
      #/dissectfn (sink-effects process)
        process)
      rev-next-processes)
  #/error "Encountered an unrecognized kind of Cene process"))

(struct-easy (cexpr-var name))

(define/contract (cexpr? v)
  (-> any/c boolean?)
  ; TODO: Add more cexpr constructors.
  (or
    (cexpr-var? v)))

(define/contract (sink-effects-get name then)
  (-> sink-name? (-> sink? sink-effects?) sink-effects?)
  (sink-effects #/fn #/cene-process-get name then))

(define/contract (sink-effects-put name dex value)
  (-> sink-name? sink-dex? sink? sink-effects?)
  (sink-effects #/fn #/cene-process-put name dex value))

(define/contract (sink-effects-claim name)
  (-> name? sink-effects?)
  (sink-effects-put
    (sink-name #/name-claimed name)
    (sink-dex #/dex-give-up)
    (make-sink-struct s-nil #/list)))

(define/contract (sink-effects-noop)
  (-> sink-effects?)
  (sink-effects #/fn #/cene-process-noop))

(define/contract (sink-effects-merge-binary a b)
  (-> sink-effects? sink-effects? sink-effects?)
  (dissect a (sink-effects a-go!)
  #/dissect b (sink-effects b-go!)
  #/sink-effects #/fn #/cene-process-merge (a-go!) (b-go!)))

(define/contract (sink-effects-merge-list effects)
  (-> (listof sink-effects?) sink-effects?)
  (list-foldl (sink-effects-noop) effects #/fn a b
    (sink-effects-merge-binary a b)))

(define/contract (sink-effects-merge . effects)
  (->* () #:rest (listof sink-effects?) sink-effects?)
  (sink-effects-merge-list effects))

(struct exn:fail:cene exn:fail ())

(define/contract (cexpr-has-free-vars? cexpr)
  (-> sink-cexpr? boolean?)
  ; TODO: Refactor this so that it becomes a call to a
  ; `cexpr-free-vars` function followed by a check that the result is
  ; nonempty.
  (dissect cexpr (sink-cexpr cexpr)
  #/mat cexpr (cexpr-var name)
    #t
  #/error "Encountered an unrecognized kind of cexpr"))

(define/contract (cexpr-eval cexpr)
  (-> (and/c sink-cexpr? #/not/c cexpr-has-free-vars?) sink?)
  ; TODO: Implement this. We'll want to compile the cexpr and then
  ; invoke the compiled code.
  'TODO)

(define/contract (sink-cexpr-var name)
  (-> sink-name? sink-cexpr?)
  (dissect name (sink-name name)
  #/sink-cexpr #/cexpr-var name))

(define/contract (sink-call-binary func args)
  (-> sink? sink? sink?)
  (mat func (sink-opaque-fn racket-func)
    (racket-func args)
  #/mat func (sink-struct tags projs)
    (sink-call-binary
      ; TODO: Implement this. We'll want to look up a name derived
      ; from `tags`. This lookup may suspend the current computation
      ; if the definition of the name hasn't been installed yet.
      (cene-definition-get 'TODO)
      func)
  #/raise #/exn:fail:cene
    "Tried to call a value that wasn't an opaque function or a struct"
  #/current-continuation-marks))

(define/contract (sink-call-list func args)
  (-> sink? (listof sink?) sink?)
  (list-foldl func args #/fn func arg #/sink-call-binary func arg))

(define/contract (sink-call func . args)
  (->* (sink?) #:rest (listof sink?) sink?)
  (sink-call-list func args))

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
  (dissect inner-name (sink-name #/unsafe:name n)
  #/sink-name #/unsafe:name #/list 'name:freestanding-cexpr-op n))

(define/contract (sink-name-for-bounded-cexpr-op inner-name)
  (-> sink-name? sink-name?)
  (dissect inner-name (sink-name #/unsafe:name n)
  #/sink-name #/unsafe:name #/list 'name:bounded-cexpr-op n))

(define/contract (sink-name-for-nameless-bounded-cexpr-op)
  (-> sink-name?)
  (sink-name #/unsafe:name #/list 'name:nameless-bounded-cexpr-op))

(define/contract (sink-text-input-stream-spend! text-input-stream)
  (-> sink-text-input-stream? input-port?)
  (dissect text-input-stream (sink-text-input-stream b)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just input-port)
    (raise #/exn:fail:cene
      "Tried to spend a text input stream that was already spent"
    #/current-continuation-marks)
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
  (sink-effects #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
  #/if (eof-object? #/peek-byte in)
    (begin (close-input-port in)
    #/dissect on-eof (sink-effects go!)
    #/go!)
  #/else #/sink-text-input-stream #/box in))

(define/contract
  (sink-effects-read-whether-at-eof text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-effects?)
    sink-effects?)
  (sink-effects #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
  #/then (sink-text-input-stream #/box in)
    (eof-object? #/peek-byte in)))

(define/contract
  (sink-effects-read-or-peek-regexp
    read-or-peek text-input-stream pattern then)
  (->
    (or/c 'read 'peek)
    sink-text-input-stream?
    (or/c regexp? string?)
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects #/fn
  #/w- in (sink-text-input-stream-spend! text-input-stream)
  #/let-values
    (
      [
        (start-line start-column start-position)
        (port-next-location in)])
  #/w- regexp-match-read-or-peek
    (if (eq? 'read read-or-peek)
      regexp-match
      regexp-match-peek)
  #/expect (regexp-match-read-or-peek pattern in) (list bytes)
    (then (sink-text-input-stream #/box in) #/nothing)
  #/let-values
    (
      [
        (stop-line stop-column stop-position)
        (port-next-location in)])
  #/then (sink-text-input-stream #/box in)
    (just #/sink-located-string #/list
      (list
        (list start-line start-column start-position)
        (bytes->string/utf-8 bytes)
        (list stop-line stop-column stop-position)))))

(define/contract
  (sink-effects-read-regexp text-input-stream pattern then)
  (->
    sink-text-input-stream?
    (or/c regexp? string?)
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects-read-or-peek-regexp 'read
    text-input-stream pattern then))

(define/contract
  (sink-effects-peek-regexp text-input-stream pattern then)
  (->
    sink-text-input-stream?
    (or/c regexp? string?)
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  (sink-effects-read-or-peek-regexp 'peek
    text-input-stream pattern then))

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
    (string-append "^" #/regexp-quote str)
    then))

(define/contract
  (sink-effects-peek-maybe-given-racket text-input-stream str then)
  (->
    sink-text-input-stream?
    string?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-effects?)
    sink-effects?)
  ; TODO: See if we should compile more of these ahead of time rather
  ; than generating regexp code at run time like this.
  (sink-effects-peek-regexp text-input-stream
    (string-append "^" #/regexp-quote str)
    then))

(define/contract
  (sink-effects-read-cexprs
    unique-name qualify text-input-stream state on-cexpr then)
  (->
    name?
    sink?
    sink-text-input-stream?
    any/c
    (->
      name?
      sink?
      any/c
      sink-cexpr?
      (-> name? sink? any/c sink-effects?)
      sink-effects?)
    (-> name? sink? sink-text-input-stream? any/c sink-effects?)
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
  
  ; NOTE: Just about every case here invokes `next` in order to read
  ; further expressions from the input.
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
    (fn text-input-stream qualify pre-qualify then
      
      ; NOTE: These are the cases we should handle here.
      ;
      ; _#
      ; _abc_:
      ; _abc_
      ; _(markup)_:
      ; _(markup)_
      ; _[markup]_:
      ; _[markup]_
      
      (sink-effects-read-whitespace text-input-stream
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
        (raise #/exn:fail:cene
          "The use of ( to delimit a macro name is not yet supported"
        #/current-continuation-marks)
      #/sink-effects-read-maybe-given-racket text-input-stream "["
      #/fn text-input-stream maybe-str
      #/mat maybe-str (just _)
        (raise #/exn:fail:cene
          "The use of [ to delimit a macro name is not yet supported"
        #/current-continuation-marks)
      
      #/sink-effects-read-maybe-identifier text-input-stream
      #/fn text-input-stream maybe-identifier
      #/mat maybe-identifier (just identifier)
        (then text-input-stream
        #/sink-call qualify #/pre-qualify #/sink-name-for-string
        #/sink-string-from-located-string identifier)
      
      #/raise #/exn:fail:cene
        "Encountered an unrecognized case of the expression operator syntax"
      #/current-continuation-marks))
  
  #/w- sink-effects-run-op
    (fn op-impl unique-name qualify text-input-stream state then
      (w- result
        ; TODO: Convert `on-cexpr` and the `fn` to sinks somehow. Note
        ; that we won't just use `sink-opaque-fn` here; we'll want to
        ; encapsulate the `state` and `on-cexpr` together into an
        ; "expression output stream" type.
        (sink-call
          op-impl unique-name qualify text-input-stream state on-cexpr
        #/fn unique-name qualify text-input-stream state
        #/then unique-name qualify text-input-stream state)
      #/expect (sink-effects? result) #t
        (raise #/exn:fail:cene
          "Expected the return value of a macro to be an effectful computation"
        #/current-continuation-marks)
        result))
  
  #/w- sink-effects-read-and-run-op
    (fn unique-name qualify text-input-stream state pre-qualify then
      (sink-effects-read-op text-input-stream qualify pre-qualify
      #/fn text-input-stream op-name
      #/sink-effects-get op-name #/fn op-impl
      #/sink-effects-run-op
        op-impl unique-name qualify text-input-stream state then))
  
  #/w- sink-effects-read-and-run-freestanding-cexpr-op
    (fn unique-name qualify text-input-stream state then
      (sink-effects-read-and-run-op
        unique-name qualify text-input-stream state
        sink-name-for-freestanding-cexpr-op
        then))
  
  #/w- sink-effects-read-and-run-bounded-cexpr-op
    (fn unique-name qualify text-input-stream state then
      (sink-effects-read-and-run-op
        unique-name qualify text-input-stream state
        sink-name-for-bounded-cexpr-op
        then))
  
  #/w- sink-effects-run-nameless-op
    (fn unique-name qualify text-input-stream state then
      (sink-effects-get
        (sink-call qualify #/sink-name-for-nameless-bounded-cexpr-op)
      #/fn op-impl
      #/sink-effects-run-op
        op-impl unique-name qualify text-input-stream state then))
  
  #/sink-effects-read-maybe-given-racket text-input-stream "\\"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-effects-read-and-run-freestanding-cexpr-op
      unique-name qualify text-input-stream state next)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "("
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- next
      (fn unique-name qualify text-input-stream state
        (sink-effects-read-maybe-given-racket text-input-stream ")"
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          (raise #/exn:fail:cene
            "Encountered a syntax that began with (. and did not end with )"
          #/current-continuation-marks)
        #/next unique-name qualify text-input-stream state))
    #/sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream state next)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream state next)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "["
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- next
      (fn unique-name qualify text-input-stream state
        (sink-effects-read-maybe-given-racket text-input-stream "]"
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          (raise #/exn:fail:cene
            "Encountered a syntax that began with [. and did not end with ]"
          #/current-continuation-marks)
        #/next unique-name qualify text-input-stream state))
    #/sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream state next)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream state next)
  
  #/sink-effects-read-maybe-given-racket text-input-stream "/"
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- next
      (fn unique-name qualify text-input-stream state
        (sink-effects-peek-maybe-given-racket text-input-stream ")"
        #/fn text-input-stream maybe-str1
        #/sink-effects-peek-maybe-given-racket text-input-stream "]"
        #/fn text-input-stream maybe-str2
        #/mat (list maybe-str1 maybe-str2) (list (nothing) (nothing))
          (raise #/exn:fail:cene
            "Encountered a syntax that began with /. and did not end at ) or ]"
          #/current-continuation-marks)
        #/next unique-name qualify text-input-stream state))
    #/sink-effects-read-maybe-given-racket text-input-stream "."
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream state next)
    #/sink-effects-run-nameless-op
      unique-name qualify text-input-stream state next)
  
  #/sink-effects-read-maybe-identifier text-input-stream
  #/fn text-input-stream maybe-identifier
  #/mat maybe-identifier (just identifier)
    (on-cexpr unique-name qualify state
      (sink-cexpr-var #/sink-name-for-string
      #/sink-string-from-located-string identifier)
    #/fn unique-name qualify state
    #/next unique-name qualify text-input-stream state)
  
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
  (-> name? sink? sink-text-input-stream? sink-effects?)
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
        (sink-call (cexpr-eval cexpr)
          (sink-name unique-name-first)
          qualify)
      #/expect (sink-effects? effects) #t
        (raise #/exn:fail:cene
          "Expected every top-level expression to evaluate to a callable value that takes two arguments and returns side effects"
        #/current-continuation-marks)
      #/sink-effects-merge effects
      #/then unique-name-rest qualify state))
  #/fn unique-name qualify text-input-stream
  #/sink-effects-read-top-level
    unique-name qualify text-input-stream))

