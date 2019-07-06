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

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> ->* and/c any any/c contract? contract-name list/c listof none/c
  or/c parameter/c rename-contract struct/c)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract make-contract raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/control reset-at shift-at)
(require #/only-in racket/generic define/generic define-generics)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-any list-foldl list-foldr list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/maybe
  just just-value maybe? maybe-bind maybe/c maybe-map nothing
  nothing?)
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
  getfx-is-eq-by-dex ordering-eq)
(require #/only-in effection/order/base
  dex? dexed-first-order/c dex-give-up dex-dex dex-name dex-struct
  dex-table fuse-by-merge getfx-call-fuse getfx-dexed-of getfx-name-of
  getfx-table-map-fuse merge-by-dex merge-table name? ordering-eq?
  table? table-empty? table-empty table-get table-shadow)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe
  dex dexed gen:dex-internals name)

(require #/only-in cene/private/textpat
  textpat? textpat-from-string textpat-lookahead
  textpat-once-or-more textpat-one textpat-one-in-range
  textpat-one-in-string textpat-one-not textpat-one-not-in-string
  textpat-or textpat-star optimized-textpat? optimized-textpat-read!
  optimize-textpat)


(provide #/all-defined-out)



; TODO: We used this in `effection/order/base`, and we're using it
; again here. See if it should be an export of Effection.
(define/contract (getmaybefx-bind effects then)
  (-> (getfx/c maybe?) (-> any/c #/getfx/c maybe?) #/getfx/c maybe?)
  (getfx-bind effects #/fn maybe-intermediate
  #/expect maybe-intermediate (just intermediate)
    (getfx-done #/nothing)
  #/then intermediate))

; TODO: We used this in `effection/order/base`, and we're using it
; again here. See if it should be an export of Effection.
(define/contract (getmaybefx-map effects func)
  (-> (getfx/c maybe?) (-> any/c any/c) #/getfx/c maybe?)
  (getmaybefx-bind effects #/fn intermediate
  #/getfx-done #/just #/func intermediate))

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

; TODO: See if we should export this from somewhere.
(define/contract (monad-list-map fx-done fx-bind list-of-fx)
  (-> (-> any/c any/c) (-> any/c (-> any/c any/c) any/c) list? any/c)
  (w-loop next rest list-of-fx rev-result (list)
    (expect rest (cons fx-first rest)
      (fx-done #/reverse rev-result)
    #/fx-bind fx-first #/fn first
    #/next rest (cons first rev-result))))


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
(struct-easy (sink-getfx cenegetfx-go)
  #:other #:methods gen:sink [])
(struct-easy (sink-extfx cenegetfx-go)
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
(struct-easy (sink-opaque-fn-fusable racket-fn)
  #:other #:methods gen:sink [])
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


; NOTE: We treat every two tag caches as `ordering-eq`. Cene
; programmers don't directly manipulate these values, and they're
; derived from the other fields of the `cene-root-info` that carries
; them, so we simply treat them as `ordering-eq` withouot bothering to
; compare them. We do this by defining a `dex-non-cene-value` dex to
; use for the appropriate field of `(dex-struct cene-root-info ...)`.

(struct-easy (dex-internals-non-cene-value)
  
  #:other
  
  #:methods unsafe:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-non-cene-value)
    
    (define (dex-internals-autoname this)
      'tag:dex-non-cene-value)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (getfx-dex-internals-is-in this x)
      (getfx-done #t))
    
    (define (getfx-dex-internals-name-of this x)
      (getfx-done #/just #/unsafe:name #/list 'name:non-cene-value))
    
    (define (getfx-dex-internals-dexed-of this x)
      (w- this (unsafe:dex this)
      #/getmaybefx-map (getfx-name-of this x) #/fn name
        (unsafe:dexed this name x)))
    
    (define (getfx-dex-internals-compare this a b)
      (getfx-done #/just #/ordering-eq))
  ])

(define/contract (dex-non-cene-value)
  (-> dex?)
  (unsafe:dex #/dex-internals-non-cene-value))


(struct-easy (cene-root-info dspace lang-impl-qualify-root tag-cache))

(define/contract (cene-root-info/c)
  (-> contract?)
  (rename-contract
    (struct/c cene-root-info dspace? sink-authorized-name? any/c)
    '(cene-root-info/c)))

(define/contract (dex-cene-root-info)
  (-> dex?)
  (dex-struct cene-root-info
    (dex-dspace)
    (dex-struct sink-authorized-name #/dex-authorized-name)
    (dex-non-cene-value)))

(struct-easy (cenegetfx getfx-run))

(define/contract (cenegetfx-run-getfx getfx)
  (-> getfx? cenegetfx?)
  (cenegetfx #/fn rinfo getfx))

(define/contract (getfx-run-cenegetfx rinfo effects)
  (-> (cene-root-info/c) cenegetfx? getfx?)
  (dissect effects (cenegetfx effects)
  #/effects rinfo))

(define/contract (cenegetfx-done result)
  (-> any/c cenegetfx?)
  (cenegetfx-run-getfx #/getfx-done result))

(define/contract (cenegetfx-bind effects then)
  (-> cenegetfx? (-> any/c cenegetfx?) cenegetfx?)
  (cenegetfx #/fn rinfo
    (getfx-bind (getfx-run-cenegetfx rinfo effects) #/fn intermediate
    #/getfx-run-cenegetfx rinfo #/then intermediate)))

(define/contract (cenegetfx-map effects func)
  (-> cenegetfx? (-> any/c any/c) cenegetfx?)
  (cenegetfx-bind effects #/fn intermediate
  #/cenegetfx-done #/func intermediate))

(define (cenegetfx/c result/c)
  (w- result/c (coerce-contract 'cenegetfx/c result/c)
  #/make-contract
    
    #:name `(cenegetfx/c ,(contract-name result/c))
    
    #:first-order (fn v #/cenegetfx? v)
    
    #:late-neg-projection
    (fn blame
      (w- result/c-late-neg-projection
        ( (get/build-late-neg-projection result/c)
          (blame-add-context blame "the anticipated value of"))
      #/fn v missing-party
        (expect (cenegetfx? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a cenegetfx effectful computation" given: "~e")
            v)
        #/cenegetfx-map v #/fn result
          (result/c-late-neg-projection result missing-party))))))

(define/contract (cenegetfx-later then)
  (-> (-> cenegetfx?) cenegetfx?)
  (cenegetfx-bind (cenegetfx-done #/trivial) #/dissectfn (trivial)
  #/then))

(define/contract (cenegetfx-join cenegetfx-cenegetfx)
  (-> (cenegetfx/c cenegetfx?) cenegetfx?)
  (cenegetfx-bind cenegetfx-cenegetfx #/fn effects effects))

(define/contract (cenegetfx-list-map list-of-cenegetfx)
  (-> (listof cenegetfx?) #/cenegetfx/c list?)
  (monad-list-map
    (fn result #/cenegetfx-done result)
    (fn effects then #/cenegetfx-bind effects then)
    list-of-cenegetfx))

(define/contract (cenegetfx-list-foldl state elems on-elem)
  (-> any/c list? (-> any/c any/c cenegetfx?) cenegetfx?)
  (w-loop next state state elems elems
    (expect elems (cons elem elems)
      (cenegetfx-done state)
    #/cenegetfx-bind (on-elem state elem) #/fn state
    #/next state elems)))

(define/contract (cenegetfx-read-root-info)
  (-> #/cenegetfx/c #/cene-root-info/c)
  (cenegetfx #/fn rinfo #/getfx-done rinfo))

(define/contract (cenegetfx-read-definition-space)
  (-> #/cenegetfx/c dspace?)
  (cenegetfx-bind (cenegetfx-read-root-info)
  #/dissectfn (cene-root-info ds lang-impl-qualify-root tag-cache)
  #/cenegetfx-done ds))


(define/contract cene-definition-get-param
  (parameter/c
    (maybe/c #/list/c (cene-root-info/c) (maybe/c #/-> getfx? any/c)))
  (make-parameter #/nothing))

(define/contract (assert-can-get-cene-definition-globals!)
  (-> void?)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected implementations of `cene-definition-dspace` and `cene-definition-lang-impl-qualify-root` to be available in the dynamic scope")
  #/void))

(define/contract (assert-cannot-get-cene-definition-globals!)
  (-> void?)
  ; TODO: We're currently being sloppy about this, but let's get to
  ; the point where we're more careful again.
  (void)
#;
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

; TODO: For now, this always succeeds. In the past, this has been
; equivalent to `assert-can-get-cene-definitions!`, but this sometimes
; needs to succeed when that doesn't. See what this should do in the
; long run.
(define/contract (assert-can-mutate!)
  (-> void?)
  (void))

(define/contract (cene-definition-root-info)
  (-> #/cene-root-info/c)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected every call to `cene-definition-oot-info` to occur with an implementation in the dynamic scope")
    rinfo))

(define/contract (cene-definition-dexed-root-info)
  (-> #/dexed-first-order/c #/cene-root-info/c)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected every call to `cene-definition-dexed-root-info` to occur with an implementation in the dynamic scope")
  #/just-value #/pure-run-getfx #/getfx-dexed-of (dex-cene-root-info)
    rinfo))

(define/contract (cene-definition-dspace)
  (-> dspace?)
  (expect (cene-definition-get-param)
    (just #/list
      (cene-root-info ds lang-impl-qualify-root tag-cache)
      maybe-run-getfx)
    (error "Expected every call to `cene-definition-dspace` to occur with an implementation in the dynamic scope")
    ds))

(define/contract (cene-definition-lang-impl-qualify-root)
  (-> sink-authorized-name?)
  (expect (cene-definition-get-param)
    (just #/list
      (cene-root-info ds lang-impl-qualify-root tag-cache)
      maybe-run-getfx)
    (error "Expected every call to `cene-definition-lang-impl-qualify-root` to occur with an implementation in the dynamic scope")
    lang-impl-qualify-root))

(define/contract (sink-getfx-get name)
  (-> sink-name? sink-getfx?)
  (dissect name (sink-name name)
  #/sink-getfx
    (cenegetfx-bind (cenegetfx-read-definition-space) #/fn ds
    #/cenegetfx-run-getfx #/getfx-get ds name
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

(define cene-definition-get-prompt-tag
  (make-continuation-prompt-tag 'cene-definition-get-prompt-tag))

(define/contract (with-gets-from rinfo body)
  (-> (cene-root-info/c) (-> any) any)
  (begin (assert-cannot-get-cene-definition-globals!)
  #/parameterize
    ([cene-definition-get-param (just #/list rinfo #/nothing)])
    (body)))

(define/contract (getfx-bind-restoring effects then)
  (-> getfx? (-> any/c getfx?) getfx?)
  (begin (assert-can-get-cene-definitions!)
  #/w- rinfo (cene-definition-root-info)
  #/getfx-bind effects #/fn intermediate
  #/with-gets-from rinfo #/fn
  #/getfx-with-run-getfx #/fn
  #/then intermediate))

(define/contract (getfx-with-run-getfx body)
  (-> (-> getfx?) getfx?)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected every call to `getfx-with-run-getfx` to occur with an implementation in the dynamic scope")
  #/parameterize
    (
      [
        cene-definition-get-param
        (just #/list rinfo #/just #/fn effects
          (shift-at cene-definition-get-prompt-tag k
          #/getfx-bind-restoring effects #/fn value
          #/k value))])
  #/reset-at cene-definition-get-prompt-tag
    (body)))

(define/contract (getfx-map-restoring effects then)
  (-> getfx? (-> any/c any/c) getfx?)
  (getfx-bind-restoring effects #/fn intermediate
  #/getfx-done #/then intermediate))

(define/contract (extfx-run-getfx-restoring effects then)
  (-> getfx? (-> any/c extfx?) extfx?)
  (begin (assert-can-get-cene-definitions!)
  #/w- rinfo (cene-definition-root-info)
  #/extfx-run-getfx effects #/fn intermediate
  #/with-gets-from rinfo #/fn
  #/extfx-with-run-getfx #/fn
  #/then intermediate))

(define/contract (extfx-with-run-getfx body)
  (-> (-> extfx?) extfx?)
  (expect (cene-definition-get-param)
    (just #/list rinfo maybe-run-getfx)
    (error "Expected every call to `extfx-with-run-getfx` to occur with an implementation in the dynamic scope")
  #/parameterize
    (
      [
        cene-definition-get-param
        (just #/list rinfo #/just #/fn effects
          (shift-at cene-definition-get-prompt-tag k
          #/extfx-run-getfx-restoring effects #/fn value
          #/k value))])
  #/reset-at cene-definition-get-prompt-tag
    (body)))

(define/contract (cenegetfx-run-getfx-with-run-getfx body)
  (-> (-> getfx?) cenegetfx?)
  (cenegetfx-bind (cenegetfx-read-root-info) #/fn rinfo
  #/cenegetfx-run-getfx
    (with-gets-from rinfo #/fn
    #/getfx-with-run-getfx body)))

(define/contract (cenegetfx-with-run-getfx body)
  (-> (-> cenegetfx?) cenegetfx?)
  (cenegetfx-join
    (cenegetfx-run-getfx-with-run-getfx #/fn
      (getfx-done #/body))))

(define/contract (ceneextfx-with-run-getfx body)
  (-> (-> #/cenegetfx/c extfx?) #/cenegetfx/c extfx?)
  (cenegetfx-with-run-getfx #/fn
  #/body))

(define/contract (sink-getfx-with-run-getfx body)
  (-> (-> sink-getfx?) sink-getfx?)
  (sink-getfx
    (cenegetfx-with-run-getfx #/fn
    #/cenegetfx-run-sink-getfx #/body)))

(define/contract (sink-extfx-with-run-getfx body)
  (-> (-> sink-extfx?) sink-extfx?)
  (make-sink-extfx
    (ceneextfx-with-run-getfx #/fn
    #/ceneextfx-run-sink-extfx #/body)))

(define/contract (getfx-ambient-run-cenegetfx effects)
  (-> cenegetfx? getfx?)
  (begin (assert-can-get-cene-definition-globals!)
  #/getfx-run-cenegetfx (cene-definition-root-info) effects))

(define/contract (sink-getfx-run-cenegetfx effects)
  (-> cenegetfx? sink-getfx?)
  (sink-getfx effects))

(define/contract (cenegetfx-run-sink-getfx effects)
  (-> sink-getfx? cenegetfx?)
  (dissect effects (sink-getfx cenegetfx-go)
    cenegetfx-go))

(define/contract (ceneextfx-run-sink-extfx effects)
  (-> sink-extfx? #/cenegetfx/c extfx?)
  (dissect effects (sink-extfx cenegetfx-go)
    cenegetfx-go))

(define/contract (sink-extfx-run-cenegetfx effects then)
  (-> cenegetfx? (-> any/c sink-extfx?) sink-extfx?)
  (make-sink-extfx
    (cenegetfx-bind effects #/fn intermediate
    #/ceneextfx-run-sink-extfx #/then intermediate)))

(define/contract (extfx-run-sink-extfx effects)
  (-> sink-extfx? extfx?)
  (begin (assert-can-get-cene-definition-globals!)
  #/extfx-run-getfx
    (getfx-ambient-run-cenegetfx #/ceneextfx-run-sink-extfx effects)
  #/fn extfx
    extfx))

(define/contract (sink-getfx-done result)
  (-> sink? sink-getfx?)
  (sink-getfx-run-cenegetfx #/cenegetfx-done result))

(define/contract (sink-getfx-bind effects then)
  (-> sink-getfx? (-> any/c sink-getfx?) sink-getfx?)
  (sink-getfx-run-cenegetfx
    (cenegetfx-bind (cenegetfx-run-sink-getfx effects)
    #/fn intermediate
    #/cenegetfx-run-sink-getfx #/then intermediate)))


(define-generics cexpr
  (cexpr-has-free-vars? cexpr env)
  (cenegetfx-cexpr-eval-in-env fault cexpr env))

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
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-var name)
        (error "Expected this to be a cexpr-var")
      #/expect (table-get name env) (just value)
        (error "Tried to eval a cexpr that had a free variable")
      #/cenegetfx-done value))
  ])

(struct-easy (cexpr-reified result)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-reified result)
        (error "Expected this to be a cexpr-reified")
        #f))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-reified result)
        (error "Expected this to be a cexpr-reified")
      #/cenegetfx-done result))
  ])

(struct-easy (cexpr-construct main-tag-name projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-construct main-tag-name projs)
        (error "Expected this to be a cexpr-construct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-construct main-tag-name projs)
        (error "Expected this to be a cexpr-construct")
      #/cenegetfx-bind
        (cenegetfx-list-map #/list-map projs
        #/dissectfn (list proj-name proj-cexpr)
          (-eval-in-env fault proj-cexpr env))
      #/fn vals
      #/cenegetfx-done #/make-sink-struct
        (cons main-tag-name
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
        vals))
  ])

(struct-easy (cexpr-call-fault fault-arg func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/or
        (-has-free-vars? fault-arg env)
        (-has-free-vars? func env)
        (-has-free-vars? arg env)))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-call-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/cenegetfx-bind (-eval-in-env fault fault-arg env)
      #/fn fault-arg
      #/cenegetfx-bind (-eval-in-env fault func env) #/fn func
      #/cenegetfx-bind (-eval-in-env fault arg env) #/fn arg
      #/expect (sink-fault? fault-arg) #t
        (cenegetfx-cene-err fault "Expected the blame argument to be a blame value")
      #/cenegetfx-sink-call-fault fault fault-arg func arg))
  ])

(struct-easy (cexpr-call func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call func arg)
        (error "Expected this to be a cexpr-call")
      #/or (-has-free-vars? func env) (-has-free-vars? arg env)))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-call func arg)
        (error "Expected this to be a cexpr-call")
      #/cenegetfx-bind (-eval-in-env fault func env) #/fn func
      #/cenegetfx-bind (-eval-in-env fault arg env) #/fn arg
      #/cenegetfx-sink-call fault func arg))
  ])

(struct-easy (cexpr-opaque-fn-fault fault-param param body)
  (#:guard-easy
    (when (names-have-duplicate? #/list fault-param param)
      (error "Expected fault-param and param to be mutually unique")))
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-opaque-fn-fault fault-param param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/-has-free-vars? body
      #/table-shadow fault-param (just #/trivial)
      #/table-shadow param (just #/trivial)
        env))
    
    (define (cenegetfx-cexpr-eval-in-env caller-fault this env)
      (expect this (cexpr-opaque-fn-fault fault-param param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/cenegetfx-done
        (sink-opaque-fn-fault #/dissectfn (list explicit-fault arg)
          (-eval-in-env caller-fault body
            (table-shadow fault-param (just explicit-fault)
            #/table-shadow param (just arg)
              env)))))
  ])

(struct-easy (cexpr-opaque-fn param body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-opaque-fn param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/-has-free-vars? body
      #/table-shadow param (just #/trivial) env))
    
    (define (cenegetfx-cexpr-eval-in-env caller-fault this env)
      (expect this (cexpr-opaque-fn param body)
        (error "Expected this to be a cexpr-opaque-fn")
      #/cenegetfx-done #/sink-opaque-fn #/fn arg
        (-eval-in-env caller-fault body
          (table-shadow param (just arg) env))))
  ])

(struct-easy (cexpr-let bindings body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
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
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-let bindings body)
        (error "Expected this to be a cexpr-let")
      #/cenegetfx-bind
        (cenegetfx-list-map #/list-map bindings
        #/dissectfn (list var val)
          (cenegetfx-bind (-eval-in-env fault val env) #/fn val
          #/cenegetfx-done #/list var val))
      #/fn bindings
      #/-eval-in-env fault body
        (list-foldl env bindings #/fn env binding
          (dissect binding (list var val)
          #/table-shadow var (just val) env))))
  ])

(struct-easy (cexpr-located location-definition-name body)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-located location-definition-name body)
        (error "Expected this to be a cexpr-located")
      #/-has-free-vars? body))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
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
; of error where the result of `cenegetfx-go` is mistakenly a
; `sink-extfx?` instead of an `extfx?`.
(define/contract (make-sink-extfx cenegetfx-go)
  (-> (cenegetfx/c extfx?) sink-extfx?)
  (sink-extfx cenegetfx-go))

(define/contract (sink-extfx-run-sink-getfx effects then)
  (-> sink-getfx? (-> any/c sink-extfx?) sink-extfx?)
  (sink-extfx
    (cenegetfx-bind (cenegetfx-run-sink-getfx effects)
    #/fn intermediate
    #/ceneextfx-run-sink-extfx #/then intermediate)))

(define/contract (sink-extfx-put name dex value)
  (-> sink-authorized-name? sink-dex? sink? sink-extfx?)
  (dissect name (sink-authorized-name name)
  #/dissect dex (sink-dex dex)
  #/make-sink-extfx
    (cenegetfx-bind (cenegetfx-read-definition-space) #/fn ds
    #/cenegetfx-done #/extfx-put ds name
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
  (make-sink-extfx #/cenegetfx-done #/extfx-noop))

(define/contract (sink-extfx-fuse-binary a b)
  (-> sink-extfx? sink-extfx? sink-extfx?)
  (make-sink-extfx
    (cenegetfx-bind (ceneextfx-run-sink-extfx a) #/fn a-go
    #/cenegetfx-bind (ceneextfx-run-sink-extfx b) #/fn b-go
    #/cenegetfx-done #/extfx-fuse a-go b-go)))

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
  (make-sink-extfx
    (cenegetfx-later #/fn #/ceneextfx-run-sink-extfx #/then)))

(define/contract (sink-extfx-pub-write p unique-name arg)
  (-> sink-pub? sink-authorized-name? sink? sink-extfx?)
  (dissect p (sink-pub p)
  #/dissect unique-name (sink-authorized-name unique-name)
  #/make-sink-extfx
    (cenegetfx-bind (cenegetfx-read-definition-space) #/fn ds
    #/cenegetfx-done #/extfx-pub-write ds p unique-name
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
  #/make-sink-extfx
    (cenegetfx-bind (cenegetfx-read-root-info) #/fn rinfo
    #/cenegetfx-bind (cenegetfx-read-definition-space) #/fn ds
    #/cenegetfx-run-getfx #/getfx-done
      (extfx-sub-write ds s unique-name
        #;on-conflict
        (success-or-error-definer
          (error-definer-uninformative)
          (extfx-noop))
        (fn arg
          (with-gets-from rinfo #/fn
          #/extfx-with-run-getfx #/fn
          #/extfx-run-sink-extfx #/func arg))))))

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
  (begin (assert-can-get-cene-definition-globals!)
  #/dissect proj-tag-names (sink-table proj-tag-names)
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
      (cenegetfx-done #/next n-args-after-next #/cons arg rev-args))))

(define/contract (sink-fn-curried n-args racket-func)
  (-> exact-positive-integer? procedure? sink-opaque-fn?)
  (dissect (nat->maybe n-args) (just n-args-after-next)
  #/w-loop next n-args-after-next n-args-after-next rev-args (list)
    (expect (nat->maybe n-args-after-next) (just n-args-after-next)
      (sink-opaque-fn #/fn arg
        (apply racket-func #/reverse #/cons arg rev-args))
    #/sink-opaque-fn #/fn arg
      (cenegetfx-done #/next n-args-after-next #/cons arg rev-args))))

; TODO: See if we have to use this as often as we do. Maybe some of
; those places should be abstracted over a fault value instead.
(define/contract (make-fault-internal)
  (-> sink-fault?)
  (sink-fault #/just #/current-continuation-marks))

(define/contract (cenegetfx-err-from-clamor clamor)
  (-> sink? cenegetfx?)
  (cenegetfx-run-getfx-with-run-getfx #/fn
  #/dissect
    (expect (unmake-sink-struct-maybe (s-clamor-err) clamor)
      (just #/list (sink-fault maybe-marks) (sink-string message))
      (list (nothing) (format "~s" clamor))
      (list maybe-marks message))
    (list maybe-marks message)
  #/w- marks
    (mat maybe-marks (just marks) marks
    #/current-continuation-marks)
  #/getfx-err #/error-definer-from-exn #/exn:fail message marks))

(define/contract (cenegetfx-cene-err fault message)
  (-> sink-fault? immutable-string? #/cenegetfx/c none/c)
  ; TODO: See if there's a way we can either stop depending on
  ; `s-clamor-err` here or move this code after the place where
  ; `s-clamor-err` is defined.
  (cenegetfx-with-run-getfx #/fn
  #/cenegetfx-err-from-clamor #/make-sink-struct (s-clamor-err) #/list
    fault
    (sink-string message)))

(define/contract (sink-getfx-cene-err fault message)
  (-> sink-fault? immutable-string? sink-getfx?)
  (sink-getfx-run-cenegetfx #/cenegetfx-cene-err fault message))

(define/contract (sink-extfx-cene-err fault message)
  (-> sink-fault? immutable-string? sink-extfx?)
  (sink-extfx-run-cenegetfx (cenegetfx-cene-err fault message)
  #/fn result
    (error "Internal error: Expected no return value from cenegetfx-cene-err")))

(define-simple-macro (cene-err fault:expr message:string)
  (begin (assert-can-get-cene-definitions!)
  #/cene-run-getfx #/getfx-ambient-run-cenegetfx
    (cenegetfx-cene-err fault message)))

(define/contract
  (cenegetfx-sink-call-fault-binary
    caller-fault explicit-fault func arg)
  (-> sink-fault? sink-fault? sink? sink? #/cenegetfx/c sink?)
  (mat func (sink-opaque-fn racket-func)
    (racket-func arg)
  #/mat func (sink-opaque-fn-fault racket-func)
    (racket-func #/list explicit-fault arg)
  #/mat func (sink-opaque-fn-fusable racket-func)
    (cenegetfx-bind (cenegetfx-read-root-info) #/fn rinfo
    #/cenegetfx-run-getfx
      (racket-func #/list explicit-fault rinfo arg))
  #/mat func (sink-struct tags projs)
    (dissect (list-map tags #/fn tag #/sink-name tag)
      (cons main-tag proj-tags)
    
    ; TODO: This lookup might be expensive. See if we should memoize
    ; it.
    #/cenegetfx-bind
      (cenegetfx-with-run-getfx #/fn
      #/cenegetfx-run-sink-getfx
        (sink-getfx-get #/sink-name-for-function-implementation-value
          main-tag
          (list-foldr proj-tags (sink-table #/table-empty)
          #/fn proj-tag rest
            (sink-table-put-maybe rest proj-tag
            ; TODO: See if there's a way we can either stop depending
            ; on `s-trivial` here or move this code after the place
            ; where `s-trivial` is defined.
            #/just #/make-sink-struct (s-trivial) #/list))))
    #/fn impl
    
    #/cenegetfx-bind
      (cenegetfx-sink-call-fault-binary
        caller-fault caller-fault impl func)
    #/fn func
    #/cenegetfx-sink-call-fault-binary
      caller-fault explicit-fault func arg)
  #/cenegetfx-cene-err caller-fault "Tried to call a value that wasn't an opaque function or a struct"))

(define/contract (cenegetfx-sink-call-binary caller-fault func arg)
  (-> sink-fault? sink? sink? #/cenegetfx/c sink?)
  (cenegetfx-sink-call-fault-binary
    caller-fault caller-fault func arg))

(define/contract
  (cenegetfx-sink-call-fault-list
    caller-fault explicit-fault func first-arg args)
  (-> sink-fault? sink-fault? sink? sink? (listof sink?)
    (cenegetfx/c sink?))
  (dissect (reverse #/cons first-arg args) (cons last-arg rev-args)
  #/cenegetfx-bind
    (cenegetfx-list-foldl func (reverse rev-args) #/fn func arg
      (cenegetfx-sink-call-binary caller-fault func arg))
  #/fn func
  #/cenegetfx-sink-call-fault-binary
    caller-fault explicit-fault func last-arg))

(define/contract (cenegetfx-sink-call-list caller-fault func args)
  (-> sink-fault? sink? (listof sink?) #/cenegetfx/c sink?)
  (cenegetfx-list-foldl func args #/fn func arg
    (cenegetfx-sink-call-binary caller-fault func arg)))

(define/contract
  (cenegetfx-sink-call-fault
    caller-fault explicit-fault func first-arg . args)
  (->* (sink-fault? sink-fault? sink? sink?) #:rest (listof sink?)
    (cenegetfx/c sink?))
  (cenegetfx-sink-call-fault-list
    caller-fault explicit-fault func first-arg args))

(define/contract (cenegetfx-sink-call caller-fault func . args)
  (->* (sink-fault? sink?) #:rest (listof sink?) #/cenegetfx/c sink?)
  (cenegetfx-sink-call-list caller-fault func args))

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
  (begin (assert-can-mutate!)
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
  (begin (assert-can-mutate!)
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
(define/contract
  (cenegetfx-sink-call-qualify fault qualify pre-qualified-name)
  (-> sink-fault? sink? sink-name?
    (cenegetfx/c sink-authorized-name?))
  (cenegetfx-bind
    (cenegetfx-sink-call fault qualify pre-qualified-name)
  #/fn qualified-name
  #/expect (sink-authorized-name? qualified-name) #t
    (cenegetfx-cene-err fault
      "Expected the result of a qualify function to be an authorized name")
  #/cenegetfx-done qualified-name))

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
  #/sink-extfx-with-run-getfx #/fn
  #/sink-extfx-run-cenegetfx
    (cenegetfx-sink-call-qualify fault qualify
      (pre-qualify #/sink-name-for-string
        (sink-string-from-located-string located-string)))
  #/fn qualified-sink-authorized-name
  #/then text-input-stream
    (just #/list located-string qualified-sink-authorized-name)))

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
  #/sink-extfx-with-run-getfx #/fn
  #/mat maybe-identifier (just identifier)
    (sink-extfx-run-cenegetfx
      (cenegetfx-sink-call-qualify fault qualify
        (pre-qualify #/sink-name-for-string
          (sink-string-from-located-string identifier)))
    #/fn qualified-sink-authorized-name
    #/then text-input-stream qualified-sink-authorized-name)
  
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
  #/sink-extfx-run-cenegetfx
    (cenegetfx-sink-call fault op-impl
      unique-name qualify text-input-stream output-stream
    #/sink-fn-curried-fault 4
    #/fn fault unique-name qualify text-input-stream output-stream
    #/cenegetfx-done
    #/expect (sink-authorized-name? unique-name) #t
      (sink-extfx-cene-err fault "Expected the unique name of a macro's callback results to be an authorized name")
    #/expect (sink-text-input-stream? text-input-stream) #t
      (sink-extfx-cene-err fault "Expected the text input stream of a macro's callback results to be a text input stream")
    #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
      (sink-extfx-cene-err fault "Expected the expression sequence output stream of a macro's callback results to be an expression sequence output stream")
    #/sink-extfx-claim-freshen unique-name #/fn unique-name
    #/then unique-name qualify text-input-stream output-stream)
  #/fn result
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
  #/sink-extfx-run-sink-getfx
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
  #/sink-extfx-run-sink-getfx
    (sink-getfx-bind
      (sink-getfx-run-cenegetfx
        (cenegetfx-sink-call-qualify fault qualify
          (sink-name-for-nameless-bounded-cexpr-op)))
    #/fn qualified
    #/sink-getfx-get #/sink-authorized-name-get-name qualified)
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
    (sink-extfx-cene-err fault "Encountered an unmatched )")
  #/sink-extfx-optimized-textpat-read-located
    fault |pat "]"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (sink-extfx-cene-err fault "Encountered an unmatched ]")
  
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
          (sink-extfx-cene-err fault "Encountered a syntax that began with ( or (. and did not end with )")
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
          (sink-extfx-cene-err fault "Encountered a syntax that began with [ or [. and did not end with ]")
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
          (sink-extfx-cene-err fault "Encountered a syntax that began with /. and did not end at ) or ]")
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
  #/sink-extfx-cene-err fault "Encountered an unrecognized case of the expression syntax"))

(struct-easy
  (core-sink-struct-metadata
    tag-cache-key main-tag-string proj-strings)
  
  #:other
  
  ; TODO: Remove this property, and change all the call sites to use
  ; `cene-definition-tag` instead. (Actually, we'll probably want to
  ; abstract over it, maybe putting the `cene-definition-tag` calls
  ; inside `make-sink-struct`, `unmake-sink-struct-maybe`, and similar
  ; places things this procedure behavior is currently being used.
  #:property prop:procedure
  (fn this
    (expect (core-sink-struct-metadata? this) #t
      (error "Expected this to be a core-sink-struct-metadata")
    #/cene-definition-tag this)))

(define/contract (core-sink-struct main-tag-string proj-strings)
  (-> immutable-string? (listof immutable-string?)
    core-sink-struct-metadata?)
  (core-sink-struct-metadata (gensym) main-tag-string proj-strings))

(define s-trivial (core-sink-struct "trivial" #/list))

(define s-nil (core-sink-struct "nil" #/list))
(define s-cons (core-sink-struct "cons" #/list "first" "rest"))

(define s-clamor-err
  (core-sink-struct "clamor-err" #/list "blame" "message"))

(define minimal-tags
  (list
    s-trivial
    
    s-nil
    s-cons
    
    s-clamor-err))

(define (cene-definition-tag metadata)
  (-> core-sink-struct-metadata? #/and/c pair? #/listof name?)
  (expect (cene-definition-get-param)
    (just #/list
      (cene-root-info ds lang-impl-qualify-root tag-cache)
      maybe-run-getfx)
    (error "Expected every call to `cene-definition-tag` to occur with an implementation in the dynamic scope")
  #/dissect metadata
    (core-sink-struct-metadata
      tag-cache-key main-tag-string proj-strings)
  #/hash-ref tag-cache tag-cache-key))

(define (make-cene-root-info ds lang-impl-qualify-root tags)
  (-> dspace? authorized-name? (listof core-sink-struct-metadata?)
    (cene-root-info/c))
  (with-gets-from (cene-root-info ds lang-impl-qualify-root (hash))
  #/fn
  #/cene-root-info
    ds
    lang-impl-qualify-root
    (list-foldl (hasheq) tags #/fn tag-cache metadata
      (dissect metadata
        (core-sink-struct-metadata
          tag-cache-key main-tag-string proj-strings)
      #/hash-set tag-cache tag-cache-key
        (w- main-tag-authorized-name
          (sink-name-qualify-for-lang-impl
          #/sink-name-for-struct-main-tag
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
          name)))))

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
  #/make-sink-extfx
    (cenegetfx-bind (cenegetfx-read-root-info) #/fn rinfo
    #/cenegetfx-done
      (extfx-claim name #/fn
      #/with-gets-from rinfo #/fn
      #/extfx-with-run-getfx #/fn
      #/extfx-run-sink-extfx #/on-success))))

(define/contract (sink-extfx-claim-and-split unique-name n then)
  (->
    sink-authorized-name? natural?
    (-> (listof sink-authorized-name?) sink-extfx?)
    sink-extfx?)
  (dissect unique-name (sink-authorized-name unique-name)
  #/make-sink-extfx
    (cenegetfx-bind (cenegetfx-read-root-info) #/fn rinfo
    #/cenegetfx-done
      (extfx-claim-and-split unique-name n #/fn names
      #/with-gets-from rinfo #/fn
      #/extfx-with-run-getfx #/fn
      #/extfx-run-sink-extfx #/then #/list-map names #/fn name
        (sink-authorized-name name)))))

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
(define/contract (cenegetfx-cexpr-eval fault cexpr)
  (-> sink-fault? (and/c cexpr? cexpr-is-closed?) #/cenegetfx/c sink?)
  (cenegetfx-cexpr-eval-in-env fault cexpr (table-empty)))

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
        (sink-extfx-cene-err fault "Encountered a top-level expression that compiled to a non-expression value")
      #/expect (cexpr-has-free-vars? cexpr #/table-empty) #f
        ; TODO FAULT: Make this `fault` more specific.
        (sink-extfx-cene-err fault "Encountered a top-level expression with at least one free variable")
      #/sink-extfx-fuse (then unique-name-rest)
      #/sink-extfx-run-cenegetfx (cenegetfx-cexpr-eval fault cexpr)
      #/expectfn (sink-directive directive)
        ; TODO FAULT: Make this `fault` more specific.
        (sink-extfx-cene-err fault "Expected every top-level expression to evaluate to a directive")
      #/sink-extfx-run-cenegetfx
        (cenegetfx-sink-call
          fault directive unique-name-first qualify)
      #/fn effects
      #/expect (sink-extfx? effects) #t
        ; TODO FAULT: Make this `fault` more specific.
        (sink-extfx-cene-err fault "Expected every top-level expression to evaluate to a directive made from a callable value that takes two arguments and returns extfx side effects")
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
        (cenegetfx-cene-err fault "Expected the input to the root qualify function to be a name")
      #/cenegetfx-done #/qualify name))
    (sink-text-input-stream #/box #/just #/open-input-string string)))
