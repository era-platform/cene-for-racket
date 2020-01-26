#lang parendown racket/base

; cene/private
;
; A Racket library with entrypoints to the Cene programming language
; (implementation details).

;   Copyright 2018-2020 The Era Authors
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
  -> ->* and/c any any/c contract? cons/c contract-name list/c listof
  none/c or/c rename-contract)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract make-contract raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/control reset-at shift-at)
(require #/only-in racket/generic define/generic define-generics)
(require #/only-in racket/math natural?)
(require #/only-in racket/port filter-read-input-port)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w- w-loop)
(require #/only-in lathe-comforts/list
  list-any list-foldl list-foldr list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/match
  define-match-expander-attenuated
  define-match-expander-from-match-and-make match/c)
(require #/only-in lathe-comforts/maybe
  just just-value maybe? maybe-bind maybe/c maybe-map nothing
  nothing?)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct
  auto-write define-imitation-simple-struct
  define-syntax-and-value-imitation-simple-struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in interconfection/extensibility/base
  authorized-name? authorized-name-get-name authorized-name-subname
  dex-authorized-name dex-dspace dspace? error-definer?
  error-definer-from-exn error-definer-from-message
  error-definer-uninformative extfx? extfx-claim-unique
  extfx-ct-continue extfx-noop extfx-pub-write extfx-run-getfx
  extfx-put extfx-split-list extfx-sub-write fuse-extfx getfx?
  getfx-bind getfx/c getfx-done getfx-err getfx-get make-pub make-sub
  optionally-dexed-dexed optionally-dexed-once pure-run-getfx
  success-or-error-definer)
(require #/only-in interconfection/order
  assocs->table-if-mutually-unique dex-immutable-string dex-trivial
  getfx-is-eq-by-dex ordering-eq)
(require #/only-in interconfection/order/base
  dex? dex-default dexed-first-order/c dex-give-up dex-name dex-table
  dex-tuple fuse-by-merge getfx-call-fuse getfx-dexed-of
  getfx-is-in-dex getfx-name-of getfx-table-map-fuse
  make-fusable-function merge-by-dex merge-table name? ordering-eq?
  table? table-empty? table-empty table-get table-shadow)
(require #/prefix-in unsafe: #/only-in interconfection/order/unsafe
  dex dexed gen:dex-internals name)

(require #/only-in cene/private/textpat
  textpat? textpat-from-string textpat-lookahead
  textpat-once-or-more textpat-one textpat-one-in-range
  textpat-one-in-string textpat-one-not textpat-one-not-in-string
  textpat-or textpat-star optimized-textpat? optimized-textpat-read!
  optimize-textpat)


(provide #/all-defined-out)



; TODO: We used this in `interconfection/order/base`, and we're using
; it again here. See if it should be an export of Interconfection.
(define/contract (getmaybefx-bind effects then)
  (-> (getfx/c maybe?) (-> any/c #/getfx/c maybe?) #/getfx/c maybe?)
  (getfx-bind effects #/fn maybe-intermediate
  #/expect maybe-intermediate (just intermediate)
    (getfx-done #/nothing)
  #/then intermediate))

; TODO: We used this in `interconfection/order/base`, and we're using
; it again here. See if it should be an export of Interconfection.
(define/contract (getmaybefx-map effects func)
  (-> (getfx/c maybe?) (-> any/c any/c) #/getfx/c maybe?)
  (getmaybefx-bind effects #/fn intermediate
  #/getfx-done #/just #/func intermediate))

; TODO: Put this into the `interconfection/order` module or something
; (maybe even `interconfection/order/base`).
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

; TODO: Put this into the `interconfection/order` module or something
; (maybe even `interconfection/order/base`).
(define/contract (table-v-map table v-to-v)
  (-> table? (-> any/c any/c) table?)
  (table-kv-map table #/fn k v #/v-to-v v))

; TODO: See if we should add this to `interconfection/extensibility`.
(define/contract (extfx-err on-execute)
  (-> error-definer? extfx?)
  (extfx-run-getfx (getfx-err on-execute) #/fn impossible-result
    (error "Internal error: Did not expect `getfx-err` to complete with a result value")))

; TODO: See if we should export this from somewhere.
(define/contract (monad-maybe-map fx-done fx-bind maybe-of-fx)
  (-> (-> any/c any/c) (-> any/c (-> any/c any/c) any/c) maybe? any/c)
  (expect maybe-of-fx (just fx-val) (fx-done #/nothing)
  #/fx-bind fx-val #/fn val
  #/fx-done #/just val))

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
; nonempty list of Interconfection name values, beginning with the
; main tag name of the struct and then listing the names of the
; projections. The projections' names should have no duplicates. The
; value of `projs` should be a list of Cene values which are the
; values of the projections.
(define-imitation-simple-struct
  (sink-struct? sink-struct-tags sink-struct-projs)
  sink-struct
  'sink-struct (current-inspector) (auto-write) (#:gen gen:sink))

(define/contract (make-sink-struct tags projs)
  (-> pair? (or/c (list) pair?) sink-struct?)
  ; NOTE: For efficiency, we don't do any checking here. The value of
  ; `tags` should be a nonempty list of Interconfection name values,
  ; beginning with the main tag name of the struct and then listing
  ; the names of the projections. The projections' names should have
  ; no duplicates. The value of `projs` should be a list of Cene
  ; values which are the values of the projections.
  (sink-struct tags projs))

(define/contract (dex-sink-must-be)
  (-> dex?)
  (dex-default
    (dex-tuple sink-you-must-be-this-lang-impl/t)
    (dex-tuple sink-you-must-be-someone/t)))

(define/contract (dex-sink-i-am)
  (-> dex?)
  (dex-default
    (dex-tuple sink-i-am-this-lang-impl/t)
    (dex-tuple sink-i-am-someone/t)))

(define/contract (sink-must-be? v)
  (-> sink? boolean?)
  (pure-run-getfx #/getfx-is-in-dex (dex-sink-must-be) v))

(define/contract (sink-i-am? v)
  (-> sink? boolean?)
  (pure-run-getfx #/getfx-is-in-dex (dex-sink-i-am) v))

(define/contract (sink-i-am-whom-i-must-be? i-am-this that-must-be)
  (-> sink-i-am? sink-must-be? boolean?)
  (if (sink-you-must-be-this-lang-impl? that-must-be)
    (sink-i-am-this-lang-impl? i-am-this)
  #/dissect (sink-you-must-be-someone? that-must-be) #t
    (sink-i-am-someone? i-am-this)))

(define/contract (dex-sink-innate-main-tag-entry)
  (-> dex?)
  (dex-tuple sink-innate-main-tag-entry/t
    (dex-tuple sink-name/t #/dex-name)
    (dex-sink-must-be)
    (dex-sink-must-be)))

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
    (pure-run-getfx #/getfx-is-eq-by-dex
      (dex-sink-innate-main-tag-entry)
      main-tag
      s-main-tag)
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


; TODO: Generally speaking, we should make sure no fault object has a
; substantial memory footprint, because that makes one fault object
; appreciably different from another and makes their `ordering-eq`
; status less meaningful. See if we can impose some arbitrary limit on
; how much information we retain in each fault object.
;
(define-imitation-simple-struct
  (cene-fault-rep-continuation-marks?
    cene-fault-rep-continuation-marks-continuation-marks)
  cene-fault-rep-continuation-marks
  'cene-fault-rep-continuation-marks (current-inspector) (auto-write))
; TODO: Somehow include the filename in this.
(define-imitation-simple-struct
  (cene-fault-rep-srcloc?
    cene-fault-rep-srcloc-line
    cene-fault-rep-srcloc-column
    cene-fault-rep-srcloc-position)
  cene-fault-rep-srcloc
  'cene-fault-rep-srcloc (current-inspector) (auto-write))
; A fault object that blames an error on a call site that calls a
; callback of a callback. In this situation, the fault object can
; indicate not only the call site of the callback-callback but also
; the call site of the original operation that the callback-callback
; is a callback-callback of.
(define-imitation-simple-struct
  (cene-fault-rep-double-callback?
    cene-fault-rep-double-callback-founder-fault
    cene-fault-rep-double-callback-callback-fault)
  cene-fault-rep-double-callback
  'cene-fault-rep-double-callback (current-inspector) (auto-write))
; A fault object that blames an error on a call site that occurs in a
; cexpr being read. In this situation, the fault object can indicate
; not only the call site being read but also the call site of the read
; operation.
(define-imitation-simple-struct
  (cene-fault-rep-read?
    cene-fault-rep-read-read-fault
    cene-fault-rep-read-expr-fault)
  cene-fault-rep-read
  'cene-fault-rep-read (current-inspector) (auto-write))
; A fault object that blames an error on a call site that occurs in a
; cexpr being evaluated. In this situation, the fault object can
; indicate not only the call site being evaluated but also the call
; site of the evaluation operation.
(define-imitation-simple-struct
  (cene-fault-rep-eval?
    cene-fault-rep-eval-eval-fault
    cene-fault-rep-eval-expr-fault)
  cene-fault-rep-eval
  'cene-fault-rep-eval (current-inspector) (auto-write))

(define-imitation-simple-struct
  (sink-fault? sink-fault-rep)
  sink-fault
  'sink-fault (current-inspector) (auto-write) (#:gen gen:sink))

(define/contract (error-definer-from-fault-and-message fault message)
  (-> sink-fault? immutable-string? error-definer?)
  (dissect fault (sink-fault rep)
  #/mat rep (cene-fault-rep-continuation-marks marks)
    (error-definer-from-exn #/exn:fail message marks)
  #/mat rep (cene-fault-rep-srcloc line column position)
    ; TODO FAULT: See if we can incorporate more of the information we
    ; have into the error message.
    (error-definer-from-exn
      (exn:fail message (current-continuation-marks)))
  #/mat rep
    (cene-fault-rep-double-callback founder-fault callback-fault)
    ; TODO FAULT: See if we can incorporate more of the information we
    ; have into the error message.
    (error-definer-from-fault-and-message callback-fault message)
  #/dissect rep (cene-fault-rep-eval eval-fault expr-fault)
    ; TODO FAULT: See if we can incorporate more of the information we
    ; have into the error message.
    (error-definer-from-fault-and-message expr-fault message)))


; Innate main tag entries contain authorization conditions for the
; people who can define the function implementation and the people who
; can create construction and deconstruction expressions.
(define-syntax-and-value-imitation-simple-struct
  (sink-you-must-be-this-lang-impl?)
  sink-you-must-be-this-lang-impl
  sink-you-must-be-this-lang-impl/t
  'sink-you-must-be-this-lang-impl (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-you-must-be-someone?)
  sink-you-must-be-someone
  sink-you-must-be-someone/t
  'sink-you-must-be-someone (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-i-am-this-lang-impl?)
  sink-i-am-this-lang-impl
  sink-i-am-this-lang-impl/t
  'sink-i-am-this-lang-impl (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-i-am-someone?)
  sink-i-am-someone
  sink-i-am-someone/t
  'sink-i-am-someone (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-innate-main-tag-entry?
    sink-innate-main-tag-entry-main-tag-name
    sink-innate-main-tag-entry-author-must-be
    sink-innate-main-tag-entry-user-must-be)
  sink-innate-main-tag-entry
  sink-innate-main-tag-entry/t
  'sink-innate-main-tag-entry (current-inspector) (auto-write)
  (#:gen gen:sink))

(define-imitation-simple-struct
  (sink-directive? sink-directive-directive)
  sink-directive
  'sink-directive (current-inspector) (auto-write) (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-dex? sink-dex-dex)
  sink-dex
  sink-dex/t
  'sink-dex (current-inspector) (auto-write) (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-name? sink-name-name)
  sink-name
  sink-name/t
  'sink-name (current-inspector) (auto-write) (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-authorized-name? sink-authorized-name-authorized-name)
  sink-authorized-name
  sink-authorized-name/t
  'sink-authorized-name (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-getfx? sink-getfx-cenegetfx-go)
  sink-getfx
  sink-getfx/t
  'sink-getfx (current-inspector) (auto-write) (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-extfx? sink-extfx-cenegetfx-go)
  sink-extfx
  sink-extfx/t
  'sink-extfx (current-inspector) (auto-write) (#:gen gen:sink))
(define-imitation-simple-struct
  (sink-pub? sink-pub-pub)
  sink-pub
  'sink-pub (current-inspector) (auto-write) (#:gen gen:sink))
(define-imitation-simple-struct
  (sink-sub? sink-sub-sub)
  sink-sub
  'sink-sub (current-inspector) (auto-write) (#:gen gen:sink))
(define-imitation-simple-struct
  (sink-cexpr-sequence-output-stream?
    sink-cexpr-sequence-output-stream-box-of-maybe-id-and-state-and-handler)
  sink-cexpr-sequence-output-stream
  'sink-cexpr-sequence-output-stream (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-imitation-simple-struct
  (sink-text-input-stream? sink-text-input-stream-box-of-maybe-input)
  sink-text-input-stream
  'sink-text-input-stream (current-inspector) (auto-write)
  (#:gen gen:sink))

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
(define-imitation-simple-struct
  (sink-located-string? sink-located-string-parts)
  sink-located-string
  'sink-located-string (current-inspector) (auto-write)
  (#:gen gen:sink))

; NOTE: Racket's basic string operations make it easy to end up with a
; mutable string by accident, so we go out of our way to check that
; all `sink-string` values are immutable.
;
; TODO: See if we can somehow get a guarded tupler instead of
; `unguarded-sink-string/t`.
;
(define-syntax-and-value-imitation-simple-struct
  (sink-string? sink-string-racket-string)
  unguarded-sink-string
  unguarded-sink-string/t
  'sink-string (current-inspector) (auto-write) (#:gen gen:sink))
(define-match-expander-attenuated
  attenuated-sink-string
  unguarded-sink-string
  [racket-string immutable-string?]
  #t)
(define-match-expander-from-match-and-make
  sink-string
  unguarded-sink-string
  attenuated-sink-string
  attenuated-sink-string)

(define-syntax-and-value-imitation-simple-struct
  (sink-opaque-fn-fusable? sink-opaque-fn-fusable-racket-fn)
  sink-opaque-fn-fusable
  sink-opaque-fn-fusable/t
  'sink-opaque-fn-fusable (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-imitation-simple-struct
  (sink-opaque-fn-fault? sink-opaque-fn-fault-racket-fn)
  sink-opaque-fn-fault
  'sink-opaque-fn-fault (current-inspector) (auto-write)
  (#:gen gen:sink))
(define-imitation-simple-struct
  (sink-opaque-fn? sink-opaque-fn-racket-fn)
  sink-opaque-fn
  'sink-opaque-fn (current-inspector) (auto-write) (#:gen gen:sink))
(define-syntax-and-value-imitation-simple-struct
  (sink-table? sink-table-racket-table)
  sink-table
  sink-table/t
  'sink-table (current-inspector) (auto-write) (#:gen gen:sink))

; NOTE: The term "cexpr" is short for "compiled expression." It's the
; kind of expression that macros generate in order to use as function
; definitions.
(define-imitation-simple-struct
  (sink-cexpr? sink-cexpr-cexpr)
  sink-cexpr
  'sink-cexpr (current-inspector) (auto-write) (#:gen gen:sink))

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
; them, so we simply treat them as `ordering-eq` without bothering to
; compare them. We do this by defining a `dex-non-cene-value` dex to
; use for the appropriate field of `(dex-tuple cene-root-info/t ...)`.

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


(define-syntax-and-value-imitation-simple-struct
  (cene-root-info?
    cene-root-info-dspace
    cene-root-info-lang-impl-qualify-root
    cene-root-info-tag-cache)
  cene-root-info
  cene-root-info/t
  'cene-root-info (current-inspector) (auto-write))

(define/contract (cene-root-info/c)
  (-> contract?)
  (rename-contract
    (match/c cene-root-info dspace? sink-authorized-name? any/c)
    '(cene-root-info/c)))

(define/contract (dex-cene-root-info)
  (-> dex?)
  (dex-tuple cene-root-info/t
    (dex-dspace)
    (dex-tuple sink-authorized-name/t #/dex-authorized-name)
    (dex-non-cene-value)))

(struct-easy (cenegetfx getfx-run))

(define/contract (cenegetfx-run-getfx getfx)
  (-> getfx? cenegetfx?)
  (cenegetfx #/make-fusable-function #/fn rinfo getfx))

(define/contract (getfx-run-cenegetfx rinfo effects)
  (-> (cene-root-info/c) cenegetfx? getfx?)
  (dissect effects (cenegetfx effects)
  #/effects rinfo))

(define/contract (cenegetfx-done result)
  (-> any/c cenegetfx?)
  (cenegetfx-run-getfx #/getfx-done result))

(define/contract (cenegetfx-bind effects then)
  (-> cenegetfx? (-> any/c cenegetfx?) cenegetfx?)
  (cenegetfx #/make-fusable-function #/fn rinfo
    (getfx-bind (getfx-run-cenegetfx rinfo effects) #/fn intermediate
    #/getfx-run-cenegetfx rinfo #/then intermediate)))

; TODO: This implementation of `cenegetfx-bind` has more helpful error
; messages. See if we should use it instead.
#;
(define/contract (cenegetfx-bind effects then)
  (-> cenegetfx? (-> any/c any/c) cenegetfx?)
  (w- marks (current-continuation-marks)
  #/cenegetfx #/make-fusable-function #/fn rinfo
    (getfx-bind (getfx-run-cenegetfx rinfo effects) #/fn intermediate
    #/getfx-run-cenegetfx rinfo
      (w- thenresult (then intermediate)
      #/begin0 thenresult
        (unless (cenegetfx? thenresult)
          (raise #/exn:fail:contract "wasn't cenegetfx" marks))))))

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

(define/contract (cenegetfx-maybe-map maybe-of-cenegetfx)
  (-> (maybe/c cenegetfx?) #/cenegetfx/c maybe?)
  (monad-maybe-map
    (fn result #/cenegetfx-done result)
    (fn effects then #/cenegetfx-bind effects then)
    maybe-of-cenegetfx))

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
  (cenegetfx #/make-fusable-function #/fn rinfo #/getfx-done rinfo))

(define/contract (cenegetfx-read-dexed-root-info)
  (-> #/cenegetfx/c #/dexed-first-order/c #/cene-root-info/c)
  (cenegetfx #/make-fusable-function #/fn rinfo
    (getfx-done #/just-value #/pure-run-getfx #/getfx-dexed-of
      (dex-cene-root-info)
      rinfo)))

(define/contract (cenegetfx-read-definition-space)
  (-> #/cenegetfx/c dspace?)
  (cenegetfx-bind (cenegetfx-read-root-info)
  #/dissectfn (cene-root-info ds lang-impl-qualify-root tag-cache)
  #/cenegetfx-done ds))

(define/contract (cenegetfx-read-lang-impl-qualify-root)
  (-> #/cenegetfx/c sink-authorized-name?)
  (cenegetfx-bind (cenegetfx-read-root-info)
  #/dissectfn (cene-root-info ds lang-impl-qualify-root tag-cache)
  #/cenegetfx-done lang-impl-qualify-root))


; TODO: For now, this always succeeds. See what this should do in the
; long run.
(define/contract (assert-can-mutate!)
  (-> void?)
  (void))

(define/contract (sink-getfx-get name)
  (-> sink-name? sink-getfx?)
  (dissect name (sink-name name)
  #/sink-getfx
    (cenegetfx-bind (cenegetfx-read-definition-space) #/fn ds
    #/cenegetfx-run-getfx #/getfx-get ds name
      #;on-stall (error-definer-uninformative)
      )))

(define/contract (cenegetfx-make-sink-pub pubsub-name)
  (-> sink-authorized-name? #/cenegetfx/c sink-pub?)
  (dissect pubsub-name (sink-authorized-name pubsub-name)
  #/cenegetfx-bind (cenegetfx-read-definition-space) #/fn ds
  #/cenegetfx-done #/sink-pub #/make-pub ds pubsub-name))

(define/contract (cenegetfx-make-sink-sub pubsub-name)
  (-> sink-authorized-name? #/cenegetfx/c sink-sub?)
  (dissect pubsub-name (sink-authorized-name pubsub-name)
  #/cenegetfx-bind (cenegetfx-read-definition-space) #/fn ds
  #/cenegetfx-done #/sink-sub #/make-sub ds pubsub-name))

(define/contract
  (cenegetfx-sink-authorized-name-for-init-package-pubsub)
  (-> #/cenegetfx/c sink-authorized-name?)
  (cenegetfx-bind (cenegetfx-read-lang-impl-qualify-root)
  #/fn lang-impl-qualify-root
  #/cenegetfx-done
    (sink-authorized-name-subname
      (sink-name-of-racket-string "init-package-pubsub")
      lang-impl-qualify-root)))

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

(define/contract (extfx-run-sink-extfx rinfo effects)
  (-> (cene-root-info/c) sink-extfx? extfx?)
  (extfx-run-getfx
    (getfx-run-cenegetfx rinfo #/ceneextfx-run-sink-extfx effects)
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

(struct-easy (cexpr-construct main-tag-entry projs)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-construct main-tag-entry projs)
        (error "Expected this to be a cexpr-construct")
      #/list-any projs #/dissectfn (list proj-name proj-cexpr)
        (-has-free-vars? proj-cexpr env)))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-construct main-tag-entry projs)
        (error "Expected this to be a cexpr-construct")
      #/cenegetfx-bind
        (cenegetfx-list-map #/list-map projs
        #/dissectfn (list proj-name proj-cexpr)
          (-eval-in-env fault proj-cexpr env))
      #/fn vals
      #/cenegetfx-done #/make-sink-struct
        (cons main-tag-entry
        #/list-map projs #/dissectfn (list proj-name proj-cexpr)
          proj-name)
        vals))
  ])

(struct-easy (cexpr-call-fault read-fault fault-arg func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call-fault read-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/or
        (-has-free-vars? fault-arg env)
        (-has-free-vars? func env)
        (-has-free-vars? arg env)))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-call-fault read-fault fault-arg func arg)
        (error "Expected this to be a cexpr-call-fault")
      #/cenegetfx-bind (-eval-in-env fault fault-arg env)
      #/fn fault-arg
      #/cenegetfx-bind (-eval-in-env fault func env) #/fn func
      #/cenegetfx-bind (-eval-in-env fault arg env) #/fn arg
      #/w- eval-fault (make-fault-eval fault read-fault)
      #/expect (sink-fault? fault-arg) #t
        (cenegetfx-cene-err eval-fault "Expected the blame argument to be a blame value")
      #/cenegetfx-sink-call-fault eval-fault fault-arg func arg))
  ])

(struct-easy (cexpr-call read-fault func arg)
  
  #:other
  
  #:methods gen:cexpr
  [
    (define/generic -has-free-vars? cexpr-has-free-vars?)
    (define/generic -eval-in-env cenegetfx-cexpr-eval-in-env)
    
    (define (cexpr-has-free-vars? this env)
      (expect this (cexpr-call read-fault func arg)
        (error "Expected this to be a cexpr-call")
      #/or (-has-free-vars? func env) (-has-free-vars? arg env)))
    
    (define (cenegetfx-cexpr-eval-in-env fault this env)
      (expect this (cexpr-call read-fault func arg)
        (error "Expected this to be a cexpr-call")
      #/cenegetfx-bind (-eval-in-env fault func env) #/fn func
      #/cenegetfx-bind (-eval-in-env fault arg env) #/fn arg
      #/w- eval-fault (make-fault-eval fault read-fault)
      #/cenegetfx-sink-call eval-fault func arg))
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

; TODO: Put this in Interconfection.
(define/contract (extfx-fuse-binary a b)
  (-> extfx? extfx? extfx?)
  (dissect (pure-run-getfx #/getfx-call-fuse (fuse-extfx) a b)
    (just result)
    result))

; TODO: Put this in Interconfection.
(define/contract (extfx-fuse-list lst)
  (-> (listof extfx?) extfx?)
  (list-foldl (extfx-noop) lst #/fn a b
    (extfx-fuse-binary a b)))

; TODO: Put this in Interconfection.
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
          (extfx-run-sink-extfx rinfo #/func arg))))))

(define/contract (sink-cexpr-var name)
  (-> sink-name? sink-cexpr?)
  (dissect name (sink-name name)
  #/sink-cexpr #/cexpr-var name))

(define/contract (sink-cexpr-reified result)
  (-> sink? sink-cexpr?)
  (sink-cexpr #/cexpr-reified result))

; TODO: See if this should be an export of Interconfection.
(define/contract (names-have-duplicate? names)
  (-> (listof name?) boolean?)
  (nothing?
  #/assocs->table-if-mutually-unique #/list-map names #/fn name
    (cons name #/trivial)))

(define/contract (sink-names-have-duplicate? names)
  (-> (listof sink-name?) boolean?)
  (names-have-duplicate?
  #/list-map names #/dissectfn (sink-name name) name))

(define/contract (sink-cexpr-construct main-tag-entry projs)
  (->
    sink-innate-main-tag-entry?
    (listof #/list/c sink-name? sink-cexpr?)
    sink-cexpr?)
  (if
    (sink-names-have-duplicate?
      (list-map projs #/dissectfn (list proj-name proj-cexpr)
        proj-name))
    (error "Encountered a duplicate projection name")
  #/sink-cexpr #/cexpr-construct main-tag-entry #/list-map projs
  #/dissectfn (list (sink-name proj-name) (sink-cexpr proj-cexpr))
    (list proj-name proj-cexpr)))

; NOTE: Since this creates authorized names out of unauthorized names,
; we shouldn't expose it as a Cene built-in. We only use this for
; convenient construction of built-in struct tags within this Cene
; implementation.
(define/contract (make-sink-cexpr-construct tags proj-cexprs)
  (->
    (cons/c sink-innate-main-tag-entry? #/listof name?)
    (listof sink-cexpr?)
    sink-cexpr?)
  (dissect tags (cons main-tag-entry proj-names)
  #/expect (= (length proj-names) (length proj-cexprs)) #t
    (error "Expected tags to have one more entry than proj-cexprs")
  #/sink-cexpr-construct main-tag-entry
  #/list-zip-map proj-names proj-cexprs #/fn proj-name proj-cexpr
    (list (sink-name proj-name) proj-cexpr)))

(define/contract (sink-cexpr-call-binary read-fault func arg)
  (-> sink-fault? sink-cexpr? sink-cexpr? sink-cexpr?)
  (dissect func (sink-cexpr func)
  #/dissect arg (sink-cexpr arg)
  #/sink-cexpr #/cexpr-call read-fault func arg))

(define/contract (sink-cexpr-call-list read-fault func args)
  (-> sink-fault? sink-cexpr? (listof sink-cexpr?) sink-cexpr?)
  (list-foldl func args #/fn func arg
    (sink-cexpr-call-binary read-fault func arg)))

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
  (cenegetfx-sink-authorized-name-for-function-implementation
    result-tag main-tag-entry proj-tag-names)
  (-> immutable-string? sink-innate-main-tag-entry? table?
    (cenegetfx/c sink-authorized-name?))
  (cenegetfx-bind (cenegetfx-read-lang-impl-qualify-root)
  #/fn lang-impl-qualify-root
  #/cenegetfx-done
    (sink-authorized-name-subname
      (sink-name-of-racket-string result-tag)
    #/sink-authorized-name-subname
      (sink-name #/just-value #/pure-run-getfx #/getfx-name-of
        (dex-table #/dex-trivial)
        proj-tag-names)
    #/sink-authorized-name-subname
      (sink-name #/just-value #/pure-run-getfx #/getfx-name-of
        (dex-sink-innate-main-tag-entry)
        main-tag-entry)
    #/sink-authorized-name-subname
      (sink-name-of-racket-string "function-implementation")
      lang-impl-qualify-root)))

(define/contract
  (cenegetfx-sink-authorized-name-for-function-implementation-code
    main-tag-entry proj-tag-names)
  (-> sink-innate-main-tag-entry? table?
    (cenegetfx/c sink-authorized-name?))
  (cenegetfx-sink-authorized-name-for-function-implementation
    "code" main-tag-entry proj-tag-names))

(define/contract
  (cenegetfx-sink-authorized-name-for-function-implementation-value
    main-tag-entry proj-tag-names)
  (-> sink-innate-main-tag-entry? table?
    (cenegetfx/c sink-authorized-name?))
  (cenegetfx-sink-authorized-name-for-function-implementation
    "value" main-tag-entry proj-tag-names))

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

(define/contract (make-fault-internal)
  (-> sink-fault?)
  (sink-fault #/cene-fault-rep-continuation-marks
    (current-continuation-marks)))

(define/contract
  (make-fault-double-callback founder-fault callback-fault)
  (-> sink-fault? sink-fault? sink-fault?)
  (sink-fault
    (cene-fault-rep-double-callback founder-fault callback-fault)))

(define/contract (make-fault-read read-fault expr-fault)
  (-> sink-fault? sink-fault? sink-fault?)
  (sink-fault #/cene-fault-rep-eval read-fault expr-fault))

(define/contract (make-fault-eval eval-fault expr-fault)
  (-> sink-fault? sink-fault? sink-fault?)
  (sink-fault #/cene-fault-rep-eval eval-fault expr-fault))

(define/contract (cenegetfx-err-from-clamor clamor)
  (-> sink? cenegetfx?)
  (cenegetfx-tag cssm-clamor-err #/fn csst-clamor-err
  #/cenegetfx-run-getfx #/getfx-err
    (mat (unmake-sink-struct-maybe csst-clamor-err clamor)
      (just #/list fault (sink-string message))
      (error-definer-from-fault-and-message fault message)
      (error-definer-from-exn
        (exn:fail (format "~s" clamor)
          (current-continuation-marks))))))

(define/contract (cenegetfx-cene-err fault message)
  (-> sink-fault? immutable-string? #/cenegetfx/c none/c)
  ; TODO: See if there's a way we can either stop depending on
  ; `cssm-clamor-err` here or move this code after the place where
  ; `cssm-clamor-err` is defined.
  (cenegetfx-tag cssm-clamor-err #/fn csst-clamor-err
  #/cenegetfx-err-from-clamor
    (make-sink-struct csst-clamor-err #/list
      fault
      (sink-string message))))

(define/contract (sink-getfx-cene-err fault message)
  (-> sink-fault? immutable-string? sink-getfx?)
  (sink-getfx-run-cenegetfx #/cenegetfx-cene-err fault message))

(define/contract (sink-extfx-cene-err fault message)
  (-> sink-fault? immutable-string? sink-extfx?)
  (sink-extfx-run-cenegetfx (cenegetfx-cene-err fault message)
  #/fn result
    (error "Internal error: Expected no return value from cenegetfx-cene-err")))

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
    (dissect tags (cons main-tag-entry proj-tags)
    
    ; TODO: This lookup might be expensive. See if we should memoize
    ; it.
    #/cenegetfx-bind
      (cenegetfx-bind
        (cenegetfx-sink-authorized-name-for-function-implementation-value
          main-tag-entry
          (list-foldr proj-tags (table-empty)
          #/fn proj-tag rest
            (table-shadow proj-tag (just #/trivial) rest)))
      #/fn authorized-name-for-impl
      #/cenegetfx-run-sink-getfx #/sink-getfx-get
        (sink-authorized-name-get-name authorized-name-for-impl))
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
    (dex-tuple unguarded-sink-string/t #/dex-immutable-string)
    string))

(define/contract (sink-name-for-string string)
  (-> sink-string? sink-name?)
  (sink-name #/name-for-sink-string string))

(define/contract (sink-authorized-name-get-name name)
  (-> sink-authorized-name? sink-name?)
  (dissect name
    (sink-authorized-name interconfection-authorized-name)
  #/sink-name
    (authorized-name-get-name interconfection-authorized-name)))

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

(define/contract
  (sink-extfx-sink-cexpr-sequence-output-stream-freshen
    output-stream on-err then)
  (-> sink-cexpr-sequence-output-stream? (cenegetfx/c none/c)
    (-> sink-cexpr-sequence-output-stream? sink-extfx?)
    sink-extfx?)
  (dissect output-stream (sink-cexpr-sequence-output-stream b)
  #/sink-extfx-later #/fn
  #/begin (assert-can-mutate!)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just id-and-state-and-handler)
    (sink-extfx-run-cenegetfx on-err)
  #/begin (set-box! b (nothing))
  #/then
    (sink-cexpr-sequence-output-stream
      (box #/just id-and-state-and-handler))))

(define/contract
  (sink-extfx-sink-cexpr-sequence-output-stream-spend
    output-stream then)
  (-> sink-cexpr-sequence-output-stream?
    (->
      (list/c
        any/c
        any/c
        (-> any/c sink-cexpr? (-> any/c sink-extfx?) sink-extfx?))
      sink-extfx?)
    sink-extfx?)
  (dissect output-stream (sink-cexpr-sequence-output-stream b)
  #/sink-extfx-later #/fn
  #/begin (assert-can-mutate!)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just id-and-state-and-handler)
    (error "Tried to spend an expression output stream that was already spent")
  #/begin (set-box! b (nothing))
  #/then id-and-state-and-handler))

(define/contract
  (sink-extfx-sink-cexpr-sequence-output-stream-track-identity
    output-stream then)
  (-> sink-cexpr-sequence-output-stream?
    (->
      sink-cexpr-sequence-output-stream?
      (-> sink-cexpr-sequence-output-stream? (cenegetfx/c none/c)
        (-> sink-cexpr-sequence-output-stream? sink-extfx?)
        sink-extfx?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-cexpr-sequence-output-stream-freshen output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-spend output-stream
  #/dissectfn (list id state on-cexpr)
  #/then
    (sink-cexpr-sequence-output-stream
      (box #/just #/list id state on-cexpr))
    (fn other-output-stream on-err then
      (sink-extfx-sink-cexpr-sequence-output-stream-freshen
        other-output-stream
        (cenegetfx-cene-err (make-fault-internal) "Expected other-output-stream to be an unspent expression sequence output stream")
      #/fn other-output-stream
      #/sink-extfx-sink-cexpr-sequence-output-stream-spend
        other-output-stream
      #/dissectfn (list other-id other-state other-on-cexpr)
      #/expect (eq? id other-id) #t
        (sink-extfx-run-cenegetfx on-err)
      #/then
        (sink-cexpr-sequence-output-stream
          (box #/just #/list other-id other-state other-on-cexpr))))))

(define/contract
  (sink-extfx-make-cexpr-sequence-output-stream
    unique-name state on-cexpr then)
  (->
    sink-authorized-name?
    any/c
    (-> any/c sink-cexpr (-> any/c sink-extfx?) sink-extfx?)
    (-> sink-cexpr-sequence-output-stream?
      (-> sink-cexpr-sequence-output-stream?
        (-> any/c sink-extfx?)
        sink-extfx?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-and-split unique-name 0 #/dissectfn (list)
  #/w- identity (box #/trivial)
  #/w- output-stream
    (sink-cexpr-sequence-output-stream
      (box #/just #/list identity state on-cexpr))
  #/sink-extfx-sink-cexpr-sequence-output-stream-track-identity
    output-stream
  #/fn output-stream sink-extfx-verify-same-output-stream
  #/then
    (sink-cexpr-sequence-output-stream
      (box #/just #/list identity state on-cexpr))
    (fn output-stream then
      (sink-extfx-sink-cexpr-sequence-output-stream-freshen
        output-stream
        (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
      #/fn output-stream
      #/sink-extfx-verify-same-output-stream output-stream
        (cenegetfx-cene-err (make-fault-internal) "Expected the expression sequence output stream given to an sink-extfx-make-cexpr-sequence-output-stream unwrapper to be a future incarnation of the same one created by that call")
      #/fn output-stream
      #/sink-extfx-sink-cexpr-sequence-output-stream-spend
        output-stream
      #/dissectfn (list found-id state on-cexpr)
      #/then state))))

(define/contract (sink-extfx-cexpr-write output-stream cexpr then)
  (-> sink-cexpr-sequence-output-stream? sink-cexpr?
    (-> sink-cexpr-sequence-output-stream? sink-extfx?)
    sink-extfx?)
  (dissect output-stream (sink-cexpr-sequence-output-stream _)
  #/sink-extfx-sink-cexpr-sequence-output-stream-freshen output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-spend output-stream
  #/dissectfn (list id state on-cexpr)
  #/on-cexpr state cexpr #/fn state
  #/then #/sink-cexpr-sequence-output-stream #/box #/just #/list
    id state on-cexpr))

(define/contract
  (sink-extfx-sink-text-input-stream-freshen
    text-input-stream on-err then)
  (-> sink-text-input-stream? (cenegetfx/c none/c)
    (-> sink-text-input-stream? sink-extfx?)
    sink-extfx?)
  (dissect text-input-stream (sink-text-input-stream b)
  #/sink-extfx-later #/fn
  #/begin (assert-can-mutate!)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just input-port)
    (sink-extfx-run-cenegetfx on-err)
  #/begin (set-box! b (nothing))
  #/then #/sink-text-input-stream #/box #/just input-port))

(define/contract
  (sink-extfx-sink-text-input-stream-spend text-input-stream then)
  (-> sink-text-input-stream? (-> input-port? sink-extfx?)
    sink-extfx?)
  (dissect text-input-stream (sink-text-input-stream b)
  #/sink-extfx-later #/fn
  #/begin (assert-can-mutate!)
  ; TODO: See if this should be more thread-safe in some way.
  #/expect (unbox b) (just input-port)
    (error "Tried to spend a text input stream that was already spent")
  #/begin (set-box! b (nothing))
  #/then input-port))

(define/contract
  (sink-extfx-sink-text-input-stream-track-identity
    text-input-stream then)
  (-> sink-text-input-stream?
    (->
      sink-text-input-stream?
      (-> sink-text-input-stream? (cenegetfx/c none/c)
        (-> sink-text-input-stream? sink-extfx?)
        sink-extfx?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-spend text-input-stream
  #/fn input-port
  #/then
    (sink-text-input-stream #/box #/just input-port)
    (fn other-text-input-stream on-err then
      (sink-extfx-sink-text-input-stream-freshen
        other-text-input-stream
        (cenegetfx-cene-err (make-fault-internal) "Expected other-text-input-stream to be an unspent text input stream")
      #/fn other-text-input-stream
      #/sink-extfx-sink-text-input-stream-spend
        other-text-input-stream
      #/fn other-input-port
      #/expect (eq? input-port other-input-port) #t
        (sink-extfx-run-cenegetfx on-err)
      #/then
        (sink-text-input-stream #/box #/just other-input-port)))))

(define/contract (sink-extfx-read-fault text-input-stream then)
  (-> sink-text-input-stream?
    (-> sink-text-input-stream? sink-fault? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-spend text-input-stream #/fn in
  #/let-values ([(line column position) (port-next-location in)])
  #/then (sink-text-input-stream #/box #/just in)
    (sink-fault #/cene-fault-rep-srcloc line column position)))

(define/contract (sink-extfx-read-eof text-input-stream on-eof else)
  (->
    sink-text-input-stream?
    sink-extfx?
    (-> sink-text-input-stream? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-spend text-input-stream #/fn in
  #/if (eof-object? #/peek-byte in)
    (begin (close-input-port in)
      on-eof)
  #/else #/sink-text-input-stream #/box #/just in))

(define/contract
  (sink-extfx-optimized-textpat-read-located
    pattern text-input-stream then)
  (-> optimized-textpat? sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-spend text-input-stream #/fn in
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

(define/contract
  (sink-extfx-sink-text-input-stream-split
    text-input-stream body)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream?
      (-> sink-text-input-stream?
        (-> sink-text-input-stream? sink-text-input-stream?
          sink-extfx?)
        sink-extfx?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected the original text input stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-track-identity
    text-input-stream
  #/fn text-input-stream sink-extfx-verify-same-text-input-stream
  #/sink-extfx-sink-text-input-stream-spend text-input-stream #/fn in
  #/let-values ([(pipe-in pipe-out) (make-pipe)])
  #/w- original-monitored-in
    (filter-read-input-port in
      ; read-wrap
      (fn storage result
        (begin0 result
          (when (natural? result)
            (write-bytes storage pipe-out 0 result))))
      ; peek-wrap
      (fn storage result result))
  #/body (sink-text-input-stream #/box #/just original-monitored-in)
  #/fn text-input-stream then
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected the updated text input stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-verify-same-text-input-stream text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected the resulting text input stream of a sink-extfx-sink-text-input-stream-split body to be a future incarnation of the body's original stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-spend text-input-stream
  #/fn modified-monitored-in
  #/begin (close-output-port pipe-out)
  #/then
    (sink-text-input-stream #/box #/just pipe-in)
    (sink-text-input-stream #/box #/just in)))

(define sink-extfx-peek-whether-eof-pat
  (optimize-textpat #/textpat-lookahead #/textpat-one))

(define/contract (sink-extfx-peek-whether-eof text-input-stream then)
  (-> sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-optimized-textpat-read-located
    sink-extfx-peek-whether-eof-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/mat maybe-located-string (just located-string)
    (then text-input-stream #f)
    (then text-input-stream #t)))

(define sink-extfx-read-whitespace-pat
  ; TODO: Support a more Unicode-aware notion of whitespace.
  (optimize-textpat #/textpat-star #/textpat-one-in-string " \t\r\n"))

(define/contract (sink-extfx-read-whitespace text-input-stream then)
  (-> sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-optimized-textpat-read-located
    sink-extfx-read-whitespace-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/dissect maybe-located-string (just located-string)
  #/then text-input-stream located-string))

(define sink-extfx-read-non-line-breaks-pat
  (optimize-textpat
  ; TODO: Support a more Unicode-aware notion of line break.
  #/textpat-star #/textpat-one-not-in-string "\r\n"))

(define/contract
  (sink-extfx-read-non-line-breaks text-input-stream then)
  (-> sink-text-input-stream?
    (-> sink-text-input-stream? sink-located-string? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-optimized-textpat-read-located
    sink-extfx-read-non-line-breaks-pat text-input-stream
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
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-optimized-textpat-read-located
    sink-extfx-read-maybe-identifier-pat text-input-stream
  #/fn text-input-stream maybe-located-string
  #/expect maybe-located-string (just located-string)
    (then text-input-stream #/nothing)
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
  (sink-extfx-read-maybe-op-character text-input-stream then)
  (->
    sink-text-input-stream?
    (-> sink-text-input-stream? (maybe/c sink-located-string?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-optimized-textpat-read-located
    sink-extfx-read-maybe-op-character-pat text-input-stream then))

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
  (sink-extfx-peek-whether-closing-bracket text-input-stream then)
  (-> sink-text-input-stream?
    (-> sink-text-input-stream? boolean? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-optimized-textpat-read-located
    sink-extfx-peek-whether-closing-bracket-pat text-input-stream
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
  
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-read-maybe-op-character text-input-stream
  #/fn text-input-stream maybe-identifier
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
        |pat ":"| text-input-stream
      #/fn text-input-stream maybe-str
      #/then text-input-stream op-name))
  
  ; TODO: Support the use of ( and [ as delimiters for macro
  ; names.
  #/sink-extfx-optimized-textpat-read-located
    |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (sink-extfx-cene-err fault "The use of ( to delimit a macro name is not yet supported")
  #/sink-extfx-optimized-textpat-read-located
    |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (sink-extfx-cene-err fault "The use of [ to delimit a macro name is not yet supported")
  
  #/sink-extfx-read-maybe-identifier
    fault qualify text-input-stream pre-qualify
  #/fn text-input-stream maybe-name
  #/mat maybe-name (just #/list located-string name)
    (then text-input-stream name)
  
  ; TODO FAULT: Make this `fault` more specific.
  #/sink-extfx-cene-err fault "Encountered an unrecognized case of the expression operator syntax"))

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
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-freshen
    output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
  #/sink-extfx-sink-text-input-stream-track-identity text-input-stream
  #/fn text-input-stream sink-extfx-verify-same-text-input-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-track-identity
    output-stream
  #/fn output-stream sink-extfx-verify-same-output-stream
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
    #/sink-extfx-sink-text-input-stream-freshen text-input-stream
      (cenegetfx-cene-err fault "Expected the text input stream of a macro's callback results to be an unspent text input stream")
    #/fn text-input-stream
    #/sink-extfx-sink-cexpr-sequence-output-stream-freshen
      output-stream
      (cenegetfx-cene-err fault "Expected the expression sequence output stream of a macro's callback results to be an unspent expression sequence output stream")
    #/fn output-stream
    #/sink-extfx-verify-same-text-input-stream text-input-stream
      (cenegetfx-cene-err fault "Expected the text input stream of a macro's callback results to be a future incarnation of the macro's original input stream")
    #/fn text-input-stream
    #/sink-extfx-verify-same-output-stream output-stream
      (cenegetfx-cene-err fault "Expected the expression sequence output stream of a macro's callback results to be a future incarnation of the macro's original output stream")
    #/fn output-stream
    #/then unique-name qualify text-input-stream output-stream)
  #/fn result
  #/expect (sink-extfx? result) #t
    (sink-extfx-cene-err fault "Expected the return value of a macro to be an effectful computation")
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
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-freshen
    output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
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
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-freshen
    output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
  #/sink-extfx-read-and-run-op
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
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-freshen
    output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
  #/sink-extfx-read-and-run-op
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
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-freshen
    output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
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
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-cexpr-sequence-output-stream-freshen
    output-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected output-stream to be an unspent expression sequence output stream")
  #/fn output-stream
  #/sink-extfx-read-whitespace text-input-stream
  #/fn text-input-stream whitespace
  #/sink-extfx-peek-whether-eof text-input-stream
  #/fn text-input-stream is-eof
  #/if is-eof
    (then unique-name qualify text-input-stream output-stream)
  
  #/sink-extfx-optimized-textpat-read-located
    |pat ")"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (sink-extfx-cene-err fault "Encountered an unmatched )")
  #/sink-extfx-optimized-textpat-read-located
    |pat "]"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    ; TODO FAULT: Make this `fault` more specific.
    (sink-extfx-cene-err fault "Encountered an unmatched ]")
  
  #/sink-extfx-optimized-textpat-read-located
    |pat "\\"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (sink-extfx-read-and-run-freestanding-cexpr-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-extfx-optimized-textpat-read-located
    |pat "("| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-extfx-optimized-textpat-read-located
          |pat ")"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          ; TODO FAULT: Make this `fault` more specific.
          (sink-extfx-cene-err fault "Encountered a syntax that began with ( or (. and did not end with )")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-extfx-optimized-textpat-read-located
      |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-extfx-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-extfx-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-extfx-optimized-textpat-read-located
    |pat "["| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-extfx-optimized-textpat-read-located
          |pat "]"| text-input-stream
        #/fn text-input-stream maybe-str
        #/expect maybe-str (just _)
          ; TODO FAULT: Make this `fault` more specific.
          (sink-extfx-cene-err fault "Encountered a syntax that began with [ or [. and did not end with ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-extfx-optimized-textpat-read-located
      |pat "."| text-input-stream
    #/fn text-input-stream maybe-str
    #/mat maybe-str (just _)
      (sink-extfx-read-and-run-bounded-cexpr-op
        fault unique-name qualify text-input-stream output-stream
        then)
    #/sink-extfx-run-nameless-op
      fault unique-name qualify text-input-stream output-stream then)
  
  #/sink-extfx-optimized-textpat-read-located
    |pat "/"| text-input-stream
  #/fn text-input-stream maybe-str
  #/mat maybe-str (just _)
    (w- then
      (fn unique-name qualify text-input-stream output-stream
        (sink-extfx-peek-whether-closing-bracket text-input-stream
        #/fn text-input-stream is-closing-bracket
        #/if (not is-closing-bracket)
          ; TODO FAULT: Make this `fault` more specific.
          (sink-extfx-cene-err fault "Encountered a syntax that began with /. and did not end at ) or ]")
        #/then unique-name qualify text-input-stream output-stream))
    #/sink-extfx-optimized-textpat-read-located
      |pat "."| text-input-stream
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
    tag-cache-key main-tag-string proj-strings))

(define/contract (core-sink-struct main-tag-string proj-strings)
  (-> immutable-string? (listof immutable-string?)
    core-sink-struct-metadata?)
  (core-sink-struct-metadata (gensym) main-tag-string proj-strings))

; NOTE: The prefix `cssm-...` stands for "core sink struct metadata."
; The prefix `csst-...` stands for "core sink struct tag."

(define cssm-nil (core-sink-struct "nil" #/list))
(define cssm-cons (core-sink-struct "cons" #/list "first" "rest"))

(define cssm-clamor-err
  (core-sink-struct "clamor-err" #/list "blame" "message"))

(define minimal-tags
  (list
    cssm-nil
    cssm-cons
    
    cssm-clamor-err))

(define (cenegetfx-tag-direct metadata)
  (-> core-sink-struct-metadata?
    (cons/c sink-innate-main-tag-entry? #/listof name?))
  (dissect metadata
    (core-sink-struct-metadata
      tag-cache-key main-tag-string proj-strings)
  #/cenegetfx-bind (cenegetfx-read-root-info)
  #/dissectfn (cene-root-info ds lang-impl-qualify-root tag-cache)
  #/cenegetfx-done #/hash-ref tag-cache tag-cache-key))

(define (cenegetfx-tag metadata then)
  (->
    core-sink-struct-metadata?
    (-> (cons/c sink-innate-main-tag-entry? #/listof name?)
      cenegetfx?)
    cenegetfx?)
  (cenegetfx-bind (cenegetfx-tag-direct metadata) #/fn tag
  #/then tag))

(define (sink-extfx-tag metadata then)
  (->
    core-sink-struct-metadata?
    (-> (cons/c sink-innate-main-tag-entry? #/listof name?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-run-cenegetfx (cenegetfx-tag-direct metadata) #/fn tag
  #/then tag))

(define (make-cene-root-info ds lang-impl-qualify-root tags)
  (-> dspace? authorized-name? (listof core-sink-struct-metadata?)
    (cene-root-info/c))
  (cene-root-info
    ds
    lang-impl-qualify-root
    ; TODO: Now that this doesn't actually depend on `ds` or
    ; `lang-impl-qualify-root`, see if we should just compute this
    ; when we construct the `core-sink-struct-metadata`.
    (list-foldl (hasheq) tags #/fn tag-cache metadata
      (dissect metadata
        (core-sink-struct-metadata
          tag-cache-key main-tag-string proj-strings)
      #/hash-set tag-cache tag-cache-key
        (cons
          (sink-innate-main-tag-entry
            (sink-name-for-string #/sink-string main-tag-string)
            (sink-you-must-be-this-lang-impl)
            (sink-you-must-be-someone))
          (list-map proj-strings #/fn proj-string
            (dissect (sink-name-for-string #/sink-string proj-string)
              (sink-name name)
              name)))))))

(define/contract (sink-list->cenegetfx-maybe-racket sink-list)
  (-> sink? #/cenegetfx/c #/maybe/c #/listof sink?)
  ; NOTE: We could call `sink-list->cenegetfx-maybe-racket` itself
  ; recursively, but we explicitly accumulate elements using a
  ; parameter (`rev-racket-list`) of a recursive helper function
  ; (`next`) so that we keep the call stack at a constant size
  ; throughout the list traversal.
  (cenegetfx-tag cssm-nil #/fn csst-nil
  #/cenegetfx-tag cssm-cons #/fn csst-cons
  #/cenegetfx-done
    (w-loop next sink-list sink-list rev-racket-list (list)
      (mat (unmake-sink-struct-maybe csst-nil sink-list)
        (just #/list)
        (just #/reverse rev-racket-list)
      #/mat (unmake-sink-struct-maybe csst-cons sink-list)
        (just #/list elem sink-list)
        (next sink-list #/cons elem rev-racket-list)
      #/nothing))))

(define/contract (racket-list->cenegetfx-sink racket-list)
  (-> (listof sink?) #/cenegetfx/c sink?)
  (cenegetfx-tag cssm-nil #/fn csst-nil
  #/cenegetfx-tag cssm-cons #/fn csst-cons
  #/cenegetfx-done
    (list-foldr racket-list (make-sink-struct csst-nil #/list)
    #/fn elem rest
      (make-sink-struct csst-cons #/list elem rest))))

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
      #/extfx-run-sink-extfx rinfo #/on-success))))

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
      #/extfx-run-sink-extfx rinfo #/then #/list-map names #/fn name
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
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-read-eof text-input-stream
    ; If we're at the end of the file, we're done. We claim the
    ; `unique-name` to stay in the habit, even though it's clear no
    ; one else can be using it.
    (sink-extfx-claim unique-name #/fn #/sink-extfx-noop)
  #/fn text-input-stream
  #/sink-extfx-claim-and-split unique-name 3
  #/dissectfn
    (list unique-name-stream unique-name-writer unique-name-main)
  #/sink-extfx-make-cexpr-sequence-output-stream
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
    immutable-string?
    sink-extfx?)
  (sink-extfx-read-top-level
    fault
    unique-name
    (sink-fn-curried-fault 1 #/fn fault name
      (expect (sink-name? name) #t
        (cenegetfx-cene-err fault "Expected the input to the root qualify function to be a name")
      #/cenegetfx-done #/qualify name))
    (sink-text-input-stream #/box #/just #/open-input-string string)))
