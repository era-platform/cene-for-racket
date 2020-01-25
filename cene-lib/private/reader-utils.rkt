#lang parendown racket/base

; cene/private/reader-utils
;
; An assortment of utilities for parsing Cene reader macro bodies
; (implementation details).

;   Copyright 2018, 2019 The Era Authors
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


(require #/only-in racket/contract/base -> any/c list/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list list-map nat->maybe)
(require #/only-in lathe-comforts/maybe just)
(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial)

(require cene/private)
(require #/only-in cene/private/textpat
  optimized-textpat? optimize-textpat textpat-one-not-in-string
  textpat-star)


(provide
  sink-name-for-local-variable
  id-or-expr?
  id-or-expr-id
  id-or-expr-expr
  id-or-expr->cexpr
  sink-extfx-read-bounded-ids-and-exprs
  sink-extfx-read-bounded-cexprs
  sink-extfx-read-bounded-specific-number-of-cexprs
  sink-extfx-read-leading-specific-number-of-cexprs
  sink-extfx-read-leading-specific-number-of-identifiers)



(define/contract (sink-name-for-local-variable inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n #/list 'name:local-variable n))

(struct-easy (id-or-expr-id located-string qualified-name))
(struct-easy (id-or-expr-expr expr))

(define/contract (id-or-expr? v)
  (-> any/c boolean?)
  (or (id-or-expr-id? v) (id-or-expr-expr? v)))

(define/contract (id-or-expr->cexpr id-or-expr)
  (-> id-or-expr? sink-cexpr?)
  (mat id-or-expr (id-or-expr-id located-string qualified-name)
    ; TODO CEXPR-LOCATED: Wrap this in a located cexpr.
    (sink-cexpr-var #/sink-authorized-name-get-name qualified-name)
  #/dissect id-or-expr (id-or-expr-expr cexpr)
    cexpr))

(define/contract
  (sink-extfx-read-ids-and-exprs-onto
    fault unique-name qualify text-input-stream rev-results
    pre-qualify then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    (listof id-or-expr?)
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof id-or-expr?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-read-maybe-identifier
    fault qualify text-input-stream pre-qualify
  #/fn text-input-stream maybe-id
  #/mat maybe-id (just #/list located-string qualified-name)
    (then unique-name qualify text-input-stream
      (cons (id-or-expr-id located-string qualified-name)
        rev-results))
  #/sink-extfx-claim-and-split unique-name 2
  #/dissectfn (list unique-name-stream unique-name)
  #/sink-extfx-make-cexpr-sequence-output-stream
    (make-fault-internal)
    unique-name-stream
    rev-results
    (fn rev-results cexpr then
      (then #/cons (id-or-expr-expr cexpr) rev-results))
  #/fn output-stream unwrap
  #/sink-extfx-read-cexprs
    fault unique-name qualify text-input-stream output-stream
  #/fn unique-name qualify text-input-stream output-stream
  #/unwrap (make-fault-internal) output-stream #/fn rev-results
  #/then unique-name qualify text-input-stream rev-results))

; This reads identifiers and cexprs until it gets to a closing
; bracket.
(define/contract
  (sink-extfx-read-bounded-ids-and-exprs
    fault unique-name qualify text-input-stream pre-qualify then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof id-or-expr?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    rev-results (list)
    
    (sink-extfx-read-whitespace text-input-stream
    #/fn text-input-stream whitespace
    #/sink-extfx-peek-whether-eof text-input-stream
    #/fn text-input-stream is-eof
    #/if is-eof
      ; TODO FAULT: Make this `fault` more specific.
      (sink-extfx-cene-err fault "Encountered end of file while expecting any number of identifiers and expressions preceding a closing bracket")
    #/sink-extfx-peek-whether-closing-bracket text-input-stream
    #/fn text-input-stream is-closing-bracket
    #/if is-closing-bracket
      (then unique-name qualify text-input-stream
        (reverse rev-results))
    #/sink-extfx-read-ids-and-exprs-onto
      fault unique-name qualify text-input-stream rev-results
      pre-qualify
    #/fn unique-name qualify text-input-stream rev-results
    #/next unique-name qualify text-input-stream rev-results)))

; This reads cexprs until it gets to a closing bracket.
(define/contract
  (sink-extfx-read-bounded-cexprs
    fault unique-name qualify text-input-stream then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof sink-cexpr?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-read-bounded-ids-and-exprs
    fault unique-name qualify text-input-stream
    sink-name-for-local-variable
  #/fn unique-name qualify text-input-stream ids-and-exprs
  #/then unique-name qualify text-input-stream
  #/list-map ids-and-exprs #/fn id-or-expr
    (id-or-expr->cexpr id-or-expr)))

; This reads cexprs until it gets to a closing bracket, and it
; verifies that there are precisely `n` of them.
(define/contract
  (sink-extfx-read-bounded-specific-number-of-cexprs
    fault unique-name qualify text-input-stream n then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    natural?
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof sink-cexpr?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-read-bounded-cexprs
    fault unique-name qualify text-input-stream
  #/fn unique-name qualify text-input-stream cexprs
  #/w- actual-n (length cexprs)
  #/if (< n actual-n)
    ; TODO FAULT: Make this `fault` more specific.
    (sink-extfx-cene-err fault "Encountered too many expressions")
  #/if (< actual-n n)
    ; TODO FAULT: Make this `fault` more specific.
    (sink-extfx-cene-err fault "Expected another expression")
  #/then unique-name qualify text-input-stream cexprs))

; This reads precisely `n` identifiers and cexprs, and it causes an
; error if it reaches a closing bracket first or if it reads too many
; cexprs in one cexpr read.
(define/contract
  (sink-extfx-read-leading-specific-number-of-ids-and-exprs
    fault unique-name qualify text-input-stream n pre-qualify then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    natural?
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof id-or-expr?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    rev-results (list)
    
    ; TODO: The way we're calling `length` on each loop iteration is a
    ; painter's algorithm. Let's see if we can stop doing that. We'll
    ; probably need to thread `n` through
    ; `sink-extfx-read-ids-and-exprs-onto`.
    (if (= n #/length rev-results)
      (then unique-name qualify text-input-stream
        (reverse rev-results))
    #/if (< n #/length rev-results)
      ; TODO FAULT: Make this `fault` more specific.
      (sink-extfx-cene-err fault "Encountered a single operation that expanded to too many expressions while expecting a specific number of identifiers and expressions")
    #/sink-extfx-read-whitespace text-input-stream
    #/fn text-input-stream whitespace
    #/sink-extfx-peek-whether-eof text-input-stream
    #/fn text-input-stream is-eof
    #/if is-eof
      ; TODO FAULT: Make this `fault` more specific.
      (sink-extfx-cene-err fault "Encountered end of file while expecting an identifier or an expression")
    #/sink-extfx-peek-whether-closing-bracket text-input-stream
    #/fn text-input-stream is-closing-bracket
    #/if is-closing-bracket
      ; TODO FAULT: Make this `fault` more specific.
      (sink-extfx-cene-err fault "Encountered a closing bracket while expecting an identifier or an expression")
    #/sink-extfx-read-ids-and-exprs-onto
      fault unique-name qualify text-input-stream rev-results
      pre-qualify
    #/fn unique-name qualify text-input-stream rev-results
    #/next unique-name qualify text-input-stream rev-results)))

; This reads precisely `n` cexprs, and it causes an error if it
; reaches a closing bracket first or if it reads too many cexprs in
; one cexpr read.
(define/contract
  (sink-extfx-read-leading-specific-number-of-cexprs
    fault unique-name qualify text-input-stream n then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    natural?
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof sink-cexpr?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-read-leading-specific-number-of-ids-and-exprs
    fault unique-name qualify text-input-stream n
    sink-name-for-local-variable
  #/fn unique-name qualify text-input-stream ids-and-exprs
  #/then unique-name qualify text-input-stream
  #/list-map ids-and-exprs #/fn id-or-expr
    (id-or-expr->cexpr id-or-expr)))

; This reads precisely `n` whitespace-and-comment-separated
; identifiers, and it causes an error if it reaches a closing bracket
; first or if it encounters a cexpr when it's trying to skip comments.
(define/contract
  (sink-extfx-read-leading-specific-number-of-identifiers
    fault unique-name qualify text-input-stream n pre-qualify then)
  (->
    sink-fault?
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    natural?
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof #/list/c sink-located-string? sink-authorized-name?)
      sink-extfx?)
    sink-extfx?)
  (sink-extfx-claim-freshen unique-name #/fn unique-name
  #/sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    n n
    rev-results (list)
    
    (expect (nat->maybe n) (just next-n)
      (then unique-name qualify text-input-stream
        (reverse rev-results))
    #/sink-extfx-read-whitespace text-input-stream
    #/fn text-input-stream whitespace
    #/sink-extfx-read-maybe-identifier
      fault qualify text-input-stream pre-qualify
    #/fn text-input-stream maybe-id
    #/mat maybe-id (just id)
      (next unique-name qualify text-input-stream next-n
        (cons id rev-results))
    
    ; We skip comments, and if there's a closing bracket, we cause an
    ; error. We do this by calling `sink-extfx-read-cexprs` with an
    ; output stream that causes errors if any expressions are actually
    ; found.
    #/sink-extfx-claim-and-split unique-name 2
    #/dissectfn (list unique-name-stream unique-name)
    #/sink-extfx-make-cexpr-sequence-output-stream
      (make-fault-internal)
      unique-name-stream
      (trivial)
      (fn state cexpr then
        (dissect state (trivial)
        #/sink-extfx-cene-err fault "Expected an identifier but found an expression"))
    #/fn output-stream unwrap
    #/sink-extfx-read-cexprs
      fault unique-name qualify text-input-stream output-stream
    #/fn unique-name qualify text-input-stream output-stream
    #/unwrap (make-fault-internal) output-stream #/dissectfn (trivial)
    
    #/next unique-name qualify text-input-stream n rev-results)))

(define/contract
  (sink-extfx-sink-text-input-stream-split-after-custom-matching-brackets
    fault
    text-input-stream
    non-bracket-characters-pattern
    bracket-patterns
    overall-open-fault
    overall-close-pattern
    overall-accepts-eof
    on-unexpected-eof-likely-extra-open
    on-unexpected-text-likely-extra-close
    on-success)
  (->
    sink-fault?
    sink-text-input-stream?
    optimized-textpat?
    (listof #/list/c optimized-textpat? optimized-textpat? boolean?)
    sink-fault?
    optimized-textpat?
    boolean?
    (-> sink-fault? sink-text-input-stream? sink-text-input-stream?
      sink-extfx?)
    (-> sink-text-input-stream? sink-text-input-stream? sink-extfx?)
    (-> sink-text-input-stream? sink-text-input-stream? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-split text-input-stream
    (fn in then
      (w-loop next-consumption in in brackets-expected (list)
        (sink-extfx-optimized-textpat-read-located
          non-bracket-characters-pattern in
        #/fn in maybe-str
        #/expect maybe-str (just _)
          (error "Expected non-bracket-characters-pattern to always match")
        #/w- try-to-consume-non-eof
          (fn in open-fault close-pattern on-consumption-succeeded
            (sink-extfx-optimized-textpat-read-located
              close-pattern in
            #/fn in maybe-str
            #/mat maybe-str (just _)
              (on-consumption-succeeded in)
            #/sink-extfx-peek-whether-eof in #/fn in is-eof
            #/if is-eof
              (then (make-fault-internal) in #/fn during after
                (on-unexpected-eof-likely-extra-open
                  open-fault during after))
            #/w-loop next-possible-open
              in in
              bracket-patterns bracket-patterns
              
              (expect bracket-patterns
                (cons open-and-close bracket-patterns)
                (then (make-fault-internal) in
                  on-unexpected-text-likely-extra-close)
              #/dissect open-and-close
                (list open-pattern close-pattern accepts-eof)
              #/
                (fn then
                  
                  ; If the opening bracket we're attempting to read
                  ; accepts end-of-file as its closing bracket, then
                  ; unmatched opening bracket errors will be reported
                  ; in terms of the existing bracket.
                  (if accepts-eof
                    (then in open-fault)
                  
                  ; Otherwise, they'll be reported in terms of the
                  ; source location just before the open bracket we're
                  ; about to read, so we capture that source location
                  ; now.
                  #/sink-extfx-read-fault in then))
              #/fn in open-fault
              #/sink-extfx-optimized-textpat-read-located
                open-pattern in
              #/fn in maybe-str
              #/mat maybe-str (just _)
                (next-consumption in
                  (cons (list open-fault close-pattern accepts-eof)
                    brackets-expected))
              #/next-possible-open in bracket-patterns)))
        #/w- try-to-consume
          (fn in bracket-expected on-consumption-succeeded
            (dissect bracket-expected
              (list open-fault close-pattern accepts-eof)
            #/if accepts-eof
              (sink-extfx-peek-whether-eof in #/fn in is-eof
              #/if is-eof
                (on-consumption-succeeded in)
              #/try-to-consume-non-eof in open-fault close-pattern
                on-consumption-succeeded)
              (try-to-consume-non-eof in open-fault close-pattern
                on-consumption-succeeded)))
        #/mat brackets-expected
          (cons first-bracket-expected brackets-expected)
          (try-to-consume in first-bracket-expected #/fn in
            (next-consumption in brackets-expected))
          (try-to-consume in
            (list
              overall-open-fault
              overall-close-pattern
              overall-accepts-eof)
          #/fn in
            (then (make-fault-internal) in on-success)))))
  #/fn during afterward on-this-situation
  #/on-this-situation during afterward))

(define
  sink-extfx-sink-text-input-stream-split-after-matching-brackets-non-bracket-characters-pat
  (optimize-textpat
    (textpat-star #/textpat-one-not-in-string "[]()")))

; TODO LEXICAL UNITS: Use this to implement anything described in
; notes/20190731-lexical-units.md with "Pre-reads its lexical extent
; to find matching brackets. As of writing this comment, these are the
; operations that should use this:
;
;   declare-matched-brackets-section-separately
;   import-from-declaration
;   import-from-file
;   export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag
;   export-metadata-op-and-bounded-expr-op
;   def-unexportable-unceremonious-export-metadata-op-as-constant
;
(define/contract
  (sink-extfx-sink-text-input-stream-split-after-matching-brackets
    fault
    text-input-stream
    overall-open-fault
    overall-close-pattern
    overall-accepts-eof
    then)
  (->
    sink-fault?
    sink-text-input-stream?
    sink-fault?
    optimized-textpat?
    boolean?
    (-> sink-text-input-stream? sink-text-input-stream? sink-extfx?)
    sink-extfx?)
  (sink-extfx-sink-text-input-stream-freshen text-input-stream
    (cenegetfx-cene-err (make-fault-internal) "Expected text-input-stream to be an unspent text input stream")
  #/fn text-input-stream
  #/sink-extfx-sink-text-input-stream-split-after-custom-matching-brackets
    fault
    text-input-stream
    sink-extfx-sink-text-input-stream-split-after-matching-brackets-non-bracket-characters-pat
    (list
      (list |pat "["| |pat "]"| #f)
      (list |pat "("| |pat ")"| #f))
    overall-open-fault
    overall-close-pattern
    overall-accepts-eof
    (fn open-fault during after
      (w- open-read-fault (make-fault-read fault open-fault)
      #/sink-extfx-cene-err open-read-fault "Encountered an unmatched opening bracket"))
    (fn during after
      (sink-extfx-read-fault after #/fn after close-fault
      #/w- close-read-fault (make-fault-read fault close-fault)
      #/sink-extfx-cene-err close-read-fault "Encountered an unmatched closing bracket"))
    then))
