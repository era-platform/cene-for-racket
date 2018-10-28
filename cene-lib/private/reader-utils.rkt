#lang parendown racket/base

; cene/private/reader-utils
;
; An assortment of utilities for parsing Cene reader macro bodies
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


(require #/only-in racket/contract/base -> list/c listof or/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts dissect expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list list-map nat->maybe)
(require #/only-in lathe-comforts/maybe just)
(require #/only-in lathe-comforts/struct struct-easy)

(require cene/private)


(provide
  sink-name-for-local-variable
  id-or-expr-id
  id-or-expr-expr
  id-or-expr->cexpr
  sink-effects-read-bounded-ids-and-exprs
  sink-effects-read-bounded-cexprs
  sink-effects-read-bounded-specific-number-of-cexprs
  sink-effects-read-leading-specific-number-of-cexprs
  sink-effects-read-leading-specific-number-of-identifiers)



(define/contract (sink-name-for-local-variable inner-name)
  (-> sink-name? sink-name?)
  (sink-name-rep-map inner-name #/fn n #/list 'name:local-variable n))

(struct-easy (id-or-expr-id located-string qualified-name))
(struct-easy (id-or-expr-expr expr))

(define/contract (id-or-expr->cexpr id-or-expr)
  (-> (or/c id-or-expr-id? id-or-expr-expr?) sink-cexpr?)
  (mat id-or-expr (id-or-expr-id located-string qualified-name)
    ; TODO: Wrap this in a located cexpr.
    (sink-cexpr-var #/sink-authorized-name-get-name qualified-name)
  #/dissect id-or-expr (id-or-expr-expr cexpr)
    cexpr))

(define/contract
  (sink-effects-read-ids-and-exprs-onto
    unique-name qualify text-input-stream rev-results pre-qualify
    then)
  (->
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    (listof #/or/c id-or-expr-id? id-or-expr-expr?)
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof #/or/c id-or-expr-id? id-or-expr-expr?)
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-maybe-identifier
    qualify text-input-stream pre-qualify
  #/fn text-input-stream maybe-id
  #/mat maybe-id (just #/list located-string qualified-name)
    (then unique-name qualify text-input-stream
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
  #/then unique-name qualify text-input-stream rev-results))

; This reads identifiers and cexprs until it gets to a closing
; bracket.
(define/contract
  (sink-effects-read-bounded-ids-and-exprs
    unique-name qualify text-input-stream pre-qualify then)
  (->
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name?
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
    #/sink-effects-read-ids-and-exprs-onto
      unique-name qualify text-input-stream rev-results pre-qualify
    #/fn unique-name qualify text-input-streams rev-results
    #/next unique-name qualify text-input-streams rev-results)))

; This reads cexprs until it gets to a closing bracket.
(define/contract
  (sink-effects-read-bounded-cexprs
    unique-name qualify text-input-stream then)
  (->
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof sink-cexpr?)
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-bounded-ids-and-exprs
    unique-name qualify text-input-stream sink-name-for-local-variable
  #/fn unique-name qualify text-input-stream ids-and-exprs
  #/then unique-name qualify text-input-stream
  #/list-map ids-and-exprs #/fn id-or-expr
    (id-or-expr->cexpr id-or-expr)))

; This reads cexprs until it gets to a closing bracket, and it
; verifies that there are precisely `n` of them.
(define/contract
  (sink-effects-read-bounded-specific-number-of-cexprs
    unique-name qualify text-input-stream n then)
  (->
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    natural?
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof sink-cexpr?)
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

; This reads precisely `n` identifiers and cexprs, and it causes an
; error if it reaches a closing bracket first or if it reads too many
; cexprs in one cexpr read.
(define/contract
  (sink-effects-read-leading-specific-number-of-ids-and-exprs
    unique-name qualify text-input-stream n pre-qualify then)
  (->
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    natural?
    (-> sink-name? sink-name?)
    (->
      sink-authorized-name?
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
    
    ; TODO: The way we're calling `length` on each loop iteration is a
    ; painter's algorithm. Let's see if we can stop doing that. We'll
    ; probably need to thread `n` through
    ; `sink-effects-read-ids-and-exprs-onto`.
    (if (= n #/length rev-results)
      (then unique-name qualify text-input-stream
        (reverse rev-results))
    #/if (< n #/length rev-results)
      (cene-err "Encountered a single operation that expanded to too many expressions while expecting a specific number of identifiers and expressions")
    #/sink-effects-read-whitespace text-input-stream
    #/fn text-input-stream whitespace
    #/sink-effects-peek-whether-eof text-input-stream
    #/fn text-input-stream is-eof
    #/if is-eof
      (cene-err "Encountered end of file while expecting an identifier or an expression")
    #/sink-effects-peek-whether-closing-bracket text-input-stream
    #/fn text-input-stream is-closing-bracket
    #/if is-closing-bracket
      (cene-err "Encountered a closing bracket while expecting an identifier or an expression")
    #/sink-effects-read-ids-and-exprs-onto
      unique-name qualify text-input-stream rev-results pre-qualify
    #/fn unique-name qualify text-input-streams rev-results
    #/next unique-name qualify text-input-streams rev-results)))

; This reads precisely `n` cexprs, and it causes an error if it
; reaches a closing bracket first or if it reads too many cexprs in
; one cexpr read.
(define/contract
  (sink-effects-read-leading-specific-number-of-cexprs
    unique-name qualify text-input-stream n then)
  (->
    sink-authorized-name?
    sink?
    sink-text-input-stream?
    natural?
    (->
      sink-authorized-name?
      sink?
      sink-text-input-stream?
      (listof sink-cexpr?)
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/sink-effects-read-leading-specific-number-of-ids-and-exprs
    unique-name qualify text-input-stream n
    sink-name-for-local-variable
  #/fn unique-name qualify text-input-stream ids-and-exprs
  #/then unique-name qualify text-input-stream
  #/list-map ids-and-exprs #/fn id-or-expr
    (id-or-expr->cexpr id-or-expr)))

; This reads precisely `n` whitespace-separated identifiers, and it
; causes an error if it runs out of identifiers to read too soon.
(define/contract
  (sink-effects-read-leading-specific-number-of-identifiers
    qualify text-input-stream n pre-qualify then)
  (->
    sink?
    sink-text-input-stream?
    natural?
    (-> sink-name? sink-name?)
    (->
      sink-text-input-stream?
      (listof #/list/c sink-located-string? sink-authorized-name?)
      sink-effects?)
    sink-effects?)
  (begin (assert-can-get-cene-definitions!)
  #/w-loop next
    text-input-stream text-input-stream
    n n
    rev-results (list)
    
    (expect (nat->maybe n) (just n)
      (then text-input-stream #/reverse rev-results)
    #/sink-effects-read-whitespace text-input-stream
    #/fn text-input-stream whitespace
    #/sink-effects-read-maybe-identifier
      qualify text-input-stream pre-qualify
    #/fn text-input-stream maybe-id
    #/expect maybe-id (just id)
      (cene-err "Expected an identifier")
    #/next text-input-stream n #/cons id rev-results)))
