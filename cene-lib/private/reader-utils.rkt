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
    fault
    unique-name-stream
    rev-results
    (fn rev-results cexpr then
      (then #/cons (id-or-expr-expr cexpr) rev-results))
  #/fn output-stream unwrap
  #/sink-extfx-read-cexprs
    fault unique-name qualify text-input-stream output-stream
  #/fn unique-name qualify text-input-stream output-stream
  #/unwrap output-stream #/fn rev-results
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
  #/w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    rev-results (list)
    
    (sink-extfx-read-whitespace fault text-input-stream
    #/fn text-input-stream whitespace
    #/sink-extfx-peek-whether-eof fault text-input-stream
    #/fn text-input-stream is-eof
    #/if is-eof
      ; TODO FAULT: Make this `fault` more specific.
      (sink-extfx-cene-err fault "Encountered end of file while expecting any number of identifiers and expressions preceding a closing bracket")
    #/sink-extfx-peek-whether-closing-bracket fault text-input-stream
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
  (sink-extfx-read-bounded-ids-and-exprs
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
  (sink-extfx-read-bounded-cexprs
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
    #/sink-extfx-read-whitespace fault text-input-stream
    #/fn text-input-stream whitespace
    #/sink-extfx-peek-whether-eof fault text-input-stream
    #/fn text-input-stream is-eof
    #/if is-eof
      ; TODO FAULT: Make this `fault` more specific.
      (sink-extfx-cene-err fault "Encountered end of file while expecting an identifier or an expression")
    #/sink-extfx-peek-whether-closing-bracket fault text-input-stream
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
  (sink-extfx-read-leading-specific-number-of-ids-and-exprs
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
  #/w-loop next
    unique-name unique-name
    qualify qualify
    text-input-stream text-input-stream
    n n
    rev-results (list)
    
    (expect (nat->maybe n) (just next-n)
      (then unique-name qualify text-input-stream
        (reverse rev-results))
    #/sink-extfx-read-whitespace fault text-input-stream
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
      fault
      unique-name-stream
      (trivial)
      (fn state cexpr then
        (dissect state (trivial)
        #/sink-extfx-cene-err fault "Expected an identifier but found an expression"))
    #/fn output-stream unwrap
    #/sink-extfx-read-cexprs
      fault unique-name qualify text-input-stream output-stream
    #/fn unique-name qualify text-input-stream output-stream
    #/unwrap output-stream #/dissectfn (trivial)
    
    #/next unique-name qualify text-input-stream n rev-results)))
