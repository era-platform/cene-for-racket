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


(require #/only-in racket/contract/base -> listof or/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts dissect fn mat w- w-loop)
(require #/only-in lathe-comforts/list list-map)
(require #/only-in lathe-comforts/maybe just)
(require #/only-in lathe-comforts/struct struct-easy)

(require #/only-in effection/order name?)

(require cene/private)


(provide
  id-or-expr-id
  id-or-expr-expr
  sink-effects-read-bounded-ids-and-exprs
  id-or-expr->cexpr
  sink-effects-read-specific-number-of-cexprs)



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
