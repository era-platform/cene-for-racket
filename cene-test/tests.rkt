#lang parendown racket/base

; cene/tests
;
; Unit tests.

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


(require rackunit)

(require #/only-in lathe-comforts dissect expect)

(require cene)

; (We provide nothing from this module.)


(define (cene-code-works code-string)
  (expect (cene-run-string (cene-runtime-essentials) code-string)
    (list cene-runtime #/list)
    #f
  #/cene-runtime? cene-runtime))

(define (cene-code-get-errors code-string)
  (dissect (cene-run-string (cene-runtime-essentials) code-string)
    (list cene-runtime errors)
    errors))

(define (cene-code-get-single-error code-string)
  (expect (cene-code-get-errors code-string) (list e)
    (error "Expected a single error")
    e))


(check-equal? (cene-runtime? #/cene-runtime-essentials) #t
  "A call to `cene-runtime-essentials` returns successfully")

(check-equal? (cene-code-works "") #t
  "Running nothing works")

(check-equal?
  (cene-code-works
    "
    \\= This is a comment.
    
    \\= This is another comment.
    
    ")
  #t
  "Running nothing but line comments works")

; TODO: Make it so errors like this are actually collected into the
; list of errors returned by `cene-run-string` rather than letting
; them escape as Racket exceptions.
(check-exn
  (lambda (e)
    (expect e (exn:fail:cene message marks clamor) #f
    #/not #/not #/regexp-match #px"trivial" message))
  (lambda ()
    (cene-code-works "(follow-heart/trivial)"))
  "Calling Cene's `follow-heart` raises an `exn:fail:cene` exception in Racket")

; TODO: Write more unit tests.
