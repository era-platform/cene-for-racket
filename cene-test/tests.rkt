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


(require #/only-in racket/format ~a)

(require rackunit)

(require #/only-in lathe-comforts dissect dissectfn expect fn w-)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/trivial trivial)

(require #/only-in effection/extensibility/base
  authorized-name-subname error-definer-from-message
  extfx-ct-continue)
(require #/only-in effection/extensibility/unsafe
  run-extfx! run-extfx-result-failure run-extfx-result-success?)

(require cene)

; (We provide nothing from this module.)


(define (cene-run-string-sample code-string)
  (w- fault (make-fault-internal)
  #/run-extfx!
    (error-definer-from-message
      "Internal error: Expected the cene-run-string-sample continuation ticket to be written to")
    (fn ds unique-name then
      (extfx-with-gets-from ds unique-name #/fn unique-name
      #/sink-extfx-run!
      #/sink-extfx-claim-and-split
        (sink-authorized-name unique-name)
        4
      #/dissectfn
        (list
          unique-name-root
          unique-name-essentials
          unique-name-package
          unique-name-sample)
      #/w- qualify
        (fn name #/sink-authorized-name-subname name unique-name-root)
      #/sink-extfx-fuse
        (sink-extfx-init-essentials fault unique-name-essentials)
        (sink-extfx-init-package fault unique-name-package qualify)
        (sink-extfx-run-string
          fault unique-name-sample qualify code-string)
      #/make-sink-extfx #/fn
        (extfx-ct-continue then
          (error-definer-from-message
            "Internal error: Expected the cene-run-string-sample continuation ticket to be written to only once")
          (trivial))))))

(define (cene-code-failure code-string)
  (w- result (cene-run-string-sample code-string)
  #/if (run-extfx-result-success? result) (nothing)
  #/just result))


(check-equal? (cene-code-failure "") (nothing)
  "Running nothing works")

(check-equal?
  (cene-code-failure
    "
    \\= This is a comment.
    
    \\= This is another comment.
    
    ")
  (nothing)
  "Running nothing but line comments works")

(check-equal?
  (dissect (cene-run-string-sample ")")
    (run-extfx-result-failure errors)
    #t)
  #t
  "Running an unmatched closing paren causes a Cene error, not a Racket error")

; TODO: See if we can stop relying on the `write` behavior of the
; internal structure type `sink-struct` and the Effection internal
; structure types `run-extfx-errors`, `error-definer-from-message`,
; and `name` here.
(check-equal?
  (dissect (cene-run-string-sample "(follow-heart/trivial)")
    (run-extfx-result-failure errors)
    (not #/not #/regexp-match #px"trivial" #/~a errors))
  #t
  "Calling this Cene implementation's `follow-heart` causes a Cene error with a message based on the given value")

; TODO: See if we can stop relying on the `write` behavior of the
; Effection internal structure types `run-extfx-errors` and
; `error-definer-from-message` here.
(check-equal?
  (dissect
    (cene-run-string-sample
      "
      (c (fn f /c f /nil) /fn-blame bl -
        [follow-heart /clamor-err bl /str-prim:Hello world])
      
      ")
    (run-extfx-result-failure errors)
    (not #/not #/regexp-match #px"Hello world" #/~a errors))
  #t
  "Calling Cene's `follow-heart` with an appropriate `clamor-err` value causes a Cene error with a custom message")

(check-equal?
  (cene-code-failure
    "(directive/fn unique-name qualify /extfx-noop)")
  (nothing)
  "Running a single top-level command that does nothing works")

(check-equal?
  (cene-code-failure
    "
    (directive/fn unique-name qualify
      (let ignored
        (fn x
          (case x
            \\= This comment makes sure comments are allowed before
            \\= the struct metadata tag of a case pattern.
            just
            \\= This comment makes sure comments are allowed before a
            \\= variable binding of a case pattern.
            v
            v
            x))
      /extfx-noop))
    
    ")
  (nothing)
  "Comments are allowed in case patterns")

; TODO: Write more unit tests.
