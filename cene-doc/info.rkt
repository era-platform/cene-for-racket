#lang info

(define collection "cene")

(define deps (list "base"))
(define build-deps
  (list "cene-lib" "parendown-lib" "racket-doc" "scribble-lib"))

(define scribblings (list (list "scribblings/cene.scrbl" (list))))
