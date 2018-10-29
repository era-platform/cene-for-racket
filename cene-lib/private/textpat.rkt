#lang parendown racket/base

; cene/private/textpat
;
; A system of reasonably efficient parser combinators which operate on
; partial input and which never backtrack. The result of a parse is
; either a failure, a success (with a stop position), or a report that
; the end of the input was reached before a result was determined.

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


(require #/only-in racket/contract/base -> any/c)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)
(require #/only-in racket/string string-contains?)

(require #/only-in lathe-comforts dissect expect fn mat w- w-loop)
(require #/only-in lathe-comforts/maybe just maybe-bind nothing)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct struct-easy)

(provide
  
  (struct-out textpat-result-matched)
  (struct-out textpat-result-failed)
  (struct-out textpat-result-passed-end)
  (rename-out
    [-textpat? textpat?]
    [-optimized-textpat? optimized-textpat?])
  textpat-result?
  
  textpat-give-up
  textpat-empty
  textpat-from-string
  textpat-one-in-string
  textpat-one
  textpat-if
  textpat-while
  textpat-until
  textpat-one-in-range
  
  textpat-has-empty?
  optimize-textpat
  optimized-textpat-match)


; TODO: See if this file should be factored out into its own Racket
; library.


(struct-easy (textpat has-empty get-data))
(struct-easy
  (textpat-data maybe-optional maybe-necessary maybe-make-func))
(struct-easy (optimized-textpat func))
(struct-easy (textpat-result-matched stop) #:equal)
(struct-easy (textpat-result-failed) #:equal)
(struct-easy (textpat-result-passed-end) #:equal)

; NOTE: This is a version of `textpat?` which doesn't satisfy
; `struct-predicate-procedure?`.
(define/contract (-textpat? v)
  (-> any/c boolean?)
  (textpat? v))

; NOTE: This is a version of `optimized-textpat?` which doesn't
; satisfy `struct-predicate-procedure?`.
(define/contract (-optimized-textpat? v)
  (-> any/c boolean?)
  (optimized-textpat? v))

(define/contract (textpat-result? v)
  (-> any/c boolean?)
  (mat v (textpat-result-matched stop) (natural? stop)
  #/mat v (textpat-result-failed) #t
  #/mat v (textpat-result-passed-end) #t
    #f))

(define/contract (textpat-trivial has-empty necessary)
  (-> boolean? immutable-string? textpat?)
  (textpat has-empty #/fn #/textpat-data
    (just #/fn next
      (string->immutable-string #/string-append necessary next))
    (just necessary)
    (nothing)))

(define/contract (textpat-optional-trivial has-empty necessary)
  (-> boolean? immutable-string? textpat?)
  (textpat has-empty #/fn #/textpat-data
    (just #/fn next
      (string->immutable-string #/string-append
        "(?:" necessary next "|$)"))
    (just necessary)
    (nothing)))

(define/contract (textpat-give-up)
  (-> textpat?)
  (textpat-trivial #f ".^"))

(define/contract (textpat-empty)
  (-> textpat?)
  (textpat-trivial #t ""))

(define/contract (textpat-from-string str)
  (-> immutable-string? textpat?)
  (textpat (= 0 #/string-length str) #/fn #/textpat-data
    (just #/fn next
      (string->immutable-string #/string-append
        (regexp-replace #rx"." str #/fn scalar-string
          (string-append "(?:" (regexp-quote scalar-string)))
        next
        (regexp-replace #rx"." str "|$)")))
    (just #/regexp-quote str)
    (nothing)))

(define/contract (textpat-one-in-string str)
  (-> immutable-string? textpat?)
  (textpat-optional-trivial #f
  #/string->immutable-string #/string-append
    "(?:.^"
    (regexp-replace #rx"." str #/fn scalar-string
      (string-append "|" (regexp-quote scalar-string)))))

(define/contract (textpat-one)
  (-> textpat?)
  (textpat-optional-trivial #f "."))

(define/contract (compile-textpat-data data)
  (-> textpat-data? textpat-data?)
  (dissect data
    (textpat-data maybe-optional maybe-necessary maybe-make-func)
  #/textpat-data maybe-optional maybe-necessary #/just #/fn
    (w- delegate
      (fn
        (dissect maybe-make-func (just make-func)
        #/make-func))
    #/expect maybe-optional (just optional) (delegate)
    #/expect maybe-necessary (just necessary) (delegate)
    #/w- compiled
      (regexp #/string-append
        "(?:" necessary "()|" (optional "") "())?")
    #/fn str start stop
      (dissect (regexp-match-positions compiled str start stop)
        (list
          (cons matched-start matched-stop)
          matched-nec
          matched-opt)
      #/if matched-nec (textpat-result-matched matched-stop)
      #/if matched-opt (textpat-result-passed-end)
      #/textpat-result-failed))))

(define/contract (textpat-if condition then else)
  (-> textpat? textpat? textpat? textpat?)
  (dissect condition (textpat c-has-empty c-get-data)
  #/dissect then (textpat t-has-empty t-get-data)
  #/dissect else (textpat e-has-empty e-get-data)
  #/textpat (or (and c-has-empty t-has-empty) e-has-empty) #/fn
    (dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-opt c-nec #/just c-make-func)
    #/dissect (compile-textpat-data #/t-get-data)
      (textpat-data t-opt t-nec #/just t-make-func)
    #/dissect (compile-textpat-data #/e-get-data)
      (textpat-data e-opt e-nec #/just e-make-func)
    #/textpat-data
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-opt #/fn c-opt
      #/maybe-bind t-opt #/fn t-opt
      #/maybe-bind e-opt #/fn e-opt
      #/just #/fn next
        (string->immutable-string #/string-append
          "(?:"
            "(?!" c-nec ")(?:"
              ; We may run out of room matching the condition.
              (c-opt "") "|"
              ; We may match the else clause.
              (e-opt next)
            ")|"
            ; We may match the then clause.
            c-nec (t-opt next) ""
          ")"))
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind t-nec #/fn t-nec
      #/maybe-bind e-nec #/fn e-nec
      #/just #/string->immutable-string #/string-append
        "(?:"
          ; We may match the then clause.
          c-nec t-nec "|"
          ; We may match the else clause.
          "(?!" c-nec ")" e-nec
        ")")
      (just #/fn
        (w- c-func (c-make-func)
        #/w- t-func (t-make-func)
        #/w- e-func (e-make-func)
        #/fn str start stop
          (w- c-result (c-func str start stop)
          #/mat c-result (textpat-result-matched c-stop)
            (t-func str c-stop stop)
          #/mat c-result (textpat-result-failed)
            (e-func str start stop)
          #/dissect c-result (textpat-result-passed-end)
            (textpat-result-passed-end)))))))

(define/contract (textpat-while condition body)
  (-> textpat? textpat? textpat?)
  (dissect condition (textpat c-has-empty c-get-data)
  #/dissect body (textpat b-has-empty b-get-data)
  ; NOTE: When the concatenation of `c-nec` and `b-nec` can match the
  ; empty string, Racket doesn't let us use the * operator on it. But
  ; we don't allow that case.
  #/if (and c-has-empty b-has-empty)
    (error "Did not expect both condition and body to match the empty string")
  #/textpat #t #/fn
    (dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-opt c-nec #/just c-make-func)
    #/dissect (compile-textpat-data #/b-get-data)
      (textpat-data b-opt b-nec #/just b-make-func)
    #/textpat-data
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-opt #/fn c-opt
      #/maybe-bind b-nec #/fn b-nec
      #/maybe-bind b-opt #/fn b-opt
      #/just #/fn next
        (string->immutable-string #/string-append
          "(?:" c-nec b-nec ")*(?:"
            "(?!" c-nec ")(?:"
              ; We may run out of room matching the condition.
              (c-opt "") "|"
              ; We may have a complete match.
              next
            ")|"
            ; We may run out of room matching the body.
            c-nec "(?!" + b-nec ")" (b-opt "") ""
          ")"))
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind b-nec #/fn b-nec
      #/just #/string->immutable-string #/string-append
        "(?:" c-nec b-nec ")*(?!" c-nec ")")
      (just #/fn
        (w- c-func (c-make-func)
        #/w- b-func (b-make-func)
        #/fn str start stop
          (w-loop next start start
            (w- c-result (c-func str start stop)
            #/mat c-result (textpat-result-failed)
              (textpat-result-matched start)
            #/mat c-result (textpat-result-passed-end)
              (textpat-result-passed-end)
            #/dissect c-result (textpat-result-matched c-stop)
            #/w- b-result (b-func str c-stop stop)
            #/expect b-result (textpat-result-matched b-stop) b-result
            #/if (= start b-stop)
              (error "Internal error: It turns out condition and body can both match the empty string after all")
            #/next b-stop)))))))

(define/contract (textpat-until body condition)
  (-> textpat? textpat? textpat?)
  (dissect condition (textpat c-has-empty c-get-data)
  #/dissect body (textpat b-has-empty b-get-data)
  ; NOTE: When `b-nec` can match the empty string, Racket doesn't let
  ; us use the * operator on it. But we don't allow that case.
  #/if b-has-empty
    (error "Did not expect body to match the empty string")
  #/textpat c-has-empty #/fn
    (dissect (compile-textpat-data #/b-get-data)
      (textpat-data b-opt b-nec #/just b-make-func)
    #/dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-opt c-nec #/just c-make-func)
    #/textpat-data
      (maybe-bind b-nec #/fn b-nec
      #/maybe-bind b-opt #/fn b-opt
      #/maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-opt #/fn c-opt
      #/just #/fn next
        (string->immutable-string #/string-append
          "(?:"
            "(?!" (c-opt "") ")" b-nec
          ")*(?:"
            "(?!" c-nec ")(?:"
              ; We may run out of room matching the condition.
              (c-opt "") "|"
              ; We may run out of room matching the body.
              "(?!" b-nec ")" (b-opt "") ""
            "}|"
            ; We may have a complete match.
            c-nec next
          ")"))
      (maybe-bind b-nec #/fn b-nec
      #/maybe-bind c-nec #/fn c-nec
      #/just #/string->immutable-string #/string-append
        "(?:(?!" c-nec ")" b-nec ")*" c-nec)
      (just #/fn
        (w- b-func (b-make-func)
        #/w- c-func (c-make-func)
        #/fn str start stop
          (w-loop next start start
            (w- c-result (c-func str start stop)
            #/expect c-result (textpat-result-failed) c-result
            #/w- b-result (b-func str start stop)
            #/expect b-result (textpat-result-matched b-stop) b-result
            #/if (= start b-stop)
              (error "Internal error: It turns out body can match the empty string after all")
            #/next b-stop)))))))

(define/contract (textpat-or a b)
  (-> textpat? textpat? textpat?)
  (textpat-if a (textpat-empty) b))

(define/contract (textpat-one-in-range a b)
  (-> char? char? textpat?)
  (w- special-range-chars "]-%\\"
  #/w- an (char->integer a)
  #/w- bn (char->integer b)
  #/expect (< an bn) #t (textpat-give-up)
  #/w- a-str (string->immutable-string #/string a)
  #/if (string-contains? special-range-chars a-str)
    (textpat-or (textpat-one-in-string a-str)
    #/textpat-one-in-range (integer->char #/add1 an) b)
  #/w- b-str (string->immutable-string #/string b)
  #/if (string-contains? special-range-chars b-str)
    (textpat-or (textpat-one-in-string b-str)
    #/textpat-one-in-range (integer->char #/add1 an) b)
  #/textpat-optional-trivial #f
  #/string->immutable-string #/string-append
    "[" a-str "-" b-str "]"))

; NOTE: This is a version of `textpat-as-empty` that doesn't satisfy
; `struct-accessor-procedure?`.
(define/contract (textpat-has-empty? t)
  (-> textpat? boolean?)
  (dissect t (textpat has-empty get-data)
    has-empty))

(define/contract (optimize-textpat t)
  (-> textpat? optimized-textpat?)
  (dissect t (textpat has-empty get-data)
  #/dissect (compile-textpat-data #/get-data)
    (textpat-data opt nec #/just make-func)
  #/optimized-textpat #/make-func))

(define/contract (optimized-textpat-match ot str start stop)
  (-> optimized-textpat? string? natural? natural? textpat-result?)
  (dissect ot (optimized-textpat func)
  #/w- n (string-length str)
  #/expect (<= start n) #t
    (error "Expected start to be an index of the string")
  #/expect (<= stop n) #t
    (error "Expected stop to be an index of the string")
  #/expect (<= start stop) #t
    (error "Expected start to be less than or equal to stop")
  #/func str start stop))
