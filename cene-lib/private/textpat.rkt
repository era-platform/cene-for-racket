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


; TODO BUILTINS: Use these utilities as the implementation for some
; Cene built-ins.

; TODO: See if these utilities should be factored out into their own
; library.

(struct-easy (textpat get-data))
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

(define/contract (textpat-result? v)
  (-> any/c boolean?)
  (mat v (textpat-result-matched stop) (natural? stop)
  #/mat v (textpat-result-failed) #t
  #/mat v (textpat-result-passed-end) #t
    #f))

(define/contract (textpat-trivial necessary)
  (-> immutable-string? textpat?)
  (textpat #/fn #/textpat-data
    (just #/fn next
      (string->immutable-string #/string-append necessary next))
    (just necessary)
    (nothing)))

(define/contract (textpat-optional-trivial necessary)
  (-> immutable-string? textpat?)
  (textpat #/fn #/textpat-data
    (just #/fn next
      (string->immutable-string #/string-append
        "(?:" necessary next "|$)"))
    (just necessary)
    (nothing)))

(define/contract (textpat-give-up)
  (-> textpat?)
  (textpat-trivial ".^"))

(define/contract (textpat-empty)
  (-> textpat?)
  (textpat-trivial ""))

(define/contract (textpat-from-string str)
  (-> immutable-string? textpat?)
  (textpat #/fn #/textpat-data
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
  (textpat-optional-trivial #/string->immutable-string #/string-append
    "(?:.^"
    (regexp-replace #rx"." str #/fn scalar-string
      (string-append "|" (regexp-quote scalar-string)))))

(define/contract (textpat-one)
  (-> textpat?)
  (textpat-optional-trivial "."))

(define/contract (compile-textpat t)
  (-> textpat? textpat-data?)
  (dissect t (textpat get-data)
  #/dissect (get-data)
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
  (textpat #/fn
    (dissect (compile-textpat condition)
      (textpat-data c-opt c-nec #/just c-make-func)
    #/dissect (compile-textpat then)
      (textpat-data t-opt t-nec #/just t-make-func)
    #/dissect (compile-textpat else)
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
  (textpat #/fn
    (dissect (compile-textpat condition)
      (textpat-data c-opt c-nec #/just c-make-func)
    #/dissect (compile-textpat body)
      (textpat-data b-opt b-nec #/just b-make-func)
    ; TODO: When the concatenation of `c-nec` and `b-nec` can match
    ; the empty string, Racket doesn't let us use the * operator on
    ; it. We shouldn't let it happen either, but we should raise a
    ; friendly error message about it.
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
          (w-loop next start start encountered-empty #f
            (w- c-result (c-func str start stop)
            #/mat c-result (textpat-result-failed)
              (textpat-result-matched start)
            #/mat c-result (textpat-result-passed-end)
              (textpat-result-passed-end)
            #/dissect c-result (textpat-result-matched c-stop)
            #/if encountered-empty
              (textpat-result-failed)
            #/w- b-result (b-func str c-stop stop)
            #/expect b-result (textpat-result-matched b-stop) b-result
            ; TODO: Stop maintaining an `encountered-empty` variable,
            ; and just cause an error if it would ever be true.
            #/next b-stop (= start b-stop))))))))

(define/contract (textpat-until body condition)
  (-> textpat? textpat? textpat?)
  (textpat #/fn
    (dissect (compile-textpat body)
      (textpat-data b-opt b-nec #/just b-make-func)
    #/dissect (compile-textpat condition)
      (textpat-data c-opt c-nec #/just c-make-func)
    ; TODO: When `b-nec` can match the empty string, Racket doesn't
    ; let us use the * operator on it. We shouldn't let it happen
    ; either, but we should raise a friendly error message about it.
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
          (w-loop next start start encountered-empty #f
            (w- c-result (c-func str start stop)
            #/expect c-result (textpat-result-failed) c-result
            #/if encountered-empty
              (textpat-result-failed)
            #/w- b-result (b-func str start stop)
            #/expect b-result (textpat-result-matched b-stop) b-result
            ; TODO: Stop maintaining an `encountered-empty` variable,
            ; and just cause an error if it would ever be true.
            #/next b-stop (= start b-stop))))))))

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
  #/textpat-optional-trivial
  #/string->immutable-string #/string-append
    "[" a-str "-" b-str "]"))

(define/contract (optimize-textpat t)
  (-> textpat? optimized-textpat?)
  (dissect (compile-textpat t) (textpat-data opt nec #/just make-func)
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
