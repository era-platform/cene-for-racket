#lang parendown racket/base

; cene/private/textpat
;
; A system of reasonably efficient parser combinators which operate on
; partial input and which never backtrack. The result of a parse is
; either a failure, a success (with a stop position), or a report that
; the end of the input was reached before a result was determined.

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


(require #/only-in racket/contract/base
  -> ->* any/c contract-out ->i listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)
(require #/only-in racket/port peeking-input-port)
(require #/only-in racket/string string-contains?)

(require #/only-in lathe-comforts dissect expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list list-foldl list-foldr)
(require #/only-in lathe-comforts/maybe
  just maybe-bind maybe/c maybe-map nothing)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)

; Essential operations
(provide
  
  textpat-result-matched
  textpat-result-failed
  textpat-result-passed-end
  (contract-out
    [textpat-result-matched? (-> any/c boolean?)]
    [textpat-result-matched-stop (-> textpat-result-matched? any/c)]
    [textpat-result-failed? (-> any/c boolean?)]
    [textpat-result-passed-end? (-> any/c boolean?)]
    [textpat? (-> any/c boolean?)]
    [productive-textpat? (-> any/c boolean?)]
    [optimized-textpat? (-> any/c boolean?)])
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
  
  (contract-out
    [textpat-has-empty? (-> textpat? boolean?)])
  optimize-textpat
  optimized-textpat-match
  optimized-textpat-read!)

; Derived operations
(provide
  textpat-or-list textpat-or
  textpat-append-list textpat-append
  textpat-not
  textpat-lookahead
  textpat-one-not
  textpat-one-not-in-string
  textpat-star
  textpat-once-or-more)

; TODO: See if this file should be factored out into its own Racket
; library.


; ===== Miscellaneous infrastructure =================================

(define/contract (regexp-maybe code)
  (-> string? #/maybe/c regexp?)
  ; TODO: See if there's any better way to work around the
  ; "regexp too big" error.
  (with-handlers
    (
      [
        (fn e
          (and (exn:fail:contract? e)
          #/equal? (exn-message e)
            "regexp: regexp too big"))
        (fn e
          (nothing))])
    (just #/regexp code)))


; ===== Essential operations =========================================

(define-imitation-simple-struct
  (textpat? textpat-has-empty? textpat-get-data)
  textpat 'textpat (current-inspector) (auto-write))
(define-imitation-simple-struct
  (textpat-data?
    
    ; A maybe of an immutable string containing the code to a regular
    ; expression. This regular expression should match if and only if
    ; the text pattern should report `(textpat-result-passed-end)`,
    ; assuming the end of the string the regex is applied to is the
    ; end of the stream so far (and not necessarily the end of the
    ; stream altogether).
    ;
    textpat-data-maybe-incomplete
    
    ; A maybe of an immutable string containing the code to a regular
    ; expression. This regular expression should fail if and only if
    ; the text pattern should report `(textpat-result-failed)`. When
    ; it succeeds, and when a `(textpat-result-passed-end)` result
    ; isn't possible (either because `textpat-data-maybe-incomplete`
    ; failed to match or the input given is a complete stream
    ; (`optimized-textpat-read!`) rather than a potentially incomplete
    ; segment of a stream (`optimized-textpat-match`)), then the place
    ; its match ends should be the appropriate place to report in a
    ; `(textpat-result-matched ...)` result.
    ;
    ; TODO: Right now the only way we distinguish whether the input is
    ; complete or not is by the choice of whether to use
    ; `optimized-textpat-read!` or `optimized-textpat-match`. Perhaps
    ; we should split `optimized-textpat-match` into two utilities
    ; that each take strings, one of which treats the string as the
    ; complete input and one of which treats it as incomplete. Perhaps
    ; we should split `optimized-textpat-read!` the same way.
    ;
    textpat-data-maybe-necessary
    
    textpat-data-make-optimized)
  textpat-data 'textpat-data (current-inspector) (auto-write))
(define-imitation-simple-struct
  (optimized-textpat?
    optimized-textpat-match-string
    optimized-textpat-read-stream!)
  optimized-textpat
  'optimized-textpat (current-inspector) (auto-write))
(define-imitation-simple-struct
  (textpat-result-matched? textpat-result-matched-stop)
  textpat-result-matched
  'textpat-result-matched (current-inspector)
  (auto-write)
  (auto-equal))
(define-imitation-simple-struct
  (textpat-result-failed?)
  textpat-result-failed
  'textpat-result-failed (current-inspector)
  (auto-write)
  (auto-equal))
(define-imitation-simple-struct
  (textpat-result-passed-end?)
  textpat-result-passed-end
  'textpat-result-passed-end (current-inspector)
  (auto-write)
  (auto-equal))

(define (productive-textpat? v)
  (expect v (textpat has-empty get-data) #f
  #/not has-empty))

(define/contract (textpat-result? v)
  (-> any/c boolean?)
  (mat v (textpat-result-matched stop) (natural? stop)
  #/mat v (textpat-result-failed) #t
  #/mat v (textpat-result-passed-end) #t
    #f))

(define/contract (textpat-one-trivial necessary maybe-make-optimized)
  (->
    immutable-string?
    (maybe/c #/-> optimized-textpat?)
    textpat?)
  (textpat #f #/fn #/textpat-data
    (just "$")
    (just necessary)
    (mat maybe-make-optimized (just make-optimized)
      make-optimized
      (fn
        (error "Internal error: Expected the regexp to be small enough to compile")))))

(define/contract (textpat-give-up)
  (-> textpat?)
  (textpat #f #/fn #/textpat-data
    (just ".^")
    (just ".^")
    (fn
      (error "Internal error: Expected the regexp to be small enough to compile"))))

(define/contract (textpat-empty)
  (-> textpat?)
  (textpat #t #/fn #/textpat-data
    (just ".^")
    (just "")
    (fn
      (error "Internal error: Expected the regexp to be small enough to compile"))))

(define/contract (textpat-from-string str)
  (-> immutable-string? textpat?)
  (w- n (string-length str)
  #/textpat (= 0 n) #/fn #/textpat-data
    
    ; NOTE: We `regexp-quote` the last Unicode scalar of the string
    ; (if it exists) and include it in the "incomplete" regex. The
    ; regex can technically be simplified by removing it, but only at
    ; the price of complicating this code.
    (just #/string->immutable-string #/string-append
      (regexp-replace* #rx"." str #/fn scalar-string
        (string-append "(?>$|" (regexp-quote scalar-string)))
      ".^"
      (regexp-replace* #rx"." str ")"))
    
    (just #/regexp-quote str)
    (fn
      (optimized-textpat
        (fn str start stop
          (if (< (- stop start) n)
            (textpat-result-passed-end)
          #/w- potential-stop (+ start n)
          #/if (equal? str #/substring str start potential-stop)
            (textpat-result-matched potential-stop)
            (textpat-result-failed)))
        (fn in
          (w- potential-result (peek-string n 0 in)
          #/if (equal? str potential-result)
            (begin
              (read-string n in)
              (just str))
            (nothing)))))))

(define/contract (textpat-one-in-string example-str)
  (-> immutable-string? textpat?)
  (textpat-one-trivial
    (string->immutable-string #/string-append
      "(?>.^"
      (regexp-replace* #rx"." example-str #/fn scalar-string
        (string-append "|" (regexp-quote scalar-string)))
      ")")
    (just #/fn
      (optimized-textpat
        (fn str start stop
          (if (= start stop)
            (textpat-result-passed-end)
          #/w- potential-stop (add1 start)
          #/if
            (string-contains? example-str
              (substring str start potential-stop))
            (textpat-result-matched potential-stop)
          #/textpat-result-failed))
        (fn in
          (w- ch (peek-char in)
          #/if (eof-object? ch)
            (nothing)
          #/w- potential-result (string->immutable-string #/string ch)
          #/if (string-contains? example-str potential-result)
            (begin
              (read-char in)
              (just potential-result))
            (nothing)))))))

(define/contract (textpat-one)
  (-> textpat?)
  (textpat-one-trivial "." (nothing)))

(define/contract (compile-textpat-data data)
  (-> textpat-data? textpat-data?)
  (dissect data
    (textpat-data maybe-incomplete maybe-necessary make-optimized)
  #/textpat-data maybe-incomplete maybe-necessary #/fn
    (expect maybe-incomplete (just incomplete) (make-optimized)
    #/expect maybe-necessary (just necessary) (make-optimized)
    #/expect
      (regexp-maybe #/string-append
        ; NOTE: We can put a "^" at the beginning of this, but it
        ; doesn't matter since this regex always matches in the first
        ; position it's attempted (thanks to the empty case).
        "(?>" incomplete "()|" necessary "()|)")
      (just compiled-match-string)
      (make-optimized)
    #/expect (regexp-maybe #/string-append "^" necessary)
      (just compiled-read-stream)
      (make-optimized)
    #/optimized-textpat
      (fn str start stop
        (dissect
          (regexp-match-positions
            compiled-match-string str start stop)
          (list
            (cons matched-start matched-stop)
            matched-inc
            matched-nec)
        #/if matched-nec (textpat-result-matched matched-stop)
        #/if matched-inc (textpat-result-passed-end)
        #/textpat-result-failed))
      (fn in
        (expect (regexp-try-match compiled-read-stream in)
          (list bytes)
          (nothing)
        #/just #/bytes->string/utf-8 bytes)))))

; NOTE SMALL REGEX: This generates regexes that may be up to three
; times as long as its inputs' regexes and up to twice as long as its
; continuation regex in terms of source code. Because of this, we
; prefer not to use it to derive other textpats even if that would be
; very convenient.
;
(define/contract (textpat-if condition then else)
  (-> textpat? textpat? textpat? textpat?)
  (dissect condition (textpat c-has-empty c-get-data)
  #/dissect then (textpat t-has-empty t-get-data)
  #/dissect else (textpat e-has-empty e-get-data)
  #/textpat (or (and c-has-empty t-has-empty) e-has-empty) #/fn
    (dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-inc c-nec c-make-optimized)
    #/dissect (compile-textpat-data #/t-get-data)
      (textpat-data t-inc t-nec t-make-optimized)
    #/dissect (compile-textpat-data #/e-get-data)
      (textpat-data e-inc e-nec e-make-optimized)
    #/textpat-data
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-inc #/fn c-inc
      #/maybe-bind t-inc #/fn t-inc
      #/maybe-bind e-inc #/fn e-inc
      #/just #/string->immutable-string #/string-append
        "(?>"
          ; We may run out of room matching the condition.
          c-inc "|"
          ; We may run out of room matching the then clause.
          c-nec t-inc "|"
          ; We may run out of room matching the else clause.
          "(?!" c-nec ")" e-inc
        ")")
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-inc #/fn c-inc
      #/maybe-bind t-nec #/fn t-nec
      #/maybe-bind e-nec #/fn e-nec
      #/just #/string->immutable-string #/string-append
        "(?>"
          ; We may match the then clause.
          c-nec t-nec "|"
          ; We may match the else clause.
          "(?!" c-nec ")" e-nec
        ")")
      (fn
        (dissect (c-make-optimized)
          (optimized-textpat c-match-string c-read-stream!)
        #/dissect (t-make-optimized)
          (optimized-textpat t-match-string t-read-stream!)
        #/dissect (e-make-optimized)
          (optimized-textpat e-match-string e-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w- c-result (c-match-string str start stop)
            #/mat c-result (textpat-result-matched c-stop)
              (t-match-string str c-stop stop)
            #/mat c-result (textpat-result-failed)
              (e-match-string str start stop)
            #/dissect c-result (textpat-result-passed-end)
              (textpat-result-passed-end)))
          (fn in
            (expect (c-read-stream! in) (just c-result)
              (e-read-stream! in)
            ; TODO READ-STREAM PEEKING: Whoops, if `t-read-stream!`
            ; fails, we need to "unread" the things `c-read-stream!`
            ; read. We need to use peeking.
            #/maybe-map (t-read-stream! in) #/fn t-result
            ; TODO: Figure out if there's a more efficient approach
            ; than `string-append` for these `read-stream!`
            ; operations.
            #/string->immutable-string #/string-append
              c-result t-result)))))))

; NOTE SMALL REGEX: This generates regexes that may be up to four
; times as long as its inputs' regexes. Because of this, we prefer not
; to use it to derive other textpats even if that would be very
; convenient.
;
(define/contract (textpat-while condition body)
  (->i ([condition textpat?] [body textpat?])
    
    ; NOTE: When the concatenation of `c-nec` and `b-nec` can match
    ; the empty string, Racket doesn't let us use the * operator on
    ; it. But we don't allow that case.
    #:pre (condition body)
    (or (productive-textpat? condition) (productive-textpat? body))
    
    [_ textpat?])
  (dissect condition (textpat c-has-empty c-get-data)
  #/dissect body (textpat b-has-empty b-get-data)
  #/textpat #t #/fn
    (dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-inc c-nec c-make-optimized)
    #/dissect (compile-textpat-data #/b-get-data)
      (textpat-data b-inc b-nec b-make-optimized)
    #/textpat-data
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-inc #/fn c-inc
      #/maybe-bind b-nec #/fn b-nec
      #/maybe-bind b-inc #/fn b-inc
      #/just #/string->immutable-string #/string-append
        "(?>(?:(?!" c-inc ")" c-nec "(?!" b-inc ")" b-nec ")*)(?>"
          ; We may run out of room matching the condition.
          c-inc "|"
          ; We may run out of room matching the body.
          c-nec b-inc
        ")")
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-inc #/fn c-inc
      #/maybe-bind b-nec #/fn b-nec
      #/just #/string->immutable-string #/string-append
        "(?>(?:" c-nec b-nec ")*)(?!" c-nec ")")
      (fn
        (dissect (c-make-optimized)
          (optimized-textpat c-match-string c-read-stream!)
        #/dissect (b-make-optimized)
          (optimized-textpat b-match-string b-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w-loop next start start
              (w- c-result (c-match-string str start stop)
              #/mat c-result (textpat-result-failed)
                (textpat-result-matched start)
              #/mat c-result (textpat-result-passed-end)
                (textpat-result-passed-end)
              #/dissect c-result (textpat-result-matched c-stop)
              #/w- b-result (b-match-string str c-stop stop)
              #/expect b-result (textpat-result-matched b-stop)
                b-result
              #/if (= start b-stop)
                (error "Internal error: It turns out condition and body can both match the empty string after all")
              #/next b-stop)))
          (fn in
            (w-loop next so-far ""
              (expect (c-read-stream! in) (just c-result)
                (just #/string->immutable-string so-far)
              ; TODO READ-STREAM PEEKING: Whoops, if `b-read-stream!`
              ; fails, we need to "unread" all the things we've read.
              ; We need to use peeking.
              #/maybe-bind (b-read-stream! in) #/fn b-result
              #/if (= 0 #/+ (length c-result) (length b-result))
                (error "Internal error: It turns out condition and body can both match the empty string after all")
              ; TODO: Figure out if there's a more efficient approach
              ; than `string-append` for these `read-stream!`
              ; operations.
              #/next #/string-append so-far c-result b-result))))))))

; NOTE SMALL REGEX: This generates regexes that may be up to four
; times as long as its inputs' regexes. Because of this, we prefer not
; to use it to derive other textpats even if that would be very
; convenient.
;
(define/contract (textpat-until body condition)
  
  ; NOTE: When `b-nec` can match the empty string, Racket doesn't let
  ; us use the * operator on it. But we don't allow that case.
  (-> productive-textpat? textpat? textpat?)
  
  (dissect condition (textpat c-has-empty c-get-data)
  #/dissect body (textpat b-has-empty b-get-data)
  #/textpat c-has-empty #/fn
    (dissect (compile-textpat-data #/b-get-data)
      (textpat-data b-inc b-nec b-make-optimized)
    #/dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-inc c-nec c-make-optimized)
    #/textpat-data
      (maybe-bind b-nec #/fn b-nec
      #/maybe-bind b-inc #/fn b-inc
      #/maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-inc #/fn c-inc
      #/just #/string->immutable-string #/string-append
        "(?>(?:"
          "(?!" c-inc "|" c-nec ")" b-nec
        ")*)(?>"
          ; We may run out of room matching the condition.
          c-inc "|"
          ; We may run out of room matching the body.
          "(?!" c-nec ")" b-inc
        ")")
      (maybe-bind b-nec #/fn b-nec
      #/maybe-bind c-nec #/fn c-nec
      #/maybe-bind c-inc #/fn c-inc
      #/just #/string->immutable-string #/string-append
        "(?>(?:(?!" c-nec ")" b-nec ")*)" c-nec)
      (fn
        (dissect (b-make-optimized)
          (optimized-textpat b-match-string b-read-stream!)
        #/dissect (c-make-optimized)
          (optimized-textpat c-match-string c-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w-loop next start start
              (w- c-result (c-match-string str start stop)
              #/expect c-result (textpat-result-failed) c-result
              #/w- b-result (b-match-string str start stop)
              #/expect b-result (textpat-result-matched b-stop)
                b-result
              #/if (= start b-stop)
                (error "Internal error: It turns out body can match the empty string after all")
              #/next b-stop)))
          (fn in
            ; TODO: Figure out if there's a more efficient approach
            ; than `string-append` for these `read-stream!`
            ; operations.
            (w-loop next so-far ""
              (mat (c-read-stream! in) (just c-result)
                (just #/string->immutable-string #/string-append
                  so-far c-result)
              ; TODO READ-STREAM PEEKING: Whoops, if `b-read-stream!`
              ; fails, we need to "unread" all the things we've read.
              ; We need to use peeking.
              #/maybe-bind (b-read-stream! in) #/fn b-result
              #/if (= 0 #/length b-result)
                (error "Internal error: It turns out body can match the empty string after all")
              #/next #/string-append so-far b-result))))))))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/contract (textpat-or-binary a b)
  (-> textpat? textpat? textpat?)
  (textpat-if a (textpat-empty) b))
(define/contract (textpat-or-binary a b)
  (-> textpat? textpat? textpat?)
  (dissect a (textpat a-has-empty a-get-data)
  #/dissect b (textpat b-has-empty b-get-data)
  #/textpat (or a-has-empty b-has-empty) #/fn
    (dissect (compile-textpat-data #/a-get-data)
      (textpat-data a-inc a-nec a-make-optimized)
    #/dissect (compile-textpat-data #/b-get-data)
      (textpat-data b-inc b-nec b-make-optimized)
    #/textpat-data
      (maybe-bind a-inc #/fn a-inc
      #/maybe-bind b-inc #/fn b-inc
      #/just #/string->immutable-string #/string-append
        "(?>" a-inc "|" b-inc ")")
      (maybe-bind a-nec #/fn a-nec
      #/maybe-bind b-nec #/fn b-nec
      #/just #/string->immutable-string #/string-append
        "(?>" a-nec "|" b-nec ")")
      (fn
        (dissect (a-make-optimized)
          (optimized-textpat a-match-string a-read-stream!)
        #/dissect (b-make-optimized)
          (optimized-textpat b-match-string b-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w- a-result (a-match-string str start stop)
            #/mat a-result (textpat-result-matched a-stop)
              (textpat-result-matched a-stop)
            #/mat a-result (textpat-result-failed)
              (b-match-string str start stop)
            #/dissect a-result (textpat-result-passed-end)
              (textpat-result-passed-end)))
          (fn in
            (mat (a-read-stream! in) (just a-result)
              (just a-result)
            #/b-read-stream! in)))))))

(define/contract (textpat-one-in-range a b)
  (-> char? char? textpat?)
  (w- special-range-chars "]-%\\"
  #/w- an (char->integer a)
  #/w- bn (char->integer b)
  #/expect (< an bn) #t (textpat-give-up)
  #/w- a-str (string->immutable-string #/string a)
  #/if (string-contains? special-range-chars a-str)
    (textpat-or-binary (textpat-one-in-string a-str)
    #/textpat-one-in-range (integer->char #/add1 an) b)
  #/w- b-str (string->immutable-string #/string b)
  #/if (string-contains? special-range-chars b-str)
    (textpat-or-binary (textpat-one-in-string b-str)
    #/textpat-one-in-range (integer->char #/add1 an) b)
  #/textpat-one-trivial
    (string->immutable-string #/string-append
      "[" a-str "-" b-str "]")
    (nothing)))

(define/contract (optimize-textpat t)
  (-> textpat? optimized-textpat?)
  (dissect t (textpat has-empty get-data)
  #/dissect (compile-textpat-data #/get-data)
    (textpat-data inc nec make-optimized)
  #/make-optimized))

(define/contract (optimized-textpat-match ot str start stop)
  (-> optimized-textpat? string? natural? natural? textpat-result?)
  (dissect ot (optimized-textpat match-string read-stream!)
  #/w- n (string-length str)
  #/expect (<= start n) #t
    (error "Expected start to be an index of the string")
  #/expect (<= stop n) #t
    (error "Expected stop to be an index of the string")
  #/expect (<= start stop) #t
    (error "Expected start to be less than or equal to stop")
  #/match-string str start stop))

(define/contract (optimized-textpat-read! ot in)
  (-> optimized-textpat? input-port? #/maybe/c string?)
  (dissect ot (optimized-textpat match-string read-stream!)
  #/read-stream! in))


; ===== Derived operations ===========================================

(define/contract (textpat-or-list ts)
  (-> (listof textpat?) textpat?)
  (list-foldl (textpat-give-up) ts #/fn a b #/textpat-or-binary a b))

(define/contract (textpat-or . ts)
  (->* () #:rest (listof textpat?) textpat?)
  (textpat-or-list ts))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; NOTE SMALL REGEX: This generates regexes that may be up to twice as
; long as its inputs' regexes. Because of this, we prefer not to use
; it to derive other textpats even if that would be very convenient.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/contract (textpat-append-binary a b)
  (-> textpat? textpat? textpat?)
  (textpat-if a b #/textpat-give-up))
(define/contract (textpat-append-binary a b)
  (-> textpat? textpat? textpat?)
  (dissect a (textpat a-has-empty a-get-data)
  #/dissect b (textpat b-has-empty b-get-data)
  #/textpat (and a-has-empty b-has-empty) #/fn
    (dissect (compile-textpat-data #/a-get-data)
      (textpat-data a-inc a-nec a-make-optimized)
    #/dissect (compile-textpat-data #/b-get-data)
      (textpat-data b-inc b-nec b-make-optimized)
    #/textpat-data
      (maybe-bind a-nec #/fn a-nec
      #/maybe-bind a-inc #/fn a-inc
      #/maybe-bind b-inc #/fn b-inc
      #/just #/string->immutable-string #/string-append
        "(?>"
          ; We may run out of room matching the first part.
          a-inc "|"
          ; We may run out of room matching the second part.
          a-nec b-inc
        ")")
      (maybe-bind a-nec #/fn a-nec
      #/maybe-bind b-nec #/fn b-nec
      #/just #/string->immutable-string #/string-append
        a-nec b-nec)
      (fn
        (dissect (a-make-optimized)
          (optimized-textpat a-match-string a-read-stream!)
        #/dissect (b-make-optimized)
          (optimized-textpat b-match-string b-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w- a-result (a-match-string str start stop)
            #/mat a-result (textpat-result-matched a-stop)
              (b-match-string str a-stop stop)
            #/mat a-result (textpat-result-failed)
              (textpat-result-failed)
            #/dissect a-result (textpat-result-passed-end)
              (textpat-result-passed-end)))
          (fn in
            (maybe-bind (a-read-stream! in) #/fn a-result
            ; TODO READ-STREAM PEEKING: Whoops, if `b-read-stream!`
            ; fails, we need to "unread" the things `a-read-stream!`
            ; read. We need to use peeking.
            #/maybe-bind (b-read-stream! in) #/fn b-result
            ; TODO: Figure out if there's a more efficient approach
            ; than `string-append` for these `read-stream!`
            ; operations.
            #/just #/string->immutable-string #/string-append
              a-result b-result)))))))

; NOTE SMALL REGEX: To keep the regexes small, we're using
; `list-foldr` here instead of `list-foldl`. The behavior of
; `textpat-append-binary` can double the length of the first part's
; regexes but not the second part's, so this keeps us from doubling
; any of our parts more than once.
;
; NOTE SMALL REGEX: This generates regexes that may be up to twice as
; long as its inputs' regexes. Because of this, we prefer not to use
; it to derive other textpats even if that would be very convenient.
;
(define/contract (textpat-append-list ts)
  (-> (listof textpat?) textpat?)
  (list-foldr ts (textpat-empty) #/fn a b
    (textpat-append-binary a b)))

; NOTE SMALL REGEX: This generates regexes that may be up to twice as
; long as its inputs' regexes. Because of this, we prefer not to use
; it to derive other textpats even if that would be very convenient.
;
(define/contract (textpat-append . ts)
  (->* () #:rest (listof textpat?) textpat?)
  (textpat-append-list ts))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/contract (textpat-not t)
  (-> textpat? textpat?)
  (textpat-if t (textpat-give-up) (textpat-empty)))
(define/contract (textpat-not t)
  (-> textpat? textpat?)
  (dissect t (textpat t-has-empty t-get-data)
  #/textpat #t #/fn
    (dissect (compile-textpat-data #/t-get-data)
      (textpat-data t-inc t-nec t-make-optimized)
    #/textpat-data
      t-inc
      (maybe-bind t-nec #/fn t-nec
      #/just #/string->immutable-string #/string-append
        "(?!" t-nec ")")
      (fn
        (dissect (t-make-optimized)
          (optimized-textpat t-match-string t-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w- t-result (t-match-string str start stop)
            #/mat t-result (textpat-result-matched t-stop)
              (textpat-result-failed)
            #/mat t-result (textpat-result-failed)
              (textpat-result-matched start)
            #/dissect t-result (textpat-result-passed-end)
              (textpat-result-passed-end)))
          (fn in
            ; TODO READ-STREAM PEEKING: See if we need to close
            ; `peeking-input-port` values.
            (mat (t-read-stream! #/peeking-input-port in)
              (just t-result)
              (nothing)
              (just ""))))))))

(define/contract (textpat-lookahead t)
  (-> textpat? textpat?)
  (textpat-not #/textpat-not t))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/contract (textpat-one-not t)
  (-> textpat? textpat?)
  (textpat-append (textpat-not t) (textpat-one)))
(define/contract (textpat-one-not t)
  (-> textpat? textpat?)
  (dissect t (textpat t-has-empty t-get-data)
  #/textpat #f #/fn
    (dissect (compile-textpat-data #/t-get-data)
      (textpat-data t-inc t-nec t-make-optimized)
    #/textpat-data
      (maybe-bind t-inc #/fn t-inc
      #/just #/string->immutable-string #/string-append
        "(?>$|" t-inc ")")
      (maybe-bind t-nec #/fn t-nec
      #/just #/string->immutable-string #/string-append
        "(?!" t-nec ").")
      (fn
        (dissect (t-make-optimized)
          (optimized-textpat t-match-string t-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w- t-result (t-match-string str start stop)
            #/if (= start stop)
              (textpat-result-passed-end)
            #/mat t-result (textpat-result-matched t-stop)
              (textpat-result-failed)
            #/mat t-result (textpat-result-passed-end)
              (textpat-result-passed-end)
            #/dissect t-result (textpat-result-failed)
              (textpat-result-matched #/add1 start)))
          (fn in
            ; TODO READ-STREAM PEEKING: See if we need to close
            ; `peeking-input-port` values.
            (mat (t-read-stream! #/peeking-input-port in)
              (just t-result)
              (nothing)
            #/w- ch (peek-char in)
            #/if (eof-object? ch)
              (nothing)
            #/just #/string->immutable-string #/string ch)))))))

(define/contract (textpat-one-not-in-string str)
  (-> immutable-string? textpat?)
  (textpat-one-not #/textpat-one-in-string str))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small and to report good error
; messages, we define it longhand.
;
; NOTE SMALL REGEX: This generates regexes that may be up to three
; times as long as its inputs' regexes. Because of this, we prefer not
; to use it to derive other textpats even if that would be very
; convenient.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/contract (textpat-star t)
  (-> productive-textpat? textpat?)
  (textpat-while t #/textpat-empty))
(define/contract (textpat-star t)
  
  ; NOTE: When `b-nec` can match the empty string, Racket doesn't let
  ; us use the * operator on it. But we don't allow that case.
  (-> productive-textpat? textpat?)
  
  (dissect t (textpat t-has-empty t-get-data)
  #/textpat #t #/fn
    (dissect (compile-textpat-data #/t-get-data)
      (textpat-data t-inc t-nec t-make-optimized)
    #/textpat-data
      (maybe-bind t-nec #/fn t-nec
      #/maybe-bind t-inc #/fn t-inc
      #/just #/string->immutable-string #/string-append
        "(?>(?:(?!" t-inc ")" t-nec ")*)" t-inc)
      (maybe-bind t-nec #/fn t-nec
      #/maybe-bind t-inc #/fn t-inc
      #/just #/string->immutable-string #/string-append
        "(?>(?:" t-nec ")*)")
      (fn
        (dissect (t-make-optimized)
          (optimized-textpat t-match-string t-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w-loop next start start
              (w- t-result (t-match-string str start stop)
              #/mat t-result (textpat-result-failed)
                (textpat-result-matched start)
              #/mat t-result (textpat-result-passed-end)
                (textpat-result-passed-end)
              #/dissect t-result (textpat-result-matched t-stop)
              #/if (= start t-stop)
                (error "Internal error: It turns out the given textpat can match the empty string after all")
              #/next t-stop)))
          (fn in
            (w-loop next so-far ""
              (expect (t-read-stream! in) (just t-result)
                (just #/string->immutable-string so-far)
              #/if (= 0 #/length t-result)
                (error "Internal error: It turns out the given textpat can match the empty string after all")
              ; TODO: Figure out if there's a more efficient approach
              ; than `string-append` for these `read-stream!`
              ; operations.
              #/next #/string-append so-far t-result))))))))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; NOTE SMALL REGEX: This generates regexes that may be up to three
; times as long as its inputs' regexes. Because of this, we prefer not
; to use it to derive other textpats even if that would be very
; convenient.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/contract (textpat-once-or-more t)
  (-> productive-textpat? textpat?)
  (textpat-append t #/textpat-star t))
(define/contract (textpat-once-or-more t)
  
  ; NOTE: When `b-nec` can match the empty string, Racket doesn't let
  ; us use the * and + operators on it. But we don't allow that case.
  (-> productive-textpat? textpat?)
  
  (dissect t (textpat t-has-empty t-get-data)
  #/textpat #t #/fn
    (dissect (compile-textpat-data #/t-get-data)
      (textpat-data t-inc t-nec t-make-optimized)
    #/textpat-data
      (maybe-bind t-nec #/fn t-nec
      #/maybe-bind t-inc #/fn t-inc
      #/just #/string->immutable-string #/string-append
        "(?>(?:(?!" t-inc ")" t-nec ")*)" t-inc)
      (maybe-bind t-nec #/fn t-nec
      #/maybe-bind t-inc #/fn t-inc
      #/just #/string->immutable-string #/string-append
        "(?>(?:" t-nec ")+)")
      (fn
        (dissect (t-make-optimized)
          (optimized-textpat t-match-string t-read-stream!)
        #/optimized-textpat
          (fn str start stop
            (w- t-result (t-match-string str start stop)
            #/mat t-result (textpat-result-failed)
              (textpat-result-failed)
            #/mat t-result (textpat-result-passed-end)
              (textpat-result-passed-end)
            #/dissect t-result (textpat-result-matched t-stop)
            #/w-loop next start t-stop
              (w- t-result (t-match-string str start stop)
              #/mat t-result (textpat-result-failed)
                (textpat-result-matched start)
              #/mat t-result (textpat-result-passed-end)
                (textpat-result-passed-end)
              #/dissect t-result (textpat-result-matched t-stop)
              #/if (= start t-stop)
                (error "Internal error: It turns out the given textpat can match the empty string after all")
              #/next t-stop)))
          (fn in
            (w- read!
              (fn
                (maybe-bind (t-read-stream! in) #/fn t-result
                #/if (= 0 #/length t-result)
                  (error "Internal error: It turns out the given textpat can match the empty string after all")
                #/just t-result))
            #/maybe-bind (read!) #/fn t-result
            #/w-loop next so-far t-result
              (expect (read!) (just t-result)
                (just #/string->immutable-string so-far)
              ; TODO: Figure out if there's a more efficient approach
              ; than `string-append` for these `read-stream!`
              ; operations.
              #/next #/string-append so-far t-result))))))))
