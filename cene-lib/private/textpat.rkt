#lang parendown racket/base

; cene/private/textpat
;
; A system of reasonably efficient parser combinators which operate on
; partial input and which never backtrack. The result of a parse is
; either a failure, a success (with a stop position), or a report that
; the end of the input was reached before a result was determined.

;   Copyright 2018-2020, 2022 The Era Authors
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


(require cene/private/shim)
(init-shim)


; Essential operations
(provide
  
  textpat-result-matched
  textpat-result-failed
  textpat-result-passed-end)
(provide #/own-contract-out
  textpat-result-matched?
  textpat-result-matched-stop
  textpat-result-failed?
  textpat-result-passed-end?
  textpat?
  productive-textpat?
  optimized-textpat?
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
  optimized-textpat-match
  optimized-textpat-read!)

; Derived operations
(provide #/own-contract-out
  textpat-or-list
  textpat-or
  textpat-append-list
  textpat-append
  textpat-not
  textpat-lookahead
  textpat-one-not
  textpat-one-not-in-string
  textpat-star
  textpat-once-or-more)

; TODO: See if this file should be factored out into its own Racket
; library.


; ===== Miscellaneous infrastructure =================================

(define/own-contract (regexp-maybe code)
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

(define/own-contract (port-next-position in)
  (-> input-port? #/or/c #f exact-positive-integer?)
  (define-values (line column position) (port-next-location in))
  position)

(define/own-contract (peeking-input-port-counting-bytes in)
  (-> input-port? input-port?)
  (peeking-input-port in))

; Calls the given `read-stream!` procedure (a concept from the
; implementation of `optimized-textpat?` values) with an input stream
; that's modified from the given one. When the `read-stream!`
; procedure reads from the stream it's given, these reads aren't
; actually consumed from the original input stream. Instead, when the
; `read-stream!` procedure would return `#t` (indicating a textpat
; match success), this returns `(just n)`, where `n` is the number of
; bytes that were read from the modified input stream.
;
(define/own-contract (read-stream-monitored read-stream! in)
  (-> (-> input-port? boolean?) input-port? #/maybe/c natural?)
  
  ; NOTE: We get two properties we need from `peeking-input-port`: The
  ; reads won't be committed to the original stream, and the modified
  ; stream will count bytes. The latter property isn't a prominently
  ; described feature of the `peeking-input-port` interface, and it's
  ; plausible that someday `peeking-input-port` could be updated to
  ; inherit its location information from the original stream.
  ;
  ; TODO: Write a unit test so that if `peeking-input-port` changes
  ; that way, we can notice it. Better yet, perhaps there's something
  ; more explicit we can do here to ensure the port counts bytes.
  ;
  ; Note that if we do need to change this use of `peeking-input-port`
  ; someday, we may also need to change the other places we use it in
  ; the implementation of `read-stream!` procedures. On the other
  ; hand, those places are creating streams with counting behavior
  ; similar to the streams they're starting with, so they probably
  ; aren't as volatile.
  ;
  (w- monitored-in (peeking-input-port in)
  
  #/w- original-position (port-next-position monitored-in)
  #/expect (read-stream! monitored-in) #t (nothing)
  #/just #/- (port-next-position monitored-in) original-position))

; Calls the given `read-stream!` procedure (a concept from the
; implementation of `optimized-textpat?` values) with an input stream
; that's modified from the given one. When the `read-stream!`
; procedure reads from the stream it's given, these reads aren't
; actually consumed from the original input stream *unless* the
; `read-stream!` procedure returns `#t` (indicating a textpat match
; success).
;
; This suits a common case where `read-stream!` procedures make calls
; to other `read-stream!` procedures. Namely, it's used in places
; where a match failure in the callee isn't necessarily promoted to a
; match failure in the caller, and a match success's stream
; consumption does indeed get promoted to stream consumption in the
; caller.
;
; In calls where failure in the callee is promoted to failure in the
; caller, we simply call the `read-stream!` callee directly. This
; essentially promotes the stream consumption in both the success case
; *and the failure case*, but since the failure is being promoted,
; logic in the caller's caller will discard that stream consumption
; already.
;
; In recursion cases where even a match success's stream consumption
; is discarded, we simply call the `read-stream!` callee directly and
; pass it a `peeking-input-port`.
;
(define/own-contract (read-stream-if-match read-stream! in)
  (-> (-> input-port? boolean?) input-port? boolean?)
  (expect (read-stream-monitored read-stream! in) (just n) #f
  ; TODO: Since we don't use the result of `read-bytes`, see if
  ; there's an alternative way to advance the stream without
  ; allocating the storage.
  #/begin (read-bytes n in)
    #t))


; ===== Essential operations =========================================

(define-imitation-simple-struct
  (textpat? textpat-has-empty? textpat-get-data)
  textpat 'textpat (current-inspector) (auto-write))
(ascribe-own-contract textpat? (-> any/c boolean?))
(ascribe-own-contract textpat-has-empty? (-> textpat? boolean?))
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
(ascribe-own-contract optimized-textpat? (-> any/c boolean?))
(define-imitation-simple-struct
  (textpat-result-matched? textpat-result-matched-stop)
  textpat-result-matched
  'textpat-result-matched (current-inspector)
  (auto-write)
  (auto-equal))
(ascribe-own-contract textpat-result-matched? (-> any/c boolean?))
(ascribe-own-contract textpat-result-matched-stop
  (-> textpat-result-matched? any/c))
(define-imitation-simple-struct
  (textpat-result-failed?)
  textpat-result-failed
  'textpat-result-failed (current-inspector)
  (auto-write)
  (auto-equal))
(ascribe-own-contract textpat-result-failed? (-> any/c boolean?))
(define-imitation-simple-struct
  (textpat-result-passed-end?)
  textpat-result-passed-end
  'textpat-result-passed-end (current-inspector)
  (auto-write)
  (auto-equal))
(ascribe-own-contract textpat-result-passed-end? (-> any/c boolean?))

(define/own-contract (productive-textpat? v)
  (-> any/c boolean?)
  (expect v (textpat has-empty get-data) #f
  #/not has-empty))

(define/own-contract (textpat-result? v)
  (-> any/c boolean?)
  (mat v (textpat-result-matched stop) (natural? stop)
  #/mat v (textpat-result-failed) #t
  #/mat v (textpat-result-passed-end) #t
    #f))

(define/own-contract
  (textpat-one-trivial necessary maybe-make-optimized)
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

(define/own-contract (textpat-give-up)
  (-> textpat?)
  (textpat #f #/fn #/textpat-data
    (just ".^")
    (just ".^")
    (fn
      (error "Internal error: Expected the regexp to be small enough to compile"))))

(define/own-contract (textpat-empty)
  (-> textpat?)
  (textpat #t #/fn #/textpat-data
    (just ".^")
    (just "")
    (fn
      (error "Internal error: Expected the regexp to be small enough to compile"))))

(define/own-contract (textpat-from-string str)
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
          (equal? str #/read-string n 0 in))))))

(define/own-contract (textpat-one-in-string example-str)
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
          (w- ch (read-char in)
          #/if (eof-object? ch)
            #f
          #/string-contains? example-str
            (string->immutable-string #/string ch)))))))

(define/own-contract (textpat-one)
  (-> textpat?)
  (textpat-one-trivial "." (nothing)))

(define/own-contract (compile-textpat-data data)
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
        (list? #/regexp-match compiled-read-stream in)))))

; NOTE SMALL REGEX: This generates regexes that may be up to three
; times as long as its inputs' regexes and up to twice as long as its
; continuation regex in terms of source code. Because of this, we
; prefer not to use it to derive other textpats even if that would be
; very convenient.
;
(define/own-contract (textpat-if condition then else)
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
            (if (read-stream-if-match c-read-stream! in)
              (t-read-stream! in)
              (e-read-stream! in))))))))

; NOTE SMALL REGEX: This generates regexes that may be up to four
; times as long as its inputs' regexes. Because of this, we prefer not
; to use it to derive other textpats even if that would be very
; convenient.
;
(define/own-contract (textpat-while condition body)
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
            (w-loop next position (port-next-position in)
              (or (not #/read-stream-if-match c-read-stream! in)
              #/and (b-read-stream! in)
              #/w- new-position (port-next-position in)
              #/if (= position new-position)
                (error "Internal error: It turns out condition and body can both match the empty string after all")
              #/next new-position))))))))

; NOTE SMALL REGEX: This generates regexes that may be up to four
; times as long as its inputs' regexes. Because of this, we prefer not
; to use it to derive other textpats even if that would be very
; convenient.
;
(define/own-contract (textpat-until body condition)
  
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
            (w-loop next position (port-next-position in)
              (or (read-stream-if-match c-read-stream! in)
              #/and (b-read-stream! in)
              #/w- new-position (port-next-position in)
              #/if (= position new-position)
                (error "Internal error: It turns out body can match the empty string after all")
              #/next new-position))))))))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/own-contract (textpat-or-binary a b)
  (-> textpat? textpat? textpat?)
  (textpat-if a (textpat-empty) b))
(define/own-contract (textpat-or-binary a b)
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
            (or (read-stream-if-match a-read-stream! in)
            #/b-read-stream! in)))))))

(define/own-contract (textpat-one-in-range a b)
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

(define/own-contract (optimize-textpat t)
  (-> textpat? optimized-textpat?)
  (dissect t (textpat has-empty get-data)
  #/dissect (compile-textpat-data #/get-data)
    (textpat-data inc nec make-optimized)
  #/make-optimized))

(define/own-contract (optimized-textpat-match ot str start stop)
  (-> optimized-textpat? immutable-string? natural? natural?
    textpat-result?)
  (dissect ot (optimized-textpat match-string read-stream!)
  #/w- n (string-length str)
  #/expect (<= start n) #t
    (error "Expected start to be an index of the string")
  #/expect (<= stop n) #t
    (error "Expected stop to be an index of the string")
  #/expect (<= start stop) #t
    (error "Expected start to be less than or equal to stop")
  #/match-string str start stop))

(define/own-contract (optimized-textpat-read! ot in)
  (-> optimized-textpat? input-port? #/maybe/c immutable-string?)
  (dissect ot (optimized-textpat match-string read-stream!)
  
  ; The `read-stream!` procedure expects a stream where
  ; `port-next-position` counts bytes and where none of the reads will
  ; ultimately be committed unless the textpat matches. We construct
  ; that using `read-stream-monitored`, which gives us a number of
  ; bytes we can read using `read-bytes` when we want to commit the
  ; reads.
  ;
  #/maybe-map (read-stream-monitored read-stream! in) #/fn n
    (string->immutable-string
      (bytes->string/utf-8 #/read-bytes n in))))


; ===== Derived operations ===========================================

(define/own-contract (textpat-or-list ts)
  (-> (listof textpat?) textpat?)
  (list-foldl (textpat-give-up) ts #/fn a b #/textpat-or-binary a b))

(define/own-contract (textpat-or . ts)
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
(define/own-contract (textpat-append-binary a b)
  (-> textpat? textpat? textpat?)
  (textpat-if a b #/textpat-give-up))
(define/own-contract (textpat-append-binary a b)
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
            (and (a-read-stream! in) (b-read-stream! in))))))))

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
(define/own-contract (textpat-append-list ts)
  (-> (listof textpat?) textpat?)
  (list-foldr ts (textpat-empty) #/fn a b
    (textpat-append-binary a b)))

; NOTE SMALL REGEX: This generates regexes that may be up to twice as
; long as its inputs' regexes. Because of this, we prefer not to use
; it to derive other textpats even if that would be very convenient.
;
(define/own-contract (textpat-append . ts)
  (->* () #:rest (listof textpat?) textpat?)
  (textpat-append-list ts))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/own-contract (textpat-not t)
  (-> textpat? textpat?)
  (textpat-if t (textpat-give-up) (textpat-empty)))
(define/own-contract (textpat-not t)
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
            (not #/t-read-stream! #/peeking-input-port in)))))))

(define/own-contract (textpat-lookahead t)
  (-> textpat? textpat?)
  (textpat-not #/textpat-not t))

; NOTE SMALL REGEX: We could implement this in a much more concise
; way, but to keep the regexes small, we define it longhand.
;
; TODO BUILTINS: Add this to Cene since the performance
; characteristics make it more expressive.
;
#;
(define/own-contract (textpat-one-not t)
  (-> textpat? textpat?)
  (textpat-append (textpat-not t) (textpat-one)))
(define/own-contract (textpat-one-not t)
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
            (and (not #/t-read-stream! #/peeking-input-port in)
            #/not #/eof-object? #/read-char in)))))))

(define/own-contract (textpat-one-not-in-string str)
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
(define/own-contract (textpat-star t)
  (-> productive-textpat? textpat?)
  (textpat-while t #/textpat-empty))
(define/own-contract (textpat-star t)
  
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
            (w-loop next position (port-next-position in)
              (or (not #/read-stream-if-match t-read-stream! in)
              #/w- new-position (port-next-position in)
              #/if (= position new-position)
                (error "Internal error: It turns out the given textpat can match the empty string after all")
              #/next new-position))))))))

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
(define/own-contract (textpat-once-or-more t)
  (-> productive-textpat? textpat?)
  (textpat-append t #/textpat-star t))
(define/own-contract (textpat-once-or-more t)
  
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
            (w- get-new-position
              (fn position
                (w- new-position (port-next-position in)
                #/if (= position new-position)
                  (error "Internal error: It turns out the given textpat can match the empty string after all")
                  new-position))
            #/w- position (port-next-position in)
            #/and (t-read-stream! in)
            #/w-loop next position (get-new-position position)
              (or (not #/read-stream-if-match t-read-stream! in)
              #/next #/get-new-position position))))))))
