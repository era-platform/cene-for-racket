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
  -> ->* any/c contract-out listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)
(require #/only-in racket/string string-contains?)

(require #/only-in lathe-comforts dissect expect fn mat w- w-loop)
(require #/only-in lathe-comforts/list list-foldl)
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
    textpat-data-maybe-optional
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
    (fn
      (error "Internal error: Expected the regexp to be small enough to compile"))))

(define/contract
  (textpat-optional-trivial has-empty necessary maybe-make-optimized)
  (-> boolean? immutable-string? (maybe/c #/-> optimized-textpat?)
    textpat?)
  (textpat has-empty #/fn #/textpat-data
    (just #/fn next
      (string->immutable-string #/string-append
        "(?:" necessary next "|$)"))
    (just necessary)
    (mat maybe-make-optimized (just make-optimized)
      make-optimized
      (fn
        (error "Internal error: Expected the regexp to be small enough to compile")))))

(define/contract (textpat-give-up)
  (-> textpat?)
  (textpat-trivial #f ".^"))

(define/contract (textpat-empty)
  (-> textpat?)
  (textpat-trivial #t ""))

(define/contract (textpat-from-string str)
  (-> immutable-string? textpat?)
  (w- n (string-length str)
  #/textpat (= 0 n) #/fn #/textpat-data
    (just #/fn next
      (string->immutable-string #/string-append
        (regexp-replace* #rx"." str #/fn scalar-string
          (string-append "(?:" (regexp-quote scalar-string)))
        next
        (regexp-replace* #rx"." str "|$)")))
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
  (textpat-optional-trivial #f
    (string->immutable-string #/string-append
      "(?:.^"
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
            #/substring str start potential-stop)
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
  (textpat-optional-trivial #f "." (nothing)))

(define/contract (compile-textpat-data data)
  (-> textpat-data? textpat-data?)
  (dissect data
    (textpat-data maybe-optional maybe-necessary make-optimized)
  #/textpat-data maybe-optional maybe-necessary #/fn
    (expect maybe-optional (just optional) (make-optimized)
    #/expect maybe-necessary (just necessary) (make-optimized)
    #/expect
      (regexp-maybe #/string-append
        "(?:" necessary "()|" (optional "") "())?")
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
            matched-nec
            matched-opt)
        #/if matched-nec (textpat-result-matched matched-stop)
        #/if matched-opt (textpat-result-passed-end)
        #/textpat-result-failed))
      (fn in
        (expect (regexp-try-match compiled-read-stream in)
          (list bytes)
          (nothing)
        #/just #/bytes->string/utf-8 bytes)))))

(define/contract (textpat-if condition then else)
  (-> textpat? textpat? textpat? textpat?)
  (dissect condition (textpat c-has-empty c-get-data)
  #/dissect then (textpat t-has-empty t-get-data)
  #/dissect else (textpat e-has-empty e-get-data)
  #/textpat (or (and c-has-empty t-has-empty) e-has-empty) #/fn
    (dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-opt c-nec c-make-optimized)
    #/dissect (compile-textpat-data #/t-get-data)
      (textpat-data t-opt t-nec t-make-optimized)
    #/dissect (compile-textpat-data #/e-get-data)
      (textpat-data e-opt e-nec e-make-optimized)
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
            #/maybe-map (t-read-stream! in) #/fn t-result
            ; TODO: Figure out if there's a more efficient approach
            ; than `string-append` for these `read-stream!`
            ; operations.
            #/string->immutable-string #/string-append
              c-result t-result)))))))

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
      (textpat-data c-opt c-nec c-make-optimized)
    #/dissect (compile-textpat-data #/b-get-data)
      (textpat-data b-opt b-nec b-make-optimized)
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
            c-nec "(?!" b-nec ")" (b-opt "") ""
          ")"))
      (maybe-bind c-nec #/fn c-nec
      #/maybe-bind b-nec #/fn b-nec
      #/just #/string->immutable-string #/string-append
        "(?:" c-nec b-nec ")*(?!" c-nec ")")
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
              #/maybe-bind (b-read-stream! in) #/fn b-result
              #/if (= 0 #/+ (length c-result) (length b-result))
                (error "Internal error: It turns out condition and body can both match the empty string after all")
              ; TODO: Figure out if there's a more efficient approach
              ; than `string-append` for these `read-stream!`
              ; operations.
              #/next #/string-append so-far c-result b-result))))))))

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
      (textpat-data b-opt b-nec b-make-optimized)
    #/dissect (compile-textpat-data #/c-get-data)
      (textpat-data c-opt c-nec c-make-optimized)
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
              #/maybe-bind (b-read-stream! in) #/fn b-result
              #/if (= 0 #/length b-result)
                (error "Internal error: It turns out body can match the empty string after all")
              #/next #/string-append so-far b-result))))))))

(define/contract (textpat-or-binary a b)
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
    (textpat-or-binary (textpat-one-in-string a-str)
    #/textpat-one-in-range (integer->char #/add1 an) b)
  #/w- b-str (string->immutable-string #/string b)
  #/if (string-contains? special-range-chars b-str)
    (textpat-or-binary (textpat-one-in-string b-str)
    #/textpat-one-in-range (integer->char #/add1 an) b)
  #/textpat-optional-trivial #f
    (string->immutable-string #/string-append
      "[" a-str "-" b-str "]")
    (nothing)))

(define/contract (optimize-textpat t)
  (-> textpat? optimized-textpat?)
  (dissect t (textpat has-empty get-data)
  #/dissect (compile-textpat-data #/get-data)
    (textpat-data opt nec make-optimized)
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

(define/contract (textpat-append-binary a b)
  (-> textpat? textpat? textpat?)
  (textpat-if a b #/textpat-give-up))

(define/contract (textpat-append-list ts)
  (-> (listof textpat?) textpat?)
  (list-foldl (textpat-empty) ts #/fn a b
    (textpat-append-binary a b)))

(define/contract (textpat-append . ts)
  (->* () #:rest (listof textpat?) textpat?)
  (textpat-append-list ts))

(define/contract (textpat-not t)
  (-> textpat? textpat?)
  (textpat-if t (textpat-give-up) (textpat-empty)))

(define/contract (textpat-lookahead t)
  (-> textpat? textpat?)
  (textpat-not #/textpat-not t))

(define/contract (textpat-one-not t)
  (-> textpat? textpat?)
  (textpat-append (textpat-not t) (textpat-one)))

(define/contract (textpat-one-not-in-string str)
  (-> immutable-string? textpat?)
  (textpat-one-not #/textpat-one-in-string str))

(define/contract (textpat-star t)
  (-> textpat? textpat?)
  (textpat-while t #/textpat-empty))

(define/contract (textpat-once-or-more t)
  (-> textpat? textpat?)
  (textpat-append t #/textpat-star t))
