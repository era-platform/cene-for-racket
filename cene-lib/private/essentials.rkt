#lang parendown racket/base

; cene/private/essentials
;
; A sufficient set of essential built-in operations for the Cene
; programming language (implementation details).

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


(require #/for-syntax racket/base)

(require #/only-in racket/contract/base -> listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts dissect expect fn mat w-)
(require #/only-in lathe-comforts/list list-foldl list-map)
(require #/only-in lathe-comforts/maybe just nothing)

(require #/only-in effection/order
  dex-give-up dex-dex dex-name name? table-empty table-shadow)

(require cene/private)


(provide cene-runtime-essentials)



; TODO: Use this in some kind of CLI entrypoint or something.
(define/contract (cene-runtime-essentials)
  (-> cene-runtime?)
  
  (define defined-dexes (table-empty))
  (define defined-values (table-empty))
  
  (define/contract (def-dexable-value! name dex value)
    (-> sink-name? sink-dex? sink? void?)
    (dissect name (sink-name name)
    #/begin
      (set! defined-dexes
        (table-shadow name (just dex) defined-dexes))
      (set! defined-values
        (table-shadow name (just value) defined-values))
    #/void))
  
  (define/contract (def-value! name value)
    (-> sink-name? sink? void?)
    (def-dexable-value! name (sink-dex #/dex-give-up) value))
  
  (define/contract (macro-impl body)
    (->
      (->
        name? sink? sink-text-input-stream?
        sink-cexpr-sequence-output-stream?
        (->
          name? sink? sink-text-input-stream?
          sink-cexpr-sequence-output-stream?
          sink-effects?)
        sink-effects?)
      sink?)
    (sink-fn-curried 5 #/fn
      unique-name qualify text-input-stream output-stream then
      
      (expect unique-name (sink-name unique-name)
        (cene-err "Expected unique-name to be a name")
      #/expect (sink-text-input-stream? text-input-stream) #t
        (cene-err "Expected text-input-stream to be a text input stream")
      #/expect (sink-cexpr-sequence-output-stream? output-stream) #t
        (cene-err "Expected output-stream to be an expression sequence output stream")
      #/body unique-name qualify text-input-stream output-stream
      #/fn unique-name qualify text-input-stream output-stream
      #/w- effects
        (sink-call then (sink-name unique-name) qualify
          text-input-stream output-stream)
      #/expect (sink-effects? effects) #t
        (cene-err "Expected the return value of a macro's callback to be an effectful computation")
        effects)))
  
  ; This creates a macro implementation function that reads a form
  ; body of precisely `n-args` cexprs, then writes a single cexpr
  ; computed from those using `body`.
  (define/contract (macro-impl-specific-number-of-args n-args body)
    (-> natural? (-> (listof sink-cexpr?) sink-cexpr?) sink?)
    (macro-impl #/fn
      unique-name qualify text-input-stream output-stream then
      
      (sink-effects-read-specific-number-of-cexprs
        unique-name qualify text-input-stream n-args
      #/fn unique-name qualify text-input-stream args
      #/sink-effects-cexpr-write output-stream (body args)
      #/fn output-stream
      #/then unique-name qualify text-input-stream output-stream)))
  
  (define/contract (def-func! main-tag-string n-args racket-func)
    (-> string? exact-positive-integer? procedure? void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify
      #/sink-name-for-struct-main-tag main-tag-name)
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (def-value!
        (sink-name-qualify
        #/sink-name-for-bounded-cexpr-op main-tag-name)
        
        ; Given precisely `n-args` cexprs, we construct a cexpr that
        ; first constructs a nullary struct with tag
        ; `qualified-main-tag-name` and then calls it with the given
        ; cexprs one by one.
        ;
        ; The JavaScript implementation of Cene doesn't verify the
        ; number of arguments to a function; instead it just passes in
        ; all the arguments it gets. But I find it's common for me to
        ; accidentally omit arguments or include extra arguments, so
        ; in `sink-effects-read-specific-number-of-cexprs`, we do some
        ; error-checking as an ad hoc line of defense against that
        ; kind of mistake.
        ;
        (macro-impl-specific-number-of-args n-args #/fn args
          (list-foldl
            (sink-cexpr-struct qualified-main-tag-name #/list)
            args
          #/fn func arg #/sink-cexpr-call func arg)))
      
      ; We define a Cene struct function implementation containing
      ; the function's run time behavior.
      (def-value!
        (sink-name-for-function-implementation qualified-main-tag-name
          (sink-table #/table-empty))
        (sink-cexpr-native #/sink-opaque-fn #/fn struct-value
          (sink-fn-curried n-args racket-func)))
      
      ))
  
  (define/contract (def-nullary-func! main-tag-string result)
    (-> string? sink? void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify
      #/sink-name-for-struct-main-tag main-tag-name)
      
      ; We define a reader macro so that the user can write code that
      ; compiles into a call to this function.
      (def-value!
        (sink-name-qualify
        #/sink-name-for-bounded-cexpr-op main-tag-name)
        
        ; Given precisely zero cexprs, we construct a cexpr that first
        ; constructs a nullary struct with tag `name` and then calls
        ; it with a trivial value.
        ;
        ; The JavaScript implementation of Cene doesn't have this
        ; special kind of compilation for nullary function calls; it
        ; just has the user pass `(trivial)` explicitly.
        ;
        (macro-impl-specific-number-of-args 0 #/fn args
          (sink-cexpr-call
            (sink-cexpr-struct qualified-main-tag-name #/list)
            (make-sink-cexpr-struct s-trivial #/list))))
      
      ; We define a Cene struct function implementation containing
      ; the function's run time behavior.
      (def-value!
        (sink-name-for-function-implementation qualified-main-tag-name
          (sink-table #/table-empty))
        (sink-cexpr-native #/sink-fn-curried 2 #/fn struct-value arg
          (expect (unmake-sink-struct-maybe s-trivial arg)
            (just #/list)
            (cene-err "Expected the argument to a nullary function to be a trivial")
            result)))
      
      ))
  
  (define/contract (def-data-struct! main-tag-string proj-strings)
    (-> string? (listof string?) void?)
    (w- main-tag-name
      (sink-name-for-string #/sink-string main-tag-string)
    #/w- qualified-main-tag-name
      (sink-name-qualify
      #/sink-name-for-struct-main-tag main-tag-name)
    #/w- qualified-proj-names
      (list-map proj-strings #/fn proj-string
        (sink-name-qualify #/sink-name-for-struct-proj
          qualified-main-tag-name
        #/sink-name-for-string #/sink-string proj-string))
      
      ; We define a reader macro so that the user can write code that
      ; compiles into an expression that constructs a struct with this
      ; tag.
      (def-value!
        (sink-name-qualify
        #/sink-name-for-bounded-cexpr-op main-tag-name)
        
        (w- n-projs (length qualified-proj-names)
        
        ; Given precisely `n-projs` cexprs, we construct a cexpr that
        ; constructs a struct.
        ;
        ; The JavaScript implementation of Cene doesn't verify that
        ; the number of arguments to a struct constructor is under a
        ; certain amount; instead it just passes all the excess
        ; arguments as function arguments. I find it's common for me
        ; to accidentally omit arguments or include extra arguments,
        ; so in `sink-effects-read-specific-number-of-cexprs`, we do
        ; some error-checking as an ad hoc line of defense against
        ; that kind of mistake.
        ;
        #/macro-impl-specific-number-of-args n-projs #/fn proj-cexprs
          (sink-cexpr-struct qualified-main-tag-name
          #/map list qualified-proj-names proj-cexprs)))
      
      ; We define a Cene struct function implementation which throws
      ; an error. We do this so that we do in fact have a function
      ; implementation for every struct we use, which might be an
      ; invariant that comes in handy. (TODO: See if it does.)
      (def-value!
        (sink-name-for-function-implementation qualified-main-tag-name
          (list-foldl (sink-table #/table-empty) qualified-proj-names
          #/fn table proj-name
            (sink-table-put-maybe table proj-name
            #/just #/make-sink-struct s-trivial #/list)))
        (sink-cexpr-native #/sink-opaque-fn #/fn struct-value
          (cene-err "Called a struct that wasn't intended for calling")))
      
      ; TODO: Also define something we can use to look up an ordered
      ; list of `sink-name-for-string` projection names, given the
      ; `sink-name-for-string` name the main tag name is made from.
      ; Once we have that in place, we'll be able to implement Cene's
      ; destructuring operations.
      
      ))
  
  (define/contract (def-macro! name-string body)
    (->
      string?
      (->
        name? sink? sink-text-input-stream?
        (-> name? sink? sink-text-input-stream? sink-cexpr?
          sink-effects?)
        sink-effects?)
      void?)
    (def-value!
      (sink-name-qualify #/sink-name-for-bounded-cexpr-op
      #/sink-name-for-string #/sink-string name-string)
      (macro-impl #/fn
        unique-name qualify text-input-stream output-stream then
        
        (body unique-name qualify text-input-stream
        #/fn unique-name qualify text-input-stream cexpr
        #/sink-effects-cexpr-write output-stream cexpr
        #/fn output-stream
        #/then unique-name qualify text-input-stream output-stream))))
  
  
  
  ; This binds the nameless bounded expression reader macro. This
  ; implementation proceeds by reading and running a (named) bounded
  ; expression reader macro.
  ;
  ; The Cene code `(foo a b c)` invokes the nameless bounded
  ; expression reader macro, and since that macro typically has this
  ; implementation, it behaves just like `(.foo a b c)`. That common
  ; behavior is that it consumes "foo", looks up an expression reader
  ; macro based on qualifying the string "foo", and runs it.
  ;
  (def-value!
    (sink-name-qualify #/sink-name-for-nameless-bounded-cexpr-op)
    (macro-impl #/fn
      unique-name qualify text-input-stream output-stream then
      
      (sink-effects-read-and-run-bounded-cexpr-op
        unique-name qualify text-input-stream output-stream then)))
  
  ; This binds the freestanding expression reader macro for `=`. This
  ; implementation is a line comment syntax: It consumes all the
  ; proceeding non-line-break characters, writes no cexprs at all, and
  ; leaves it at that.
  ;
  ;   \= This is an example comment.
  ;
  (def-value!
    (sink-name-qualify #/sink-name-for-freestanding-cexpr-op
    #/sink-name-for-string #/sink-string "=")
    (macro-impl #/fn
      unique-name qualify text-input-stream output-stream then
      
      (sink-effects-read-non-line-breaks text-input-stream
      #/fn text-input-stream non-line-breaks
      #/then unique-name qualify text-input-stream output-stream)))
  
  
  ; Miscellaneous
  
  (def-data-struct! "trivial" #/list)
  
  (def-data-struct! "nothing" #/list)
  (def-data-struct! "just" #/list "val")
  
  (def-data-struct! "yep" #/list "val")
  (def-data-struct! "nope" #/list "val")
  
  (def-data-struct! "nil" #/list)
  (def-data-struct! "cons" #/list "first" "rest")
  
  (def-data-struct! "assoc" #/list "key" "val")
  
  
  ; Errors and conscience
  
  ; TODO: Test this.
  (def-func! "follow-heart" 1 #/fn clamor
    (raise-cene-err (current-continuation-marks) clamor))
  
  (def-data-struct! "clamor-err" #/list "message")
  
  ; TODO: Implement the macro `err`.
  
  
  ; Order
  
  ; TODO: Implement this.
  (def-nullary-func! "dex-cline" (sink-dex 'TODO))
  
  (def-func! "cline-by-dex" 1 #/fn dex
    ; TODO: Implement this.
    'TODO)
  
  ; TODO: Implement this.
  (def-nullary-func! "cline-give-up" (sink-cline 'TODO))
  
  ; TODO: Consider implementing the following. This list was taken
  ; from the docs of the JavaScript version of Cene, but Effection has
  ; incorporated some lessons learned since then, so we might want to
  ; work against the list of Effection building blocks instead.
  ;
  ;   cline-default
  ;   cline-by-own-method
  ;   cline-fix
  ;   call-cline
  ;   in-cline
  ;   dexable
  ;   dex-dex
  ;   dex-by-cline
  ;   name-of
  ;   dex-name
  ;   dex-merge
  ;   merge-by-dex
  ;   merge-default
  ;   merge-by-own-method
  ;   merge-fix
  ;   call-merge
  ;   dex-fuse
  ;   fuse-by-merge
  ;   fuse-default
  ;   fuse-by-own-method
  ;   fuse-fix
  ;   call-fuse
  
  
  ; Structs and function calls
  
  ; TODO: Consider implementing the following.
  ;
  ;   cexpr-cline-struct
  ;   cline-struct
  ;   cexpr-merge-struct
  ;   merge-struct
  ;   cexpr-fuse-struct
  ;   fuse-struct
  ;   cexpr-construct
  ;   cexpr-case
  ;   case
  ;   cexpr-call
  ;   c
  ;   constructor-tag
  ;   function-implementation-from-cexpr
  ;   constructor-glossary
  ;   procure-constructor-glossary-getdef
  ;   copy-function-implementations
  ;   committing-to-define-function-implementations
  ;   procure-function-definer
  ;   def-struct
  ;   defn
  ;   caselet
  ;   cast
  
  (def-macro! "fn" #/fn unique-name qualify text-input-stream then
    (sink-effects-read-bounded-ids-and-exprs
      unique-name qualify text-input-stream
    #/fn unique-name qualify text-input-stream args
    #/expect (reverse args) (cons body rev-params)
      (cene-err "Expected a fn form to have a body expression")
    #/then unique-name qualify text-input-stream
    #/list-foldl (id-or-expr->cexpr body) rev-params #/fn body param
      (expect param
        (id-or-expr-id param-located-string param-qualified-name)
        (cene-err "Expected every parameter of a fn form to be an identifier")
      #/sink-cexpr-opaque-fn param-qualified-name body)))
  
  
  ; Tables
  
  (def-func! "dex-table" 1 #/fn dex-val
    ; TODO: Implement this.
    (sink-dex 'TODO))
  
  (def-func! "merge-table" 1 #/fn merge-val
    ; TODO: Implement this.
    'TODO)
  
  (def-func! "fuse-table" 1 #/fn fuse-val
    ; TODO: Implement this.
    'TODO)
  
  (def-nullary-func! "table-empty" (sink-table #/table-empty))
  
  (def-func! "table-shadow" 3 #/fn key maybe-val table
    (expect (sink-name? key) #t
      (cene-err "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err "Expected table to be a table")
    #/mat (unmake-sink-struct-maybe s-nothing maybe-val) (just #/list)
      (sink-table-put-maybe table key #/nothing)
    #/mat (unmake-sink-struct-maybe s-just maybe-val)
      (just #/list val)
      (sink-table-put-maybe table key #/just val)
    #/cene-err "Expected maybe-val to be a nothing or a just"))
  
  (def-func! "table-get" 2 #/fn key table
    (expect (sink-name? key) #t
      (cene-err "Expected key to be a name")
    #/expect (sink-table? table) #t
      (cene-err "Expected table to be a table")
    #/w- result (sink-table-get-maybe table key)
    #/expect result (just result)
      (make-sink-struct s-nothing #/list)
    #/make-sink-struct s-just #/list result))
  
  (def-func! "table-map-fuse" 3 #/fn table fuse key-to-operand
    ; TODO: Implement this.
    'TODO)
  
  (def-func! "table-sort" 2 #/fn cline table
    ; TODO: Implement this.
    'TODO)
  
  
  ; Effects
  
  ; NOTE: In the JavaScript version of Cene, this was known as
  ; `no-effects`.
  ;
  ; TODO: See which name we prefer.
  ;
  (def-nullary-func! "effects-noop" (sink-effects-noop))
  
  ; TODO: Consider implementing the following.
  ;
  ;   fuse-effects
  ;   get-mode
  ;   assert-current-mode
  ;   later
  ;   make-promise-later
  ;   getdef
  ;   definer-define
  ;   committing-to-define
  
  
  ; Unit tests
  
  ; TODO: Consider implementing the following.
  ;
  ;   test-async
  
  
  ; Namespaces
  
  ; TODO: Consider implementing the following.
  ;
  ;   procure-sub-ns-table
  ;   procure-name
  ;   procure-contributed-element-getdef
  ;   procure-contribute-listener
  ;   procure-contributed-elements
  ;   nsset-empty
  ;   fuse-nsset-by-union
  ;   nsset-not
  ;   nsset-ns-descendants
  ;   contributing-only-to
  
  
  ; Macros
  
  ; TODO: Consider implementing the following. This is the list of
  ; macro-relevant operations from the JavaScript implementation of
  ; Cene, which has an s-expression-based macro system. Now that we're
  ; using a text-stream-based macro system here, several of these will
  ; be unnecessary.
  ;
  ;   istring-nil
  ;   istring-cons
  ;   foreign
  ;   scope
  ;   macro-occurrence
  ;   local-occurrence
  ;   constructor-occurrence
  ;   projection-occurrence
  ;   obtain-by-unqualified-name
  ;   obtain-by-qualified-name
  ;   obtain-directly
  ;   stx
  ;   stx-details-empty
  ;   stx-details-join
  ;   stx-details-macro-call
  ;   procure-claim
  ;   procure-macro-implementation-getdef
  ;   cexpr-var
  ;   cexpr-reified
  ;   cexpr-located
  ;   cexpr-let
  ;   let
  ;   eval-cexpr
  ;   compile-expression-later
  ;   read-all-force
  ;   def-macro
  
  
  ; Integers
  
  ; TODO: Implement this.
  (def-nullary-func! "cline-int" (sink-cline 'TODO))
  
  (def-nullary-func! "int-zero" (sink-int 0))
  
  (def-nullary-func! "int-one" (sink-int 1))
  
  ; TODO: Implement this.
  (def-nullary-func! "fuse-int-by-plus" (sink-fuse 'TODO))
  
  ; TODO: Implement this.
  (def-nullary-func! "fuse-int-by-times" (sink-fuse 'TODO))
  
  (def-func! "int-minus" 2 #/fn minuend subtrahend
    (expect minuend (sink-int minuend)
      (cene-err "Expected minuend to be an int")
    #/expect subtrahend (sink-int subtrahend)
      (cene-err "Expected subtrahend to be an int")
    #/sink-int #/- minuend subtrahend))
  
  (def-func! "int-div-rounded-down" 2 #/fn dividend divisor
    (expect dividend (sink-int dividend)
      (cene-err "Expected dividend to be an int")
    #/expect divisor (sink-int divisor)
      (cene-err "Expected divisor to be an int")
    #/mat divisor 0 (make-sink-struct s-nothing #/list)
    #/make-sink-struct s-just #/list
    #/let-values ([(q r) (quotient/remainder dividend divisor)])
    #/if (<= 0 r)
      (make-sink-struct s-carried #/list (sink-int q) (sink-int r))
    #/if (<= 0 divisor)
      (make-sink-struct s-carried
      #/list (sink-int #/- q 1) (sink-int #/+ r divisor))
      (make-sink-struct s-carried
      #/list (sink-int #/+ q 1) (sink-int #/- r divisor))))
  
  (def-data-struct! "carried" #/list "main" "carry")
  
  
  ; Strings
  
  ; TODO: Implement this.
  (def-nullary-func! "dex-string" (sink-dex 'TODO))
  
  (def-nullary-func! "string-empty" (sink-string ""))
  
  (def-func! "string-singleton" 1 #/fn unicode-scalar
    (expect unicode-scalar (sink-int unicode-scalar)
      (cene-err "Expected unicode-scalar to be an int")
    #/expect
      (and
        (<= 0 unicode-scalar #x10FFFF)
        (not #/<= #xD800 unicode-scalar #xDFFF))
      #t
      (cene-err "Expected unicode-scalar to be in the range of valid Unicode scalars")
    #/sink-string #/list->string #/list #/integer->char
      unicode-scalar))
  
  (def-func! "string-append-later" 3 #/fn a b then
    (expect a (sink-string a)
      (cene-err "Expected a to be a string")
    #/expect b (sink-string b)
      (cene-err "Expected b to be a string")
    #/make-sink-effects #/fn
    #/sink-effects-run!
    #/sink-call then #/sink-string #/string-append a b))
  
  ; TODO: Implement the macro `str`.
  
  (def-func! "string-length" 1 #/fn string
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/sink-int #/string-length string))
  
  (def-func! "string-get-unicode-scalar" 2 #/fn string start
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/expect start (sink-int start)
      (cene-err "Expected start to be an int")
    #/expect (<= 0 start) #t
      (cene-err "Expected start to be a nonnegative int")
    #/expect (< start #/string-length string) #t
      (cene-err "Expected start to be an int less than the length of string")
    #/sink-int #/char->integer #/string-ref string start))
  
  (def-func! "string-cut-later" 4 #/fn string start stop then
    (expect string (sink-string string)
      (cene-err "Expected string to be a string")
    #/expect start (sink-int start)
      (cene-err "Expected start to be an int")
    #/expect stop (sink-int stop)
      (cene-err "Expected stop to be an int")
    #/expect (<= 0 start) #t
      (cene-err "Expected start to be a nonnegative int")
    #/expect (<= start stop) #t
      (cene-err "Expected start to be an int no greater than stop")
    #/expect (<= stop #/string-length string) #t
      (cene-err "Expected stop to be an int no greater than the length of string")
    #/make-sink-effects #/fn
    #/sink-effects-run!
    #/sink-call then #/sink-string #/substring string start stop))
  
  
  ; Regexes
  
  ; TODO: Consider implementing the following.
  ;
  ;   regex-give-up
  ;   regex-empty
  ;   regex-if
  ;   regex-while
  ;   regex-until
  ;   regex-one-in-range
  ;   regex-one
  ;   regex-from-string
  ;   regex-one-in-string
  ;   optimize-regex-later
  ;   optimized-regex-match-later
  ;   regex-result-matched
  ;   regex-result-failed
  ;   regex-result-passed-end
  
  
  ; File I/O for simple builds
  
  ; TODO: Consider implementing the following.
  ;
  ;   encapsulated-string
  ;   cli-arguments
  ;   cli-input-directory
  ;   cli-output-directory
  ;   input-path-get
  ;   input-path-type
  ;   file-type-directory
  ;   file-type-blob
  ;   file-type-missing
  ;   input-path-directory-list
  ;   input-path-blob-utf-8
  ;   output-path-get
  ;   output-path-directory
  ;   output-path-blob-utf-8
  ;   cli-output-environment-variable-shadow
  
  
  ; FFI
  
  ; TODO: The JavaScript version of Cene has FFI operations for
  ; interacting with JavaScript, naturally. See if we should do
  ; something similar for interacting with Racket.
  
  
  
  (cene-runtime
    (sink-table defined-dexes)
    (sink-table defined-values)))
