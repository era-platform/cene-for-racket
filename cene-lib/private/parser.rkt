#lang parendown racket/base

; cene/private/parser
;
; A traditional parser for Cene syntax.

;   Copyright 2020 The Era Authors
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


(require #/only-in racket/pretty pretty-print)

(require #/only-in brag/support token)
(require #/only-in br-parser-tools/lex
  char-set define-lex-abbrev lexeme lexer-src-pos)
(require #/only-in br-parser-tools/lex-sre + ? * or seq)

(require #/only-in lathe-comforts dissect fn mat)

(require #/only-in cene/private/parser-grammar parse)


(define-lex-abbrev lex-inline-whitespace (char-set " \t"))
(define-lex-abbrev lex-cr "\r")
(define-lex-abbrev lex-lf "\n")

; TODO: Include Unicode whitespace.
(define-lex-abbrev lex-whitespace
  (or lex-inline-whitespace lex-cr lex-lf))

(define-lex-abbrev lex-eq "=")
(define-lex-abbrev lex-colon ":")

; TODO: Include other Unicode open brackets.
(define-lex-abbrev lex-open-misc-bracket (char-set "([{"))

; TODO: Include other Unicode close brackets.
(define-lex-abbrev lex-close-misc-bracket (char-set ")]}"))

; TODO: Include other Unicode punctuation marks (probably the whole
; Pattern_Syntax set).
(define-lex-abbrev lex-direct-punctuation-mark (char-set "!\"$,;?`"))

; TODO: Include other Unicode identifier characters (probably the
; whole XID_Continue set and many of the optional recommended
; characters, but maybe not "." and ":" which we're already using as
; syntax or "$" which Unicode probably mainly recommends out of Java
; precedent).
;
(define-lex-abbrev lex-identifier-character
  (or
    (char-set "'-_")
    (or "0" #/char-range "1" "9")
    (char-range "a" "z")
    (char-range "A" "Z")))

(define-lex-abbrev lex-backslash "\\")
(define-lex-abbrev lex-slash "/")
(define-lex-abbrev lex-open-angular-bracket "<")
(define-lex-abbrev lex-neutral-angular-bracket "^")
(define-lex-abbrev lex-close-angular-bracket ">")
(define-lex-abbrev lex-dot ".")
(define-lex-abbrev lex-pipe "|")
(define-lex-abbrev lex-hash "#")
(define-lex-abbrev lex-grawlixable-apropos-punctuation-mark
  (or
    lex-eq
    lex-open-misc-bracket
    lex-close-misc-bracket
    lex-direct-punctuation-mark
    lex-backslash
    lex-slash
    lex-open-angular-bracket
    lex-neutral-angular-bracket
    lex-close-angular-bracket
    lex-dot
    lex-pipe
    lex-hash))
(define-lex-abbrev lex-apropos-punctuation-mark
  (or lex-colon lex-grawlixable-apropos-punctuation-mark))

; These punctuation marks are reserved for future use at the tokenizer
; level (e.g. the way we use ":" and "=" to delimit certain tokens).
; Nevertheless, we do acknowledge them just enough to allow them in
; grawlixes and escaped punctuation marks, unlike our forbidden
; characters (e.g. unassigned Unicode code points), which we don't
; allow anywhere.
;
; We're already using the rest of the non-control ASCII characters for
; whitespace, identifier characters, direct punctuation marks, and
; various specific-use punctuation marks. The control characters are
; forbidden in our syntax (not even part of these reserved
; characters).
;
(define-lex-abbrev lex-reserved-punctuation-mark (char-set "%&*+@~"))

(define-lex-abbrev lex-grawlixable-character
  (or
    lex-identifier-character
    lex-grawlixable-apropos-punctuation-mark
    lex-reserved-punctuation-mark))
(define-lex-abbrev lex-escapable-punctuation-mark
  (or lex-apropos-punctuation-mark lex-reserved-punctuation-mark))


(define (beginningless-cene-lexer)
  (define has-ended #f)
  (lexer-src-pos
    [
      (eof)
      (if has-ended
        (void)
        (begin
          (set! has-ended #t)
          (token 'END-OF-FILE lexeme)))]
    
    ; This matches any nonempty text consisting of only space and tab.
    [(+ lex-inline-whitespace) (token 'INLINE-WHITESPACE lexeme)]
    
    ; This matches carriage return, newline, or both in succession.
    [(or (seq lex-cr #/? lex-lf) lex-lf) (token 'NEWLINE lexeme)]
    
    ; This matches any nonempty text that contains only letters,
    ; digits, "'", "-", and "_".
    ;
    [(+ lex-identifier-character) (token 'IDENTIFIER lexeme)]
    
    ; This matches any colon-delimited text that does not contain
    ; colons, whitespace, or forbidden characters.
    ;
    ; TODO: For security, prefer text that doesn't mix scripts,
    ; perhaps by requiring each segment of text between Pattern_Syntax
    ; characters to conform to at least one known-good combination of
    ; scripts.
    ;
    [
      (seq lex-colon (* lex-grawlixable-character) lex-colon)
      (token 'GRAWLIX lexeme)]
    
    ; This matches "=" followed by any of the various punctuation
    ; marks we care about here, any of the ones we allow to be used
    ; directly, and any of the ones we reserve for future tokenization
    ; control.
    [
      (seq lex-eq lex-escapable-punctuation-mark)
      (token 'ESCAPED-PUNCTUATION-MARK lexeme)]
    
    ; This matches an open bracket.
    [lex-open-misc-bracket (token 'OPEN-MISC-BRACKET lexeme)]
    
    ; This matches a close bracket.
    [lex-close-misc-bracket (token 'CLOSE-MISC-BRACKET lexeme)]
    
    ; This matches a single punctuation mark either that we don't ever
    ; intend to use for tokenization control or that we do intend to
    ; make available for use as a convenient delimiter or escape
    ; sequence.
    ;
    [lex-direct-punctuation-mark
      (token 'CLOSE-DIRECT-PUNCTUATION-MARK lexeme)]
    
    [lex-backslash (token 'BACKSLASH lexeme)]
    [lex-slash (token 'SLASH lexeme)]
    [lex-open-angular-bracket (token 'OPEN-ANGULAR-BRACKET lexeme)]
    [
      lex-neutral-angular-bracket
      (token 'NEUTRAL-ANGULAR-BRACKET lexeme)]
    [lex-close-angular-bracket (token 'CLOSE-ANGULAR-BRACKET lexeme)]
    [lex-dot (token 'DOT lexeme)]
    [lex-pipe (token 'PIPE lexeme)]
    [lex-hash (token 'HASH lexeme)]))

(define (cene-lexer)
  (define beginningless-lexer (beginningless-cene-lexer))
  (define has-started #f)
  (fn in
    (if has-started
      (beginningless-lexer in)
      (begin
        (set! has-started #t)
        ; TODO: See if we can put some kind of lexeme in this token.
        (token 'BEGINNING-OF-FILE)))))

(define (parse-string str)
  (define lex (cene-lexer))
  (define in (open-input-string str))
  (port-count-lines! in)
  (parse #/fn #/lex in))

(define (parser-test str)
  (pretty-print #/syntax->datum #/parse-string str))

(parser-test "")
(parser-test " ")
(parser-test "(a b .c d)")
