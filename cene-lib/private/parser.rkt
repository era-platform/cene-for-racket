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
(require #/only-in br-parser-tools/lex-sre + ~ ? * or seq)

(require #/only-in lathe-comforts dissect fn mat)

(require #/only-in cene/private/parser-grammar parse)


(define-lex-abbrev lex-inline-whitespace (char-set " \t"))
(define-lex-abbrev lex-cr "\r")
(define-lex-abbrev lex-lf "\n")
(define-lex-abbrev lex-whitespace
  (or lex-inline-whitespace lex-cr lex-lf))

(define-lex-abbrev lex-eq "=")
(define-lex-abbrev lex-colon ":")
(define-lex-abbrev lex-open-misc-bracket (char-set "([{"))
(define-lex-abbrev lex-close-misc-bracket (char-set ")]}"))
(define-lex-abbrev lex-backslash "\\")
(define-lex-abbrev lex-slash "/")
(define-lex-abbrev lex-open-angular-bracket "<")
(define-lex-abbrev lex-neutral-angular-bracket "^")
(define-lex-abbrev lex-close-angular-bracket ">")
(define-lex-abbrev lex-dot ".")
(define-lex-abbrev lex-pipe "|")
(define-lex-abbrev lex-hash "#")
(define-lex-abbrev lex-apropos-punctuation-mark
  (or
    lex-eq
    lex-colon
    lex-open-misc-bracket
    lex-close-misc-bracket
    lex-backslash
    lex-slash
    lex-open-angular-bracket
    lex-neutral-angular-bracket
    lex-close-angular-bracket
    lex-dot
    lex-pipe
    lex-hash))
    


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
    ;
    ; TODO: We should consider including Unicode whitespace, blank,
    ; and control characters in this. We should not include carriage
    ; returns or newlines.
    ;
    [(+ lex-inline-whitespace) (token 'INLINE-WHITESPACE lexeme)]
    
    ; This matches carriage return, newline, or both in succession.
    [(or (seq lex-cr #/? lex-lf) lex-lf) (token 'NEWLINE lexeme)]
    
    ; This matches any nonempty text that does not contain any of the
    ; various whitespace characters and punctuation marks we care
    ; about here.
    ;
    ; TODO: Only include code points that are assigned in Unicode and
    ; are neither whitespace nor Pattern_Syntax.
    ;
    [
      (+ #/~ #/or lex-whitespace lex-apropos-punctuation-mark)
      (token 'IDENTIFIER lexeme)]
    
    ; This matches any colon-delimited text that does not contain
    ; whitespace or colons.
    ;
    ; TODO: Include Unicode whitespace. For security, prefer text that
    ; doesn't mix scripts, perhaps by requiring each segment of text
    ; between Pattern_Syntax characters to conform to at least one
    ; known-good combination of scripts.
    ;
    [
      (seq lex-colon (* #/~ #/or lex-whitespace lex-colon) lex-colon)
      (token 'GRAWLIX lexeme)]
    
    ; This matches "=" followed by any of the various punctuation
    ; marks we care about here.
    ;
    ; TODO: Include non-ASCII Unicode Pattern_Syntax characters here.
    ;
    [
      (seq lex-eq lex-apropos-punctuation-mark)
      (token 'ESCAPED-PUNCTUATION-MARK lexeme)]
    
    ; This matches an open bracket.
    ;
    ; TODO: Include other Unicode open brackets.
    ;
    [lex-open-misc-bracket (token 'OPEN-MISC-BRACKET lexeme)]
    
    ; This matches a close bracket.
    ;
    ; TODO: Include other Unicode close brackets.
    ;
    [lex-close-misc-bracket (token 'CLOSE-MISC-BRACKET lexeme)]
    
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
