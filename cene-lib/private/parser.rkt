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
(require #/only-in br-parser-tools/lex char-set lexeme lexer-src-pos)
(require #/only-in br-parser-tools/lex-sre + ~ ? or seq)

(require #/only-in lathe-comforts dissect fn mat)

(require #/only-in cene/private/parser-grammar parse)


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
    [(+ #/char-set " \t") (token 'INLINE-WHITESPACE lexeme)]
    
    ; This matches carriage return, newline, or both in succession.
    [(or (seq "\r" #/? "\n") "\n") (token 'NEWLINE lexeme)]
    
    ; This matches "=" followed by any of the various punctuation
    ; marks we care about here.
    [
      (seq "=" #/char-set "=()[]{}\\/<^>.:|#")
      (token 'ESCAPED-PUNCTUATION-MARK lexeme)]
    
    ; This matches any nonempty text that does not contain any of the
    ; various whitespace characters and punctuation marks we care
    ; about here.
    [
      (+ #/~ #/char-set " \t\r\n=()[]{}\\/<^>.:|#")
      (token 'IDENTIFIER lexeme)]
    
    ; This matches an open bracket.
    ;
    ; TODO: Include other Unicode open brackets.
    ;
    [(char-set "([{") (token 'OPEN-MISC-BRACKET lexeme)]
    
    ; This matches a close bracket.
    ;
    ; TODO: Include other Unicode close brackets.
    ;
    [(char-set ")]}") (token 'CLOSE-MISC-BRACKET lexeme)]
    
    ["\\" (token 'BACKSLASH lexeme)]
    ["/" (token 'SLASH lexeme)]
    ["<" (token 'OPEN-ANGULAR-BRACKET lexeme)]
    ["^" (token 'NEUTRAL-ANGULAR-BRACKET lexeme)]
    [">" (token 'CLOSE-ANGULAR-BRACKET lexeme)]
    ["." (token 'DOT lexeme)]
    [":" (token 'COLON lexeme)]
    ["|" (token 'PIPE lexeme)]
    ["#" (token 'HASH lexeme)]))

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
