#lang brag

; cene/private/parser-grammar
;
; The grammar for a traditional parser for Cene syntax.

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


; TODO: Document the purpose of this new parser. It represents a major
; change to the way we're treating Cene's syntax. The major goals, in
; rough order from most important to least important, are the
; following:
;
;   - By parsing text into a tree format, we avoid having to design
;     lexical-unit-level declaration macros so that they "pre-read
;     matching brackets" to find out where the next concurrently
;     declaration starts. This kind of design was a bit error-prone
;     in the reader macro style since the pre-reading logic and the
;     macros themselves were duplicating some behavior, which would
;     invite discrepancies.
;
;   - By defining a more traditional EBNF syntax, we can potentially
;     use a far more optimized parser framework and speed up our parse
;     times considerably.
;
;   - Since we don't have to worry about letting macros pre-read their
;     lexical extents, we can define line comments that comment out
;     unmatched parentheses.
;
;   - We can take this opportunity to make sure the tree-shaped syntax
;     is round-trippable back to text.
;
;   - We can take this opportunity to incorporate some ideas we've had
;     for terse hyperbracket notation (as previously explored in
;     notes/20200202-syntax-design-2.txt).
;
;   - We can take this opportunity to incorporate an idea we've had
;     for mixing prefix and infix notation, namely the idea to change
;     Parendown's "/" notation into a sigil that starts every prefix
;     operation. (Here, we change that sigil to ".".) If a block
;     doesn't start with a sigiled prefix operation, it represents a
;     nameless operation instead, which can often be given a trivial
;     semantics such as grouping parentheses.
;
;   - We can take this opportunity to make sure our comment syntaxes
;     and simple escape sequence syntaxes can incorporate hyperbracket
;     sigils to specify at which quoting depth the comments are erased
;     or the escape sequences apply.
;
;   - By using a more traditional EBNF syntax, we can potentially get
;     better support for more traditional syntax highlighers and code
;     editors.


; NOTE: Since this comes first in the file, it's the root nonterminal.
;
; TODO: At one point, this was the one place we used `ws` without
; making it optional (`[ws]`). Now it's optional too. See if we can
; make `ws` more like `[ws]` to simplify things.
;
header-tokens
  : [ws] [(IDENTIFIER | COLON | compound-token) header-tokens]


; tokens:
;   BEGINNING-OF-FILE
;     (a token that appears once at the beginning of the file)
;   END-OF-FILE (a token that appears once at the end of the file)
;   INLINE-WHITESPACE
;     (any nonempty text consisting of only space and tab)
;     (TODO: We should consider including Unicode whitespace, blank,
;     and control characters in this.)
;   IDENTIFIER
;     (any nonempty text that does not contain space, tab, carriage
;     return, newline, backlsash, "/", "(", ")", "<", "^", ">", ".",
;     or ":")
;   NEWLINE (matches carriage return, newline, or both in succession)
;   BACKSLASH (the \ character)
;   SLASH ("/")
;   OPEN-ROUND-BRACKET ("(")
;   CLOSE-ROUND-BRACKET (")")
;   OPEN-ANGULAR-BRACKET ("<")
;   NEUTRAL-ANGULAR-BRACKET ("^")
;   CLOSE-ANGULAR-BRACKET (">")
;   DOT (".")
;   COLON (":")
;   PIPE ("|")
;   HASH ("#")

; Any nonempty text that does not include carriage return or newline.
inline-text
  :
    (INLINE-WHITESPACE
      | IDENTIFIER
      | BACKSLASH
      | SLASH
      | OPEN-ROUND-BRACKET
      | CLOSE-ROUND-BRACKET
      | OPEN-ANGULAR-BRACKET
      | NEUTRAL-ANGULAR-BRACKET
      | CLOSE-ANGULAR-BRACKET
      | DOT
      | COLON
      | PIPE
      | HASH)+

any-prefix-sigil-starter
  : DOT
  | OPEN-ANGULAR-BRACKET
  | NEUTRAL-ANGULAR-BRACKET
  | CLOSE-ANGULAR-BRACKET

inline-then-inline-and-compound-whitespace
  : INLINE-WHITESPACE [compound-then-inline-and-compound-whitespace]
compound-then-inline-and-compound-whitespace
  : compound-whitespace inline-and-compound-whitespace

inline-and-compound-whitespace
  : inline-then-inline-and-compound-whitespace
  | compound-then-inline-and-compound-whitespace

whitespace-lines-and-indent
  : END-OF-FILE
  | INLINE-WHITESPACE [whitespace-lines-and-indent]
  | inactive-comment-sigil [whitespace-lines-and-indent]
  |
    active-comment-sigil
    [inline-text]
    new-whitespace-lines-and-indent
  |
    compound-then-inline-and-compound-whitespace
    [new-whitespace-lines-and-indent]

new-whitespace-lines-and-indent
  : END-OF-FILE
  | NEWLINE [whitespace-lines-and-indent]

ws
  : inline-and-compound-whitespace [new-whitespace-lines-and-indent]
  | BEGINNING-OF-FILE [whitespace-lines-and-indent]
  | new-whitespace-lines-and-indent

; NOTE: We allow many constructs to be commented out. For the sake of
; readability, the only things we allow to be commented out in a way
; that has sophisticated unquoting logic are lines and paren-wrapped
; blocks. If something else must be commented out with sophisticated
; unquoting logic, it's clearer to wrap that thing with an extra layer
; of parens so the unquoting logic has something to be attached to.
; Those things which permit sophisticated unquoting logic use
; `active-comment-sigil` for their comments, and the rest use
; `simple-comment-sigil`.

simple-comment-sigil: SLASH SLASH

active-comment-sigil
  : simple-comment-sigil
  | SLASH piped-comment-sigil

; NOTE: We allow our more sophisticated commenting-out operations to
; be commented out themselves.
inactive-comment-sigil: SLASH simple-comment-sigil piped-comment-sigil

piped-comment-sigil
  : PIPE grouping-or-operation-and-header piped-comment-sigil-rest

piped-comment-sigil-rest
  : prefix-sigil piped-comment-sigil-rest
  | PIPE SLASH

prefix-sigil
  :
    any-prefix-sigil-starter
    [simple-comment-sigil]
    operation-and-header

; NOTE:
;
; We could specify `operation-and-header` like this:
;
;   operation-and-header: [ws] [IDENTIFIER [ws]] [COLON] header-tokens
;
; However, that introduces ambiguity into the grammar. Instead, we
; allow the operation name (if any) and colon (if any) to be treated
; as header tokens, and (TODO) we'll process them on a second pass.
;
operation-and-header: header-tokens

; NOTE:
;
; We could specify `grouping-or-operation-and-header` like this:
;
;   grouping-or-operation-and-header
;     : [ws]
;     | operation-and-header
;
; However, that introduces ambiguity into the grammar. Instead, (TODO)
; we'll process whitespace-only headers on a second pass.
;
grouping-or-operation-and-header: operation-and-header

compound-token-inline-after-pipe
  : prefix-sigil compound-token-inline-after-pipe
  | PIPE compound-token-inline-after-comment
compound-token-inline-after-comment
  :
    PIPE
    grouping-or-operation-and-header
    compound-token-inline-after-pipe
  | IDENTIFIER
nonnameless-compound-token-block-after-comment
  : CLOSE-ROUND-BRACKET
  
  ; NOTE: In a block, we use `HASH` to end the preceding header and
  ; designate a tail of the block. This isn't meant to be good for
  ; anything except commenting out. When we comment it out, the rest
  ; of the block is commented out too.
  ;
  | HASH [simple-comment-sigil] compound-token-block-after-comment
  
  | prefix-sigil nonnameless-compound-token-block-after-comment
compound-token-block-after-comment
  :
    grouping-or-operation-and-header
    nonnameless-compound-token-block-after-comment

compound-token
  : BACKSLASH compound-token-inline-after-comment
  |
    OPEN-ROUND-BRACKET
    [inactive-comment-sigil]
    compound-token-block-after-comment

compound-whitespace
  : BACKSLASH simple-comment-sigil compound-token-inline-after-comment
  |
    OPEN-ROUND-BRACKET
    active-comment-sigil
    compound-token-block-after-comment
  
  ; NOTE:
  ;
  ; We've thought about having a clause in here to comment out colons:
  ;
  ;   | COLON simple-comment-sigil
  ;
  ; However, there's not much point to this. A standalone colon can be
  ; commented out by wrapping it in parens and commenting out the
  ; parens.
