#lang brag

; cene/private/parser-grammar
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


; TODO: Uncomment this file once we have this parser working, and once
; we're ready to commit to a dependency on a parser library.

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
;     nameless operation instead, which can often have a trivial
;     semantics like grouping parentheses.
;
;   - We can take this opportunity to make sure our comment syntaxes
;     and simple escape sequence syntaxes can incorporate hyperbracket
;     sigils to specify at which quoting depth the comments are erased
;     or the escape sequences apply.
;
;   - By using a more traditional EBNF syntax, we can potentially get
;     better support more traditional syntax highlighers and code
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
      | COLON)+

any-hyperbracket-sigil-starter
  : OPEN-ANGULAR-BRACKET
  | NEUTRAL-ANGULAR-BRACKET
  | CLOSE-ANGULAR-BRACKET

inline-then-inline-and-compound-whitespace
  : INLINE-WHITESPACE [compound-then-inline-and-compound-whitespace]
compound-then-inline-and-compound-whitespace
  : compound-whitespace [inline-then-inline-and-compound-whitespace]

inline-and-compound-whitespace
  : inline-then-inline-and-compound-whitespace
  | compound-then-inline-and-compound-whitespace

whitespace-lines-and-indent
  : INLINE-WHITESPACE [whitespace-lines-and-indent]
  | inactive-comment-sigil [whitespace-lines-and-indent]
  |
    active-comment-sigil
    inline-text
    [new-whitespace-lines-and-indent]
  |
    compound-then-inline-and-compound-whitespace
    [new-whitespace-lines-and-indent]

new-whitespace-lines-and-indent
  : NEWLINE [whitespace-lines-and-indent]
  | END-OF-FILE

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

active-comment-sigil: SLASH SLASH [comment-sigil-after-comment]

; NOTE: We allow our more sophisticated commenting-out operations to
; be commented out themselves.
inactive-comment-sigil
  : SLASH SLASH simple-comment-sigil [comment-sigil-after-comment]

comment-sigil-after-comment
  : hyperbracket-sigil comment-sigil-after-comment
  | DOT optional-operation

hyperbracket-sigil
  :
    any-hyperbracket-sigil-starter
    [simple-comment-sigil]
    operation-and-header

operation-and-header: optional-operation header-tokens
optional-operation: [ws] [IDENTIFIER [ws]] [COLON]

dotted-compound-token-inline-after-comment
  : hyperbracket-sigil dotted-compound-token-inline-after-comment
  | DOT [ws] IDENTIFIER
compound-token-inline-after-comment
  : dotted-compound-token-inline-after-comment
  | [ws] IDENTIFIER
compound-token-block-after-comment
  : CLOSE-ROUND-BRACKET
  | DOT DOT compound-token-block-after-comment
  | DOT DOT simple-comment-sigil compound-token-block-after-comment
  | hyperbracket-sigil compound-token-block-after-comment
  | prefix-or-nameless-header compound-token-block-after-comment

compound-token
  : BACKSLASH compound-token-inline-after-comment
  |
    OPEN-ROUND-BRACKET
    [inactive-comment-sigil]
    compound-token-block-after-comment

prefix-or-nameless-header
  :
    DOT
    simple-comment-sigil
    operation-and-header
    [ws]
    prefix-or-nameless-header
  | DOT operation-and-header
  | header-tokens

compound-whitespace
  : BACKSLASH simple-comment-sigil compound-token-inline-after-comment
  |
    OPEN-ROUND-BRACKET
    active-comment-sigil
    compound-token-block-after-comment
  
  ; NOTE:
  ;
  ; We don't handle the commenting-out of `DOT` here. That's because
  ; we use `DOT` for a few distinct purposes that each have their own
  ; commenting-out design rationale. Only a couple of those purposes
  ; benefit from a comment syntax, and they're better off handled
  ; individually in the context that gives the `DOT` its meaning.
  ;
  ; Actually, all the uses of `DOT` serve a similar purpose: To
  ; specify where a hyperbracket sigil's header should end, when
  ; there isn't a `CLOSE-ROUND-BRACKET`, `END-OF-FILE`, or another
  ; hyperbracket sigil to end it first. However, that purpose alone
  ; doesn't justify a syntax for commenting out the `DOT`, because the
  ; hyperbracket sigil's header has to end sometime.
  ;
  ; The reason we start wanting to comment out the `DOT` at all is
  ; because in some contexts, we've used a single occurrence of `DOT`
  ; to fulfill more than one purpose.
  ;
  ; These are the purposes we use `DOT` for and their commenting-out
  ; design rationales:
  ;
  ;   - In a comment sigil, we use `DOT` to end the final hyperbracket
  ;     sigil and/or signal that we may be supplying a comment
  ;     operation identifier.
  ;
  ;     - This one can't be commented out. The final hyperbracket
  ;       sigil has to end sometime, and a clearer way to comment out
  ;       the dot-and-identifier combination would be to wrap it in
  ;       commented-out parens.
  ;
  ;   - In an inline (backslashed) compound token, we use `DOT` to end
  ;     the final hyperbracket sigil.
  ;
  ;     - This one can't be commented out. The final hyperbracket
  ;       sigil has to end sometime.
  ;
  ;   - In a prefix or nameless header, we use `DOT` to end the
  ;     preceding header and to signal that we may be explicitly
  ;     designating a prefix operation identifier and/or using `COLON`
  ;     to designate a precise starting position.
  ;
  ;     - This one can be commented out. If it is, the preceding
  ;       header is still terminated, and this commented-out prefix
  ;       or nameless header becomes part of the prefacing whitespace
  ;       that belongs to the next prefix or nameless header.
  ;
  ;   - In a block, we use `DOT DOT` to end the preceding header and
  ;     designate a tail of the block. This isn't meant to be good for
  ;     anything except commenting out.
  ;
  ;     - This one can be commented out. If it is, the rest of the
  ;       block is commented out. Essentially, this is *a different
  ;       way* to comment out a hyperbracket sigil or a prefix or
  ;       nameless header appearing in a block. The usual way comments
  ;       out just that one sigil/header while leaving the rest of the
  ;       block intact, but this way comments out the rest of the
  ;       block too.
  
  ; NOTE:
  ;
  ; We've thought about having a clause in here to comment out colons:
  ;
  ;   | COLON simple-comment-sigil
  ;
  ; However, there's not much point to this. A standalone colon can be
  ; commented out by wrapping it in parens and commenting out the
  ; parens.
