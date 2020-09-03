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
header-tokens: header-token*


; tokens:
;   BEGINNING-OF-FILE
;     (a token that appears once at the beginning of the file)
;   END-OF-FILE (a token that appears once at the end of the file)
;   INLINE-WHITESPACE
;     (any nonempty text consisting of only space and tab)
;     (TODO: We should consider including Unicode whitespace, blank,
;     and control characters in this.)
;   NEWLINE (matches carriage return, newline, or both in succession)
;   IDENTIFIER
;     (any nonempty text that that contains only letters, digits, "'",
;     "-", and "_".)
;   GRAWLIX
;     (any colon-delimited text that does not contain colons,
;     whitespace, or forbidden characters)
;   ESCAPED-PUNCTUATION-MARK
;     ("=" followed by any of the various punctuation marks we care
;     about here, any of the ones we allow to be used directly
;     (`DIRECT-PUNCTUATION-MARK`), and any of the ones we reserve for
;     future tokenization control)
;   OPEN-MISC-BRACKET (an open bracket, namely "(", "[", or "{")
;   CLOSE-MISC-BRACKET (a close bracket, namely ")", "]", or "}")
;   DIRECT-PUNCTUATION-MARK
;     (a single punctuation mark either that we don't ever intend to
;     use for tokenization control (namely, the characters "," and ";"
;     which are too easily confused with "." and ":", the characters
;     "!" and "?" which are too tinted by sentiment, and the character
;     "$" which is too region-specific) or that we do intend to use as
;     a convenient delimiter or escape sequence (namely,
;     the " character, which is good for delimiting string-like
;     inputs, and the character "`" which lets us write "\`" as an
;     escape sequence for a backslash))
;   BACKSLASH (the \ character)
;   SLASH ("/")
;   OPEN-ANGULAR-BRACKET ("<")
;   NEUTRAL-ANGULAR-BRACKET ("^")
;   CLOSE-ANGULAR-BRACKET (">")
;   DOT (".")
;   PIPE ("|")
;   HASH ("#")
;
; characters we reserve for future use in controlling tokenization:
;   % & * + @ ~
;
; forbidden characters:
;   ASCII control characters
;   for the moment, non-ASCII characters (TODO: Support more Unicode.)
;
; Thanks to the `GRAWLIX` tokens, we can have DSLs with nicely
; symmetrical arrow notations like `(1 :->: 2 :<-: 3)` without having
; to first parse `>` and `<` according to their hyperbracket-related
; purposes, flatten that parse result into a string again, and
; re-tokenize. If a DSL does care to go to all that trouble, it
; potentially can get `(1 -> 2 <- 3)` to work, but it'll probably be
; pretty inefficient.


; Any token other than `BEGINNING-OF-FILE`, `END-OF-FILE`, and
; `NEWLINE`.
;
inline-text-token
  : INLINE-WHITESPACE
  | IDENTIFIER
  | GRAWLIX
  | ESCAPED-PUNCTUATION-MARK
  | OPEN-MISC-BRACKET
  | CLOSE-MISC-BRACKET
  | BACKSLASH
  | SLASH
  | OPEN-ANGULAR-BRACKET
  | NEUTRAL-ANGULAR-BRACKET
  | CLOSE-ANGULAR-BRACKET
  | DOT
  | PIPE
  | HASH

; Any text that does not include carriage return or newline (or the
; beginning- or end-of-file markers).
inline-text: inline-text-token*

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
  | active-comment-sigil inline-text new-whitespace-lines-and-indent
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
  | SLASH PIPE comment-sigil-after-comment

; NOTE: We allow our more sophisticated commenting-out operations to
; be commented out themselves.
inactive-comment-sigil
  : SLASH PIPE simple-comment-sigil comment-sigil-after-comment

comment-sigil-after-comment: prefixes PIPE SLASH

prefix-sigil-starter
  : DOT
  | OPEN-ANGULAR-BRACKET
  | NEUTRAL-ANGULAR-BRACKET
  | CLOSE-ANGULAR-BRACKET
prefix-sigil
  : prefix-sigil-starter [simple-comment-sigil] operation-and-header

; NOTE:
;
; We could specify `operation-and-header` like this:
;
;   operation-and-header
;     : [ws] [IDENTIFIER [ws]] [EMPTY-GRAWLIX] header-tokens
;
; In this case, `EMPTY-GRAWLIX` would match the specific grawlix `::`,
; which serves the purpose of a simple delimiter.
;
; However, that specification introduces ambiguity into the grammar.
; Instead, we allow the operation name (if any) and grawlix delimiter
; (if any) to be treated as header tokens, and (TODO) we'll process
; them on a second pass.
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

prefixes: grouping-or-operation-and-header prefix-sigil*

compound-token-inline-after-piped
  : IDENTIFIER
  | GRAWLIX
  | ESCAPED-PUNCTUATION-MARK
  
  ; This is for writing an escaped close bracket.
  | CLOSE-MISC-BRACKET
  
  | DIRECT-PUNCTUATION-MARK

compound-token-inline-after-comment
  : prefixes PIPE compound-token-inline-after-piped
active-compound-token-inline-after-backslash
  : PIPE compound-token-inline-after-comment
  | compound-token-inline-after-piped

compound-token-backward-after-piped: BACKSLASH
compound-token-backward-after-comment
  : prefixes PIPE compound-token-backward-after-piped
active-compound-token-backward-after-bracket
  : PIPE compound-token-backward-after-comment
  | compound-token-backward-after-piped

compound-token-block-after-prefixes
  : CLOSE-MISC-BRACKET
  
  ; NOTE: In a block, we use `HASH` to end the preceding header and
  ; designate a tail of the block. This isn't meant to be good for
  ; anything except commenting out. When we comment it out, the rest
  ; of the block is commented out too.
  ;
  | HASH [simple-comment-sigil] compound-token-block-after-comment
compound-token-block-after-comment
  : prefixes compound-token-block-after-prefixes

header-token
  : BACKSLASH active-compound-token-inline-after-backslash
  
  ; This is for escaping an open bracket. The escape sequence is
  ; written backwards, like (\ or (|...|\ for instance, but the part
  ; within the |...| is still written in the usual direction. The
  ; backwards design makes escaped brackets appear more symmetrical
  ; and raises the visibility of the open bracket by letting it be
  ; placed at the very beginning of a line of code.
  ;
  | OPEN-MISC-BRACKET active-compound-token-backward-after-bracket
  
  ; TODO: Verify the open and close brackets match up on a second
  ; pass.
  |
    OPEN-MISC-BRACKET
    [inactive-comment-sigil]
    compound-token-block-after-comment
  
  | IDENTIFIER
  | GRAWLIX
  | ESCAPED-PUNCTUATION-MARK
  | DIRECT-PUNCTUATION-MARK
  | ws

compound-whitespace
  :
    BACKSLASH
    PIPE
    simple-comment-sigil
    compound-token-inline-after-comment
  
  ; This is for a commented-out escape of an open bracket. The escape
  ; sequence is written backwards, like (|//...|\ for instance, but
  ; the part within the |...| is still written in the usual direction.
  |
    OPEN-MISC-BRACKET
    PIPE
    simple-comment-sigil
    compound-token-backward-after-comment
  
  ; TODO: Verify the open and close brackets match up on a second
  ; pass.
  |
    OPEN-MISC-BRACKET
    active-comment-sigil
    compound-token-block-after-comment
  
  ; NOTE:
  ;
  ; We've thought about having a clause in here to comment out
  ; grawlixes:
  ;
  ;   | GRAWLIX simple-comment-sigil
  ;
  ; However, there's not much point to this. A standalone grawlix can
  ; be commented out by wrapping it in parens and commenting out the
  ; parens.
