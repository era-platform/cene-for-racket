#lang parendown scribble/manual

@; cene/scribblings/cene.scrbl
@;
@; A Racket library with entrypoints to the Cene programming language.

@;   Copyright 2018 The Era Authors
@;
@;   Licensed under the Apache License, Version 2.0 (the "License");
@;   you may not use this file except in compliance with the License.
@;   You may obtain a copy of the License at
@;
@;       http://www.apache.org/licenses/LICENSE-2.0
@;
@;   Unless required by applicable law or agreed to in writing,
@;   software distributed under the License is distributed on an
@;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
@;   either express or implied. See the License for the specific
@;   language governing permissions and limitations under the License.


@title{Cene Mk. II}

@nested[#:style 'inset]{
  ℹ️ This language is a work in progress. If you're here, you might be interested in the latest usable incarnation of Cene, @hyperlink["https://github.com/era-platform/cene"]{Cene Mk. I}.
}

Cene is a programming language that's designed to let programs be compiled to multiple target languages at once. Cene also has a variety of special features including weak opening parens, string quasiquotation, and a macroexpander that uses LVars-style deterministic concurrency. Cene Mk. I was written in JavaScript, and Cene Mk. II is a recreation of Cene in Racket.

In the process of developing Cene Mk. II, I've split out several of these systems into their own individual Racket libraries and made further refinements to them. @hyperlink["https://github.com/lathe/parendown-for-racket"]{Parendown} implements weak opening parens, @hyperlink["https://github.com/lathe/punctaffy-for-racket"]{Punctaffy} solidifies some ideas related to quasiquotation syntaxes, and @hyperlink["https://github.com/lathe/interconfection-for-racket"]{Interconfection} implements a deterministic concurrency framework.

In addition, Cene Mk. II has more informative error messages than Cene Mk. I, and it contains some preliminary work on additional module system features to facilitate separate compilation.

However, Cene Mk. II isn't ready for use yet. In particular, the error-checking code could use some optimization.



@table-of-contents[]



(TODO: Write this documentation. We'll need at least one export in the library before we have anything to document.)
