# Cene Mk. II

<p align="center">
  <img width="312" height="166" src="https://era-platform.github.io/cene/assets/logo/cene-5-cropped-164.png" alt="Cene logo" title="Cene logo" />
</p>

[![CI](https://github.com/era-platform/cene-for-racket/actions/workflows/ci.yml/badge.svg)](https://github.com/era-platform/cene-for-racket/actions/workflows/ci.yml)

> ℹ️ This language is a work in progress. If you're here, you might be interested in the latest usable incarnation of Cene, [Cene Mk. I](https://github.com/era-platform/cene).

Cene is a programming language that's designed to let programs be compiled to multiple target languages at once. Cene also has a variety of special features including weak opening parens, string quasiquotation, and a macroexpander that uses LVars-style deterministic concurrency. Cene Mk. I was written in JavaScript, and Cene Mk. II is a recreation of Cene in Racket.

In the process of developing Cene Mk. II, I've split out several of these systems into their own individual Racket libraries and made further refinements to them. [Parendown](https://github.com/lathe/parendown-for-racket) implements weak opening parens, [Punctaffy](https://github.com/lathe/punctaffy-for-racket) solidifies some ideas related to quasiquotation syntaxes, and [Interconfection](https://github.com/lathe/interconfection-for-racket) implements a deterministic concurrency framework.

In addition, Cene Mk. II has more informative error messages than Cene Mk. I, and it contains some preliminary work on additional module system features to facilitate separate compilation.

However, Cene Mk. II isn't ready for use yet. In particular, the error-checking code could use some optimization.


## Installation and use

This is a library for Racket. To install it, run `raco pkg install --deps search-auto` from the `cene-lib/` directory, and then put an import like `(require cene)` in your Racket program.

The interface to Cene will eventually be documented in the `cene-doc/` package's documentation.
