# Lexical units


Cene codebases will be full of files. Files aren't quite the same as modules, and we need to decide what definition forms files typically use. These will ideally be the same as the forms used by lexically scoped local definition blocks, so rather than simply talking about "files," we'll use the term "lexical unit."


## Background on Era modules

Era modules are collected in sets; the semantics of an Era module doesn't depend on what its filename is, what directory it's in, what order it's in relative to other modules, or how many times it appears in the set.

Era modules are distributed as authorized binary files tagged with a particular UUID that specifies the language of the rest of the file. In the short term this UUID will usually determine a particular text encoding and a particular (possibly very informal) specification of the Cene programming language with which to interpret that text. In the short term, the way that a binary file is "authorized" may be rather trivial and insecure, but the idea is that each Era module has a set of people it can act on behalf of.

Semantically, and specifically for purposes of Cene, the content of an Era module is that it defines function implementations for certain sink struct tags (but only those for which the module is authorized to be the author of), and those function implementations may be Cene expressions which construct or deconstruct certain sink struct tags (but only those for which the module is authorized to be the user of). "Sink" here refers to Cene's kitchen sink type. Cene is an untyped language, so the kitchen sink type is the only type it has, and we rarely refer to a sink in the context of talking about Cene, but Era modules someday may have a more sophisticated type system.


## Background on lexically scoped local definitions

Several languages such as Scheme, JavaScript, and Racket make it easy to copy definitions from the top level of a file into a local lexical region and back with at most minor adjustments. This feature lessens the effort of performing those particular forms of refactoring, it facilitates metaprogramming transformations that programmatically perform this refactoring, and in some sense these increases in power correspond with an increase in simplicity, since we can understand the global scope to be just another layer of local scope.

In fact, there are often slight discrepancies between the top level and local definitions in these languages, but there's enough of a common subset of functionality that the similarity is useful anyway.

To facilitate local *macros* in Cene (like Scheme's `let-syntax`), the Cene macroexpander takes a `qualify` function, uses that function to qualify names of macros it looks up, and passes that function into macro calls. That way, macros that need to bind local macros can do so by recursively invoking the macroexpander with a modified `qualify` function that intercepts uses of the names it's binding.

That helps for something like Racket's `let-syntax`, but it doesn't get us to local definitions that are quite as convenient as they are in those other languages, because it means we have to know what names *will* be locally bound before we begin to expand the definitions. Fortunately, a local definition block syntax that required forward declarations for all the local bindings would still be rather useful both for refactoring and for metaprogramming, so the `qualify` function is probably already sufficient for this purpose.

Nevertheless, if we do want to be more ambitious, we may be able to make use of Interconfection's closed-world assumption extensibility features to forgo forward declarations. Specifically, a call to the `qualify` function would block until either the name was determined to have a local definition or all the familiarity tickets that could have registered local definitions have already been spent. This implies most top-level declaration forms would have to receive familiarity tickets from the macroexpander, and most of them would have to very quickly either dispose of the ticket or use the ticket to register some local definitions.

This is something we will be taking into account in the design of definition forms for lexical units.

([A different approach was considered at one point.](https://gist.github.com/rocketnia/7fddafee7433a8a0c63f732babc6c489) In that approach, the local definition block would have contained explicit declarations of which names were and were not bound by the block. Since those explicit declarations would be just about as verbose as the forward declarations needed by the `let-syntax`-style approach, we're exploring another option here.)


## Background context on the role of files in Cene codebases

Cene libraries are expected to be maintained as codebases composed of several Cene source files with a Cene build script that makes use of those files to create a sharable Era module. Some of their dependencies are modules and language extensions available at build time, and some of them are modules that need only be available when the compiled module is run. While compile-time dependencies on modules couldn't be eliminated entirely (because the core language is itself a set of modules), many of them could be eliminated by including files that declared another library's syntaxes without its run time functionality (essentially C header files), so sharing these files would be another part of Cene code-sharing culture. Altogether that's:

* Build-time dependencies on modules.
* Build-time dependencies on language extensions.
* Run-time dependencies on modules.
* Manually copied "header files" in the codebase.

In this model, the role of a *file* is much different than the role of a module. Some modules may provide functionality that behaves similarly to the act of macroexpanding a Cene file, but those probably aren't typical modules.

Instead, a file bears more resemblance to a set of locally scoped definitions. The definitions in the file are local to the file, and definition that needs to cross file boundaries must be explicitly transported somehow (perhaps using exports and imports in each file or a dependency injection configuration in the build script).

It's becoming apparent lately that there may be some value to compiling files of Cene code *individually*, if only for speedy build times. That is, files are an abstraction which may be worth distributing, if only distributing for use across multiple local builds. Since "header files" are not something the programmer is actively maintaining, those files are a perfect candidate for compiling for use across multiple builds, and that gives us a very specific use case for modules which act like files.

A file is an embarrassingly syntax-heavy entity in Cene. Part of the behavior of a file is the behavior of reader macros as they parse its text, and reader macros are allowed to install definitions visible to the rest of the build as they go along, so their work can't be skipped. This means if the behavior of a file were reproduced faithfully by a module, there probably wouldn't be any performance boost at all.

But just because reader macros are *allowed* to install arbitrary definitions doesn't mean they always have to. Not all modules correspond to files, and the modules that *do* correspond to files can for the most part correspond to well-behaved files whose reader macro calls don't install any visible definitions.

At this point, Cene's top-level declaration syntaxes macroexpand into expressions that return `directive` values, and those values contain the logic for installing the particular definitions that the top-level declarations are designed to define (rather than intermediate ones needed only for the macroexpansion process). As long as we stick to this convention, then it should be reasonably easy to give Cene builds a way to compile a Cene file to a module which "macroexpands" to the same directive expressions but without doing all the expensive text processing that the file's macroexpansion has to do. (TODO MOBILE: Follow through on that plan.)

If someday we find a need to apply this policy more strictly, we might arrive at something like Racket's separate compilation guarantee. In Racket, files roughly correspond with modules, and Racket's design goes out of its way to ensure a module that requires another module can almost never observe whether the other module was loaded from its compiled form or its source form. It does this using phase separation where an expression that implements a macro is evaluated in a different phase (and with different bindings) than the expressions that make use of that macro.

Recently, we've implemented definition forms for use in Cene's prelude. This is a start, but the prelude is a little special compared to other Cene files in that most of its definitions are meant to blend in with the other built-in operations of this Cene implementation.


## Other ideas for what could help in Cene codebases

It would be nice to be able to associate reference documentation, syntax-highlighting hints, and indentation hints with Cene macros. Cene's definition effects seem to make it easy enough to pick places for these things to be defined, so it's "just" a matter of designing how they each work and building tools for them.

It would be nice for a debugging printout of a Cene first-class value to make use of names that are actually in scope in some specific part of the Cene program. Ideally, the printout could be copied directly into that part of the code. This would seem to suggest that every Cene lexical unit (file top level or local definition block) should use familiarity tickets to contribute tentative printout functions that a single big printout function can recursively delegate to, as well as names of shadowed operations that should no longer be possible to print in that scope.


## Designing definition forms for lexical units

It's time to iron out the design of the definition forms that Cene codebases will primarily use, both in the context of file top levels and in the context of local definition forms.

Based on all the use case considerations laid out above, it's important for performance that their primary effects are achieved in the form of `directive` expressions, and it's important for convenient refactoring and metaprogramming that they make use of familiarity tickets to say which names are being locally defined as opposed to being looked up from a parent lexical scope.

There are three kinds of things usually defined in a Cene program:

* Struct tags.

* Functions. Each function definition is really a definition of the function implementation for a nullary struct tag, so this mostly falls under "struct tags."

* Macros. The definitions of struct tags and functions also tend to define corresponding macros to make them easier to use.


### Struct tag export conditions

Struct tags are a bit of a surprising case to consider here because we've just recently changed them to *stop* using the `qualify` function for their innate main tag name and their innate projection names. The reason we did this is because most struct tags defined by the prelude are exported for use by all Cene users, and an exported struct tag will typically need a name a potential user can easily write an import for.

It's quite likely we'll want exported struct tags and exported functions to use easy-to-access names for their innate main tag names but for non-exported ones to use... maybe not qualified names... maybe unique names. (Whether we use easy-to-access names for the projections probably doesn't matter.) A unique name used for this purpose should be derived from the unique name provided to a `directive` body rather than one obtained from the macroexpander; that way the name doesn't have to be reified when compiling a file to a file-like module.

Or (considering some things from the "other ideas" section) perhaps a unique name would make it too hard to share stable links to documentation of local definitions, or too hard to encode a value's debugging printout in a way that works in a local definition block. Maybe we should allow a local definition form to declare a specific name that makes it easy to refer to from those external places, and then instead of using a unique name for a non-exported struct tag's innate main tag name, we can use a name derived from that stable path. Maybe the default name shouldn't even be a unique name, but instead a name based on the source location or something. Or maybe it *should* be a unique name, but a definition should be installed that associates that unique name with that source location so that a stable external path can be determined. We already intend for run time errors to refer to source locations, so using source locations as the interface for debug printouts would probably be just fine,

Let's assume that "is this struct tag supposed to be exported or not?" is a question with the same kind of answer as "is this name defined in this lexical unit or not?" That is, we'll use familiarity tickets to register certain struct tags as being non-exported, and the rest will use whatever export status is implied for them in the surrounding lexical scope.

Export status is a detailed thing. A struct type can be defined for some author and some user. A non-exported struct tag's author and user will usually be the codebase's author, but a codebase may compile to multiple modules, and each module may have a different combination of authors, so it may be unclear which one the struct tag should be associated with. This means we'll likely want to set up a "default author-user for non-exported struct types" variable in the lexical scope which may be bound explicitly when necessary.


### Using run time local variables from local function definitions

Oh, another challenge has to do with allowing a local definition block to refer to run-time values bound to variables in the surrounding code. One of the most compelling uses of local definitions tends to be the easy creation of mutually recursive local functions which make use of local bindings, so it would be good to make sure this is possible. A macro or `directive` expression, as opposed to a function, should not be able to refer to a run-time value.

Let's do this like so: First we introduce a concept of "unceremonious expression operation," a kind of macro where the call sites look like variable references. Let's have all variable bindings actually bind these operations; let's have the actual variables in the expression AST be unique names which these operations refer to; and let's have the free variables of an expression be associated with certain metadata. Then when we compile a function definition, let's make it so a free variable in the function body actually causes the function's struct tag to have another projection on it (named according to the metadata), specifically to hold the value of that variable. The function's bounded expression operation (i.e. macro) still expands to a call to a newly constructed struct instance, but the subexpression that constructs that instance supplies projection values as expressions obtained from the metadata. Thanks to the use of unique names for local variables, these expressions can access the variables even in situations where they would otherwise appear to be shadowed. Note that with this design, top-level function definitions still behave like always since they have no free variables.


### "Inner" and "outer" scopes of a declaration operation

If we treat top-level declarations the same way as expressions, there's a design problem. How can the macroexpander begin even one top-level expression's macro call if it still doesn't know if that macro will be rebound by one of the definitions?

Here's what we'll do: Forget that the top-level declarations are expressions. They're now "declarations," and their macro calls use "bounded declaration operations" instead of "bounded expression operations." These operations can have a slightly different interface.

The way this helps is that we can have the declaration operations take in two qualify functions instead of one. One qualify function (the "outer" one) can be used for reading subforms where the lexical unit's definitions aren't in scope, and the other (the "inner" one) can be used for reading subforms where they are. The top-level declarations themselves will be expanded using the outer qualify function, so we don't have to worry about whether those macros will be locally shadowed. Most of the declarations' subexpressions will be expanded in the inner scope, so they can be mutually recursive.

We'll do something a little different than actually passing in the two functions separately: We'll set up a binding (named by `name-for-local-qualify`) in the outer qualify function that maps via `extfx-get` to a maybe containing the inner qualify function. Most other qualify functions will be set up to bind this to `(nothing)`.

If someone needs to define a definition form and then use it right away, this approach makes that nontrivial to do. They'll need to use it from within a lexical unit where the outer scope includes the definition form. Just for this purpose, we'll design a top-level declaration named `importing-as-nonlocal` that processes a local block of declarations within an outer scope where part of the original outer scope is shadowed.


### Using familiarity tickets for declarations

We've mentioned using familiarity tickets to manage exports. We've mentioned using "bounded declaration operations" instead of "bounded expression operations" for declarations. Specifically, a declaration operation will not receive an expression sequence output stream from the macroexpander or give one back; instead, it will receive two familiarity tickets from the macroexpander, which we will call the "interface familiarity ticket" and the "implementation familiarity ticket."

The interface familiarity ticket's contributions decide what this lexical unit defines, what it exports, and what struct tag export circumstances it determines. When all these contributions are available, we know what definitions and struct tag export circumstances from the outer scope are _not_ shadowed.

The implementation familiarity ticket's contributions decide the set of `directive` expressions which can be executed to compute the actual definitions of the lexical unit's exports. When all these contributions are available, we can compile the lexical unit by compiling its complete set of `directive` expressions.


---

All right, this seems like enough to work with to begin to design a specific suite of operations.


---

## Designs for forms that relate to lexical units

(TODO: Design things that can specify a struct tag's export conditions (i.e. whether to obscure its main tag name, who can author it, and who can use it).)

(TODO: Design something that can locally override the default export conditions of a struct that has no explicit export conditions.)

(TODO: Design things that can perform imports of struct tags and functions from specific modules. This might just be a combination of specifying a struct tag's export conditions and defining a struct metadata operation and a bounded expression operation for local convenience.)

(TODO: Eventually consider whether we need more bounded export metadata operations such as Racket's `except-in`.)

(TODO: Eventually consider designing a way to define callable structs with one or more projections.)

(TODO: Eventually consider whether there's a good design for a way to define local-variable-capturing function definitions so that they have better control over the projections they use to capture variables and what order those projections appear in in their struct metadata entries.)

(TODO: Eventually consider splitting up some of the things each of the operations defines into their own definition forms to provide better control. We'd need the following operations:

* Defines a struct metadata operation with automatically determined struct export conditions, with a main tag name and projection names based on the given identifiers, and with some of the projection names being positional while some are (orderless) captures of given expressions. Also defines a corresponding unceremonious export metadata operation so that the struct's export conditions can be declared.

* Given struct metadata, defines a bounded expression operation that constructs a struct based on that metadata and using positional arguments provided at the call site.

* Given struct metadata and an arity N, defines a bounded expression operation that constructs a struct based on that metadata using no positional arguments, then calls the result with the call site's N given arguments in a curried style. If N is zero, it calls the result with `(nil)` instead.

* Given struct metadata, defines a function implementation. To convert the struct to an opaque function, it returns an opaque function that causes an error when called.

* Given struct metadata, defines a function implementation. To convert the struct to an opaque function, it runs the given code.

* A variant of `case` (and we'll likely also want `cast`, `construct`, and `{dex,cline,merge,fuse}-struct`) that matches based on surface-syntax projection strings rather than based on position.)

(TODO: Eventually see if we should have `def-struct-metadata-op`.)

(TODO: Eventually see if we should have `def-{,unexportable-}{bounded,freestanding,unceremonious}-export-metadata-op`. Once we work on implementing the operations we have defined, an appropriate abstraction for users to define their own will probably reveal itself.)

(TODO: Eventually see if we should have more operations for working with located input file handles, such as replacing their file registry and replacing their authentication registry. If nothing else, we may at least want a way to "chroot" into a context where only a subtree of the original file registry is available.)

(TODO: Eventually see if we should have ways to replace the current self-referential file handle.)


---

Table of contents:

* Bounded declaration operations:
  * `declare`
  * `declare-matched-brackets-section-separately`
  * `include-file`
  * `importing-as-nonlocal`
  * `import-from-declaration`
  * `import-from-file`
  * `export`
  * `def-struct`
  * `defn`
  * Macro definition forms for expression operations:
    * `def-bounded-expr-op`
    * `def-unexportable-nameless-bounded-expr-op`
    * `def-freestanding-expr-op`
    * `def-unceremonious-expr-op`
  * Macro definition forms for declaration operations:
    * `def-bounded-decl-op`
    * `def-unexportable-nameless-bounded-decl-op`
    * `def-freestanding-decl-op`
    * `def-unceremonious-decl-op`
  * `def-unexportable-unceremonious-export-metadata-op-as-constant`
* Bounded export metadata operations:
  * `exports`
  * `local`
  * Export metadata operations for specific kinds of operations:
    * `exports-for-struct-metadata-op`
    * Export metadata operations for expression operations:
      * `exports-for-bounded-expr-op`
      * `exports-for-nameless-bounded-expr-op`
      * `exports-for-freestanding-expr-op`
      * `exports-for-unceremonious-expr-op`
    * Export metadata operations for declaration operations:
      * `exports-for-bounded-decl-op`
      * `exports-for-nameless-bounded-decl-op`
      * `exports-for-freestanding-decl-op`
      * `exports-for-unceremonious-decl-op`
    * Export metadata operations for export metadata operations:
      * `exports-for-bounded-export-metadata-op`
      * `exports-for-nameless-bounded-export-metadata-op`
      * `exports-for-freestanding-export-metadata-op`
      * `exports-for-unceremonious-export-metadata-op`
* Bounded expression operations:
  * `let-declared`
* Functions:
  * `name-for-local-qualify`
  * `name-for-self-referential-file-handle`
  * `is-input-file`
* Bounded expression operations:
  * `this-input-file`
* Functions:
  * `input-file-base-maybe`
  * `input-file-base`
  * `input-file-root`
  * `input-file-elem`


---

```
(declare decl ...)
```

A bounded declaration operation.

Declares all the things the given declarations declare. This can be good for situations where only one declaration is expected syntactically.

Note that this doesn't pre-read its lexical extent. The macroexpander won't proceed to process the remainder of the stream after the closing bracket until the last declaration before the closing bracket has been processed. If more concurrency is desired, use `declare-matched-brackets-section-separately`.


---

```
(declare-matched-brackets-section-separately decl ...)
```

A bounded declaration operation.

(In short: Declares all the things the given declarations declare, but allows the macroexpander to work on the code after the closing paren at the same time.)

Pre-reads its lexical extent to find matching brackets. Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Declares all the things the given declarations declare.


---

```
(include-file input-file)
```

A bounded declaration operation.

(In short: Performs all the declarations that appear in another file. The other file can use all the same macros the current lexical unit can, and when the file has export declarations, those specify exports from the current lexical unit. This will tend to come in handy for programs which use some sort of global-looking framework but which make use of multiple implementations of that framework.)

Reads an expression (`input-file`) using the current lexical unit's outer scope. Evaluates that expression, which must have no free variables, to obtain a located input file handle. Obtains and processes the file's located input stream as a series of declarations which use nearly the same outer scope as the current lexical unit's outer scope, but with the self-referential file handle binding changed so as to refer to the handle being processed.

Note that this doesn't pre-read its lexical extent. The macroexpander won't proceed to process the remainder of the stream after the closing bracket until the last declaration of the file has been processed. If more concurrency is desired, use `declare-matched-brackets-section-separately` around this operation.

Note that unlike `import-from-file`, using this operation more than once can cause a file to be processed more than once, potentially with different outer scopes. Since the reader macros that parse the file may differ depending on what's in scope at the place the file is included, typically this kind of file won't be possible to compile to a more compile-time-efficient equivalent. It's recommended to use `import-from-file` instead wherever possible.


---

```
(importing-as-nonlocal export-metadata decl ...)
```

A bounded declaration operation.

Reads the given export metadata term using the current lexical unit's outer scope. Processes the given declarations with the same inner scope as the current lexical unit, but with part of the outer scope shadowed according to the export metadata.

This can be good for situations where a lexical unit contains a definition of a declaration operation that needs to be used right away. Usually that declaration operation can't be used from the same part of the code since it only exists in the inner scope, not the outer scope where the declaration operations are looked up. However, it can be used inside one of these regions with the appropriate bindings imported.

Note that this doesn't pre-read its lexical extent. The macroexpander won't proceed to process the remainder of the stream after the closing bracket until the last declaration before the closing bracket has been processed. If more concurrency is desired, use `declare-matched-brackets-section-separately`.


---

```
(import-from-declaration decl export-metadata ...)
```

A bounded declaration operation.

(In short: Starts a lexical unit inside this one which can see all the things visible in this one, but which doesn't interfere with this one's definitions and exports. The outer lexical unit only sees things it imports from the inner lexical unit. This can be useful when a user wants to make verbatim use of an existing piece of code that has exports (such as a file or code example obtained from someone else) but doesn't want their own code to export those exports.)

Pre-reads its lexical extent to find matching brackets. Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Processes the given declaration (which may perform multiple declarations using `declare`) as its own lexical unit, using nearly the same outer scope as the current lexical unit's outer scope, but with the inner scope binding changed so as to refer to this inner lexical unit's definitions.

Reads the given export metadata terms using the inner lexical unit's outer scope, and spends its interface familiarity ticket to say that it defines each of the things listed by each of those export metadata operations.

Concurrently looks up each of the things it promised to define and defines it using its implementation familiarity ticket.

Note that there may be interdependencies between the declaration and the surrounding lexical unit, so there may be a nontrivial chain of logical dependency in between spending the interface familiarity ticket to say what definitions will be imported and finally spending the implementation familiarity ticket to propagate those definitions.


---

```
(import-from-file input-file export-metadata ...)
```

A bounded declaration operation.

(In short: Performs imports corresponding to exports of another file. The file referred to this way will be macroexpanded if it hasn't been already.)

Pre-reads its lexical extent to find matching brackets. Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Reads an expression (`input-file`) using the current lexical unit's outer scope. Evaluates that expression, which must have no free variables, to obtain a located input file handle.

Causes that located input file handle to be instantiated if it hasn't been instantiated yet. To instantiate it, first we create an outer scope that's nearly the same as that handle's file registry's outer scope, but with the inner scope binding changed so as to refer to the file's own definitions and with the self-referential file handle binding changed so as to refer to the handle being instantiated. We write that outer scope to the handle's file registry, and we obtain and process the file's located input stream as its own lexical unit which may consist of multiple declarations which use the derived outer scope.

Concurrently, this operation reads the given export metadata terms using the derived outer scope, and it spends its interface familiarity ticket to say that it defines each of the things listed by each of those export metadata operations.

Concurrently, it looks up each of the things it promised to define and defines it using its implementation familiarity ticket.

Note that there may be interdependencies between the declaration and the surrounding lexical unit, so there may be a nontrivial chain of logical dependency in between spending the interface familiarity ticket to say what definitions will be imported and finally spending the implementation familiarity ticket to propagate those definitions.


---

```
(export export-metadata ...)
```

A bounded declaration operation.

(In short: Reads the given export metadata terms using the current lexical unit's outer scope and exports all the things each one of them specifies.)

Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and does the rest of its work concurrently with that.

Spends its interface familiarity ticket to say that it exports each of the things listed by each of the given export metadata entries. The export metadata terms are read using the current lexical unit's outer scope.

Spends its implementation familiarity ticket without defining anything.


---

```
(def-struct
  export-metadata-op-and-struct-metadata-op-and-bounded-expr-op-and-main-tag
  proj-tag ...)
```

A bounded declaration operation.

(In short: Defines a struct using inferred export conditions, and defines its function implementation so that it causes an error when called.)

Spends its interface familiarity ticket to say that it defines an unceremonious export metadata operation, a struct metadata operation, and a bounded expression operation, each with its name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`. For the sake of assisting with debug printouts, the declarations that this defines a struct metadata operation and a bounded expression operation each come with metadata to express that the operations are capable of being used for constructing structs.

Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and does the rest of its work concurrently with that.

Contributes a first `directive` expression that defines something with a name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`:

* An unceremonious export metadata operation that exports this unceremonious export metadata operation as well as the struct metadata operation and the bounded expression operation defined below.

Determines the export conditions of the struct tag. These may depend on declarations in this lexical unit.

Contributes a second `directive` expression that defines three more things, the first two of which have names based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`:

* A struct metadata operation with automatically determined struct export conditions, with a main tag name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`, and with projection names based on the given `proj-tag` identifiers.

* A bounded expression operation. The definition of this operation reads a number of expressions equal to the number of `proj-tag` identifiers. It expands to an expression that constructs a struct with the tag specified above, using the given expressions to populate each field of the struct.

* A function implementation for the tag specified above. To convert the struct to an opaque function, it returns an opaque function that causes an error when called.


---

```
(defn
  export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag
  blame-arg
  positional-arg ...
  body)
```

A bounded declaration operation.

(In short: Defines a function using inferred export conditions. The `body` may refer to local variables in the lexical scope surrounding this lexical unit.)

Pre-reads its lexical extent to find matching brackets. Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Spends its interface familiarity ticket to say that it defines an unceremonious export metadata operation, a struct metadata operation, and a bounded expression operation, each with its name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`. For the sake of assisting with debug printouts, the declaration that this defines a struct metadata operation comes with metadata to express that the operation is capable of being used for constructing structs.

Contributes a first `directive` expression that defines something with a name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`:

* An unceremonious export metadata operation that exports this unceremonious export metadata operation as well as the struct metadata operation and the bounded expression operation defined below.

Reads `body` as an expression in the lexical unit's inner scope, but while augmenting that scope with unceremonious expression operations for each of the `...-arg` identifiers that associate them with local variables. Wraps the result in a blamed lambda expression which binds `blame-arg` as its blame argument and the last `positional-arg` as its primary argument. Wraps this again in function expressions that bind the other arguments. The expression that results from all this wrapping may have free variables, and each of those free variables must have metadata associating it with a distinct innate projection name and an expression that makes sense (but which we don't necessarily verify to make sense) in the lexical scope surrounding this lexical unit.

Determines the export conditions of the struct tag. These may depend on declarations in this lexical unit.

Contributes a second `directive` expression that defines three more things, the first two of which have names based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`:

* A struct metadata operation with automatically determined struct export conditions, with a main tag name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`, and with projection names based on the metadata of the free variables of the wrapped `body` expression.

* A bounded expression operation. The definition of this operation reads a number of expressions equal to the number of `positional-arg` identifiers. It expands to an expression that constructs a struct with the tag specified above, then calls it with each of the given expressions in turn. The construction expression uses the metadata of the free variables of the wrapped `body` expression to determine what expressions to use to populate each field of the struct. These expressions will usually refer to local variables bound in the lexical scope surrounding this lexical unit, referring to them by obscure alternative names that can't be shadowed by any of the usual variable-binding forms.

* A function implementation for the tag specified above. To convert the struct to an opaque function, it deconstructs the struct to bind the appropriate local variables, and it executes the wrapped `body` expression.


---

```
(def-bounded-expr-op
  export-metadata-op-and-bounded-expr-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  text-input-stream-arg
  expression-sequence-output-stream-arg
  extfx-then-arg
  body)
```

A bounded declaration operation.

(In short: Defines a macro, or more specifically a bounded expression operation.)

Pre-reads its lexical extent to find matching brackets. Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Spends its interface familiarity ticket to say that it defines an unceremonious export metadata operation and a bounded expression operation, each with its name based on `export-metadata-op-and-bounded-expr-op`.

Contributes a first `directive` expression that defines something with a name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`:

* An unceremonious export metadata operation that exports this defined operation as well as a bounded expression operation defined below.

Reads `body` as an expression in the lexical unit's inner scope, but while augmenting that scope with unceremonious expression operations for each of the `...-arg` identifiers that associate them with local variables. Wraps the result in a blamed lambda expression which binds `read-blame-arg` as its blame argument and `expression-sequence-output-stream-arg` as its primary argument. Wraps this again in function expressions that bind the other arguments. Verifies that the expression that results from all this wrapping has no free variables.

Contributes a second `directive` expression that defines another thing with a name based on `export-metadata-op-and-bounded-expr-op`:

* A bounded expression operation. The definition of this operation incorporates the wrapped function expression, passing its result some specific values for `definition-site-unique-name-arg` and `definition-site-qualify-arg` based on the `directive`'s unique name and qualify function and otherwise passing it the same arguments the operation receives from the macroexpander. The expected result is an extfx effectful computation, just as the macroexpander would need. The arguments corresponding to the ones received from the macroexpander are:

  * `read-blame-arg`: A blame value that indicates the call site of the operation that invoked the macroexpander.

  * `expression-blame-arg`: A blame value that indicates the source location of the brackets surrounding this bounded expression operation.

  * `call-site-unique-name-arg`: A authorized name that hasn't yet been claimed as unique, which represents the unique identity of the call site.

  * `call-site-qualify-arg`: A qualify function which represents the way names are qualified at the call site.

  * `text-input-stream-arg`: An unspent text input stream which can be used to read the body of this bounded expression operation.

  * `expression-sequence-output-stream-arg`: An unspent expression sequence output stream which can be used to write the sequence of expressions that this bounded expression operation expands into. Typically, an expression operation that represents a comment will write precisely zero expressions, and an expression operation that represents an expression will write precisely one.
  
  * `extfx-then-arg`: A function that produces an extfx effectful computation given updated versions of `call-site-unique-name`, `call-site-qualify-arg`, `text-input-stream-arg`, and `expression-sequence-output-stream-arg`. The updated versions of the streams must be future incarnations of the same streams that were supplied originally. The updated unique name can be any authorized name that hasn't yet been claimed as unique. The qualify function can be any qualify function, and it will typically be used for code that follows this bounded expression operation's closing bracket.


---

```
(def-unexportable-nameless-bounded-expr-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  text-input-stream-arg
  expression-sequence-output-stream-arg
  extfx-then-arg
  body)
```

A bounded declaration operation.

Defines a nameless bounded expression operation. This works just like `def-bounded-expr-op` except for the fact that it doesn't take a name for the defined operation and it doesn't define a corresponding unceremonious export metadata operation for convenience with exporting this definition.

A named bounded expression operation `foo` is typically called with the syntax `(foo ...)` or `(.foo ...)`. The former is really a call to a nameless bounded expression operation, the default binding of which proceeds to read and process the `foo` identifier and to call the result as a macro. When the nameless bounded expression operation may be bound to something else, the alternate syntax `(.foo ...)` is a more reliable way to call named operations `foo`.


---

```
(def-freestanding-expr-op
  export-metadata-op-and-freestanding-expr-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  text-input-stream-arg
  expression-sequence-output-stream-arg
  extfx-then-arg
  body)
```

A bounded declaration operation.

Defines a freestanding expression operation, which is a kind of macro. This works just like `def-bounded-expr-op` except for the kind of macro it defines. Unlike a bounded expression operation, which is typically called with syntax like `(foo ...)`, a freestanding expression operation is typically called with syntax like `\foo ...`.


---

```
(def-unceremonious-expr-op
  export-metadata-op-and-unceremonious-expr-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  expression-sequence-output-stream-arg
  extfx-then-arg
  body)
```

A bounded declaration operation.

Defines an unceremonious expression operation, which is a kind of macro. This works just like `def-bounded-expr-op` except for the kind of macro it defines and the fact that this kind of macro neither receives an `text-input-stream-arg` nor passes along an updated text input stream to its `extfx-then-arg`. Unlike a bounded expression operation, which is typically called with syntax like `(foo ...)`, an unceremonious expression operation is typically called with syntax like `foo` -- which is to say, just an identifier by itself.


---

```
(def-bounded-decl-op
  export-metadata-op-and-bounded-decl-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  interface-familiarity-ticket-arg
  implementation-familiarity-ticket-arg
  text-input-stream-arg
  extfx-then-arg
  body)
(def-unexportable-nameless-bounded-decl-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  interface-familiarity-ticket-arg
  implementation-familiarity-ticket-arg
  text-input-stream-arg
  extfx-then-arg
  body)
(def-freestanding-decl-op
  export-metadata-op-and-freestanding-decl-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  interface-familiarity-ticket-arg
  implementation-familiarity-ticket-arg
  text-input-stream-arg
  extfx-then-arg
  body)
(def-unceremonious-decl-op
  export-metadata-op-and-unceremonious-decl-op
  read-blame-arg
  expression-blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  interface-familiarity-ticket-arg
  implementation-familiarity-ticket-arg
  extfx-then-arg
  body)
```

Bounded declaration operations.

These work just like `def-bounded-expr-op`, `def-nameless-bounded-expr-op`, `def-freestanding-expr-op`, and `def-unceremonious-expr-op`, except that instead of defining expression operations, they define declaration operations. Each kind of declaration operation is similar to the corresponding kind of expression operation except that it does not receive an expression sequence output stream, it does not pass an expression sequence output stream to its `extfx-then-arg`, and instead it receives an additional two arguments:

* The operation receives an `interface-familiarity-ticket-arg`, a familiarity ticket which it can spend to contribute information about what this lexical unit defines, what it exports, and what struct tag export circumstances it determines.

* The operation receives an `implementation-familiarity-ticket-arg`, a familiarity ticket which it can spend to contribute `directive` expressions that serve to compute the actual definitions of the things this lexical unit defines.

The operation's `call-site-qualify-arg` is specifically the qualify function for the *outer scope* of the lexical unit, which is for looking things up in the scope surrounding the lexical unit this operation's call occurs in. The inner qualify function, which is for the scope where the lexical unit's definitions are visible, can be obtained from this one using `name-for-local-qualify`. and the value of that binding will usually be a maybe of a function that represents the inner scope where the lexical unit's definitions are in force. Calls made to the inner/local qualify function will tend to block until all the lexical unit's declarations' interface familiarity tickets have been spent.

Likewise, the `extfx-then-arg` callback is specifically to be passed an *outer* qualify function; the inner qualify function will be passed along by nature of the fact that the outer one carries it.

The expressions a declaration operation contributes to its `implementation-familiarity-ticket-arg` will be evaluated to obtain `directive` values, which will be used to perform the actual definitions promised when the `interface-familiarity-ticket-arg` was spent.


---

```
(def-unexportable-unceremonious-export-metadata-op-as-constant
  unceremonious-export-metadata-op original-export-metadata)
```

A bounded declaration operation.

(In short: Defines an export metadata entry that can be referred to with syntax like `foo` -- which is to say, just an identifier by itself. The definition is expanded immediately rather than expanded every time this name is used. Exporting using this name only exports the exports it refers to, not the export operation itself.)

Pre-reads its lexical extent to find matching brackets. Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Spends its interface familiarity ticket to say that it defines an unceremonious export metadata operation with a name based on `unceremonious-export-metadata-op`.

Reads `original-export-metadata` as an export metadata term in the lexical unit's outer scope to obtain an export metadata entry.

Contributes a `directive` expression that defines something with a name based on `unceremonious-export-metadata-op`:

* An unceremonious export metadata operation that expands to the export metadata entry that was obtained from `original-export-metadata`.


---

```
(exports export-metadata ...)
```

A bounded export metadata operation.

(In short: Combines export metadata.)

Reads the given export metadata terms using the current scope. Expands to export metadata that specifies to export each of the things that each of those metadata entries specifies to export. If the metadata entries conflict, this causes an error instead.


---

```
(local export-metadata ...)
```

A bounded export metadata operation.

(In short: Reads export metadata using the current lexical unit's inner scope instead of its outer scope. Most exports will need to use this.)

Reads the given export metadata terms using the current lexical unit's inner scope. Expands to export metadata that specifies to export each of the things that each of those metadata entries specifies to export. If the metadata entries conflict, this causes an error instead.


---

```
(exports-for-struct-metadata-op op ...)
(exports-for-bounded-expr-op op ...)
(exports-for-nameless-bounded-expr-op)
(exports-for-freestanding-expr-op op ...)
(exports-for-unceremonious-expr-op op ...)
(exports-for-bounded-decl-op op ...)
(exports-for-nameless-bounded-decl-op)
(exports-for-freestanding-decl-op op ...)
(exports-for-unceremonious-decl-op op ...)
(exports-for-bounded-export-metadata-op op ...)
(exports-for-nameless-bounded-export-metadata-op)
(exports-for-freestanding-export-metadata-op op ...)
(exports-for-unceremonious-export-metadata-op op ...)
```

Bounded export metadata operations.

Each of these operations expands to export metadata that specifies to export the operations indicated (struct metadata operation, bounded expression operation, etc.). This doesn't depend on actually looking up the operations given.

For nameless operations, this operation doesn't take any arguments, and it just expands to the export metadata entry for the appropriate nameless operation.

For named operations, this operation takes any number of identifiers, and it expands to a single export metadata entry that exports operations by every one of those names.


---

```
(let-declared decl body)
```

A bounded expression operation.

At compile time, this processes the given declaration (which may perform multiple declarations using `declare`) as its own lexical unit, using nearly the same outer scope as the current expression's scope, but with the inner scope binding changed so as to refer to this lexical unit's definitions. If the lexical unit has any exports, this causes an error. Either way, this proceeds by compiling the given `body` expression in that lexical unit's inner scope.


---

```
(name-for-local-qualify)
```

A function.

Returns a name which can typically be qualified and then looked up using `extfx-get` to obtain a maybe of a qualify function.

If the name is qualified using the outer qualify function of a lexical unit (the qualify function representing the bindings that are visible in the area surrounding that lexical unit), then the qualify function obtained this way represents the same lexical unit's inner qualify function (the one representing the bindings that are visible once that lexical unit's definitions are taken into account).


---

```
(name-for-self-referential-file-handle)
```

A function.

Returns a name which can typically be qualified and then looked up using `extfx-get` to obtain a located input file handle.

If the name is qualified using the outer qualify function of a lexical unit, then the file handle usually refers to the file that lexical unit appears in. In the context of that lexical unit, we call this the current self-referential file handle.


---

```
(is-input-file v)
```

A function.

Returns `(yep/trivial)` if `v` is a located input file handle. Otherwise, returns `(nope/trivial)`.


---

```
(this-input-file)
```

A bounded expression macro.

Expands to an expression that returns the current self-referential file handle, which is a located input file handle.

This expression will not typically be permitted in Cene subprograms that are compiled to other platforms. Most subprogram compilers will reject it with an error. This is because a located input file handle carries information about the build process's filesystem locations, an entire directory tree full of file contents, and a mapping between friendly names and authentication keys.


---

```
(input-file-base-maybe file)
```

A function.

Given a located input file handle, if that handle's path has a parent directory, returns a `just` of another located input file handle which is nearly the same except that the path refers to that parent directory instead. Otherwise, returns a `nothing`.


---

```
(input-file-base file)
```

A function.

Given a located input file handle whose path has a parent directory, returns another located input file handle which is nearly the same except that the path refers to that parent directory instead.


---

```
(input-file-root file)
```

A function.

Given a located input file handle, returns another located input file handle which is nearly the same except that the path refers to the root ancestor of that handle's path.


---

```
(input-file-elem key file)
```

A function.

Given a string and a located input file handle, returns another located input file handle which is nearly the same except that the path refers to a subdirectory or file named by that string under the original file path. This only builds a path; it doesn't check that a file or subdirectory with that string name actually exists.
