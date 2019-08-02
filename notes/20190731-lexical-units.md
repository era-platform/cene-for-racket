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

Nevertheless, if we do want to be more ambitious, we may be able to make use of Effection's closed-world assumption extensibility features to forego forward declarations. Specifically, a call to the `qualify` function would block until either the name was determined to have a local definition or all the familiarity tickets that could have registered local definitions have already been spent. This implies most top-level declaration forms would have to receive familiarity tickets from the macroexpander, and most of them would have to very quickly either dispose of the ticket or use the ticket to register some local definitions.

This is something we will be taking into account in the design of definition forms for lexical units.

([A different approach was considered at one point.](https://gist.github.com/rocketnia/7fddafee7433a8a0c63f732babc6c489). In that approach, the local definition block would have contained explicit declarations of which names were and were not bound by the block. Since those explicit declarations would be just about as verbose as the forward declarations needed by the `let-syntax`-style approach, we're exploring another option here.)


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

Struct tags are a bit of a surprising case to consider here because we've just recently changed them to *stop* using the `qualify` function for their innate main tag name and their innate projection names. The reason we did this is because most struct tags defined by the prelude are exported for use by all Cene users, and an exported struct tag will typically need a name a potential user can easily write an import for.

It's quite likely we'll want exported struct tags and exported functions to use easy-to-access names for their innate main tag names but for non-exported ones to use... maybe not qualified names... maybe unique names. (Whether we use easy-to-access names for the projections probably doesn't matter.) A unique name used for this purpose should be derived from the unique name provided to a `directive` body rather than one obtained from the macroexpander; that way the name doesn't have to be reified when compiling a file to a file-like module.

Or (considering some things from the "other ideas" section) perhaps a unique name would make it too hard to share stable links to documentation of local definitions, or too hard to encode a value's debugging printout in a way that works in a local definition block. Maybe we should allow a local definition form to declare a specific name that makes it easy to refer to from those external places, and then instead of using a unique name for a non-exported struct tag's innate main tag name, we can use a name derived from that stable path. Maybe the default name shouldn't even be a unique name, but instead a name based on the source location or something. Or maybe it *should* be a unique name, but a definition should be installed that associates that unique name with that source location so that a stable external path can be determined. We already intend for run time errors to refer to source locations, so using source locations as the interface for debug printouts would probably be just fine,

Let's assume that "is this struct tag supposed to be exported or not?" is a question with the same kind of answer as "is this name defined in this lexical unit or not?" That is, we'll use familiarity tickets to register certain struct tags as being non-exported, and the rest will use whatever export status is implied for them in the surrounding lexical scope.

Export status is a detailed thing. A struct type can be defined for some author and some user. A non-exported struct tag's author and user will usually be the codebase's author, but a codebase may compile to multiple modules, and each module may have a different combination of authors, so it may be unclear which one the struct tag should be associated with. This means we'll likely want to set up a "default author-user for non-exported struct types" variable in the lexical scope which may be bound explicitly when necessary.

Oh, another challenge has to do with allowing a local definition block to refer to run-time values bound to variables in the surrounding code. One of the most compelling uses of local definitions tends to be the easy creation of mutually recursive local functions which make use of local bindings, so it would be good to make sure this is possible. A macro or `directive` expression, as opposed to a function, should not be able to refer to a run-time value.

Let's do this like so: First we introduce a concept of "identifier expression operation," a kind of macro where the call sites look like variable references. Let's have all variable bindings actually bind these operations, let's have the actual variables in the expression AST be unique names which these operations refer to, and let's have the free variables of an expression be associated with certain metadata. Then when we compile a function definition, let's make it so a free variable in the function body actually causes the function's struct tag to have another projection on it (named according to the metadata), specifically to hold the value of that variable. The function's bounded expression operation (macro) still expands to a call to the structs, but the subexpression that constructs that struct supplies projection values based on the metadata. Note that with this design, top-level function definitions still behave like always.

All right, this seems like enough to work with to begin to design a specific suite of operations.


---

## Designs for the top-level definition forms

(TODO: Design something that can perform imports corresponding to exports of other files. The file imported from this way will be macroexpanded if it hasn't been already.)

(TODO: Design something that can include other files as though they're directly part of the current lexical unit, including seeing the same macros this one does, and treating their exports as being part of the current lexical unit's exports. This doesn't have to play nicely with the idea of compiling files to modules, and the file will have to be expanded each time it's imported. This will tend to come in handy for programs which use some sort of global-looking framework which has various possible implementations.)

(TODO: Design something that can start a lexical unit inside this one which can see all the things visible in this one, but which doesn't interfere with this one's exports. The outer lexical unit only sees things it imports from the inner lexical unit. A common pattern will be to use one of these blocks to include a file that has exports but whose exports we don't want to export ourselves.)

(TODO: Design things that can specify a struct tag's export conditions (i.e. whether to obscure its main tag name, who can author it, and who can use it).)

(TODO: Design something that can locally override the default export conditions of a struct that has no explicit export conditions.)

(TODO: Design things that can perform imports of struct tags and functions from specific modules. This might just be a combination of specifying a struct tag's export conditions and defining a struct metadata operation and a bounded expression operation for local convenience.)

(TODO: Eventually consider designing things like Racket's `all-defined-out`, `except-in`, etc. Maybe we'll at least want a way to define an export metadata operation that combines multiple existing export metadata operations.)

(TODO: Eventually consider designing a way to define callable structs with one or more projections.)

(TODO: Eventually consider whether there's a good design for a way to define local-variable-capturing function definitions so that they have better control over the projections they use to capture variables and what order those projections appear in in their struct metadata entries.)

(TODO: Eventually consider splitting up some of the things each of the operations defines into their own definition forms to provide better control.)


---

```
(export export-metadata-op ...)
```

(In short: It looks up the given export operations and exports all the things each one of them specifies.)

Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and does the rest of its work concurrently with that.

Spends its "what does this lexical unit define, what does it export, and what struct tag export circumstances does it determine?" familiarity ticket to say that it exports each of the things listed by each of the given export metadata operations.


---

```
(def-struct
  export-metadata-op-and-struct-metadata-op-and-bounded-expr-op-and-main-tag
  proj-tag ...)
```

(In short: Defines a struct using inferred export conditions, and defines its function implementation so that it causes an error when called.)

Spends its "what does this lexical unit define, what does it export, and what struct tag export circumstances does it determine?" familiarity ticket to say that it defines an export metadata operation, a struct metadata operation, and a bounded expression operation, each with its name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`. For the sake of assisting with debug printouts, the declarations that this defines a struct metadata operation and a bounded expression operation each come with metadata to express that the operations are capable of being used for constructing structs.

Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and does the rest of its work concurrently with that.

Expands to a `directive` expression that defines four things. The first three have names based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`:

* An export metadata operation that exports all three of these defined operations.

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

(In short: Defines a function using inferred export conditions. The `body` may refer to local variables in the lexical scope surrounding this lexical unit.)

Pre-reads its lexical extent to find matching brackets.

Spends its "what does this lexical unit define, what does it export, and what struct tag export circumstances does it determine?" familiarity ticket to say that it defines an export metadata operation, a struct metadata operation, and a bounded expression operation, each with its name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`. For the sake of assisting with debug printouts, the declaration that this defines a struct metadata operation comes with metadata to express that the operation is capable of being used for constructing structs.

Returns control to the macroexpander to continue expanding the portion of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Reads `body` as an expression with identifier expression operations for each of the `...-arg` identifiers that associate them with local variables. Wraps the result in a blamed lambda expression which binds `blame-arg` as its blame argument and the last `positional-arg` as its primary argument. Wraps this again in function expressions that bind the other arguments. The expression that results from all this wrapping may have free variables, and each of those free variables must have metadata associating it with a distinct innate projection name and an expression that makes sense (but which we don't necessarily verify to make sense) in the lexical scope surrounding this lexical unit.

Expands to a `directive` expression that defines four things. The first three have names based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`:

* An export metadata operation that exports all three of these defined operations.

* A struct metadata operation with automatically determined struct export conditions, with a main tag name based on `export-metadata-op-and-struct-medatata-op-and-bounded-expr-op-and-main-tag`, and with projection names based on the metadata of the free variables of the wrapped `body` expression.

* A bounded expression operation. The definition of this operation reads a number of expressions equal to the number of `positional-arg` identifiers. It expands to an expression that constructs a struct with the tag specified above, then calls it with each of the given expressions in turn. The construction expression uses the metadata of the free variables of the wrapped `body` expression to determine what expressions to use to populate each field of the struct. These expressions will usually refer to local variables bound in the lexical scope surrounding this lexical unit, referring to them by obscure alternative names that can't be shadowed by any of the usual variable-binding forms.

* A function implementation for the tag specified above. To convert the struct to an opaque function, it deconstructs the struct to bind the appropriate local variables, and it executes the wrapped `body` expression.


---

```
(def-bounded-expr-op
  export-metadata-op-and-bounded-expr-op
  blame-arg
  definition-site-unique-name-arg
  definition-site-qualify-arg
  call-site-unique-name-arg
  call-site-qualify-arg
  text-input-stream-arg
  expression-sequence-output-stream-arg
  body)
```

(In short: Defines a macro, or more specifically a bounded expression operation.)

Pre-reads its lexical extent to find matching brackets.

Spends its "what does this lexical unit define, what does it export, and what struct tag export circumstances does it determine?" familiarity ticket to say that it defines an export metadata operation and a bounded expression operation, each with its name based on `export-metadata-op-and-bounded-expr-op`.

Returns control to the macroexpander to continue expanding the portino of the stream that follows the closing paren, and concurrently does the rest of its work by expanding a modified copy of its input stream where the stream ends after the closing paren.

Reads `body` as an expression with identifier expression operations for each of the `...-arg` identifiers that associate them with local variables. Wraps the result in a blamed lambda expression which binds `blame-arg` as its blame argument and `expression-sequence-output-stream-arg` as its primary argument. Wraps this again in function expressions that bind the other arguments. Verifies that the expression that results from all this wrapping has no free variables.

Expands to a `directive` expression that defines two things with names based on `export-metadata-op-and-bounded-expr-op`:

* An export metadata operation that exports both of these defined operations.

* A bounded expression operation. The definition of this operation incorporates the wrapped function expression, passing its result some specific values for `definition-site-unique-name-arg` and `definition-site-qualify-arg` based on the `directive`'s unique name and qualify function and otherwise passing it the same arguments the operation receives from the macroexpander.
