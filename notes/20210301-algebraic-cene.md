# Algebraic Cene

A file defines a few things:

* Functions (and macros, which may have a lot in common with functions)
  * which have arguments with types, contracts, and friendly names for debugging
  * which have optional keyword arguments
    * which are annotated with some type
      * whose higher inductive structure (if any) is extrapolated to a function extensionality theorem for this function
      * whose parametricity structure (i.e. what indexed set of free type variables they range over, the kinds of those types, and how to make a relation that zips over values of this type by relating each occurrence of inhabitants of one of the free types according to a given relation for that type) is extrapolated to a parametricity theorem for this function
      * whose error-handling guarded structure (i.e. what information is available as part of the type's main API and what information is strictly for error-handling purposes) is extrapolated to the error-handling guarded structure of this function
      * whose induction guarded structure (i.e. what information is available at each step of some induction-in-progress; cf. "guarded computational type theory") is extrapolated to the induction guarded structure of this function
  * which have effect types
  * which have arguments representing the location of the call site for the sake of error messages
    * which may be detailed enough to display the call site's run time call stack as well
  * for which the details can vary at compile time depending on the expected effect types and the expected types in positive positions of the result type
  * which carry enough information to quote them and their dependencies for clients who have permission
* Compile-time information such as
  * Expression macros (to whatever extent they're not covered by functions)
  * Export bundles
* Lexical unit compile-time optional arguments
  * which are annotated with some type
    * whose various higher-dimensional structure (listed above for function arguments) is extrapolated into the structure of this lexical unit
  * which may be generative (TODO: How? Should passing in an argument require a proof that two generative arguments don't overlap? Should there be a way to take a not-yet-argument-instantiated lexical unit and specify a permutation of generative names to obtain a facilitator between the lexical unit instantiations that use those permutations of names? Perhaps there's only one type of generative optional argument, one that specifies nothing but a unique name, and in order to have others, usually they're just computed in terms of arguments of that type.)
* Type synonyms
  * which have constructors
    * which have fields
  * which have relation constructors, e.g. equality, for a higher inductive type:
    * Nil = Nil
    * (x = y) -> (xs = ys) -> (Cons x xs = Cons y ys)
  * which have pattern-matching eliminators
  * which have induction principles
  * which have corresponding lexical unit compile-time optional arguments to specify what their representation actually is
    * which by default are generative (thus preventing two types with identical specifications from being identical for the purposes of the lexical unit's internal typechecking)

An expression macro doesn't just expand its entire body; it leaves behind some parts that are unprocessed expressions, and it specifies how it would modify the compilation environment to process those expressions. In particular, this specification of how an expression is modified tends to be detailed enough so that it can be undone for the purposes of a given subexpression (e.g. for an unquoted subexpression or a subexpression that comes from a macro call's inputs rather than its generated code). It also tends to be specific enough to allow incremental recompilation to proceed when that expression is modified without re-expanding the macro.



Generativity is one part of this plan that doesn't make much sense. How will we deal with it? We have several use cases to worry about:

* Era modules: With Era's "meaning-preserving modularity" modules, each module may have its own content-based identity (e.g. hash of the code), its own installation-based identity (e.g. some ID associated with the user's decision to install it), and the identity/ies of its signature-supplying author(s) to work with to make deterministic unique names.
* A Cene codebase is more complex to think about; one codebase can generate multiple Era modules, and it isn't one of those Era modules itself, but lexical units in the codebase may be compiled. Rarely, there may be legitimate reasons for subsets of a codebase to take measures to protect themselves from each other's interference, although programmers will usually be able to address this through treating different subdirectories as different Cene codebases. For deterministically generating unique names, a codebase can work with its own installation-based identity (e.g. some ID associated with the developer's decision to build it) and the identity/ies of its signature-supplying author(s).
* An Era communication channel is even more complex, since it's a codebase and IDE configuration state that multiple authors with mutual distrust are editing through negotiation in view of a witness.
And an Era library upgrade feed, as in a live-updating sequence of Era modules with version tags, security recommendations, and migration tools, is something we haven't considered much.

For simplicity, let's just worry about Era modules and Cene codebases. When a Cene codebase builds an Era module, certain IDs can be more or less preserved:

* Cene codebase's installation identity -> ???
  * (There are a few choices here.)
  * -> Era module's installation identity
    * Note that the codebase's installation identity is not the same as this identity (especially since the same Cene codebase can generate multiple Era modules), but both of these are identities that don't risk collision even with additional copies of the same code, so they're similarly unique. These are both basically the gratuitous `unique-name` arguments passed in at the start of their respective definition-computing behaviors.
  * -> Era module's content-based identity
    * If an Era module can only be installed once, then this may be a good way to ensure the name is something future Era modules can reliably refer to if they needed to. Hmm, I suppose they could refer to the installation identity the same way.
* Cene codebase's signature-providing author identities -> Era module's signature-providing author identities
  * These are the exact same identities, so this translation is easy.

However, preserving the identities isn't necessarily the goal. It seems like within a codebase, function and type definitions are mainly identifiable by a filename and a simple identifier. Some are not identified at all, just somehow used immediately at the specification site (e.g., lambdas), but a quined form of the program would nevertheless need to care how to translate the name.

I suppose, honestly speaking, the name of a lambda isn't specified yet. Maybe there should be some way for a later Era module to clarify some of the names an earlier module is using. However, that would require us to give some particular name to the mystery name so the clarification could refer to it. Ooh, since the only people who need to refer to this name-of-a-name without cracking open the black box are the original authors as they clarify their intent, the name of the name could refer to the lambda's source location.

Hmm... This could be the way the entire codebase is translated: Most things are translated into mystery names with some codebase-internals-specific name for clarifying them. Then only certain names are actually clarified, and that set of names to clarify is specified alongside the specification of which part of the codebase to include in the quine. As a (distantly) conservative approximation of the full expressiveness of this idea, we can initially design all our quine backends to require that every type constructed or matched in a quine be fully clarified, except for any that are immediately used for a function call or immediately used to construct an opaque function.

Hmm... That might just leave the question of how clarified the names in the codebase itself are. Perhaps the codebase clarifies its own names the same way.
