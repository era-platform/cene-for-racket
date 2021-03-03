# Algebraic Cene (take 2)

A lexical unit defines a few things:

* Identities of authors who need to demonstrate authorization of this lexical unit (e.g. through code signing)
  * Naturally, these may only be declared in lexical units that have some authorization method available to them; otherwise the authorization requirement can't be met. This means these only make sense to declare at the top level of a file (which is authorized by a keychain passed through behind the scenes by whatever mechanism allows one file to refer to another), the top level of a codebase (where that keychain originates), or the top level of a meaning-preserving modularity package.
  * As a lexical unit matures (or otherwise becomes more specific), it may declare more needed authorization demonstrations.
* Arguments to this lexical unit, each with an assertion that must pass for any given candidate argument value, along with possibly a default value
  * Each argument passed in will be a lexical unit supplied at compile time (but, being a lexical unit, may have run time components), and it must pass the assertion.
  * If a default value is given, it must pass the assertion.
  * As a lexical unit matures (or otherwise becomes more specific), it may declare more arguments, it may make arguments' assertions more lenient, and it may make default values more specific.
* Whether or not this lexical unit is assertdexed (equipped with the ability to assert that another value is known to be equal to it, with at least enough knowledge available that it passes its own assertion)
  * in order to declare this, all the arguments and argument-bound free variables of this lexical unit must have assertions requiring them to be assertdexed
  * As a lexical unit matures (or otherwise becomes more specific), it may declare itself to be assertdexed.
* Whether or not this lexical unit is dexed (equipped with the ability to observe, *without* risk of errors, whether or not another value is equal to it)
  * in order to declare this, all the arguments and argument-bound free variables of this lexical unit must have assertions requiring them to be dexed, and this lexical unit must be declared to be assertdexed as well
  * As a lexical unit matures (or otherwise becomes more specific), it may declare itself to be dexed.
* Lexical units that reside in this one, with some key to look them up by
  * for instance, macros are lexical units which are parameterized by a compile-time syntax value (which itself is represented by a lexical unit)
  * for instance, functions are macros that have a run time component
  * for instance, constants are functions with no arguments and a requirement of constant-time execution
  * As a lexical unit matures (or otherwise becomes more specific), it may make resident lexical units more specific.
* Positive guarantees as to what things this lexical unit does *not* define, which may go so far as to limit this module to an enumerable or even finite set of definitions (for representing closed data structures)
  * As a lexical unit matures (or otherwise becomes more specific), it may make more guarantees as to what it does not define.
* Passthroughs of all the declarations of a given lexical unit, which must not contradict declarations made in this lexical unit, possibly modified by a relatedness-preservation modality
  * By "relatedness-preservation modality," we mean the kind of modality referred to simply as a "modality" in the paper "Degrees of Relatedness: A Unified Framework for Parametricity, Irrelevance, Ad Hoc Polymorphism, Intersections, Unions and Algebra in Dependent Type Theory" by Andreas Nuyts and Dominique Devriese.
    * For instance, if the modality of one of the lexical unit optional arguments of a lexical unit is "par," meaning parametric quantification, then besides instantiating that lexical unit with a particular argument, the user can also instantiate it with a notion of relatedness and two argument candidates that are related according to that notion. The things this instantiation defines will be pairs of candidate definitions that are related in a way that's compatible with the given relatedness notion.
  * (TODO: Consider having modalities for error-handling-only structure (i.e., distinguishing what information is available as part of the type's main API and what information is strictly for error-handling purposes).)
  * (TODO: Consider having modalities for induction steps (i.e., what information is available at each step of some induction-in-progress; cf. "guarded computational type theory").)
  * The spliced lexical unit may be specified in several ways:
    * Specified inline as a set of declarations, with all the lexical variables still in scope
    * Specified by a reference to a lexical variable in scope
    * Specified by a lexical-unit-locating path and lexical unit arguments
    * Specified by a lexical unit, the key of a resident lexical unit inside it, and lexical unit arguments
    * Specified by a lexical unit and an assertion to require of it
    * Specified by a set of lexical units which must not contradict each other
  * As a lexical unit matures (or otherwise becomes more specific), it may refactor passthroughs into direct declarations or vice versa, as long as the declarations themselves are at least as specific as before.
* Whether or not this lexical unit is a compile-time authorization of some collection of author identities, and if so, whether it's one that's known to this particular lexical unit or merely one that may be linked in during a future stage of compilation
  * This lexical unit must be positively guaranteed not to have any other features.
  * In order for this lexical unit to be a known authorization, these author identities must be declared as ones that need to authorize this lexical unit or one it appears lexically inside of.
  * As a lexical unit matures (or otherwise becomes more specific), it may declare itself to be a compile-time authorization, go from a not-yet-known compile-time authorization to a not-yet-known one for a later linking stage, or go from a not-yet-known one to a known one.
* Whether or not this lexical unit is a value constructor with some set of authors it's private to and some set of interfaces the authors call dibs on implementing specializations for or call dibs on leaving for interface authors to implement
  * This constructor may be declared to support `dex-struct`, `cline-struct`, `merge-struct`, `fuse-struct`, and `case` branching. If it is, all the arguments and argument-bound free variables of this lexical unit must have assertions requiring them to be dexed.
  * This constructor may be declared to support `assertdex-struct` and `assert-case` (a restricted kind of `case` branching where the failure branch must lead to an error). If it is, all the arguments and argument-bound free variables of this lexical unit must have assertions requiring them to be assertdexed.
  * This lexical unit must be positively guaranteed not to have any other features.
  * As a lexical unit matures (or otherwise becomes more specific), it may declare itself to be a value constructor, call dibs on letting more specializations be up to the interface authors, declare support for `dex-struct`, or declare support for `assertdex-struct`.
  * (TODO: We meant to think about constructor definitions as existential captures. Like other existential captures, these could be refactored into other representations by future versions of the lexical unit. Are these notes consistent with that? How should we integrate this information into them?)
    * (TODO: Also, is there a use for constructor declarations in arguments rather than in existential captures? Come to think of it, between this and "not-yet-known compile-time authorizations," we'll probably benefit from modeling ML-style module signatures, not just modules. Although... it seems like a module under the "par" modality would become a module signature as far as its users are concerned, so maybe that's the way we should unify these concepts instead.)
    * (TODO: Should we let constructors specify they're generative? If we don't, the only time the representation would usally leak is when the constructor is declared in an argument. An existential capture would be the everyday place to put types that shouldn't leak, not an argument, so this probably isn't a problem.)
  * (TODO: This lets us define types in some fashion, but how do we define types that have custom notions of relatedness? The prototypical example here is a type of types, which would probably support something like `assertdex-struct` (maybe a more impure variant that caused unification to occur...) and whose notion of relatedness between inhabitants `A` and `B` is to have an instance of `(A -> B -> Type)` rather than a witness of `(A = B)`.)
* Whether or not this lexical unit is an interface definition with some set of authors it's private to and some set of value constructors the authors call dibs on implementing specializations for or call dibs on leaving for value constructor authors to implement
  * All the arguments and argument-bound free variables of this lexical unit must have assertions requiring them to be dexed.
  * This lexical unit must be positively guaranteed not to have any other features.
  * As a lexical unit matures (or otherwise becomes more specific), it may declare itself to be an interface declaration or call dibs on letting more specializations be up to the value constructor authors.
* Interface specializations
  * (TODO: How would this work? In particular, interface specializations don't seem tied to the lexical unit they're in in any essential way, except possibly if we give them names. It's at least nice to put them in lexical units related to where their interfaces and constructors are defined so the compiler will know where to look for them. Perhaps there should be "interface specialization implementations" that hold the functionality and "interface specialization notices" that guide a compiler to the locations of the needed implementations.)
  * authorization must be provided from the combination of authors the constructor is private to (TODO: We really only need something like a mutually exclusive predicate, rather than a constructor per se.)
  * authorization to specialize this combination must come from the authors of the given constructor or from their dibs not to specialize the given interface
  * authorization to specialize this combination must come from the authors of the given interface or from their dibs not to specialize for the given constructor
  * As a lexical unit matures (or otherwise becomes more specific), it may declare more interface specializations.
  * (TODO: How does this interact with parametricity and function extensionality? By parametricity, when we instantiate a lexical unit with two candidate arguments and a relation they satisfy, each type the lexical unit defines becomes a relation that passes the given relation through to the type's contained values without making further distinctions. By function extensionality, when we instantiate a lexical unit with two candidate arguments, one more specific than the other, each type the lexical unit defines becomes a relation attesting which values of the two candidate types correspond to each other, along with a conversion from the less specific type to the more specific type. For types that are based on constructors, that makes sense, but for interfaces... I suppose every interface specialization needs to specialize in a way that's parametric over all the things the interface definition was parametric in *and* parametric over all the things the constructor definition was parametric in, possibly in such a way that some of these dependencies are overlapping rather than independent.)
  * (TODO: The specialization is essentially a partial interpretation of the constructed value as a first-class module. With that in mind, should we support associated types? Would this have bearing on the question of how to represent types with custom notions of relatedness?)

(TODO: We say functions are lexical units with run-time components, but how does that work? Where do we actually get to define a run-time component of a lexical unit? We should probably add a case for that. The specification of run-time behavior should carry enough compile-time information to let privileged clients compile that behavior and all its dependencies to another platform.)

(TODO: Consider what to do with effect types. We should likely allow effect dependencies to be declared when specifying how to compute a run-time component of a lexical unit. We should likely also allow these computations to parametrically quantify over effect suites so that we can write higher-order procedures with callbacks whose impurity exceeds what we can anticipate. The parametricity of this quantification is likely something we'll want to model similarly to our other parametric quantification, which means we probably need effects to be definable the way constructors are.)

(TODO: Whenever we write up how run-time components work, be sure to pass the call site in as a run-time value with error-handling-only content.)

(TODO: In several places above, we require things to be dexed or assertdexed. Is that right, or do parametricity or error-handling-only modalities make the story more subtle than that?)
