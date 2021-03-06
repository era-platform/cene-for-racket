# Contractful vs Contractless

When we define a module, let's have contractful, internally contractless, and fully contractless versions. Likewise, for internationalization, we might have different-language versions.

(Perhaps these notes mostly pertain to the design of Racket libraries like Lathe Morphisms and Punctaffy, since at the moment, Punctaffy's tests spend roughly an hour checking contracts. However, these thoughts are also relevant to the design of module systems in general. By "module" here, we mean something like a Racket module or ML module, where it's one file in a codebase (or some file-like region of that file) that can refer to others by name. In Cene, we would call that a "lexical unit," reserving "module" for the open-world assumption and closed-world assumption ideas that require the whole set of installed modules to be loaded before we know whether any given definition is installed or not. We're also assuming a Racket-like approach to the Expression Problem here, where each user-defined representation type implements some combination of interfaces with dynamic dispatch.)

Though the different versions of a module may define types differently, the values of those types will be compatible in some ways. Specifically, we can take a value and substitute one module configuration choice for another.

With an approach like that, when a module defines various things, they can be a bit more complicated:

* Functions, macros, and constants: These take an additional argument representing the module's configuration. When the configuration affects compilation, there may be more than one definition to implement that.

* Value constructors, or functions or macros that return them: These carry an extra field representing the module's configuration, and this field is always *checked for equality with an expected value* rather than accessed. When the configuration affects compilation or what generic interfaces the constructor implements, there may be more than one constructor defined to implement that. These also have a function which, given a module configuration conversion function for each field, returns a module configuration conversion function for instances of the constructor.

* Generic interfaces, or functions or macros that return them: These carry an extra method to get a set of module configuration objects for the interface instance's immediate structure (letting the module's configuration be checked for equality with a set of possible expected values) and another extra method which, given module configuration conversion functions for related value types and a module configuration substitution object which starts from the module configuration object obtained by the other method, returns an updated value with those conversions and substitutuons applied. In addition, each method of the interface takes another argument, the module configuration object to indicate which version of the interface is being invoked. When the configuration affects compilation or what value predicates the generic interface defines default behaviors for, there may be more than one generic interface defined to implement that. These also have a function which, given module configuration conversion functions for related value types, returns a module configuration conversion function for instances of the interface (which works by calling the extra methods).

* Value constructors or generic interfaces for which there are functions or generic interface methods that assert or compute equality or perform forced or fallible zipping on a collection of instances: More generally, these check the heterogeneous relatedness of (module configuration, instance for that configuration) pairs under a given module configuration substitution object.

* Value constructors or generic interfaces for which there are value constructors or generic interfaces which represent bisimulations that can explore the equality of multiple instances up to whatever depth the user pursues the question (possibly with failure nodes indicating the bisimulation doesn't go all the way through), or for which there exist functions that compute such bisimulations (possibly in terms of some existing representation rather than a new one, like making a stream that zips two streams): More generally, these represent the heterogeneous relatedness of (module configuration, instance for that configuration) pairs under a given module configuration substitution object.

A module configuration conversion function is a bundle of two values: One is a function that takes a value to convert and a module configuration substitution object and returns a value, ideally a value similar to the original one but with the substitutions applied. The other is a module configuration object that the substitution must start from.

A module configuration substitution object is a hash-like mapping where each entry maps a module family identity and a module configuration value for that family to a new module configuration value for that family. In general, module configuration values usually carry a symbolic name that serves no purpose other than to give the user more selective control over substitution.

A module configuration object is like a module configuration substitution object, but it doesn't map to a "new" configuration value. Instead, it's just a set of pairs of module family identities and configuration values.

To let users perform precise substitutions, we need a system of "variables" users can coin so that they can perform substitutions on only the parts of a structure their code is in charge of. For instance, they might substitute one dictionary data structure implementation for another without modifying other dictionaries that happen to be stored inside it. For this purpose, a configuration value should typically have a slot for it to carry an arbitrary symbolic identity, so that distinctions can be made during substitution even if the configured behavior is the same.

Sometimes a generic interface is contravariant in a type, in which case it should receive a module configuration conversion function that goes in the opposite direction. Sometimes one of the types it varies in depends on a value, in which case it should receive a function that takes that value and returns a module configuration conversion function.

Sometimes when talking about allowing symbolic names to be created deterministically from some component parts (like Cene's `getfx-name-of`), we've discussed "dexed values." In context of the approach we're describing here, dexed values are essentially what we require module family identities and module configuration values to be. However, when a value is known well enough to be dexed in this context, we additionally know a module configuration object for it, and we know how to apply a module configuration substitution object to it. (Sometimes a choice of certain module configurations is an implementation detail of the value, in which case the dexed value may be seen as having a module configuration object that omits some entries that could have been present. This kind of dexed value still carries enough information to know the value is equal to itself, but it changes which alternately configured values the value can be related to by substitution.) When creating a key-value mapping (such as module configuration objects and module configuration substitution objects) where the keys are specially dexed values like these, we don't allow two keys to be equal, even up to possible substitutions from their two module configuration objects. That way, we can perform a substitution on the key-value mapping itself without the risk of creating colliding keys.

There are some subtleties we've neglected around compile-time information. Generally, we can consider compile-time values to have associated run-time components. The module configuration object for a compile-time value can be a compile-time value that specifies not only the configurations it uses at compile time, but also has run-time components that specify the configurations of the run-time components. Equality/relatedness checks and bisimulations that take advantage of compile-time information can use fast implementations that don't bother to double-check configurations that were already confirmed equal at compile time; this optimization is essential so that there's any benefit to using a contractless version of a module, since configuration substitutions will likely have overhead similar to contracts otherwise.

Dexing of compile-time values with run-time components is interesting: It seems like a value may be dexable at compile time based on knowledge that its run-time components are built out of equal expressions, and at run time, the run-time components are dexable based on different knowledge. Hm, perhaps that means each function should have a corresponding version that takes dexed values to dexed values! And this is like function extensionality, which in turn suggests that we might have some "dexed values" which are higher-dimensional, representing collections of values which can attest to their own relatedness up to some module configuration substitution objects, collections of those collections, and so on.

Run-time components of a compile-time value also basically serve as unspecified configurations: Each time that component's expression is run, it may receive a different configuration via its free variables.

Parametricity... It seems the notion of relatedness that parametricity preserves is that two values are each substitution results of some more vague common ancestor.


## Keeping it simple

Let's design a language, "Cronut," that can be used as a Racket `#lang` and has the property that every expression can be cross-compiled by anyone. This will not be a well encapsulated system (at least in the first version of the language), but it will be rather useful for writing cross-platform libraries. ("Cro" = cross-compilable language, "nut" = with holes in its abstractions.)

Let's say that every value is dexed, so that we automatically have that all our functions and macros preserve dexed values. Thus, let's say every function and macro is fully polymorphic over dexed values of all possible dimensions (e.g. dexed values that verify the equality of two dexed values that each verify the equality of two values). And to make this make sense without having to pass in a list of dimension names to every function and macro, let's say every *value* is polymorphically an instance of all the dexed value interfaces for all possible dimensions.

Let's also say that every value is quotable. We might as well, if we're not concerned about encapsulation. Of course, just because a value is quoted to turn it into an expression doesn't mean it's an expression with a constructor every compiler backend recognizes.

Let's consider lambdas (and other anonymous interface instances) to be constructors that are *named after their source location*. When we're cross-compiling, let's do a pass to turn constructors that are only used once back into lambdas.

A Cronut lexical unit can contain definitions of:

* Sets of value constructors grouped under classes, along with associated instance specializations. (What we're calling a "class" here is a statically recognized set of constructors. Interface specializations can specify a class to specialize several constructors at once. Built-in values like integers and strings will tend to have a public class but no public constructors. (TODO: Come up with a better name than "class."))

* Functions.

* Macros, using a macro system roughly similar to Racket's.

* Run-time constants, initialized at unit instantiation time with limited use of side effects.

* Compile-time constants, for use by macros.

* Interfaces with sets of method signatures, along with associated instance specializations. Methods can be classes, functions, run-time constants (initialized at instance construction time with limited use of side effects), and interfaces... and maybe even subunits. (TODO: Hmm.... Run-time constants on interfaces could makes it expensive to construct something.)

* Subunits, named collections of declarations like these.

* One useful kind of compile-time constant would be a specialization matrix, which carries a collection of interface specializations. When invoking a compiler backend on an expression, a specialization matrix can be passed in. We may decide to have modules define some specialization matrices automatically to carry their declared interface specializations. "Orphaned" specializations (like orphaned instances in Haskell), which are the ones declared in a module that declares neither the class nor the interface being specialized, have to be passed around manually in specialization matrices, since they won't be found in either of the places that specialization is usually found.

* Perhaps a module could also declare some strategies for how to look up instances of the classes or interfaces it declares. That way, some specializations can be explicitly delegated to other files without having to pass around specialization matrices.

Considering the correspondence between interface specializations and module implementations, we could say that each value is a first-class module (or at least the run-time parts of a module, since we aren't intending to give static types to values). So, once those declarations are processed, a module can implicitly define:

* A class with a trivial name.

* A constructor with a trivial name, under that branch type.

* An interface with a trivial name. The signatures of the interface's methods are the signatures of the functions defined by this module. (If interfaces can have constant definitions (initialized at instance construction time), branch types, or constructors, there's

* A specialization of that interface for that constructor. The implementation expressions of the specialization are made up of the implementation expressions of the functions defined by this module.
