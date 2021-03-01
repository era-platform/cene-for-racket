# Taking stock of Cene's expressions

Inspired by the strawman code example in notes/20210301-algebraic-cene.md, how many binding forms generalize to pattern-matching forms in a way that could be considered a mere syntactic sugar distinction? Are there other useful grammar nonterminals that would let more forms share their parsing infrastructure? Let's look at Cene's expressions for examples.

We're also considering developing a Racket `#lang` with cross-compilation functionality similar to Cene's. That means we need to settle on a type of expressions of fully-expanded programs the compiler backends can work with. Cene's current expressions don't address the Expression Problem quite the way we originally intended -- more on this later -- but we can take a look at what to do about that.

This is the current set of expressions implemented in Cene (which we can find in the code by looking for `prop:cexpr`):

```
var
let

reified

located

construct
call-fault
call
opaque-fn-fault
opaque-fn
dex-struct
cline-struct
merge-struct
fuse-struct
case
```

Quite a number of those revolve around the various ways to introduce and eliminate structs. Structs are leading a double life as both a way to represent functions and as a way to represent records. Every feature they have is in service of their role as functions, because they let us require some things of a function that traditional lambdas aren't transparent enough to allow. Treating a function as a struct this way is varying degrees of "unsafe" since the client may observe information that they couldn't have learned by calling the function. This unsafety is ironic, since the reason Cene needs functions that are transparent in this way is so that it can ensure that all code that works with modules or declarative DSLs is invariant with respect to the order those modules or declarations are processed in. Still, Cene does not compromise on enforcing order-obliviousness, and most Cene functions don't need to expose their details in this way.

These are the levels of safety if we regard a struct as a function, as a record, or as something in between (a "coordination error check device," less intentionally transparent than a struct but less featureful than a function):

```
       safety level when regarding a struct as a function
?***** good for opaque functions
  **** good for opaque functions which may need to be coordinated in ways that need to be checked for mismatch errors
   *** good for opaque functions with backward-compatibility equality guarantees to keep
    ** good for opaque functions with the need for opportunistic AOT optimizations, caches, safe-for-space abstractions, or friendly error messages
     * good only for records which may not even be used as functions at all

       safety level when regarding a struct as a coordination error check device
?***** extraneous and meaningless for coordination error check devices
  **** good for coordination error check devices
   *** good for coordination error check devices with backward-compatibility equality guarantees to keep
    ** good for coordination error check devices with the need for caches or friendly error messages
     * good only for records which may not even be used as coordination error check devices at all

       safety level when regarding a struct as a record
?***** extraneous and meaningless for records
  **** good for records
   *** good for records
    ** good for records
     * good for records

?***** call-fault
?***** call
?***** opaque-fn-fault
?***** opaque-fn
?***** fuse-opaque-fn (a function rather than an expression)
  **** def-struct (a lexical unit declaration rather than an expression)
  **** construct
  **** assertdex-struct (not in Cene (yet?); this would be like a dex, but treating inequality as an error)
   *** dex-struct
   *** cline-struct
   *** merge-struct
   *** fuse-struct
   *** case
```

We could imagine a few more levels of unsafety below the one-star level:

* Mechanisms which expose details of our abstractions to the users of those abstractions rather than just to our own defining module, or which don't allow our clients' higher-order abstractions to wrap our abstractions in identity-obscuring wrappers to prevent us from spying on their implementation details. Since functions can always be wrapped in other functions, and since the `construct`, `case`, etc. expressions require a constructor with a known author that authorized these expressions, Cene's functions aren't this unsafe.

* Mechanisms which expose these kinds of details even when we don't opt in. For instance, Racket hash tables expose their iteration order, and there doesn't seem to be a viable alternative, so the users of Racket hash tables aren't opting into that information leak.

* Mechanisms which break the module system abstraction itself. For instance, if Cene were to allow arbitrary Racket side effects in a Cene module, it would be possible to detect when one module runs before another. Avoiding this kind of unsafety is the primary reason for most of Cene's complexity, from its order-oblivious tables to its extfx effect system to its dexes, merges, clines, and fuses.

For as many kinds of unsafety as Cene manages to avoid, we could still try seek an even more principled design.

Suppose we were to split up this design so that non-function records are a separate type. Since none of these operations is in the one-star safety zone, they're likely to all stick around in some form for functions.

We can still simplify the interface for functions, but not by much: We can change all the record-like operations so that there's only a single record field involved. If we do that, multi-field functions can still be simulated by storing a multi-field record in a function's single field.

Meanwhile, one of the conundrums we're currently having with structs is that they're not quite the Expression Problem solution we originally had in mind. Potentially, we could treat `call`/`call-fault` as just one of many interfaces a struct could have an implementation for. But... if a *user* of Cene comes up with a new interface like that, they can only compile their program if all the structs in the program have implementations for all the interfaces. That means the user is isolated on an island; none of the libraries they're using is aware of these new interfaces, and the user doesn't have authority to extend the libraries with new interface implementations, so they can't use those libraries in their program. (Hmm, oh yeah, they actually do have that authority. See [NOTE GENERALIZATION].)

So, in the long run, what we probably want to do is let users coin new struct-like types that support specific sets of interfaces. The original Cene structs would be dedicated to supporting `call`/`call-fault`, and when a program contains an expression that constructs those structs, it only needs `call`/`call-fault` to be implemented. If the user creates, say, a UI widget interface, then the only construction expressions that require a UI widget implementation to exist are the UI widget construction expressions.

Another conundrum on our minds has been getting the names generated by most Cene programs to be more predictable (less reliant on unique names). This would cache macroexpansion results in a fine-grained way without the cache constantly being invalidated by edits to the code. To make names more predictable, it would probably help to be able to take names apart and re-normalize them programmatically as we go along.

Considering that, we might want names to have a set of operations for name-structs similar to our operations for function-structs. Instead of `call`/`call-fault`, the high-level operation would be `dex-name`. There would be a separate `dex-name-struct` for expressing a dex that only matches a name with a known name-struct constructor:

```
       safety level when regarding a name-struct as a name
?***** good for opaque names
  **** good for opaque names which may need to be coordinated in ways that need to be checked for mismatch errors
   *** good for opaque names with backward-compatibility equality guarantees to keep
    ** good for opaque names with the need for caches, safe-for-space abstractions, or friendly error messages
     * good only for records which may not even be used as names at all

       safety level when regarding a name-struct as a dexable coordination error check device
?***** good for coordination error check devices which need to be dexable, i.e., need to be compared with others for equality
  **** good for coordination error check devices
   *** good for coordination error check devices with backward-compatibility equality guarantees to keep
    ** good for coordination error check devices with the need for caches or friendly error messages
     * good only for records which may not even be used as coordination error check devices at all

       safety level when regarding a name-struct as a dexable record
?***** good for records which need to be dexable, i.e., need to be compared with others for equality
  **** good for records
   *** good for records
    ** good for records
     * good for records

?***** dex-name (a function rather than an expression)
  **** def-name-struct (not in Cene (yet?); a lexical unit declaration rather than an expression)
  **** name-construct (not in Cene (yet?))
  **** assertdex-name-struct (not in Cene (yet?); this would be like a dex, but treating inequality as an error)
   *** dex-name-struct (not in Cene (yet?))
   *** cline-name-struct (not in Cene (yet?))
   *** merge-name-struct (not in Cene (yet?))
   *** fuse-name-struct (not in Cene (yet?))
   *** name-struct-case (not in Cene (yet?))
```

Of course, coordination error-checking is probably the primary use of a name, so there isn't much of a difference between four stars and five stars in this case. However, the primary way to compare names would be `dex-name`, not these finer-grained mechanisms. Similarly, the primary way to construct names would still be `getfx-name-of` and other built-in name constructors/procurers. By designating these finer-grained operations as more unsafe, we can keep the analogy to functions going, possibly leading to fewer surprises for users and more obvious opportunities for abstraction.

Note that we don't quite have a seamless opportunity for abstraction here, because `name-construct` would need to require the field value(s) to be dexed values.

Another conundrum we've been considering in Cene is how to keep the identities of dexes, clines, merges, and fuses backwards-compatible. So we might want to be able to create `def-dex-struct`, `def-cline-struct`, etc., with the custom behavior specified according to mechanisms similar to `dex-by-own-method`, `cline-by-own-method`, etc.

Of course, there may be a bit of a loop to tie off here, since names, dexes, clines, merges, and fuses are implicated in the interfaces of *all* these things (via `dex-_____-struct`, `cline-_____-struct`, etc., as well as `getfx-name-of` applied to `dex-_____-struct`). Even if we extend the same abstraction to all of these things, we might still need to special-case them.

[NOTE GENERALIZATION] Oh yeah, a user defining a new interface *does* have the authority to implement that interface for some existing types. Perhaps a constructor (or anonymous constructor like `opaque-fn`) is associated with a set of interfaces its author calls dibs on implementing or not implementing, an interface (or anonymous interface like `case`) is associated with a set of constructors its author calls dibs on providing or not providing implementations for, and combinations which aren't called dibs on this way can only be implemented by a joint authorized action of both authors. In most situations, it's reasonable to provide all-encompassing dibs information for a constructor or interface when it's defined, so programs will only need the intervention of a joint action on rare occasions. In particular, the approach we described above was for the user to coin a new struct type when they have a new specific set of interfaces they want to implement, and this dibs model is a generalization over that which allows the "set of interfaces" to be incompletely specified.
