---
title: Thoughts on "kinds as calling conventions"
author: ielliott95
permalink: /thoughts-on-kinds-as-calling-conventions
date: 2023-08-21
tags:
  - programming
---

How should one compile a polymorphic function, such as `id : forall a. a -> a`?
The optimal machine code differs based on `a`.
On a 64-bit system, `id : Int32 -> Int32` (`a = Int32`) should recieve its argument and return its result via registers.

A common solution, typically for higher-level languages, is to choose a *uniform representation* for values.
Any type that *could* be substituted for a type variable has the same runtime representation: a pointer to heap-allocated memory.
This allows polymorphic functions to be compiled once, decreasing compile times and final binary size.

The convenience of single compilation is paid for every time the program is run.
Allocating and freeing heap memory uses CPU cycles; computation that isn't visible in the program's result.
Reading from RAM is much slower than from a register.
Heap-allocated data reduces the effectiveness of CPU caches.
If all the `Int32`s in an `Array Int32` are represented by pointers, then iterating over the array requires a random memory access per element.
When an array contains integers directly, instead of pointers to integers, then the first memory access while iterating will put later elements into the cache,
speeding up subsequent iterations.
If all `Int32`s were heap-allocated, then a loop that computed the sum of millions of integers would perform millions of heap allocations.
A more efficient program could accumulate the sum in a register.

GHC uses "representation polymorphism"[^levity].
Type variables have *kinds*.
The `a` in `id : forall a. a -> a` has kind `Type`, which I'll write as `id : forall (a : Type). a -> a`.
To encode the fact that different types have different runtime representations, GHC indexes the kind of types by their runtime representation.
`id` would look more like this: `id : forall (a : Type PointerRep). a -> a`.
Types that fit into a regular register can have kind `Type IntRep`, and floats (which are usually passed in floating-point registers) have kind `Type FloatRep`.
Now we can have unboxed integers and floats: `Int32# : Type IntRep`, `Int64# : Type IntRep`, `Float# : Type FloatRep`.
A type's runtime representation has to be statically known.
We can't write `id : forall r (a : Type r). a -> a` which accepts arguments of any runtime representation, because the we need to know the argument's runtime representation in order to compile the definition.

Cyclone[^safe-c] also uses kinds to this effect. The kind `B` (mnemonic: boxed) is given to types that are represented by a pointer,
and the kind `A` (mnemonic: any) can be given to any type.
Type variables of kind `B` can be used directly as function inputs or outputs, whereas types of kind `A` must be used in a context that has a known runtime representation.
`id : forall (a : B). a -> a` and `id : forall (a : A). Pointer a -> Pointer a` are allowed, but `id : forall (a : A). a -> a` is not.

I think solving this problem with more granular kinds is a commendable engineering solution, and I also don't like it.
In "kinds as calling conventions", a kind carries information about a type.
But the exact information we put into a type's kind seems arbitrary; at the compiler author's whim, based on whatever the code generator needs to know about that type.
Putting something implementation-dependent like runtime representation into a foundational type theory concept like a *kind* feels inelegant.

I find Rust's approach to be an interesting alternative.
Rust tracks runtime representation using qualified types[^qualified] (traits / type classes) instead of kinds.
All type variables are implicitly given a [`Sized`](https://doc.rust-lang.org/std/marker/trait.Sized.html) constraint, indicating that the type must have a statically known runtime representation.
This is analogous to Cyclone's `B` kind.
The implicit `Sized` constraint can be removed using the `?Sized` constraint, which is analgous to Cyclone's `A` kind, having similar rules about where `?Sized` type variables can and cannot be used.

As type theory extensions go, qualified types are much less arbitrary and implementation-dependent than indexing a type's kind by its runtime representation.

Rust monomorphises polymorphic functions, opting for lower runtime cost at the expense of compile time and binary size.
Is this a relevant divergence from Haskell and Cyclone?
Or can the "runtime representation via qualified types" approach also work for a language with uniform representation of polymorphism?  
I see no reason for doubt.
A polymorphic function like `id : forall a. a -> a` is similar to the Rust function `id<A>(x: Box<A>) -> Box<A>`, which explicitly takes and returns a heap pointer.
Boxed types in our hypothetical language would have a `Sized` instance describing them as having pointer representation.

Another potentially relevant difference is that Haskell has higher-kinded types and Rust has no kind system.
This isn't an issue; see my [previous work](https://blog.ielliott.io/sized-hkts) on extending `Sized` constraints to higher kinds.

[^levity]: Eisenberg, R. A., & Peyton Jones, S. (2017). Levity polymorphism. ACM SIGPLAN Notices, 52(6), 525-539.
[^safe-c]: Grossman, D. J. (2003). Safe programming at the C level of abstraction. Cornell University.
[^qualified]: Jones, M. P. (1992, February). A theory of qualified types. In European symposium on programming (pp. 287-306). Berlin, Heidelberg: Springer Berlin Heidelberg.
