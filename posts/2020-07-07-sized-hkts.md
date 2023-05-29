---
title: Statically Sized Higher-kinded Polymorphism
permalink: /sized-hkts
date: 2020-07-07 15:00:00 +1000
tags:
    - programming
excerpt: |
  <p>
    Memory-sensitive languages like C++ and Rust use compile-time information to calculate
    sizes of datatypes. These sizes are used to inform alignment, allocation, and calling conventions in ways
    that improve runtime performance. Modern languages in this setting support generic types, but so far
    these languages only allow parameterisation over types, not type constructors. In this article I describe
    how to enable parameterisation over arbitrary type constructs, while still retaining compile-time calculation
    of datatype sizes.
  </p>
---

<div class="intro-wrapper">
  <div class="intro">

  Memory-sensitive languages like C++ and Rust use compile-time information to calculate
  sizes of datatypes. These sizes are used to inform alignment, allocation, and calling conventions in ways
  that improve runtime performance. Modern languages in this setting support generic types, but so far
  these languages only allow parameterisation over types, not type constructors. In this article I describe
  how to enable parameterisation over arbitrary type constructs, while still retaining compile-time calculation
  of datatype sizes.

  The code for this project can be found <a href="https://github.com/LightAndLight/sized-hkts">here</a>.

  </div>

  <div id="toc"><!-- generated --></div>

</div>

## Background

### Generics

Many typed languages support some form of generic (parameterised) datatypes. This ability to abstract
over types is known as 'parametric polymorphism' (polymorphism for short). In Rust, for example, one
can define type of polymorphic pairs as `struct Pair<A, B>(fst: A, snd: B)`. In this definition, `A` and `B` are type
variables (or type parameters), and can be substituted for other types:
`Pair<bool, bool>`, `Pair<bool, char>`, and `Pair<String, int32>` are all valid pairs.

The name of a type, without any parameters, is known as a type constructor. `Pair` is not a type on its own;
`Pair<A, B>` (for some types `A` and `B`) is. The number of types required to 'complete' a type constructor is known
as its arity (so `Pair` has arity 2). The arity of a type constructor must always be respected; it's an error to
provide greater or fewer type parameters than are expected. For example, `Pair<bool>` and
`Pair<char, int32, String>` are invalid.

### Sizing

When using C++ or Rust, the compiler will calculate how many bytes of memory each datatype requires. Simple
types like `int32` and `bool` have a constant size; 4 bytes and 1 byte respectively. The size of datatypes
built using of other simple types is easy to calculate. The simplest way to calculate the size of a struct
is to sum the sizes of the fields, and the simplest way to calculate the size of an enum (or tagged union)
is to find the largest variant, and add 1 (for a tag byte). This is rarely the exact formula used by production
compilers, because they take [alignment](https://en.wikipedia.org/wiki/Data_structure_alignment) into account.
This article will assume the simple sizing formula, because the results can easily be adapted to more nuanced
formulae.

The size of a datatype like `struct TwoInts(x: int32, y: int32)` is known immediately at its definition. `TwoInts`
requires 8 bytes of memory. On the other hand, the size of a generic datatype is not always known at its definition.
What is the size of `struct Pair<A, B>(fst: A, snd: B)`? It's the size of `A` plus the size of `B`, for some
unknown `A` and `B`.

This difficulty is usually addressed by only generating code for datatypes and functions when all the generic
types have been replaced with concrete types. This process is known as monomorphisation. If the program contains a
`Pair(true, true)`, then the compiler will generate
a new type `struct PairBoolBool(fst: bool, snd: bool)` whose size is statically known. If `Pair(true, true)`
is passed to a function `fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A>`, then the compiler generates a new
function `fn swapBoolBool(p: PairBoolBool) -> PairBoolBool`. Because this new function only uses types with known
sizes, the code for memory allocation and calling conventions can be generated correctly.

There are also generic types that *don't* depend on the size of their parameters. An example of
this is the pointer, commonly known in Rust as `Box<A>`. A pointer has the same size (often 4 or 8 bytes depending
on your CPU) regardless of what it points to. But in order to allocate a new pointer, the size of the item must
be known.

For each generic datatype or function, the compiler keeps track of which type variables are important for sizing
calculations. The specifics of this is discussed in the [Type Classes](#type-classes) section.

### Kinds

A consequence of all this is that in these languages, type variables can only stand for types. But there
are good reasons to have type variables that stand for type constructors, too:

```rust
struct One<A>(A)

impl <A> One<A>{
  map<B, F: Fn(A) -> B>(self, f: F) -> One<B> { ... }
}

struct Two<A>(A, A)

impl <A> Two<A>{
  map<B, F: Fn(A) -> B>(self, f: F) -> Two<B> { ... }
}

struct Three<A>(A, A, A)

impl <A> Three<A>{
  map<B, F: Fn(A) -> B>(self, f: F) -> Three<B> { ... }
}
```

Here are some 1-arity container types. The only difference between these datatypes is the number of elements
they contain. They all support a `map` operation, which applies a function to all the datatype's elements. Functions
that use `map` need to be implemented once for each type, even when their implementations are identical:

```rust
fn incrOne(x: One<int32>) -> One<int32> { x.map(|n| n + 1) }

fn incrTwo(x: Two<int32>) -> Two<int32> { x.map(|n| n + 1) }

fn incrThree(x: Three<int32>) -> Three<int32> { x.map(|n| n + 1) }
```

To remedy this, there must first be a way to abstract over the type constructors, so that the code can
be written *once* and for all:

```rust
fn incr<F>(x: F<int32>) -> F<int32> { x.map(|n| n + 1) } // when F<A> has map, for all types A
```

Then, there must be some way to rule out invalid types. For example, replacing `F` with `bool` in `F<int32>`
is invalid, because `bool<int32>` is not a type. This is the job of kinds<sup><a href="#reference-constructor-classes" id="reference-constructor-classes:1">1</a></sup>.

Kinds describe the 'shape' of types (and type constructors) in the same way that types describe the 'shape'
of values. A type's kind determines whether or not it takes any parameters. Here's the syntax of kinds:

```
kind ::=
  Type
  kind -> kind
```

Types that take no arguments (like `bool`, `char`, and `String`) have kind `Type`. Types that take one argument,
like `One`, have kind `Type -> Type`. In the code for `incr` above, `F` implicitly has kind `Type -> Type`. Types
that take more than one argument are represented in [curried form](https://en.wikipedia.org/wiki/Currying). This
means that `Two` has kind `Type -> Type -> Type`, not `(Type, Type) -> Type`. `Three` has kind `Type -> Type -> Type -> Type`,
and so on.

Curried type constructors are standard in this setting, but not *necessary*. The results in this article could
also be applied to a setting with uncurried type constructors, at cost to expressiveness or implementation complexity.

Kinds put types and type constructors on equal footing. For the remainder of the article, both concepts will be
referred to as types. The kind becomes the distinguishing feature. For example, "type constructor of arity 2" would
be replaced by "type of kind `Type -> Type -> Type`".

Some final jargon: types with a kind other than `Type` are known as 'higher-kinded types', and parameterising
over higher-kinded types is known as 'higher-kinded polymorphism'.

### Type Classes

Rust uses [traits](https://blog.rust-lang.org/2015/05/11/traits.html) to coordinate sizing calculations. Each
datatype implicitly receives an implementation of the `Sized` trait, and every type variable that is relevant for
a sizing calculation is given a `Sized` bound. This means that trait resolution, an already useful feature, can
be re-used to perform size calculations.

Closely related to traits is the functional programming concept of type classes<sup><a href="#reference-constructor-classes" id="reference-constructor-classes:2">1</a></sup>. There are differences between the two,
but those differences don't impact the results of this article. Type classes will prove a more convenient language
in which to discuss these ideas.

A type class (or trait) can be considered a predicate on types. A type class constraint (or trait bound) is an assertion
that the predicate must be true. For each constraint that is satisfied, there is corresponding 'evidence' that the
predicate is true.

When a type `T` has a `Sized` constraint, it is being asserted that the statement "`T` has a known size" is true. For
brevity, this will be written as `Sized T`. When this statement satisfied (for instance, when `T` is `int32`), the
evidence is produced is *the actual size* of `T` (when `Sized int32` is satisfied, the evidence
is the number `4` - the size of `int32`).

Generic types like `Two<A>` have a size that depends on their type parameter. In terms of constraints, it can
be said that `Sized A` *implies* `Sized Two<A>`. If `A` is `int32`, then its size is `4`, which implies that
`Two<int32>` has a size of `4 + 4 = 8`. Similarly, of `Pair` it can be said that `Sized A` implies [ `Sized B` implies
`Sized Pair<A, B>` ]. There is a choice between a curried an uncurried version; it could also be said that
[ `Sized A` *and* `Sized B` ] implies `Sized Pair<A, B>`, but the curried version will be used for convenience.

Note that type *constructors* don't have a size. In other words, only types of kind `Type` have a size. A type constructor
such as `Two` (of kind `Type -> Type`) has a size *function*. Given the sizes of the type constructor's parameters,
a size function computes the size of the resulting datatype. `Two`'s size function is `\a -> a + a`. `Pair`'s size
function `\a -> b -> a + b` (it could also be `\(a, b) -> a + b` in an uncurried setting).

### Problem Statement

With the background out of the way, the specific problem can be stated:

When a type of kind `Type` is relevant for a size calculation, it is given a `Sized` constraint, which will be
satisfied with a concrete size as evidence. What is the equivalent notion of constraint and evidence for
higher-kinded types that contribute to size calculations?

## Solution

An elegant solution to this problem can found by introducing quantified class constraints<sup><a href="#reference-quantified-constraints" id="reference-quantified-constraints:1">2</a></sup>. Quantified constraints
are an extension to type classes that add implication and quantification to the language of constraints, and corresponding
notions of evidence.

Here's new syntax of quantified size constraints:

```
constraint ::=
  Sized type               (size constraint)
  constraint => constraint (implication constraint)
  forall A. constraint     (quantification constraint)
```

The evidence for a constraint `c1 => c2` is a function that takes evidence for `c1` and produces evidence for `c2`, and the
evidence for `forall A. c` is just the evidence for `c`. The evidence for quantification constraints is a bit more nuanced
in general, but this description is accurate when only considering size constraints.

Concretely, this means that the sizing rules for higher-kinded types can now be expressed using constraints, and size
calculations involving higher-kinded types can be performed using type class resolution. It is now the
case that `forall A. Sized A => Sized Two<A>`, and the evidence for this constraint is the function `\a -> a + a`.
The relevant constraint for `Pair` is `forall A. forall B. Sized A => Sized B => Sized Pair<A, B>` with evidence function
`\a b -> a + b`.

This extends to types of *any* kind. For all types, there is a mechanical way to derive an appropriate size constraint based
only on type's kind;
`T` of kind `Type` leads to `Sized T`, `U` of kind `Type -> Type` leads to `forall A. Sized A => Sized U<A>`, and so on. In
datatypes and functions, any size-relevant type variables can be assigned a size constraint in this way, and the compiler
will use this extra information when monomorphising definitions.

[sized-hkts](https://github.com/LightAndLight/sized-hkts) is a minimal compiler that implements these ideas. It supports
higher-kinded polymorphism, functions and algebraic datatypes, and compiles to C. Kinds and size constraints are inferred,
requiring no annotations from the user.

Here's some example code that illustrates the
[higher-kinded data](https://reasonablypolymorphic.com/blog/higher-kinded-data/) pattern
([source](https://github.com/LightAndLight/sized-hkts/blob/master/examples/ex2.src), [generated C code](https://github.com/LightAndLight/sized-hkts/blob/master/examples/ex2.out)):

```
enum ListF f a { Nil(), Cons(f a, ptr (ListF f a)) }
enum Maybe a { Nothing(), Just(a) }
struct Identity a = Identity(a)

fn validate<a>(xs: ListF Maybe a) -> Maybe (ListF Identity a) {
  match xs {
    Nil() => Just(Nil()),
    Cons(mx, rest) => match mx {
      Nothing() => Nothing(),
      Just(x) => match validate(*rest) {
        Nothing() => Nothing(),
        Just(nextRest) => Just(Cons(Identity(x), new[nextRest]))
      }
    }
  }
}

fn main() -> int32 {
  let
    a = Nil();
    b = Cons(Nothing(), new[a]);
    c = Cons(Just(1), new[b])
  in
    match validate(c) {
      Nothing() => 11,
      Just(xs) => match xs {
        Nil() => 22,
        Cons(x, rest) => x.0
      }
    }
}
```

This code defines a linked list whose elements are wrapped in a generic 'container' type. It defines two possible
container types: `Maybe`, which is a possibly-empty container, and `Identity`, the single-element container.
`validate` takes a list whose elements are wrapped in `Maybe` and tries to replace all the `Just`s with `Identity`s.
If any of the elements of the list are `Nothing`, then the whole function returns `Nothing`.

Points of interest in the generated code include:

* 5 types are generated, corresponding to:
  `ListF Maybe int32`, `ListF Identity int32`, `Maybe int32`, `Identity int32`, and `Maybe (ListF Identity int32)`
* Only 1 version of `validate` is generated, because it is only used at one instantiation of `a`.
* The generated code makes no use of `sizeof`; the datatype sizes are known after typechecking and inlined during
  code generation. The compiler knows that `ListF Maybe int32` is naively `14` bytes wide
  (`1 + max(1, 1 + 4) + 8`), whereas  `ListF Identity int32` is `13` bytes wide (`max(1, 1 + 4) + 8`).
* The datatype sizes are not necessarily consistent with `sizeof`, because they ignore alignment for simplicity.
  At this point, factoring alignment into the size calculations is straightforward.

## Conclusion

Quantified class constraints provide an elegant framework for statically-sized higher-kinded types. On its own, this
can raise the abstraction ceiling for high-performance languages, but it also serves as the groundwork for 'zero-cost'
versions of functional programming abstractions such as Functor, Applicative, and Traversable.

This work shows it's definitely possible for Rust to support higher-kinded types in a reasonable manner, but
there are some less theoretical reasons why that might not be a good idea in practice. Adding 'quantified trait bounds'
would require new syntax, and represents an additional concept for users to learn. Adding a kind system to Rust
would also be a controversial change; choosing to keep types uncurried would disadvantage prospective users of the
system, and changing to curried types would require rethinking of syntax and educational materials to maintain Rust's
high standard of user experience.

## References

1. <span id="reference-constructor-classes">Jones, M. P. (1995). A system of constructor classes:
   overloading and implicit higher-order polymorphism. *Journal of functional programming*, 5(1),
   1-35.</span> <a href="#reference-constructor-classes:1">↩︎<sup>1</sup></a> <a href="#reference-constructor-classes:2">↩︎<sup>2</sup></a>

1. <span id="reference-quantified-constraints">Bottu, G. J., Karachalias, G., Schrijvers, T.,
   Oliveira, B. C. D. S., & Wadler, P. (2017). Quantified class constraints. *ACM SIGPLAN Notices*,
   52(10), 148-161.</span> <a href="#reference-quantified-constraints:1">↩︎</a>
