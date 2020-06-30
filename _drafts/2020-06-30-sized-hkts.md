---
layout: post
title: Statically Sized Higher-kinded Polymorphism
author: ielliott95
permalink: /sized-hkts/
date: 2020-06-30 08:00:00 +1000
tags:
    - programming
---

<div style="display: flex">
  <div>
    <p>
        Memory-sensitive languages like C++ and Rust use compile-time information to calculate
        sizes of datatypes. These sizes are used to inform alignment, allocation, and calling conventions in ways
        that improve runtime performance. Modern languages in this setting support generic types, but so far
        these languages only allow parameterisation over types, not type constructors. In this article I describe
        how to permit parameterisation over arbitrary type constructs, while still retaining compile-time calculation
        of datatype sizes.
    </p>
  </div>

  <div style="min-width: 30%; margin-left: auto; margin: 1em;">
    <div style="background: #f2f2f2; padding-left: 1.5em; padding-right: 1.5em; border: 1px solid #dddddd; border-radius: 0.25em;">
      <h3>Contents</h3>
      <ul style="list-style-type: none">
          <li>
              <a href="#background">Background</a>
          </li>
          <ul style="list-style-type: none">
              <li><a href="#kinds">Kinds</a></li>
              <li><a href="#type-classes">Type Classes</a></li>
          </ul>
          <li><a href="#solution">Solution</a></li>
      </ul>
    </div>
  </div>
</div>

## Background

### Kinds

Many typed languages support some form of generic (parameterised) datatypes. In Rust, for example, one
can define the type of pairs as `struct Pair<A,B>(fst: A, snd: B)`. In this definition, `A` and `B` are type 
variables (or type parameters), and can be substituted for other types: 
`Pair<bool, bool>`, `Pair<bool, char>`, and `Pair<String, int32>`
are all valid pairs.

The name of a type, without any parameters, is known as a type constructor. `Pair` is not a type on its own; 
`Pair<A,B>` (for some types `A` and `B`) is. The number of types required to 'complete' a type constructor is known
as its arity (so `Pair` has arity 2). The arity of a type constructor must always be respected; it's an error to 
provide greater or fewer type parameters than are expected. For example, `Pair<bool>` and 
`Pair<char, int32, String>` are invalid.

A consequence of all this is that in such languages, type variables can only stand for types. But there
are good reasons to have type variables that stand for type constructors, too:

```
enum One<A>(A)

impl <A> One<A>{
  map<B>(f: Fn(A) -> B) -> One<B> { ... }
}

enum Two<A>(A, A)

impl <A> Two<A>{
  map<B>(f: Fn(A) -> B) -> Two<B> { ... }
}

enum Three<A>(A, A, A)

impl <A> Three<A>{
  map<B>(f: Fn(A) -> B) -> Three<B> { ... }
}
```

Here are some 1-arity container types. The only difference between these datatypes is the number of elements
they contain. They all support a `map` operation, which applies a function to all the datatype's elements. Functions
that use `map` need to be implemented once for each type, even when their implementations are identical:

```
fn incrOne(x: One<int32>) -> One<int32> { x.map(|n| n + 1) }

fn incrTwo(x: Two<int32>) -> Two<int32> { x.map(|n| n + 1) }

fn incrThree(x: Three<int32>) -> Three<int32> { x.map(|n| n + 1) }
```

To remedy this, there must first be a way to abstract over the type constructors, so that the code can
be written *once* and for all:

```
fn incr<F>(x: F<int32>) -> F<int32> { x.map(|n| n + 1) } // when F<A> has map, for all types A
```

Then, there must be some way to rule out invalid types. For example, replacing `F` with `bool` in `F<int32>`
is invalid, because `bool<int32>` is not a type. This is the job of kinds.

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

### Type Classes

## Solution
