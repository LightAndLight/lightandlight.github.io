---
title: Lambdas are Codatatypes
date: 2019-07-01 00:00:00
permalink: /lambdas-are-codatatypes
tags:
    - programming
---

I was first clued into this a while ago by 
[a comment on Bob Harper's blog](https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/) that "exponentials are coinductive", but it only really clicked for me today. Let's get into it.

## Datatypes

### Defining them

When you define a datatype, you list the ways to construct values of that type. For example,
this definition:

```
data Bool : Type where { 
  True;
  False
}
``` 

says there are two ways to construct a `Bool`: `True` and `False`. 

Similarly, this definition:

```
data These (a : Type) (b : Type) : Type where { 
  This[a];
  That[b];
  These[a, b]
} 
```

gives three ways to construct a `These a b` (for any values of `a` and `b`). `This[0]` has type 
`These Int x`, for any `x`. `That[True]` has type `These x Bool` for any `x`. `These[0, True]` has 
type `These Int Bool`.

I want to note that constructors aren't functions; they have a fixed number of arguments and must 
be fully applied.

Datatypes can also be defined recursively:

```
data List (a : Type) : Type where {
  Nil;
  Cons[a, List a]
}
```

### Using them

The way you construct a value of a datatype is unique to that datatype; there are a finite number of
constructors, and each represents a different way to build a value of that type. In contrast, there is
a universal way to *destruct* values: pattern matching.

If some expression `x` has type `Bool` then we can destruct `x` using pattern matching:

```
case x of {
  True -> ...;
  False -> ...
}
```

A pattern match acknowledges all the ways that a value could have been constructed, and provides
a branch for each possible case. When constructors carry around other values 
(like those of `These` or `List`), pattern matching is used to write programs that extract and 
process the inner values:

```
case y of {
  This[a] -> f a;
  That[b] -> g b;
  These[c, d] -> h c d
}
```

When a program is running, the value that is being matched will eventually reduce to a constructor form:

```
case (These 0 True) of {
  This[a] -> f a;
  That[b] -> g b;
  These[c, d] -> h c d
}
```

at which point, the appropriate branch is selected and the contents of the constructor are substituted
to the right of the `->`. The above code will pick the `These` branch, substituting `0` for `c` and `True` 
for `d`, so that the final result is `h 0 True`.

Pattern matching is enough to process non-recursive datatypes, but recursive datatypes require recursive
function definitions:

```
sum : List Int -> Int
sum n =
  case n of {
    Nil -> 0;
    Cons[x, xs] -> x + sum xs
  }
```

Hopefully this is all familiar to you. I've covered all this so that it contrasts with *codatatypes*.

## Codatatypes

Codatatypes are the dual to datatypes. Formally, this means a lot of things that I don't yet understand. What
follows is how this duality arises in practise.

To begin, I'd like to share some hand-wavy intuition for the concepts I'm
discussing.

Datatypes *are*. They're finite, fully-evaluated structures. They're inert; they just exist and won't ever
"do anything". Haskell doesn't have true 'datatypes' in this sense because its constructors don't force their 
arguments to be evaluated, which means you can hide computations inside them. Haskell lets you partially 
apply constructors, which further diverges from what I've laid out here.

Codatatypes *do*. They have 'potential energy'; they have the capacity to do more work when prodded. Haskell's
'datatypes' are more codata-like in this respect because they can contain suspended computations.

### Defining them

Since datatypes are defined by their constructors, codatatypes will be defined by their *destructors*.

This definition:

```
codata Pair (a : Type) (b : Type) : Type where {
  fst : a;
  snd : b
}
```

says that there are two ways to *destruct* a `Pair a b` (for any `a` and `b`). If some expression `x` has 
type `Pair a b`, then `x.fst` has type and `a`, and `x.snd` has type a `b`.

`Pair` really is pair, it has just been defined by the ways you can pull things out of it- you can either
extract the first thing, or you can extract the second.

I also want to note that destructors aren't functions, either. You can't partially apply a destructor, and 
they're not first-class.

Codatatypes can also be recursive:

```
codata Stream (a : Type) : Type where {
  head : a;
  tail : Stream a
}
```

A stream is like an infinite list; every stream value contains a head and a tail, and no matter how many
times you extract the tail, there will always be another stream waiting for you.

### Using them

There is a universal way to destruct datatypes, and there is a universal way to *construct* *codatatypes*.
For lack of a better term, you can call it 'copattern matching'. Here's how you would construct a
`Pair Int Bool`:

```
cocase Pair Int Bool of {
  fst -> 0;
  snd -> True
}
```

A copattern match acknowledges every way it could be destructed, and provides a branch for each case.
Remember, copattern matching *constructs* values. The above code is a value that produces `0` when
destructed using `fst`, and `True` when destructed using `snd`. It is defining a pair of `0` with `True`.

When a program is running, a value that is being destructed will eventually reduce to a copattern match form.
So `x.fst` might reduce to `(cocase Pair Int Bool of { fst -> 0; snd -> True }).fst`. At this point,
the appropriate branch in the copattern match will be chosen, and the right hand side of the `->` will be
selected. In this case, `(cocase Pair Int Bool of { fst -> 0; snd -> True }).fst` reduces to `0`.

Recursive codatatypes like `Stream` need to be constructed by recursive definitions:

```
countFrom : Int -> Stream Int
countFrom n =
  cocase Stream Int of {
    head -> n;
    tail -> countFrom (n+1)
  }
```

`countFrom 0` produces an infinite stream of integers starting at `0`. However, it doesn't spin forever,
trying to construct the entire stream in one go. This is because a lone copattern match won't reduce; reduction
only continues after a destructor has been applied and the correct branch has been selected. Because of
this, codatatypes can represent infinite values that are only generated on demand.

Datatype constructors can carry around values, and so can codatatype *destructors*.
Here's what that looks like:

```
codata Lambda (a : Type) (b : Type) : Type where {
  apply[a] : b
}
```

There is one way to destruct a value of type `Lambda a b` called `apply`, and this destructor takes a
parameter. If `f` has type `Lambda a b`, and `x` has type `a`, then `f.apply[x]` has type `b`.

To create a value of type `Lambda a b`, you would use a copattern match:

```
cocase Lambda a b of {
  apply[x] -> ...
}
```

The destructor's parameter is abstract and is to be filled by the value that the destructor will be carrying.
For example, `(cocase Lambda Int Int of { apply[x] -> x + 1 }).apply[2]` selects the appropriate branch
(there's only one), and substitutes `2` for `x` to the right of the `->`. It steps to `2 + 1`.

So lambdas can be defined as codatatypes. Their destructor corresponds to function application, and 
copattern matching corresponds to abstraction. This is awesome!
