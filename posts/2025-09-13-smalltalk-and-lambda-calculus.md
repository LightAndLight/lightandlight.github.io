---
title: Smalltalk and Lambda Calculus
permalink: /smalltalk-and-lambda-calculus
date: 2025-09-13
tags:
  - programming
---

<div id="toc"><!-- generated --></div>

In [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk), everything is an "object".
An "object" is a thing that receives and processes "messages".
A "message" has a name and zero or more arguments.
How do you build express common datatypes, such as booleans and numbers, using only objects?

## Boolean objects

A Smalltalk boolean is an object that understands a message named `ifTrue:ifFalse:`.
`ifTrue:ifFalse:` takes two arguments, which should both be ["code blocks"](https://en.wikipedia.org/wiki/Smalltalk#Code_blocks).
`true` is the boolean that, when it receives `ifTrue: firstArgument ifFalse: secondArgument`, runs `firstArgument`.
On the other hand, `false` is the boolean that runs `secondArgument`.
This can be concisely specified by two equations:

1. `true ifTrue: a ifFalse b = a value`
2. `false ifTrue: a ifFalse: b = b value`

## (Natural) number objects

What about numbers?
The introductions to Smalltalk that I've skimmed don't talk about constructing numbers.
One message to integers stands out, though: `timesRepeat:`.
`timesRepeat:` takes one argument, a code block.
An integer object `n` processes `timesRepeat:` by running the code block `n` times (and zero times if the integer is negative).
The caveat for negative integers suggests that `timesRepeat:` is better suited for natural numbers, but Smalltalk doesn't have them in its numeric hierarchy.

`timesRepeat:` almost characterises natural numbers, but not quite.
Here's my characterisation.
A natural number is an object that understands a message named `from:iterate:` (I made this up so I don't know if there's a standard Smalltalk equivalent).
The message takes two arguments, an arbitrary object and a single-argument code block.
`0` is the natural number that, when it receives `from: initial iterate: next`, just returns `initial`.
A natural number `n` (where `n > 0`) is an object that contains a reference to its predecessor (called `pred`).
`from: initial iterate: next` is implemented as follows:

```smalltalk
from: initial iterate: next [
  ^next value: (pred from: initial iterate: next)
]
```

`timesRepeat:` can be implemented in terms of `from:iterate:`:

```smalltalk
timesRepeat: block [
  from: nil iterate: [ :ignored | block value ]
]
```

The specification is:

1. `0 from: initial iterate: next = initial`
2. `n succ from: initial iterate: next = n from: (next value: initial) iterate: next`

## Lambda calculus

Where objects are the fundamental building blocks of Smalltalk, (single-argument) functions are the fundamental building blocks of [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).
Datatypes like booleans and (natural) numbers can also be expressed using only functions.

A boolean is a function that takes two arguments; the `true` boolean chooses the first argument and the `false` boolean chooses the second:

```haskell
true = \x y -> x
false = \x y -> y
ifTrueIfFalse cond x y = cond x y
```

A natural number `n` is a function that takes two arguments, and applies the second argument to the first `n` times:

```haskell
zero = \x f -> x
succ n = \x f -> f (n x f)
fromIterate n x f = n x f
```

These definitions (known as [Church encodings](https://en.wikipedia.org/wiki/Church_encoding)) express exactly the same intent as the Smalltalk equivalents.
This is not a coincidence: Smalltalk's objects and lambda calculus' functions are actually the same sort of thing.

## Objects ↔ lambas

A lambda is an object that understands a single-argument message called `value:`.
The object responds to the `value:` message by transforming the argument and returning a result.

An object is a lambda that takes two arguments: a message name, and the message payload.
When then lambda is applied to a message name and payload, it uses the message name to look up a handler function and applies that function to the payload.

Variables captured by the lambda are the object's instance variables, and vice versa.
In both cases, these variables aren't detectable from the outside. 

While I haven't mentioned types thus far, in the end type theory explains the connection between objects and lambdas:
they're both [coinductive types](https://en.wikipedia.org/wiki/Coinduction) (see also: [Lambdas are Codatatypes](https://blog.ielliott.io/lambdas-are-codatatypes)).

## Thoughts

* In general, Smalltalk feels like a nicer syntax for church encoding. I like it.

* I wonder why Smalltalk doesn't have anonymous objects?

  The language already has a small conceptual footprint, but because objects can only be created by messaging a class (which is itself an object),
  classes and objects need to be defined in terms of each other.
  Maybe classless, anonymous objects could serve as the foundation for a classy object system.
