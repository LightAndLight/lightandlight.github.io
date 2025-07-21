---
title: Sized types and coinduction in Safe Agda
permalink: /sized-types-and-coinduction-in-safe-agda
date: 2024-09-04
math: true
excerpt: |
  Agda's <a href="https://agda.readthedocs.io/en/latest/language/sized-types.html">sized types</a> are inconsistent.
  This post is about why I care and how I've worked around it.
---

<div id="toc"><!-- generated --></div>

Agda's [sized types](https://agda.readthedocs.io/en/latest/language/sized-types.html) are inconsistent.
This post is about why I care and how I've worked around it.
If you just want to read the code, go [here](https://github.com/LightAndLight/agda-safe-sized-types).

## Background

Here's a [typical](https://agda.readthedocs.io/en/v2.7.0/language/coinduction.html) coinductive type in Agda&mdash;a continuing stream:

```agda
record Stream (A : Set) : Set where
  coinductive
  field
    head : A
    tail : Stream A
```

Some might describe `Stream` as an "infinite data structure"; an infinite list of `A`s.
I think of it as a sequential process that can always yield another `A`.

Coinductive types (codata for short) are important because they help us reason about potentially unending processes.
A stream has no end&mdash;if you try to collect all of its elements then you'll never finish&mdash;and yet there are many useful terminating programs we can write involving streams.

The following function, `zipWith`, is an example of such a function.
It defines an output stream in terms of two input streams,
where each element of the output is the combination of the two input elements at that position.
It uses [copattern syntax](https://agda.readthedocs.io/en/latest/language/copatterns.html), which is essentially a syntax to define record values by cases.

```agda
{-# OPTIONS --guardedness #-}
open Stream

zipWith : {A B C : Set} → (A → B → C) → Stream A → Stream B → Stream C
head (zipWith f sa sb) = f (sa .head) (sb .head)
tail (zipWith f sa sb) = zipWith f (sa .tail) (sb .tail)
```

In words:

* The head of `zipWith f sa sb` is the combination of `sa`'s head and `sb`'s head via `f`
* The tail of `zipWith f sa sb` is the the `zipWith`ed tails of `sa` and `sb`.

The stream is *productive* the `head` case and the `tail` case always terminates.
Definitions like `head (zipWith f sa sb) = head (zipWith f sa sb)` and `tail (zipWith f sa sb) = tail (zipWith f sa sb)` are not productive because they create infinite loops.
And a function like `filter`:

```agda
filter : {A : Set} → (A → Bool) → Stream A → Stream A
head (filter f s) =
  if f (s .head)
  then s .head
  else (filter f (s .tail)) .head
tail (filter f s) = filter f (s .tail)
```

is not productive when all elements of the input stream fail the predicate.
If you have a stream `s` of odd numbers and then `filter isEven s`, then `(filter isEven s) .head` will spin forever,
trying to find the first even number.

Productivity is undecidable, so productivity checking is always conservative.
The `--guardedness` option in the definition of `zipWith` enables a particular productivity checking heuristic for recursive functions.

I've found it quite easy to hit the limit of Agda's productivity checker.
For example, it rejects this definition of the fibonacci sequence[^fibs], which (coming from a Haskell background) I consider an elegant and idiomatic use of streams:

```agda
fib : Stream ℕ
head fib = 0
head (tail fib) = 1
tail (tail fib) = zipWith _+_ fib (tail fib)
```

The limits of Agda's productivity checking recently came up when I was working through
[Symbolic and Automatic Differentiation of Languages by Conal Elliott](http://conal.net/papers/language-derivatives/).
It defines a coinductive parser for context-free languages[^coinductive-context-free], and some of the corecursive definitions are incorrectly rejected by the productivity checker. 
Conal uses Agda's [sized types](https://agda.readthedocs.io/en/v2.7.0/language/sized-types.html) to fix this.
Sized types[^sized-types] are a type-based approach to termination and productivity checking,
and they seem to be the canonical move in this situation;
their section in the Agda manual uses the same kind of coinductive parser in its explanation of the feature.

Unfortunately Agda's implementation of sized types is inconsistent[^agda-sized-types-inconsistent].
I'm not very comfortable using known inconsistent features because I don't want the burden of figuring out whether I'm using them correctly.
When playing with infinite coinductive types like streams, I need the extra assurance that I'm not going to create an infinite loop or unsafe type cast.
I started playing with my own encoding of sized types under Agda's [`--safe` flag](https://agda.readthedocs.io/en/v2.7.0/language/safe-agda.html),
which disables inconsistent features, and had some success.
Here's what I found.

## The encoding

First we need propositional equality for
[erased](https://agda.readthedocs.io/en/v2.7.0/language/runtime-irrelevance.html)
values, because I found erased size indices necessary for reasonable performance.

```agda
data _≡_ {A : Set} (@0 x : A) : @0 A → Set where
  refl : x ≡ x
```

### Example: streams

Here's how streams are defined:

```agda
data Stream (A : Set) (@0 i : ℕ) : Set where
  stream :
    ({@0 j : ℕ} → i ≡ suc j → A × Stream A j) →
    Stream A i
```

In words: a `Stream A i` is a function that returns a head `A` and a tail `Stream A j` whenever
you can show `i = j + 1`.

The destructors look like this:

```agda
head : {A : Set} {@0 i : ℕ} → Stream (suc i) A → A
head (stream s) = proj₁ (s refl)

tail : {A : Set} {@0 i : ℕ} → Stream (suc i) A → Stream i A
tail (stream s) = proj₂ (s refl)
```

The size parameter indicates how much of the stream is known to be usable (productive and consistent).
`head` says that a stream with at least one element can produce a head.
`tail` says that a stream with at least one element can produce a tail whose size is 1 smaller than the original.
A stream with size parameter 0 is unusable because `0 ≡ suc j` is false;
we have no information about the stream's elements.

This is enough to define streams via recursive functions:

```agda
map : {A B : Set} {@0 i : ℕ} → (A → B) → Stream A i → Stream B i
map f s =
  stream λ{ refl →
    f (head s) , map f (tail s)
  }

zipWith : {A B C : Set} {@0 i : ℕ} → (A → B → C) → Stream A i → Stream B i → Stream C i
zipWith f s1 s2 =
  stream λ{ refl →
    f (head s1) (head s2) , zipWith f (tail s1) (tail s2)
  }
```

I was excited to write the Haskell-style fibonacci sequence[^haskell-fib]:

```agda
fib : {@0 i : ℕ} → Stream ℕ i
fib =
  stream λ{ refl →
    0 , stream λ{ refl →
      1 , zipWith _+_ fib (tail fib)
    }
  }
```

Agda considers a function like `fib` total because the recursive calls are given a strictly smaller size parameter.
`fib` takes `i` as an implicit argument.
Inside the first `refl` case, `i` is refined to `suc j` for some `j`.
Then inside the second `refl` case, `j` is refined to `suc j'` so `i` is refined to `suc (suc j')` for some `j'`.
`zipWith _+_ fib (tail fib)` has type `Stream ℕ j'`, and size indices are passed implicitly as follows:
`zipWith _+_ (fib {i = j'}) (tail (fib {i = suc j'}))`.
Agda sees this is
[structural recursion](https://agda.readthedocs.io/en/v2.7.0/language/termination-checking.html#structural-recursion)&mdash;each recursive `fib` call is given a strictly smaller size&mdash;and accepts the definition as terminating.

Streams can also have more complex sizes.
The following function averages each consecutive pair of elements,
and the sizes say that for every element in the output stream, two elements are required from the input stream:

```agda
fil : {@0 i : ℕ} → Stream ℕ (i * 2) → Stream ℕ i
fil s =
  stream λ{ refl →
    avg (head s) (head (tail s)) , fil (tail (tail s))
  }
  where
    avg : ℕ → ℕ → ℕ
    avg m n = (m + n) / 2
```

The size parameter of a stream can also be "forgotten", for situations where sizes don't matter:

```agda
data Stream-∞ (A : Set) : Set where
  stream-∞ : ({@0 i : ℕ} → Stream A i) → Stream-∞ A

to-∞ : {A : Set} → ({@0 i : ℕ} → Stream A i) → Stream-∞ A
to-∞ = stream-∞

from-∞ : {A : Set} → Stream-∞ A → ({@0 i : ℕ} → Stream A i)
from-∞ (stream-∞ f) = f
```

`Stream-∞` is the typical coinductive stream type, equivalent to the coinductive definition I gave at the start of this post.
Such a stream is unboundedly usable; you can always get its head or tail:
  
```agda
head-∞ : {A : Set} → Stream-∞ A → A
head-∞ s = head (from-∞ s {i = 1})

tail-∞ : {A : Set} → Stream-∞ A → Stream-∞ A
tail-∞ s = to-∞ (tail (from-∞ s))
```

A program that consumes an entire `Stream-∞` is productive but non-terminating.
Non-termination isn't useful for theorem proving, so it's usually avoided in Agda, but it is useful for everyday programs.
The [`yes`](https://en.wikipedia.org/wiki/Yes_(Unix)) command is a reasonable non-terminating program, and so is a program that prints the fibonacci sequence.
Here's one way to consume infinite streams that I consider morally okay, despite requiring unsafe Agda[^unsafe]:

```agda
postulate _*>_ : {A B : Set} → IO A → IO B → IO B
{-# COMPILE GHC _*>_ = \_ _ -> (*>) #-}

{-# NON_TERMINATING #-}
consume-∞ : {A B : Set} → (A → IO ⊤) → Stream-∞ A → IO B
consume-∞ f s = f (head-∞ s) *> consume-∞ f (tail-∞ s)
```

`consume-∞` is a loop that [arbitrarily](http://conal.net/blog/posts/is-haskell-a-purely-functional-language) observes each stream element in turn.
Its output type is `IO B` for all types `B` because it never returns.

This can be used to print the fibonacci sequence:

```agda
main : IO ⊤
main = consume-∞ putStrLn (to-∞ (map show-ℕ fib))
  where
    open import Data.Nat.Show renaming (show to show-ℕ)

    postulate putStrLn : String → IO ⊤
    {-# FOREIGN GHC import qualified Data.Text.IO #-}
    {-# COMPILE GHC putStrLn = Data.Text.IO.putStrLn #-}
```

```
$ ./fib | head -n 10
0
1
1
2
3
5
8
13
21
34

$ ./fib | head -n 1000 | tail -n 1
26863810024485359386146727202142923967616609318986952340123175997617981700247881689338369654483356564191827856161443356312976673642210350324634850410377680367334151172899169723197082763985615764450078474174626
```

### Example: (co)lists

Here's a brief second example so you can start to extrapolate.
`Colist` is the coinductive list; an `A`-producing process that could end, but might also continue forever:

```agda
data Colist (A : Set) (@0 i : ℕ) : Set where
  colist :
    ({@0 j : ℕ} → i ≡ suc j → Maybe (A × Colist A j)) →
    Colist A i
```

### Summary

In short: a datatype definition is given a size parameter, and recursive uses of the type are "guarded" by a proof that the size parameter decreases.

In the
[actual code](https://github.com/LightAndLight/agda-safe-sized-types)
I've packaged this pattern as its own type `■ : (A : @0 ℕ → Set) → @0 ℕ → Set`, so `Stream` and `Colist` become:

```agda
data Stream (A : Set) (@0 i : ℕ) : Set where
  stream : ■ (λ j → A × Stream A j) i → Stream A i

data Colist (A : Set) (@0 i : ℕ) : Set where
  colist : ■ (λ j → Maybe (A × Colist A j)) i → Colist A i
```

## Proof

`Stream-∞` is clearly a coinductive stream, but I still had my doubts about `Stream`.
The definitive test is whether it obeys [coalgebraic semantics](https://en.wikipedia.org/wiki/Coinduction#Relationship_with_F-coalgebras).
I was able to prove that `Stream` is the final coalgebra of a particular functor, in the category of size-indexed families of types.
The functor in question is like the one that gives rise to unsized streams (`F(X) = A * X`), but it also keeps track of the change in size indices.

The proofs are not very fun to read, so I'll omit them from this post.
You can find them
[here](https://github.com/LightAndLight/agda-safe-sized-types/blob/8876b3de679618d352481a69e2e1b8ed48b460e6/src/Codata/SafeSized/Stream/Coalgebra.agda#L141-L243).

## Future work

* Parser combinators

  This all started because I wanted to figure out how to do parsing in Agda.
  I successfully used this style of sized coinductive types to write a coinductive parser.
  It was easy to write the connectives for parsing regular languages, as in Conal's paper.
  I then added a guarded fixpoint combinator to parse non-regular languages, like expression trees.
  I've now got a total, `--safe` parser combinator library for Agda, which I'll introduce in another post.

* Generalised greatest fixed point proofs

  I proved that `Stream` was a final coalgebra, but I have no proofs for any other codatatypes.
  I'm confident that I could write them, but it would be a chore.
  Is it possible to validate the approach once and for all for the greatest fixed point `ν : ((@0 ℕ → Set) → @0 ℕ → Set) → @0 ℕ → Set`?
  Then, justifying a particular codatatype definition would be as simple as defining an isomorphism with `ν F` for a particular `F`.

* Where's the limit?

  I'm curious about the limitations of my approach.
  It's good enough for the practical coinductive problems I've tested it on, so where does it break down?
  Consistency often comes at the expense of completeness.
  Does this technique rule out some sound programs that Agda's normal sized types can encode?  

## Appendix: philosophical ramblings

### Infinity as an object

The original sized types paper (Hughes & Pareto, 1996) and Agda's implementation of sized types both introduce an explicit size for unbounded usability;
called `ω` and `∞` respectively.
I'm suspicious of this move, because it feels like it's begging for paradoxes[^paradox].

The sized types paper is very careful about uses of `ω`, noting that for types `∀(i : Size). T` it's not always sound
to instantiate `i` with `ω` in `T`.
The resulting type system is more complex and harder to implement correctly,
and I doubt that the convenience is worth it.

I think my intuitions here are justified given the state of Agda's built-in sized types.
Agda has an issue label called [infinity-less-than-infinity](https://github.com/agda/agda/labels/infinity-less-than-infinity),
because there are a number of ways to prove `false` by assuming that the size `∞` is smaller than itself.
Whether this is due to software bugs or broken theory,
I think the root cause is the complexity of trying to think of infinity as "the same sort of thing" as a finite size.

### Codata via $\Pi$-types

I like this encoding of sized types because it puts a $\Pi$-type at the center of the coinductive definition.
It could be made even more obvious like this:

```agda
data Field : Set where
  head tail : Field

data Stream (A : Set) (@0 i : ℕ) : Set where
  stream :
    ({@0 j : ℕ} → i ≡ suc j → (f : Field) → case f of λ{
      head → A ;
      tail → Stream A j
    }) →
    Stream A i
```

Then defining a stream by cases on `Field` is pretty close to copattern matching:

```agda
repeat : {A : Set} {@0 i : ℕ} → A → Stream A i
repeat x =
  stream λ{ refl →
    λ{
      head → x ;
      tail → repeat x
    }
  }
```

The relationship between data and codata seems analogous to the relationship between $\Sigma$-types and $\Pi$-types.
I don't have anything rigorous to say about this; it's just a feeling.
Here's a table that expands on the feeling:

<style>
.padded-columns tr > td + td {
  padding-left: 2em;
}
</style>
<table class="padded-columns" align="center">
<tbody>
<tr><td>Data</td><td>Codata</td></tr>
<tr><td>Induction</td><td>Coinduction</td></tr>
<tr><td>Positive types</td><td>Negative types</td></tr>
<tr><td>$\Sigma$-types</td><td>$\Pi$-types</td></tr>
<tr><td>Intensional equality</td><td>Extensional equality</td></tr>
</tbody>
</table>

[^sized-types]:
    See:

    * Hughes, J., Pareto, L., & Sabry, A. (1996, January).
      Proving the correctness of reactive systems using sized types.      In Proceedings of the 23rd ACM SIGPLAN-SIGACT symposium on Principles of programming languages (pp. 410-423).

    * Abel, A. (2008). Semi-continuous sized types and termination. Logical methods in computer science, 4.

[^fibs]: Here's a version that works:

    ```agda
    fib : Stream ℕ
    fib = fib' 0 1
      where
        fib' : ℕ → ℕ → Stream ℕ
        head (fib' a b) = a
        tail (fib' a b) = fib' b (a + b)
    ```

[^coinductive-context-free]:
    See also:

    * Hasuo, I., & Jacobs, B. (2005, September).
      Context-free languages via coalgebraic trace semantics.
      In International Conference on Algebra and Coalgebra in Computer Science (pp. 213-231). Berlin, Heidelberg: Springer Berlin Heidelberg.

    * Winter, J., Bonsangue, M. M., & Rutten, J. (2011, August).
      Context-free languages, coalgebraically.
      In International Conference on Algebra and Coalgebra in Computer Science (pp. 359-376). Berlin, Heidelberg: Springer Berlin Heidelberg.

    * Abel, A. (2016).
      Equational reasoning about formal languages in coalgebraic style.
      preprint available at http://www.cse.chalmers.se/~abela/jlamp17.pdf.

[^agda-sized-types-inconsistent]:
    See:
    
    * Relevant GitHub activity: <https://github.com/agda/agda/pull/5354>
      * <https://github.com/agda/agda/issues/1201>
      * <https://github.com/agda/agda/issues/1946>
      * <https://github.com/agda/agda/issues/2820>
      * <https://github.com/agda/agda/issues/3026>

    * [The State of Sized Types (2021)](https://ionathan.ch/2021/08/04/sized-types.html)

[^haskell-fib]: In Haskell: `fib = 0 : 1 : zipWith (+) fib (tail fib)`

[^paradox]: [nLab](https://ncatlab.org/nlab/show/HomePage) redirects Girard's paradox to [Burali-Forti's paradox](https://ncatlab.org/nlab/show/Burali-Forti%27s+paradox),
    which describes a paradox using ordinal numbers which has a very similar vibe to Agda's sized types situation.

[^unsafe]: In the real code I defined this in a separate file so that I can use `--safe` everywhere else.
