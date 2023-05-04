---
layout: math-post
title: Nominal Sets
author: ielliott95
permalink: /nominal-sets/
date: 2023-04-24 11:30:00 +1000
excerpt: |
  Developing a variable binding library for Rust, based on the
  theory of nominal sets.
---

<div class="contents" style="float: right; margin-left: 1em;">
<h3>Contents</h3>

<ul style="list-style-type: none; padding: 0;">
  <li><a href="#introduction">Introduction</a></li>

  <li><a href="#names-and-binders">Names and Binders</a></li>
  <li>
    <a href="#nominal-sets" style="display: inline-block; padding-bottom: 0.5em;">Nominal Sets</a>
    <ul style="list-style-type: none; padding-left: 1em;">
      <li><a href="#names">Names</a></li>
      <li>
        <a href="#permutations" style="display: inline-block; padding-bottom: 0.5em;">Permutations</a>
        <ul style="list-style-type: none; padding-left: 1em;">
          </li><a href="#permutations-on-functions">Permutations on Functions</a>
        </ul>
      </li>
      <li>
        <a href="#support" style="display: inline-block; padding-bottom: 0.5em;">Support</a>
        <ul style="list-style-type: none; padding-left: 1em;">
          <li><a href="#minimal-support">Minimal Support</a></li>
          <li><a href="#the-support-of-a-function">The Support of a Function</a></li>
        </ul>
      </li>
      <li><a href="#freshness">Freshness</a></li>
      <li><a href="#the-category-of-nominal-sets">The Category of Nominal Sets</a></li>
      <li><a href="#some-adjunctions">Some Adjunctions</a></li>
    </ul>
  </li>
  <li><a href="#showing-off">Showing Off</a></li>
</ul>

</div>

For years I've used [De
Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index), in part because
of Haskell's [bound](https://hackage.haskell.org/package/bound) library, which abstracts (pun
intended) De Bruijn indexed binders in a simple and type-safe way. Having learned to "think" using de Bruijn indices, I naturally used them when I wrote
[Ipso](https://github.com/lightandlight/ipso), which is written in Rust. I wished I had `bound`, so I tried to port it to
Rust. Unfortunately, `bound` relies on [polymorphic
recursion](https://en.wikipedia.org/wiki/Polymorphic_recursion), which Rust 
[doesn't support](https://github.com/rust-lang/rust/issues/4287#issuecomment-11846582).

Writing all that variable binding machinery in Rust was tolerable, but Ipso probably isn't the last
programming language that I'll build with Rust. When it's time for me to build the next one, I'd
like to use a variable binding library instead. I think
[`moniker`](https://docs.rs/moniker/latest/moniker/) is the only[^please-correct-me] library on
[crates.io](https://crates.io/), and I might yet use it. In the meantime, I'd
like to explore an alternative way of tackling the problem, inspired by nominal sets.

## Names and Binders

Here is the core Rust implementation of my nominal-sets-inspired approach:

```rust
pub mod name {
  #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
  pub struct Name(usize); // (1)

  lazy_static! {
      static ref COUNTER: AtomicU64 = AtomicU64::new(0);
  }

  pub(crate) fn fresh() -> Name {
      Name(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
  }
}

pub mod binder {
  pub struct Binder<T>{ // (2)
    name: Name,
    body: T
  }

  impl <T> Binder<T>{
    /* (5)

    Correctness condition: `f` should only use its argument to construct the `T`.
    The name shouldn't be made to outlive `f` in any other way, e.g. by storing
    it in a mutable variable.
    */
    pub fn bind(f: impl FnOnce(Name) -> T) -> Self { // (3)
      use super::name::fresh;
      
      let name = fresh(); // (4)
      Binder{ name, body: f(name) }
    }

    /* (6)

    Correctness condition: `f` should not "leak" its `Name` argument. After `f` has
    returned, the name shouldn't be accessible by the program.
    */
    pub fn unbind<R>(self, f: impl FnOnce(Name, T) -> R) -> R {
      f(self.name, self.body)
    }
  }
}
```

Names (1) are opaque and can be compared for equality.

A binder (2) is a pair of a name with some type `T`, within which the name is considered *bound*.

`bind` (3) is the only way to create binders, which is also the only time new names are introduced (4).
Since the fields of `Binder` are hidden, every binder binds a unique name.

Binder introduction (`bind`) and elimination (`unbind`) come with correctness conditions (5, 6) that
prevent bound names from "escaping their scope". Programs that follow these rules are
capture-avoiding by construction; any terms that are substituted under a binder will not contain the
name bound by that binder.

There's more to add, such as `Clone` and `Eq` implementations for `Binder`. As I explain nominal
sets I'll translate the important concepts to code, so that by the end we'll have a pretty good
variable binding library.

## Nominal Sets

Nominal sets[^nominal-logic]<sup>,</sup>[^alpha-structural-recursion-and-induction]<sup>,</sup>[^nominal-techniques] is a theory of
names and name binding, intended to help with implementing and verifying programming languages. Its most
remarkable feature is an account of algebraic datatypes and recursion modulo [alpha
equivalence](https://stackoverflow.com/a/47762545/2884502). In practise, this gives an elegant way
to work with abstract syntax trees while being able to ignore the *specific* choice of names.

### Names

Names can be drawn from any [countably infinite](https://mathworld.wolfram.com/CountablyInfinite.html) set. In the literature, this set is written as
\\( \mathbb{A} \\) (for **A**tom). I'll keep to this convention while explaining the math.

The only operation on names is equality comparison.

### Permutations

A theory that deals with alpha equivalence needs a notion of "renaming variables". Nominal sets uses
[permutations](https://en.wikipedia.org/wiki/Permutation) of names.

A permutation of names (from here on, just "a permutation") is a bijection on names. I'll write
\(\Pi\) for the set of permutations, and \(\pi\) for any particular permutation.

The fundamental permutation is the swapping of two names, written \\( (a \; b) \\). \\( (a \; b) \\) is represented by
the bijection mapping \\( a \\) to \\( b \\), \\( b \\) to \\( a \\), and any other name to itself.

Being functions, permutations are used by *applying* them to names, written \\( \pi(a) \\).

In Rust I represent permutations using a `HashMap`. Applying takes keys to values, and any names not
in the `HashMap` are mapped to themselves. In other words, the `HashMap` represents a
permutation \\( \pi \\) by storing a pair \\( (x, \pi(x)) \\) for
each \\( x \\) where \\( \pi(x) \neq x \\).

```rust
pub mod permutation {
  use super::name::Name;

  #[derive(Debug, Clone)]
  pub struct Permutation(HashMap<Name, Name>);
  
  impl Permutation {
    pub fn swap(a: Name, b: Name) -> Self {
      Self(HashMap::from([(a, b), (b, a)]))
    }

    pub fn apply(&self, name: &Name) -> Name {
      self.get(name).copied().unwrap_or(*name)
    }

```

Permutations form a [group](https://en.wikipedia.org/wiki/Group_(mathematics)). The identity
(\\( \iota \\)) is the identity function. Multiplication (\\( \circ \\)) is function composition. Every
permutation \\( \pi \\) has an inverse \\( \pi^{-1} \\) (because they're bijections).

```rust
    pub fn id() -> Self {
      Permutation(HashMap::new())
    }

    pub fn after(&self, other: &Self) -> Self {
      let mut permutation = other
          .iter()
          .map(|(key, value)| (*key, self.apply(value)))
          .collect::<HashMap<Name, Name>>(); // (1)
      
      permutation.extend(self
          .iter()
          .filter(|(key, value)| {
            if other.contains(key) { 
              None 
            } else { 
              Some((key, value)) 
            }
          })); // (2)
      
      Permutation(permutation)
    }

    pub fn inverse(&self) -> Self {
      Permutation(self.iter().map(|(key, value)| (*value, *key)).collect())
    }
  }

```

I've named the multiplication function `after` so it's easier to remember the order of the
permutations. `after`, acting on `HashMap`s under the hood, needs a more clever definition than I
first expected. The final permutation \\( \pi_f \circ \pi_g \\) is constructed in two parts. The first
part (1) computes \\( \pi_f(\pi_g(x)) \\) for all \\( x \\) where \\( \pi_g(x) \neq x \\). The second part (2)
computes \\( \pi_f(x) \\) for all \\( x \\) where \\( \pi_g(x) = x \\). For these values, \\( \pi_f(\pi_g(x)) =
\pi_f(x) \\).

Names are aren't the only thing that can be affected by a permutation. "Applying" a permutation
generalises to other sets as the [action](https://mathworld.wolfram.com/GroupAction.html) of
permutations on those sets. Permutations can act on a set \\( X \\) when there exists a function \\( \alpha_X : (\Pi, X)
\rightarrow X \\) satisfying the following properties:

1. \\( \forall x. \; \alpha_X(\iota, x) = x \\) (identity)
1. \\( \forall x. \; \alpha_X(\pi_1 \circ \pi_2, x) = \alpha_X(\pi_1, \alpha_X(\pi_2, x)) \\) (composition)

Instead of writing \\( \alpha_X(\pi, x) \\) for a specific permutation action, I'll use the notation \\( \pi \cdot x \\).

```rust
  /*
  Laws:
  
  * `forall x. x.permute_by(Permutation::id()) == x` (identity)
  * `forall x. x.permute_by(f.after(g)) == x.permute_by(g).permute_by(f)` (composition)
  
  */
  pub trait Permutable {
    fn permute_by(&self, permutation: &Permutation) -> Self;
  }

```

Permutations trivially act on names: \\( \pi \cdot a = \pi(a) \\).

```rust
  impl Permutable for Name {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      permutation.apply(self)
    }
  }

```

Permutations also trivially act on themselves: \\( \pi_f \cdot \pi_g = \pi_f \circ \pi_g \\).

```rust
  impl Permutable for Permutation {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      permutation.after(self)
    }
  }

```

Permutations act on pairs element-wise: \\( \pi \cdot (x, y) = (\pi \cdot x, \pi \cdot y) \\).

```rust
  impl <A: Permutable, B: Permutable> Permutable for (A, B) {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      (self.0.permute_by(permutation), self.1.permute_by(permutation))
    }
  }

```

And similarly for sums: \\( \pi \cdot \text{in}_L(x) =
\text{in}_L(\pi \cdot x) \; | \; \pi \cdot \text{in}_R(y) =
\text{in}_R(\pi \cdot y) \\).

```rust
  impl <A: Permutable, B: Permutable> Permutable for either::Either<A, B> {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      use either::Either;
      match self {
        Left(a) => Left(a.permute_by(permutation)),
        Right(b) => Right(b.permute_by(permutation))
      }
    }
  }
} // mod permutation
```

#### Permutations on Functions

Permutations can also act on functions. To me this is only important for the theory, so I won't
implement it in Rust.

<div class="math">

$$ (\pi \cdot f)(x) = \pi \cdot f(\pi^{-1} \cdot x) $$

</div>

This is derived from the requirement that permutations distribute over function application: \\( \pi
\cdot f(x) = (\pi \cdot f)(\pi \cdot x) \\).

### Support

A set of names *supports* a value when the value "depends" on those names. Here's the formal definition:

<div class="math">

$$
\begin{array}{l}
\text{supports} : \mathcal{P}(\mathbb{A}) \times X
\\
\text{supports} =
\{ \; (\bar{a}, x) \; | \; 
\bar{a} \in \mathcal{P}(\mathbb{A}), \;
x \in X, \;
\forall \pi. \; (\forall a \in \bar{a}. \; \pi(a) = a) \implies
\pi \cdot x = x 
\}
\end{array}
$$

</div>

\\( \bar{a} \; \text{supports} \; x \\) when all permutations that keep the elements of \\( \bar{a} \\) the same
(\\( \forall a \in \bar{a}. \; \pi(a) = a) \\) also keep \\( x \\)
the same (\\( \pi \cdot x = x \\)).

For example, every name must support itself: \\( \{a\} \; \text{supports} \; a \\). 

<div class="math">

$$
\begin{array}{lll}
& \forall \pi. \;
(\forall a \in \{a\}. \; \pi(a) = a) \implies \pi \cdot a = a
\\
= & \forall \pi. \; \pi(a) = a \implies \pi \cdot a = a & (\text{singleton set})
\\
= & \forall \pi. \; \pi(a) = a \implies \pi(a) = a & (\text{definition of } \cdot)
\end{array}
\\ \; \\
\forall \pi. \; \pi(a) = a \implies \pi(a) = a \;\;\;\; \text{trivial}
$$

</div>

More importantly, \\( \neg (\{a\} \; \text{supports} \; a) \\) is false:

<div class="math">

$$
\begin{array}{lll}
& \neg (\{a\} \; \text{supports} \; a)
\\
= & \neg (\forall \pi. \; (\forall a \in \{a\}. \; \pi(a) = a) \implies \pi \cdot a = a)
\\
= & \neg (\forall \pi. \; (\pi(a) = a) \implies \pi \cdot a = a) & (\text{singleton set})
\\
= & \neg (\forall \pi. \; \pi(a) = a \implies \pi(a) = a) &
(\text{definition of } \cdot)
\end{array}
\\ \; \\
\neg (\forall \pi. \; \pi(a) = a \implies \pi(a) = a) \;\;\;\; \text{contradiction}
$$

</div>

For pairs: \\( (\bar{a} \cup \bar{b}) \; \text{supports} \; (x, y) \iff \bar{a} \; \text{supports} \; x
\land \bar{b} \; \text{supports} \; y \\).

For sums: \\( a \; \text{supports} \; \text{in}_L(x) \iff a \; \text{supports} \; x \\) and \\( a \;
\text{supports} \; \text{in}_R(y) \iff a \; \text{supports} \; y \\).

TODO: support of functions

#### Minimal Support

The definition of \\( \text{supports} \\) is a bit "loose", in that it allows names that don't occur in a value
to support said value.

For example, for names \\( a \\) and \\( b \\), \\( \{a,b\} \; \text{supports} \; a \\):

<div class="math">

$$
\begin{array}{ll}
& \forall \pi. \; (\forall a \in \{a, b\}. \; \pi(a) = a) \implies \pi \cdot a = a
\\
= & \forall \pi. \; \pi(a) = a \land \pi(b) = b \implies \pi \cdot a = a &
(\text{expand set})
\\
= & \forall \pi. \; \pi(a) = a \land \pi(b) = b \implies \pi(a) = a &
(\text{definition of } \cdot)
\end{array}
\\ \; \\
\forall \pi. \; \pi(a) = a \land \pi(b) = b \implies \pi(a) = a \;\;\;\;
\text{trivial}
$$

</div>

The notion of *minimal* support tightens this up. The minimal support of a value consists of only
the names the value *actually* depends on.

<div class="math">

$$
\begin{array}{l}
\text{supports}_{min} : \mathcal{P}(\mathbb{A}) \times X
\\
\text{supports}_{min} = \{ (\bar{a}, x) \; | \; \bar{a} \in \mathcal{P}(\mathbb{A}), \; x \in X, \;
\bar{a} \; \text{supports} \; x, \forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x} \}
\end{array}
$$

</div>

\\( \bar{a} \\) is the minimal support of \\( x \\) when it is a subset of all other sets that support \\( x \\). I can say "the" minimal support, because it's unique for every value.

<div class="math">

$$
\bar{a} \; \text{supports}_{min} \; x
\land
\bar{b} \; \text{supports}_{min} \; x
\implies
\bar{a} = \bar{b}
\\ \; \\
\begin{array}{ll}
&
\bar{a} \; \text{supports}_{min} \; x
\land
\bar{b} \; \text{supports}_{min} \; x
\\
= &
(\bar{a}, x) \in
\{ (\bar{a}, x) \; | \; 
\bar{a} \in \mathcal{P}(\mathbb{A}), \;
x \in X, \;
\bar{a} \; \text{supports} \; x, \;
\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x}
\} \;
\land
\\
&
(\bar{b}, x) \in
\{ (\bar{a}, x) \; | \; 
\bar{a} \in \mathcal{P}(\mathbb{A}), \;
x \in X, \;
\bar{a} \; \text{supports} \; x, \;
\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x}
\}
\\
= &
\bar{a} \; \text{supports} \; x \; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x})
\; \land \;
\\
&
\bar{b} \; \text{supports} \; x
\; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{b} \subseteq \bar{x})
\\ \; \\
&
\bar{a} \; \text{supports} \; x
\; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{b} \subseteq \bar{x})
\implies
\bar{b} \subseteq \bar{a}
\\
&
\bar{b} \; \text{supports} \; x
\; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x})
\implies
\bar{a} \subseteq \bar{b}
\\ \; \\
&
\bar{a} \subseteq \bar{b} \land \bar{b} \subseteq \bar{a} \implies a = b
\end{array}
$$

</div>

From now on I'll refer to "the minimal support" as "the support".

Putting it all into code:

```rust
pub mod support {
  use super::name::Name;

  pub trait Supported: Permutable {
    /// Computes the minimal support of a value.
    fn support(&self) -> HashSet<Name>
  }

  impl Supported for Name {
    fn support(&self) -> HashSet<Name> {
      HashSet::from([*self])
    }
  }

  impl <A: Supported, B: Supported> Supported for (A, B) {
    fn support(&self) -> HashSet<Name> {
      self.0.support().union(self.1.support()).collect()
    }
  }

  impl <A: Supported, B: Supported> Supported for either::Either<A, B> {
    fn support(&self) -> HashSet<Name> {
      use either::Either;
      match self {
        Left(a) => a.support(),
        Right(b) => b.support()
      }
    }
  }
}
```

#### The Support of a Function

Functions have a permutation action, and therefore the notion of support also applies to them.

<div class="math">

$$
\begin{array}{ll}
\bar{a} \; \text{supports} \; f \iff & \forall \pi. \; (\forall a \in \bar{a}. \; \pi(a) = a)
\implies \pi \cdot f = f
\\
& \forall \pi. \; (\forall a \in \bar{a}. \; \pi(a) = a) 
\implies \forall x. \; (\pi \cdot f)(x) = f(x)
\\
& \forall \pi. \; (\forall a \in \bar{a}. \; \pi(a) = a) 
\implies \forall x. \; \pi \cdot f(\pi^{-1} \cdot x) = f(x)
\end{array}
$$

</div>

I think of the support of a function as the set of names that have been "captured" by the function.
The identity function captures no names, so it's supported by the empty set:

<div class="math">

$$
\begin{array}{l}
\forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x. \; \pi \cdot id(\pi^{-1}
\cdot x) = id(x)
\\
= \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x. \; \pi \cdot \pi^{-1} \cdot x = x
\\
= \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x. \; x = x
\\
= \forall \pi. \; \text{true} \implies \forall x. \; x = x \;\;\;\; \text{trivial}
\end{array}
$$

</div>

A function that compares its two name arguments is also supported by the empty set:

<div class="math">

$$
\text{cmp}(a, b) = a \stackrel{?}{=} b
\\ \; \\
\begin{array}{l}
\forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; \pi \cdot \text{cmp}(\pi^{-1}
\cdot (x, y)) = \text{cmp}(x, y)
\\
= \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; \pi \cdot \text{cmp}(\pi^{-1}
\cdot x, \pi^{-1} \cdot y) = \text{cmp}(x, y)
\\
= \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; \pi \cdot ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = (x \stackrel{?}{=} y)
\\
= \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = (x \stackrel{?}{=} y) \;\;\;\; (\text{booleans contain no names})
\\
= \forall \pi. \; \forall x, y. \; ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = (x \stackrel{?}{=} y)
\end{array}
\\ \; \\
\forall \pi. \; \forall x, y. \; x = y \implies ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = \text{true}
\\ \; \\
\begin{array}{l}
(\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)
\\
= (\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot x)
\\
= \text{true}
\end{array}
\\ \; \\
\forall \pi. \; \forall x, y. \; x \neq y \implies ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = \text{false}
\\ \; \\
\begin{array}{l}
(\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)
\\
= x' \stackrel{?}{=} y' \text{ where } x' \neq y' \;\;\;\; (\text{injectivity of } \pi^{-1})
\\
= \text{false}
\end{array}
$$

</div>

A function that references names other than its arguments has those names in its support. For
example, \\( a \\) and \\( b \\) must be in the support of \\( \text{iffy}(x) = \text{if } a \stackrel{?}{=} x \text{ then } b
\text{ else } x \\). 

<div class="math">

$$
\begin{array}{l}
\forall \pi. \; (\forall a \in \{ b \}. \; \pi(a) = a) \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\\
= \forall \pi. \; \pi(b) = b \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\end{array}
\\ \; \\
\begin{array}{l}
\text{can't prove for all } \pi, x \text{. counterexample: } \pi = (a \;
n), \; x = a
\\ \; \\
\begin{array}{ll}
(a \; n) \cdot \text{iffy}((a \; n) \cdot a) & = (a \; n) \cdot \text{iffy}(n) = (a \; n) \cdot n = a
\\
\text{iffy}(a) & = b
\end{array}
\end{array}
$$

</div>

<div class="math">

$$
\begin{array}{l}
\forall \pi. \; (\forall x \in \{ a \}. \; \pi(x) = x) \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\\
= \forall \pi. \; \pi(a) = a \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\end{array}
\\ \; \\
\begin{array}{l}
\text{can't prove for all } \pi, x \text{. counterexample: } \pi = (b \;
n), \; x = a
\\ \; \\
\begin{array}{ll}
(b \; n) \cdot \text{iffy}((b \; n) \cdot a) & = (b \; n) \cdot \text{iffy}(a) = (b \; n) \cdot b = n
\\
\text{iffy}(a) & = b
\end{array}
\end{array}
$$

</div>

### Freshness

TODO

### The Category of Nominal Sets

Any set \\( X \\) with a permutation action \\( \pi \\) is called a *nominal set* when for each \\(
x \in X \\), \\( x \\) has a finite, minimal support.

Nominal sets are the objects of a category (\\( \text{Nom}\\)) whose arrows are functions that preserve permutation
actions: \\( \forall \pi, x. \; f(\pi \cdot x) = \pi \cdot f(x) \\). These are called
[equivariant](https://en.wikipedia.org/wiki/Equivariant_map) functions.

The identity arrows are just the identity function on each nominal set. The identity function is equivariant:

<div class="math">

$$
\text{id}(\pi \cdot x) = \pi \cdot x = \pi \cdot \text{id}(x)
$$

</div>

And composition of arrows is the composition of equivariant functions, which itself preserves equivariance:

<div class="math">

$$
\begin{array}{lll}
& f(g(\pi \cdot x))
\\
= & f(\pi \cdot g(x)) & (\text{equivariance of } g)
\\
= & \pi \cdot f(g(x)) & (\text{equivariance of } f)
\end{array}
$$

</div>

\\( \text{Nom} \\) has [products](https://en.wikipedia.org/wiki/Product_(category_theory)), which
are pairs of nominal sets. Introduction and elimination of pairs are equivariant:

<div class="math">

$$
\begin{array}{l}
\text{fst} : X \times Y \rightarrow X
\\
\text{fst}(x, y) = x
\\ \; \\
\text{fst}(\pi \cdot (x, y)) =
\text{fst}(\pi \cdot x, \pi \cdot y) =
\pi \cdot x =
\pi \cdot \text{fst}(x, y)
\\ \; \\
\text{snd} : X \times Y \rightarrow Y
\\
\text{snd}(x, y) = y
\\ \; \\
\text{snd}(\pi \cdot (x, y)) =
\text{snd}(\pi \cdot x, \pi \cdot y) =
\pi \cdot y =
\pi \cdot \text{snd}(x, y)
\\ \; \\
\text{pair} : (Z \rightarrow X) \times (Z \rightarrow Y) \rightarrow (Z \rightarrow X \times Y)
\\
\text{pair}(f, g) = \lambda x. \; (f(x), g(x))
\\ \; \\
\text{pair}(\pi \cdot (f, g))
\\
= \text{pair}(\pi \cdot f, \pi \cdot g)
\\
= \lambda x. \; ((\pi \cdot f)(x), (\pi \cdot g)(x))
\\
= \lambda x. \; (\pi \cdot f(\pi^{-1} \cdot x), \pi \cdot g(\pi^{-1} \cdot x))
\\
= \lambda x. \; \pi \cdot (f(\pi^{-1} \cdot x), g(\pi^{-1} \cdot x))
\\
= \lambda x. \; \pi \cdot (\lambda y. \; (f(y), g(y))) (\pi^{-1} \cdot x)
\\
= \lambda x. \; \pi \cdot \text{pair}(f, g) (\pi^{-1} \cdot x)
\\
= \pi \cdot \text{pair}(f, g)
\end{array}
$$

</div>

\\( \text{Nom} \\) has [coproducts](https://en.wikipedia.org/wiki/Coproduct), which is the normal
disjoint union on sets. Introduction and elimination of coproducts are equivariant:

<div class="math">

$$
\begin{array}{l}
\text{left} : X \rightarrow X + Y
\\
\text{left}(x) = (\text{L}, x)
\\ \; \\
\text{left}(\pi \cdot x) = (\text{L}, \pi \cdot x) = \pi \cdot (\text{L}, x) = \pi \cdot \text{left}(x)
\\ \; \\
\text{right} : Y \rightarrow X + Y
\\
\text{right}(y) = (\text{R}, y)
\\ \; \\
\text{right}(\pi \cdot y) = (\text{R}, \pi \cdot y) = \pi \cdot (\text{R}, y) = \pi \cdot \text{right}(y)
\\ \; \\
\text{match} : (X \rightarrow Z) \times (Y \rightarrow Z) \rightarrow X + Y \rightarrow Z
\\
\text{match}(f, g) = (\lambda (\text{L}, x). \; f(x)) \; | \; (\lambda (\text{R}, x). \; g(x))
\\ \; \\
\text{match}(\pi \cdot (f, g))
\\
= \text{match}(\pi \cdot f, \pi \cdot g)
\\
= (\lambda (\text{L}, x). \; (\pi \cdot f)(x)) \; | \; (\lambda (\text{R}, x). \; (\pi \cdot g)(x))
\\
= (\lambda (\text{L}, x). \; \pi \cdot f(\pi^{-1} \cdot x)) \; | \; (\lambda (\text{R}, x). \; \pi
\cdot g(\pi^{-1} \cdot x))
\\
= \lambda y. \; ((\lambda (\text{L}, x). \; \pi \cdot f(x)) \; | \; (\lambda (\text{R}, x). \; \pi
\cdot g(x)))(\pi^{-1} \cdot y)
\\
= \lambda y. \; \pi \cdot ((\lambda (\text{L}, x). \; f(x)) \; | \; (\lambda (\text{R}, x). \; g(x)))(\pi^{-1} \cdot y)
\\
= \lambda y. \; \pi \cdot \text{match}(f,g)(\pi^{-1} \cdot y)
\\
= \pi \cdot \text{match}(f,g)
\end{array}
$$

</div>

### Name Binding

Name binding (written \\( [\mathbb{A}]X \\)) is the [quotient](https://en.wikipedia.org/wiki/Equivalence_class) of name-value pairs
by a sort of "generalised alpha equivalence". Elements of \\( [\mathbb{A}]X \\) are written as
\\( \langle a \rangle x \\).

<div class="math">

$$
\begin{array}{l}
[\mathbb{A}]X = (\mathbb{A} \times X) / \sim_\alpha
\\ \; \\
\sim_\alpha \; : (\mathbb{A} \times X) \times (\mathbb{A} \times X)
\\
\sim_\alpha \; = \{ \; 
((a, x), (a', x')) \; | \;
a, a' \in \mathbb{A}, \;
x, x' \in X, \;
\exists b. \; 
b \# (a, x, a', x') \land
(a \; b) \cdot x = (a' \; b) \cdot x'
\; \}
\end{array}
$$

</div>

Two name binders are considered equal when renaming their bound name to a completely
fresh name makes their bodies equal.

\\( [\mathbb{A}]X ) is the `Binder` type that I defined at the beginning. Now we have machinery to
define equality on `Binder`s:

```rust
impl <T: PartialEq> PartialEq for Binder<T> {
  fn eq(&self, other: &Self) -> bool {
    use super::name::fresh;
    
    let b = fresh();
    self.permute_by(Permutation::swap(self.name, b)) == 
        other.permute_by(Permutation::swap(other.name, b))
  }
}

impl <T: Eq> Eq for Binder<T> {}
```

We can use the facts that every binder binds a unique name to add a fast path. Two binders that have
the bind same name are actually the same binder.

```rust
impl <T: PartialEq> PartialEq for Binder<T> {
  fn eq(&self, other: &Self) -> bool {
    use super::name::fresh;
    
    if self.name == other.name {
      true
    } else {
      let b = fresh();
      self.permute_by(Permutation::swap(self.name, b)) == 
          other.permute_by(Permutation::swap(other.name, b))
    }
  }
}

impl <T: Eq> Eq for Binder<T> {}
```

Being a nominal set, name binding has a permutation action and a finite support.

Permutation action: \\( \pi \cdot \langle a \rangle x = \langle \pi \cdot a \rangle (\pi \cdot x) \\)

```rust
impl <T: Permutable> Permutable for Binder<T> {
  fn permute_by(&self, permutation: &Permutation) -> Self {
    Binder{
      name: name.permute_by(permutation),
      body: body.permute_by(permutation)
    }
  }
}
```

Support: \\( b \; \text{supports}\_{min} (\langle a \rangle x) \iff b \neq a \land b \; \text{supports}\_{min} x
\\). The support of a name binder excludes its bound name. This is due to it being a quotient. The definition of support uses equality in its consequent, \\(\pi \cdot x = x \\),
and name binders are considered equal when their bound names can be renamed
consistently, which means that we can have permutations \\( \pi \\) where \\( \pi(a) \neq a \\)
that still satisfy \\( \pi \cdot \langle a \rangle x = x \\).

```rust
impl <T: Supported> Supported for Binder<T> {
  fn support(&self) -> HashSet<Name> {
    let mut support = self.body.support();
    support.remove(&self.name);
    support
  }
}
```

### Some Adjunctions

## Showing Off

Names are drawn
from a countably infinite set (written as \\( \mathbb{A} \\) for **A**tom), and they are opaque. The only operation they support is equality
testing.

To me, most important construction is "name abstraction", written \\( [\mathbb{A}]X \\) for some nominal
set \\( X \\). A nominal-sets-inspired Rust library for variable binding would be built around a
datatype that performs name abstraction. Let's call this datatype `Binder<N, T>`.

Permutations: the theory of nominal sets is built upon [permutations](https://en.wikipedia.org/wiki/Permutation)
of names. \\( (a \; b) \\) denotes a permutation that swaps names \\( a \\) with \\( b \\), that is, it maps \\( a \\) to
\\( b \\), \\( b \\) to \\( a \\), and maps all other names to themselves. Permutations can be applied to
name-containing values, written \\( \_\cdot\_ : \Pi \rightarrow X \rightarrow X \\) / `fn
apply<N, T>(permutation: Permutation<N>, value: T) -> T`.

Equivariance: a function \\( f \\) is equivariant when it preserves the action of permutatations: \\( f(\pi
\cdot x) = \pi \cdot f(x) \\).

Support: the "support" of a value is essentially the set of free variables contained within
it. \\( support : X \rightarrow P(\mathbb{A}) \\) / `fn support<N, T>(value: T) -> HashSet<N>`.

Freshness: a name is "fresh" with respect to a value when it doesn't occur in that value's support.
In the literature it's defined as a relation \\( \{ \; a \# x \; | \; a \notin support(x) \; \} \\), and
in Rust it is a boolean-returning function:

```
fn fresh_in<N, T>(name: N, value: T) -> bool {
  !support(value).contains(&name)
}
```

\\( [\mathbb{A}] X \\) is defined as the quotient \\( (\mathbb{A} \times X) / \sim \\) over the following
equivalence relation: \\( \{ (a, x) \sim (a', x') \; | \; \exists b. \; b \# (a, x, a', x') \land (a \; b) \cdot x = (a' \; b)
\cdot x' \} \\) <a href="reference-nominal-techniques">(Pitts, 2016, Figure 8)</a>. Intuitively, name abstraction is a name paired with a value where all pairs that
are equivalent up to renaming are considered equal.

I found the article ["Quotient Types for
Programmers"](https://www.hedonisticlearning.com/posts/quotient-types-for-programmers.html)
extremely helpful in getting an intuition for quotients.

Alright, `Binder`s are fundamentally pairs:

```rust
struct Binder<N, T>{
  name: N,
  body: T
}
```

Name abstraction has an introduction form, written \\( \langle \_ \rangle \_ : \mathbb{A} \rightarrow X
\rightarrow [\mathbb{A}]X \\).

In Rust, it's as simple as

```rust
fn new<N, T>(name: N, body: T) -> Binder<N, T> { Binder{ name, body } }
```

The elimination form is where things get difficult. Let's call it `fold` in Rust, and start with the
simplest conceivable implementation: unpacking the pair.

```rust
fn fold<N, T, R>(binder: Binder<N, T>, f: impl FnOnce(N, T) -> R) -> R {
  f(binder.name, binder.value)
}
```

Because \\( [\mathbb{A}] X \\) is a quotient, its elimination comes with a side condition.

\\[ 
\frac{
\begin{array}{c}
v : [\mathbb{A}] X \\
f : \mathbb{A} \times X \rightarrow R \\
\text{wellDefined} : \forall a, x, a', x'. \;
(\exists b. \; b \# (a, x, a', x') \land (a \; b)
\cdot x = (a' \; b) \cdot x' )
\implies f(a, x) = f(a', x')
\end{array}
}{
elim_{[\mathbb{A}] X} \; v \; f \; : R
}
 \\]

Problem: `f` must be "equivalence-preserving", but `impl FnOnce(N, T) -> R` is too permissive. \\( [\mathbb{A}] X \\) can only be consumed in ways that
do not distinguish between equivalent values. One example of a function that breaks this property is
\\( \lambda (a, x). \; a \\). For some \\( (a, x) \in \mathbb{A} \times X \\) and \\( b \in \mathbb{A} \\) where
\\( b\#(a, x) \\) we have \\( (a, x) \sim (a \; b) \cdot (a, x) \\); swapping the abstracted name \\( a \\) and any
"references" to it in \\( x \\) with a yet-unseen name results in an equivalent value. But \\( elim_{[\mathbb{A}] X} \; (\lambda (a,
x). \; a) \\) can distinguish between these equivalent values:

\\[ 
\begin{array}{l}
elim_{[\mathbb{A}] X} \; (\lambda (a, x). \; a) \; (a, x)
\\
= (\lambda (a, x). \; a) (a, x)
\\
= a
\\
\;
\\
elim_{[\mathbb{A}] X} \; (\lambda (a, x). \; a) \; ((a \; b) \cdot (a, x))
\\
= elim_{[\mathbb{A}] X} \; (\lambda (a, x). \; a) \; (b, (a \; b) \cdot x)
\\
= (\lambda (a, x). \; a) \; (b, (a \; b) \cdot x)
\\
= b
\end{array}
 \\]

I think an elimination function is well-defined when it is equivariant and it doesn't leak its name argument.
I got stuck trying to prove it, but <a href="reference-nominal-techniques">(Pitts, 2013, Figures
14-15)</a> seem like evidence in favour.

<a href="reference-nominal-techniques">(Pitts, 2013, Figures
11-15)</a> defines left and right adjoints to \\( [\mathbb{A}](-) \\) which induce a nice API for
`Binders`.

\\( [\mathbb{A}](-) \\) is right adjoint to the functor \\( {}- * \; \mathbb{A} \\) arising from the following
nominal set: \\( X * \mathbb{A} = \{ \; (x, a) \; | \; x \in X, a \# x  \;\} \\).

\\[ 
\frac{
X * \mathbb{A} \rightarrow_{Nom} Y
}{
X \rightarrow_{Nom} [\mathbb{A}] Y
}
\\ \; \\
\begin{array}{l}
\text{bind} : (X * \mathbb{A} \rightarrow_{Nom} Y) \rightarrow
X \rightarrow_{Nom} [\mathbb{A}] Y
\\
\text{bind}(f)(x) = f(x, a) \; \text{for some} \; a \# x
\end{array}
\\ \; \\
\text{bind}^{-1} : (X \rightarrow_{Nom} [\mathbb{A}] Y) \rightarrow
X * \mathbb{A} \rightarrow_{Nom} Y
\\ \; \\
 \\]

The "rightward" direction of the adjunction (\\( bind \\)) describes a way to create binders.
It says that you can create a binder using a name that has never been seen before.

```rust
fn bind<N, T>(f: impl FnOnce(N) -> T) -> Binder<N, T> {
  let name = todo!("generate fresh name");
  Binder { name, body: f(name) }
}
```

\\( [\mathbb{A}](-) \\) is left adjoint to the following functor:

\\[ 
R \; Y = \{ \; f \; | \; f \in \mathbb{A} \rightarrow_{Nom} Y, \; \forall a. \; a \# f(a) \; \}
\\ \; \\
\frac{
[\mathbb{A}] X \rightarrow_{Nom} Y
}{
X \rightarrow_{Nom} R \; Y
}
\\ \; \\
\text{unbind}^{-1} : ([\mathbb{A}] X \rightarrow_{Nom} Y) \rightarrow X \rightarrow_{Nom} R \; Y
\\ \; \\
\begin{array}{l}
\text{unbind} : (X \rightarrow_{Nom} R \; Y) \rightarrow [\mathbb{A}] X \rightarrow_{Nom} Y
\\
\text{unbind}(f)(a, x) = f(a)(x)
\end{array}
 \\]

The "leftward" direction of this adjunction describes how to consume binders.
You can consume a binder using a function that doesn't "leak" its bound name. 

```rust
/*
*/
fn unbind<N, T, R>(binder: Binder<N, T>, f: impl FnOnce(N, T) -> R) -> R {
  f(binder.name, binder.body)
}
```

[^please-correct-me]: Let me know if (when?) I'm wrong about this!

[^nominal-logic]: Pitts, A. M. (2003). Nominal logic, a first order theory of names and binding.
    Information and computation, 186(2), 165-193.

    https://doi.org/10.1016/S0890-5401(03)00138-X

[^alpha-structural-recursion-and-induction]: Pitts, A. M. (2006). Alpha-structural recursion and
    induction. Journal of the ACM (JACM), 53(3), 459-506.

    https://doi.org/10.1145/1147954.1147961

[^nominal-techniques]: Pitts, A. (2016). Nominal techniques. ACM SIGLOG News, 3(1), 57-72.

  https://doi.org/10.1145/2893582.2893594