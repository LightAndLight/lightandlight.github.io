---
title: Nominal Sets
author: ielliott95
permalink: /nominal-sets
date: 2023-07-04T06:41:00Z
excerpt: |
  <p>Developing a <a href="https://github.com/LightAndLight/binders.rs">variable binding library</a> for Rust, based on the
  theory of nominal sets.</p>
math: true
tags:
  - programming
  - mathematics
---

<div class="intro-wrapper">
<div class="intro">

For years I've used [De
Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index), in part because
of Haskell's [`bound`](https://hackage.haskell.org/package/bound) library, which abstracts (pun
intended) De Bruijn indexed binders in a simple and type-safe way. Having learned to "think" using de Bruijn indices, I naturally used them when I wrote
[Ipso](https://github.com/lightandlight/ipso), which is written in Rust. I wished I had `bound`, so I tried to port it to
Rust. Unfortunately, `bound` relies on [polymorphic
recursion](https://en.wikipedia.org/wiki/Polymorphic_recursion), which Rust 
[doesn't really support](https://github.com/rust-lang/rust/issues/4287#issuecomment-11846582).

Writing all that variable binding machinery in Rust was tolerable, but Ipso probably isn't the last
programming language that I'll build with Rust. When it's time for me to build the next one, I'd
like to use a variable binding library instead. I think
[`moniker`](https://docs.rs/moniker/latest/moniker/) is the only[^please-correct-me] such library on
[crates.io](https://crates.io/), and I might yet use it. In the meantime, I'd
like to explore an alternative way of tackling the problem, inspired by a formalism called "nominal sets".

</div>
<div id="toc"><!-- generated --></div>
</div>

## Why did I write this?

I have two reasons for writing this article. The first is to improve my understanding of
nominal sets, and my general mathematical ability. 
Years ago I [wrote about](https://blog.ielliott.io/writing>) my sense of the importance of writing,
and that line of reasoning continues to motivate me. More recently, [Paul Graham](http://www.paulgraham.com) has written a much
more eloquent [essay](http://www.paulgraham.com/words.html) on the topic.

The second is to contribute another introduction to nominal sets. I learned about nominal sets from
primary sources: a book (via my state library) and few papers from the pioneering authors, and slides from tutorials they'd given.
In total, five or six resources from a couple of authors. When I didn't understand something I cycled
between resources, as if trying to triangulate an understanding. I think that more explanations, written by different people, would have increased the chances
of finding an explanation that clicked for me. While I cover the same introductory topics
as the primary sources, I hope that I'll do it differently enough to be valuable to someone.

## Names and binders

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
    returned, the name shouldn't be accessible outside the binder.
    */
    pub fn unbind<R>(&self, f: impl FnOnce(&Name, &T) -> R) -> R {
      f(&self.name, &self.body)
    }
  }
}
```

Names (1) are opaque and can be compared for equality.

A binder (2) is a pair of a name with some type `T`, within which the name is considered *bound*.

`bind` (3) is the only way to create binders, which is also the only time new names are introduced (4).
Since the fields of `Binder` are hidden, every binder binds a unique name.

Binder introduction (`bind`) and elimination (`unbind`) come with correctness conditions (5) (6) that
prevent bound names from "escaping their scope". Programs that follow these rules are
capture-avoiding by construction; any terms that are substituted under a binder will not contain the
name bound by that binder.

There's more to add, such as `Clone` and `Eq` implementations for `Binder`. As I explain nominal
sets I'll translate the important concepts to code, so that by the end we'll have a decent
variable binding library.

## Nominal sets

Nominal sets[^names-and-symmetry]<sup>,</sup>[^nominal-logic]<sup>,</sup>[^alpha-structural-recursion-and-induction]<sup>,</sup>[^nominal-techniques] is a theory of
names and name binding, intended to help with implementing and verifying programming languages. Its most
remarkable feature is an account of algebraic datatypes and recursion modulo [alpha
equivalence](https://stackoverflow.com/a/47762545/2884502). In practise, this gives an elegant way
to work with abstract syntax trees while being able to ignore any specific choice of names.

I like nominal sets as a formalism because they are a good example of how category theory can inform library design.

### Names

Names can be drawn from any [countably infinite](https://mathworld.wolfram.com/CountablyInfinite.html) set. In the literature, this set is written as
$\mathbb{A}$ (for **A**tom). I'll keep to this convention while explaining the math.

The only operation on names is equality comparison, which I'll write as $a \stackrel{?}{=} b$.

### Permutations

A theory that deals with alpha equivalence needs a notion of "renaming variables". Nominal sets uses
[permutations](https://en.wikipedia.org/wiki/Permutation) of names.

A permutation of names (from here on, just "a permutation") is a bijection on names. I'll write
$\Pi$ for the set of permutations, and $\pi$ for any particular permutation.
Being functions, permutations are used by *applying* them to names, written $\pi(a)$.
A permutation
$\pi$ is "finite" when a finite set of atoms $a$ satisfies $\pi(a) \neq a$.

The fundamental permutation is the swapping of two names, written $(a \; b)$. $(a \; b)$ is
the bijection mapping $a$ to $b$, $b$ to $a$, and any other name to itself.
Every finite permutation can be
decomposed into a sequence of swaps
(<a id="proof-1-link" href="nominal-sets-proofs#proof-1">A.1</a>).

In Rust I represent (finite) permutations using a `HashMap`. Applying takes keys to values, and any names not
in the `HashMap` are mapped to themselves. In other words, the `HashMap` represents a
permutation $\pi$ by storing a pair $(x, \pi(x))$ for
each $x$ where $\pi(x) \neq x$.

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
($\iota$) is the identity function. Multiplication ($\circ$) is function composition. Every
permutation $\pi$ has an inverse $\pi^{-1}$ (because they're bijections).

In the code I call the multiplication function `after` so it's easier to remember the order of the
permutations. 

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

`after`, acting on `HashMap`s under the hood, needs a more clever definition than I
first expected. The final permutation $\pi_f \circ \pi_g$ is constructed in two parts. The first
part (1) computes $\pi_f(\pi_g(x))$ for all $x$ where $\pi_g(x) \neq x$. The second part (2)
computes $\pi_f(x)$ for all $x$ where $\pi_g(x) = x$. For these values, $\pi_f(\pi_g(x)) =
\pi_f(x)$. The first time I wrote this function, I mistakenly thought the first part would be enough.

Names are aren't the only thing that can be affected by a permutation. "Applying" a permutation
generalises to other sets as the [action](https://mathworld.wolfram.com/GroupAction.html) of
permutations on those sets. Permutations can act on a set $X$ when there exists a function
$\alpha_X \; : \; \Pi \times X \rightarrow X$
satisfying the following properties:

1. $\forall x. \; \alpha_X(\iota, x) = x$ (identity)
1. $\forall x. \; \alpha_X(\pi_1 \circ \pi_2, x) = \alpha_X(\pi_1, \alpha_X(\pi_2, x))$ (composition)

Instead of writing $\alpha_X(\pi, x)$ for a specific permutation action, I'll use the notation $\pi \cdot x$.

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

Permutations trivially act on names: $\pi \cdot a = \pi(a)$.

```rust
  impl Permutable for Name {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      permutation.apply(self)
    }
  }

```

Permutations also trivially act on themselves: $\pi_f \cdot \pi_g = \pi_f \circ \pi_g$.

```rust
  impl Permutable for Permutation {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      permutation.after(self)
    }
  }

```

Permutations act on pairs element-wise: $\pi \cdot (x, y) = (\pi \cdot x, \pi \cdot y)$.

```rust
  impl <A: Permutable, B: Permutable> Permutable for (A, B) {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      (self.0.permute_by(permutation), self.1.permute_by(permutation))
    }
  }

```

And similarly for sums: $\pi \cdot \text{in}_L(x) =
\text{in}_L(\pi \cdot x) \; \land \; \pi \cdot \text{in}_R(y) =
\text{in}_R(\pi \cdot y)$.

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
```

On a more Rust-specific note, heap allocation is `Permutable` because it's essentially a
single-element product:

```rust
  impl <A> Permutable for Box<A> {
    fn permute_by(&self, permutation: &Permutation) -> Self {
      let inner = self.as_ref().permute_by(permutation);
      Box::new(inner)
    }
  }
} // mod permutation
```

#### Permutations on functions

Permutations can also act on functions: $(\pi \cdot f)(x) = \pi \cdot f(\pi^{-1} \cdot x)$. For my purposes this is only important in theory, so I won't
implement it in Rust. This definition is derived from the requirement that permutations distribute over function application: $\pi
\cdot f(x) = (\pi \cdot f)(\pi \cdot x)$.

### Support

A set of names *supports* a value when the value "depends" on those names. Here's the formal definition:

$$
\begin{array}{l}
\text{supports} \; : \; \mathcal{P}(\mathbb{A}) \times X
\\
\text{supports} =
\{ \;
(\bar{a}, x) \; | \; 
\bar{a} \in \mathcal{P}(\mathbb{A}), \;
x \in X, \;
\forall \pi. \; (\forall a \in \bar{a}. \; \pi(a) = a) \implies
\pi \cdot x = x 
\; \}
\end{array}
$$

In English: $\bar{a} \; \text{supports} \; x$ when all permutations that keep the elements of $\bar{a}$ the same
($\forall a \in \bar{a}. \; \pi(a) = a)$ also keep $x$
the same ($\pi \cdot x = x$).

For example, every name must support itself: $\{a\} \; \text{supports} \; a$
(<a id="proof-2-link" href="nominal-sets-proofs#proof-2">A.2</a>). 
More importantly, $\exists b. \; b \neq a \; \land \; \{b\} \; \text{supports} \; a$ is false
(<a id="proof-3-link" href="nominal-sets-proofs#proof-3">A.3</a>).

Pairs are supported element-wise:

$$
\bar{a}  \; \text{supports} \; (x, y) \iff \bar{a} \; \text{supports} \; x
\land \bar{a} \; \text{supports} \; y
$$

And sums variant-wise:

$$
\begin{array}{l}
\bar{a} \; \text{supports} \; \text{in}_L(x) \iff \bar{a} \; \text{supports} \; x
\\
\bar{a} \;
\text{supports} \; \text{in}_R(y) \iff \bar{a} \; \text{supports} \; y
\end{array}
$$

Functions have a permutation action, and therefore the notion of support also applies to them.

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

#### Minimal support

The definition of $\text{supports}$ is a bit "loose", because it allows names that don't occur in a value
to support said value. For example, for names $a$ and $b$, $\{a,b\} \; \text{supports} \; a$
(<a id="proof-4-link" href="nominal-sets-proofs#proof-4">A.4</a>).

The notion of *minimal* support tightens this up. The minimal support of a value consists of only
the names the value *actually* depends on. $\bar{a}$ is the minimal support of $x$ when it is a subset of all other sets that support $x$:

$$
\begin{array}{l}
\text{supports}_{min} \; : \; \mathcal{P}(\mathbb{A}) \times X
\\
\text{supports}_{min} =
\{ \;
(\bar{a}, x) \; | \; \bar{a} \in \mathcal{P}(\mathbb{A}), \; x \in X, \;
\bar{a} \; \text{supports} \; x, \forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x} 
\; \}
\end{array}
$$

I can say *the* minimal support, because it's unique for every value
(<a id="proof-5-link" href="nominal-sets-proofs#proof-5">A.5</a>). From now on I'll just refer to "the minimal support" as "the support", and use a "minimal
support" function instead of the $\text{supports}_{min}$ relation:

$$
\begin{array}{l}
\text{support} \; : \; X \rightarrow \mathcal{P}(\mathbb{A}) \text{ such that } \forall x. \; \text{support}(x) \; \text{supports}_{min} \; x
\end{array}
$$

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

  impl <A> Supported for Box<A> {
    support(&self) -> HashSet<Name> {
      self.as_ref().support()
    }
  }
}
```

#### The support of a function

I think of the support of a function as the set of names that have been "captured" by the function.
The identity function returns its argument and does nothing else, so it's supported by the empty set
(<a id="proof-6-link" href="nominal-sets-proofs#proof-6">A.6</a>).
A function that compares its two name arguments and nothing else 
(like $\text{cmp}(a, b) = a \stackrel{?}{=} b$)
is also supported by the empty set
(<a id="proof-7-link" href="nominal-sets-proofs#proof-7">A.7</a>).
A function that references names other than its arguments has those names in its support. For
example, $a$ and $b$ must be in the support of $\text{iffy}(x) = \text{if } a \stackrel{?}{=} x \text{ then } b
\text{ else } x$
(<a id="proof-8-link" href="nominal-sets-proofs#proof-8">A.8</a>). 

### Freshness

A name $a$ is fresh for a value $x$ (written $a \; \# \; x$) when $a \notin \text{support}_{min}(x)$.

```rust
pub mod support {
  ...

  pub fn fresh_for<T: Supported>(name: &Name, value: &T) -> bool {
    !value.support().contains(name)
  }
}
```

Since atoms are drawn from a countably infinite set, we take as an axiom that for any set of atoms there exists an atom that is fresh for them:
<span id="axiom-choose-a-fresh-name">
$\forall \bar{x} \in \mathcal{P}(\mathbb{A}). \; \exists a. \; a \; \# \; \bar{x}$
</span>.
This is known as the "choose-a-fresh-name" principle, and it's what motivates the global number generator I use for
`Name`s.

Some useful properties involving freshness:

* Swapping fresh names does nothing: $a \; \# \; x \land b \; \# \; x \implies (a \; b) \cdot x = x$
  (<a id="proof-9-link" href="nominal-sets-proofs#proof-9">A.9</a>).

* Freshness "distributes" across functions: $a \; \# \; f \land a \; \# \; x \implies a \; \# \; f(x)$
  (<a id="proof-10-link" href="nominal-sets-proofs#proof-10">A.10</a>).

### Name binding

Name binding (written $[\mathbb{A}]X$) is the [quotient](https://en.wikipedia.org/wiki/Equivalence_class)[^quotient] of name-value pairs
by a sort of "generalised alpha equivalence". Elements of $[\mathbb{A}]X$ are written as
$\langle a \rangle x$.

$$
\begin{array}{l}
[\mathbb{A}]X = (\mathbb{A} \times X) / \sim_\alpha
\\ \; \\
\sim_\alpha \; : \; (\mathbb{A} \times X) \times (\mathbb{A} \times X)
\\
\sim_\alpha \; = \{ \; 
((a, x), (a', x')) \; | \;
a, a' \in \mathbb{A}, \;
x, x' \in X, \;
\exists b. \; 
b \; \# \; (a, x, a', x')
\; \land \;
(a \; b) \cdot x = (a' \; b) \cdot x'
\; \}
\end{array}
$$

Two name binders are considered equal when renaming their bound name to a completely
fresh name makes their bodies equal.

$[\mathbb{A}]X$  is the `Binder` type that I defined at the beginning. Now we have tools to
define equality on `Binder`s:

```rust
impl <T: PartialEq> PartialEq for Binder<T> {
  fn eq(&self, other: &Self) -> bool {
    use super::name::fresh;
    
    let b = fresh();
    self.body.permute_by(Permutation::swap(self.name, b)) == 
        other.body.permute_by(Permutation::swap(other.name, b))
  }
}

impl <T: Eq> Eq for Binder<T> {}
```

Since every binder binds a unique name and binders are immutable, there is a fast path for equality: two binders that
bind the same name are actually the same binder.

```rust
impl <T: PartialEq> PartialEq for Binder<T> {
  fn eq(&self, other: &Self) -> bool {
    use super::name::fresh;
    
    if self.name == other.name {
      true
    } else {
      let b = fresh();
      self.body.permute_by(Permutation::swap(self.name, b)) == 
          other.body.permute_by(Permutation::swap(other.name, b))
    }
  }
}

impl <T: Eq> Eq for Binder<T> {}
```

Name binding has a permutation action and a finite support.

Permutation action: $\pi \cdot \langle a \rangle x = \langle \pi \cdot a \rangle (\pi \cdot x)$

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

The support of a name binder excludes its bound name: $\text{support} (\langle a \rangle x) \; = \text{support}(x) - \{ a \}$ (<a id="proof-11-link" href="nominal-sets-proofs#proof-11">A.11</a>).
Freshness is the negation of this: $b \; \# \; \langle a \rangle x \; \iff \; b = a \; \lor \; b \; \# \; x$ (<a id="proof-12-link" href="nominal-sets-proofs#proof-12">A.12</a>).

```rust
impl <T: Supported> Supported for Binder<T> {
  fn support(&self) -> HashSet<Name> {
    let mut support = self.body.support();
    support.remove(&self.name);
    support
  }
}
```

We can now define `Clone` for binders. It respects the property that every `Binder` binds a unique name.

```rust
impl Clone for Binder<T> {
  fn clone(&self) -> Self {
    self.unbind(|name, body|
      Binder::new(|new_name|
        body.permute_by(Permutation::swap(name, new_name))
      )
    )
  }
}
```

When reasoning about binder equality, it's often inconvenient to find an atom $b \; \# \; (a, x, a', x')$
such that $(a \; b) \cdot x = (a' \; b) \cdot x'$. When that's the case, we prove an
equivalent property: $\forall b. \; b \; \# \; (a, x, a', x') \implies (a \; b) \cdot x = (a' \; b) \cdot x'$
(<a id="proof-13-link" href="nominal-sets-proofs#proof-13">A.13</a>).
Any specific fresh atom is interchangeable with all fresh atoms that satisfy the same conditions.

### The category of nominal sets

Any set $X$ with a permutation action $\pi$ is called a *nominal set* when for each
$x \in X$, $x$ has a finite, minimal support.

Nominal sets are the objects of a category ($\text{Nom}$) whose arrows are functions that preserve permutation
actions: $\forall \pi, x. \; f(\pi \cdot x) = \pi \cdot f(x)$. These are called
[equivariant](https://en.wikipedia.org/wiki/Equivariant_map) functions. One important fact about equivariant functions
is that they're supported by the empty set (<a id="proof-14-link" href="nominal-sets-proofs#proof-14">A.14</a>).

The identity arrows are just the identity function on each nominal set. The identity function is
equivariant (<a id="proof-15-link" href="nominal-sets-proofs#proof-15">A.15</a>). Composition of arrows is the composition
of equivariant functions, which preserves equivariance
(<a id="proof-16-link" href="nominal-sets-proofs#proof-16">A.16</a>). I'll use $\rightarrow_{Nom}$ for $\text{Nom}$ arrows,
e.g. $X \rightarrow_{Nom} Y$.

$\text{Nom}$ has a [terminal object](https://en.wikipedia.org/wiki/Terminal_object), which is the
singleton set (<a id="proof-17-link" href="nominal-sets-proofs#proof-17">A.17</a>).

$\text{Nom}$ has [products](https://en.wikipedia.org/wiki/Product_(category_theory)), which
are pairs of nominal sets with an element-wise permutation action, because introduction and elimination of pairs is equivariant
(<a id="proof-18-link" href="nominal-sets-proofs#proof-18">A.18</a>).

$\text{Nom}$ has [coproducts](https://en.wikipedia.org/wiki/Coproduct), which is the normal
disjoint union on sets with an element-wise permutation action, because introduction and elimination of coproducts is equivariant
(<a id="proof-19-link" href="nominal-sets-proofs#proof-19">A.19</a>).

$\text{Nom}$ has [exponentials](https://en.wikipedia.org/wiki/Exponential_object), in the form of
finitely supported functions between nominal sets (<a id="proof-20-link" href="nominal-sets-proofs#proof-20">A.20</a>).

These facts have two important consequences for programmers:

1. $\text{Nom}$ is a [cartesian closed category](https://en.wikipedia.org/wiki/Cartesian_closed_category), which means it contains the lambda calculus. You can create a "nominal programming language" that has first class names[^a-simple-nominal-type-theory].

2. $\text{Nom}$ can express [initial algebra semantics](https://en.wikipedia.org/wiki/Initial_algebra), which means your "nominal programming language" can have "nominal algebraic datatypes".

In a sense $\text{Nom}$ is fundamentally compatible with programming, and I think that's why nominal sets are such a good inspiration for a library.

### Some adjunctions

$[\mathbb{A}]({-})$ is an endofunctor on $\text{Nom}$ with the following action on
$\text{Nom}$-arrows:

$$
\begin{array}{l}
[\mathbb{A}](f) \; : \; [\mathbb{A}] X \rightarrow_{Nom} [\mathbb{A}] Y
\\
[\mathbb{A}](f)(\langle a \rangle x) = \langle a \rangle f(x)
\end{array}
$$

This means `Binder` has a `map` method:

```rust
pub mod binder {
  impl <T> Binder<T> {
    ...

    /** Correctness condition: `f` should not capture any [`Name`]s. When this is the case, we have `binder.map(f).map(g) == binder.map(|x| g(f(x)))`.
    */
    pub fn map<B>(self, f: impl FnOnce(T) -> B) -> Binder<B> { // (7)
      Binder{ name: self.name, body: f(self.body) }
    }
  }
}
```

$[\mathbb{A}]({-})$ has left and right [adjoints](https://en.wikipedia.org/wiki/Adjoint_functors) that induce a nice API for working with `Binder`s.

$[\mathbb{A}]({-})$ is right adjoint to the functor ${}- * \; \mathbb{A}$ arising from the following
nominal set: $X * \mathbb{A} = \{ \; (x, a) \; | \; x \in X, a \; \# \; x  \;\}$ (<a id
="proof-22-link" href="nominal-sets-proofs#proof-22">A.22</a>).

$$
\frac{
X * \mathbb{A} \rightarrow_{Nom} Y
}{
X \rightarrow_{Nom} [\mathbb{A}] Y
}
$$

<br>

$$
\begin{array}{l}
\text{bind} \; : \; (X * \mathbb{A} \rightarrow_{Nom} Y) \rightarrow
X \rightarrow_{Nom} [\mathbb{A}] Y
\\
\text{bind}(f)(x) = \langle a \rangle f(x, a) \;\;\; \text{for some} \; a \# x
\\ \; \\
\text{bind}^{-1} \; : \; (X \rightarrow_{Nom} [\mathbb{A}] Y) \rightarrow
X * \mathbb{A} \rightarrow_{Nom} Y
\\
\text{bind}^{-1}(f)(x) = \mathit{omitted}
\end{array}
$$

The "rightward" direction of the adjunction ($\text{bind}$) describes a way to create binders. It says that you can create a binder using a name that has never been seen before.  This corresponds to the `bind` function from [Names and Binders](#names-and-binders).

$[\mathbb{A}]({-})$ is left adjoint to this functor:
$R(Y) = \{ \; f \; | \; f \in Y^{\mathbb{A}}, \; \forall a. \; a \; \# \; f(a) \;
\}$ (<a id
="proof-23-link" href="nominal-sets-proofs#proof-23">A.23</a>).

$$
\frac{
[\mathbb{A}] X \rightarrow_{Nom} Y
}{
X \rightarrow_{Nom} R(Y)
}
$$

<br>

$$
\begin{array}{l}
\text{unbind}^{-1} \; : \; ([\mathbb{A}] X \rightarrow_{Nom} Y) \rightarrow X \rightarrow_{Nom} R(Y)
\\
\text{unbind}^{-1} = \mathit{omitted}
\\ \; \\
\text{unbind} \; : \; (X \rightarrow_{Nom} R(Y)) \rightarrow [\mathbb{A}] X \rightarrow_{Nom} Y
\\
\text{unbind}(f)(\langle a \rangle x) = f(a)(x)
\end{array}
$$

The "leftward" direction of this adjunction ($\text{unbind}$) describes how to consume binders.
You can consume a binder, accessing both its body and bound name, using a function that doesn't
"leak" the name. This corresponds to `unbind` in [Names and Binders](#names-and-binders).

## Showing off

Having gone through the theoretical justifications for the design of the `Binder` type, let's examine
some of its benefits in practise.

### Alpha equivalence

Given implementations of `Permutable` and `Supported`, an abstract syntax tree can derive an `Eq`
instance that implements alpha equivalence:

```rust
#[deriving(PartialEq, Eq, Clone)]
enum Expr {
  Var(Name),
  Lam(Binder<Box<Expr>>),
  App(Box<Expr>, Box<Expr>)
}

impl Permutable for Expr {
  fn permute_by(&self, permutation: &Permutation) -> Self {
    match self {
      Expr::Var(name) => Expr::Var(name.permute_by(permutation)),
      Expr::Lam(binder) => Expr::Lam(binder.permute_by(permutation)),
      Expr::App(left, right) => Expr::App(
        left.permute_by(permutation),
        right.permute_by(permutation)
      )
    }
  }
}

impl Supported for Expr {
  fn support(&self) -> HashSet<Name> {
    match self {
      Expr::Var(name) => HashSet::from([name]),
      Expr::Lam(binder) => binder.support(),
      Expr::App(left, right) => {
        let mut support = left.support();
        support.extend(right.support());
        support
      }
    }
  }
}
```

Which means the following are true:

```rust
// (\x -> x) =_{alpha} (\y -> y)
assert_eq!(
  Expr::Lam(Binder::bind(|x| Box::new(Expr::Var(x)))),
  Expr::Lam(Binder::bind(|y| Box::new(Expr::Var(y))))
)

// (\x y -> x) =_{alpha} (\y x -> y)
assert_eq!(
  Expr::Lam(Binder::bind(|x|
    Box::new(Expr::Lam(Binder::Bind(|y|
      Box::new(Expr::Var(x))
    ))))
  ),
  Expr::Lam(Binder::bind(|y|
    Box::new(Expr::Lam(Binder::Bind(|x|
      Box::new(Expr::Var(y))
    ))))
  ),
)

// (\x y -> x) !=_{alpha} (\x y -> y)
assert_neq!(
  Expr::Lam(Binder::bind(|x|
    Box::new(Expr::Lam(Binder::Bind(|y|
      Box::new(Expr::Var(x))
    ))))
  ),
  Expr::Lam(Binder::bind(|x|
    Box::new(Expr::Lam(Binder::Bind(|y|
      Box::new(Expr::Var(y))
    ))))
  ),
)
```

### Capture-avoiding substitution

Substituting a value for a name is defined by the `Subst` trait:

```rust
pub mod subst {
  pub trait Subst<V>: Permutable {
    fn subst(&self, name: &Name, value: &V) -> Self
  }

```

It has all the usual implementations:

```rust
  impl <A: Subst<V>, B: Subst<V>> Subst<V> for (A, B) {
    fn subst(&self, name: &Name, value: &V) -> Self {
      (self.0.subst(name, value), self.1.subst(name, value))
    }
  } 
  
  impl <A: Subst<V>, B: Subst<V>> Subst<V> for either::Either<A, B> {
    fn subst(&self, name: &Name, value: &V) -> Self {
      use either::Either;
      match self {
        Left(a) => Left(a.subst(name, value)),
        Right(b) => Right(b.subst(name, value))
      }
    }
  } 

  impl <T: Subst<V>> Subst<V> for Box<T> {
    fn subst(&self, name: &Name, value: &V) -> Self {
      Box::new(self.as_ref().subst(name, value))
    }
  }

```

And the `Binder` implementation clones the binder before substituting into the body, which
guarantees capture-avoidance by binding a name that hasn't occurred in `name` or `value`.

```rust
  use super::binder::Binder;
  impl <T: Subst<V>> Subst<V> for Binder<T> {
    fn subst(&self, name: &Name, value: &V) -> Self {
      self.clone().map(|body| body.subst(name, value))
    }
  }
} // mod subst
```

Now capture-avoiding substitution can be defined for `Expr`:

```rust
impl Subst<Expr> for Expr {
  fn subst(&self, name: &Name, value: &Expr) -> Self {
    match self {
      Expr::Var(var) => if var == name { 
        value.clone()
      } else { 
        Expr::Var(var) 
      },
      Expr::Lam(body) => Expr::Lam(body.subst(name, value)),
      Expr::App(left, right) => Expr::App(
        left.subst(name, value),
        right.subst(name, value)
      )
  }
}
```

## Final thoughts

While the library I've sketched so far is *okay*, it's not something I'd publish. Here are some outstanding issues, concerns, and questions:

* I don't like the lack of support for mutability. Functions like `permute_by` and
  `subst` end up rebuilding the value they're acting on. This is a waste of time when I have exclusive
  access to the value; I should be able to mutate the value in place and skip "reconstructing" the result.

* The implementation of `PartialEq` for `Binder` is wasteful:

  ```rust
  fn eq(&self, other: &Self) -> bool {
    use super::name::fresh;
  
    let b = fresh();
    self.permute_by(Permutation::swap(self.name, b)) == 
        other.permute_by(Permutation::swap(other.name, b))
  }
  ```

  The function (immutably) permutes each argument, which amounts to cloning them. I should be able to
  compare binders without cloning! What's more, the structure is essentially walked from root to tip
  once for each binder it contains. Comparison should be done a *single* downward pass.

* `Permutable`, `Supported`, and `Subst` for user-defined types are boilerplate. They should be automatically derived, or based on a single user-defined function that locates names in the user's datatypes.

* Should `Eq` actually implement alpha equivalence, or should I have a separate trait? I'm not sure how to implement `Eq` efficiently given its signature, and my intuition suggests `Eq` should be strict structural equality rather than including any quotienting.

* Should the user be able to choose different `fresh` functions? This doesn't really matter if `Eq` implements alpha equivalence, but if `Eq` is structural equivalence then it might be more convenient to use a different "name generator" for testing.

You can follow my explorations at <https://github.com/LightAndLight/binders.rs>.

## Appendix A: Proofs

See [Nominal Sets: Appendix A (Proofs)](./nominal-sets-proofs).

[^please-correct-me]: Let me know if (when?) I'm wrong about this!

[^names-and-symmetry]: Pitts, A. M. (2013). Nominal sets: Names and symmetry in computer science.
    Cambridge University Press.

[^nominal-logic]: Pitts, A. M. (2003). Nominal logic, a first order theory of names and binding.
    Information and computation, 186(2), 165-193.

    <https://doi.org/10.1016/S0890-5401(03)00138-X>

[^alpha-structural-recursion-and-induction]: Pitts, A. M. (2006). Alpha-structural recursion and
    induction. Journal of the ACM (JACM), 53(3), 459-506.

    <https://doi.org/10.1145/1147954.1147961>

[^nominal-techniques]: Pitts, A. (2016). Nominal techniques. ACM SIGLOG News, 3(1), 57-72.

    <https://doi.org/10.1145/2893582.2893594>

[^quotient]: I found ["Quotient Types for
Programmers"](https://www.hedonisticlearning.com/posts/quotient-types-for-programmers.html) very
    helpful for understanding quotients.

[^a-simple-nominal-type-theory]: Cheney, J. (2009). A simple nominal type theory. Electronic Notes
    in Theoretical Computer Science, 228, 37-52.

    <https://doi.org/10.1016/j.entcs.2008.12.115>