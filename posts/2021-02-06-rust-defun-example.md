---
title: An Example of Defunctionalisation in Rust
permalink: /rust-defun-example
date: 2021-02-06 14:00:00 +1000
tags:
    - programming
---

I'm an experienced Haskell programmer, and I've been writing a lot of Rust lately.
I recently ran into a little trouble when porting a simple function from Haskell to
Rust. This article is a short description of my journey.

---

[De Bruijn indexed](https://en.wikipedia.org/wiki/De_Bruijn_index) terms are 
[functorial](https://en.wikipedia.org/wiki/Functor) in their free variables. 
This means that given a datatype `Expr`, we can write a function 
`map_freevars : (Int -> Int) -> Expr -> Expr` such that `map_freevars id == id` 
and `map_freevars f ∘ map_freevars g == map_freevars (f ∘ g)`. In Haskell, I'd
implement this as follows:

```haskell
data Expr
  = Var Int
  | App Expr Expr
  | Lam Expr
  
map_freevars :: (Int -> Int) -> Expr -> Expr
map_freevars f e =
  case e of
    Var n -> Var (f n)
    App a b -> App (map_freevars f a) (map_freevars f b)
    Lam b -> Lam (map_freevars (\n -> if n == 0 then 0 else 1 + f (n - 1)) b)
```

Now, here's a direct translation from Haskell to Rust:

```rust
enum Expr {
    Var(usize),
    App(Box<Expr>, Box<Expr>),
    Lam(Box<Expr>),
}

fn map_freevars<F: Fn(usize) -> usize>(f: F, e: &Expr) -> Expr {
    match e {
        Expr::Var(n) => Expr::Var(f(*n)),
        Expr::App(a, b) => Expr::App(Box::new(map_freevars(f, a)), Box::new(map_freevars(f, b))),
        Expr::Lam(b) => Expr::Lam(Box::new(map_freevars(
            |n| {
                if n == 0 {
                    0
                } else {
                    1 + f(n - 1)
                }
            },
            b
        ))),
    }
}
```

This doesn't typecheck because the call to `map_freevars(f, a)` takes ownership of `f`,
which means `f` can no longer be used in the call to `map_freevars(f, b)`.

To avoid this, `map_freevars` should *borrow* the mapping function:

```rust
fn map_freevars<F: Fn(usize) -> usize>(f: &F, e: &Expr) -> Expr {
    match e {
        Expr::Var(n) => Expr::Var(f(*n)),
        Expr::App(a, b) => Expr::App(Box::new(map_freevars(f, a)), Box::new(map_freevars(f, b))),
        Expr::Lam(b) => Expr::Lam(Box::new(map_freevars(
            &|n| {
                if n == 0 {
                    0
                } else {
                    1 + f(n - 1)
                }
            },
            b
        ))),
    }
}
```

But this doesn't compile either! The Rust compiler reports 
that it `reached the recursion limit while instantiating map_freevars::<[closure@...]>`.
Rust generates all its closures at compile time, and this code causes the compiler to
generate a countably infinite number of closures.

For every known closure that is passed to `map_freevars` as `f`, Rust generates another
closure for `|n| if n == 0 { 0 } else { 1 + f(n - 1) } }`. But `|n| if n == 0 { 0 } else { 1 + f(n - 1) } }`
is also passed to `map_freevars`, so another closure needs to be generated. And *that* closure is
also passed to `map_freevars`, so another closure needs to be generated. And so on.

The next natural step is to use a [trait object](https://doc.rust-lang.org/book/ch17-02-trait-objects.html).

```rust
fn map_freevars(f: &dyn Fn(usize) -> usize, e: &Expr) -> Expr {
    match e {
        Expr::Var(n) => Expr::Var(f(*n)),
        Expr::App(a, b) => Expr::App(Box::new(map_freevars(f, a)), Box::new(map_freevars(f, b))),
        Expr::Lam(b) => Expr::Lam(Box::new(map_freevars(
            &|n| {
                if n == 0 {
                    0
                } else {
                    1 + f(n - 1)
                }
            },
            b
        ))),
    }
}
```

A `&dyn` reference is a pair of pointers; one pointer to a value of a type that implements the trait,
and another pointer to the implementation of the trait for that type[^1].

This code is perfectly usable, and I'd guess it's the 'idiomatic' Rust solution. But there's one final
step I'd like to take, mostly for educational perposes, and for a small efficiency gain.

For all intents and purposes, there are only two possible 'origins' for `f`: 

1. It was passed to `map_freevars` unchanged, either from a top-level call or from a recursive call at an
   `App` node
2. It was wrapped in a closure before being passed to `map_freevars` at a `Lam` node

This structure is described by the following datatype:

```rust
enum Origin<'a, F> {
    Unchanged(F),
    LamNode(&'a Origin<'a, F>)
}
```

The `Origin` datatype can be interpreted as a function from `usize` to `usize`: 

```rust
impl <'a, F: Fn(usize) -> usize> Origin<'a, F> {
    fn apply(&self, n: usize) -> usize {
        match self {
            Origin::Unchanged(f) => f(n),
            Origin::LamNode(f) => if n == 0 { 0 } else { 1 + f.apply(n-1) }
        }
    }
}
```

*Challenge: implement `Origin::apply` using constant stack space.*

Now the `Origin::LamNode` constructor replaces the fresh closure in the `Lam` branch:

```rust
fn map_freevars<'a, F: Fn(usize) -> usize>(f: &'a Origin<'a, F>, e: &Expr) -> Expr {
    match e {
        Expr::Var(n) => Expr::Var(f.apply(*n)),
        Expr::App(a, b) => Expr::App(Box::new(map_freevars(f, a)), Box::new(map_freevars(f, b))),
        Expr::Lam(b) => Expr::Lam(Box::new(map_freevars(&Origin::LamNode(f)))),
    }
}
```

This transformation is an example of [defunctionalisation](https://en.wikipedia.org/wiki/Defunctionalization).

Here, the practical benefit is that `&Origin` is half the size of a `&dyn Fn(usize) -> usize` (a single pointer 
instead of two), so recursing over a `Lam` node uses less stack space.

The interface to `map_freevars` can then be cleaned up using the worker/wrapper pattern:

```rust
fn map_freevars<F: Fn(usize) -> usize>(f: F, e: &Expr) -> Expr {
    fn go<'a, F: Fn(usize) -> usize>(f: &'a Origin<'a, F>, e: &Expr) -> Expr {
        match e {
            Expr::Var(n) => Expr::Var(f.apply(*n)),
            Expr::App(a, b) => Expr::App(Box::new(go(f, a)), Box::new(go(f, b))),
            Expr::Lam(b) => Expr::Lam(Box::new(go(&Origin::LamNode(f)))),
        }
    }
    
    go(&Origin::Unchanged(f), e)
}
```

---

I haven't benchmarked the defunctionalised approach and compared it against the trait object 
implementation. If anyone has suggestions for easily measuring the time and memory usage of
Rust programs, preferably by function, then please let me know.

[^1]: This is a form of existential quantification. The Haskell equivalent looks something like
      `data Some (c :: Constraint) where; Some :: c a => a -> Some c`
