---
title: A Haskell "Foldable" quiz
permalink: /haskell-foldable-quiz
date: 2025-08-21
tags:
  - haskell
---

How does GHCi respond to `fold [] 42`?

<details>
<summary>Answer</summary>
```
ghci> fold [] 42
()
```
</details>

How does GHCi respond to `fold [] 42 == []`?

<details>
<summary>Answer</summary>
```
ghci> fold [] 42 == []
True
```
</details>

What is the type of `fold [] 42`?

<details>
<summary>Answer</summary>
```
ghci> :t fold [] 42
fold [] 42 :: Monoid t => t
```
</details>

What the hell is going on!?

<details>
<summary>Answer</summary>
`fold` has type `(Foldable t, Monoid a) => t a -> a`, so `fold []` has type `Monoid a => a`.

`fold []` is `mempty`, which means `fold [] 42` is equal to `mempty 42`.
This is well-typed because *functions* have a Monoid instance.

`mempty` for functions is `const mempty`.
So `mempty 42` is `const mempty 42` is `mempty :: Monoid b => b`.

In GHCi, `b` is defaulted to `()`, which is why the first question prints `()`.

When we evaluate `fold [] 42 == []`, the list monoid is chosen instead, reducing to `[] == []`.
</details>

Why am I even thinking about this?

<details>
<summary>Answer</summary>
I just found a bug due to replacing `fromMaybe Set.empty` (which has type `Maybe (Set a) -> Set a`) with `fold Set.empty` (which has type `Monoid b => a -> b` and is equivalent to `const mempty`).
This meant that all my `Just`s containing non-empty sets were being ignored.

I meant to replace `fromMaybe Set.empty` with `fold`, which really are the same.
</details>
