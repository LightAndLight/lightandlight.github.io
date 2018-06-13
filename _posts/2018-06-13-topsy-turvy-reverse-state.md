---
layout: post
title: Turning bottom-up into top-down with Reverse State
author: ielliott95
permalink: /topsy-turvy-reverse-state/
tags:
    - programming
    - haskell
---

I love [bound](https://hackage.haskell.org/package/bound) - it makes De
Bruijn indices mindlessly easy. I also love
[Plated](https://hackage.haskell.org/package/lens/docs/Control-Lens-Plated.html)
for all sorts of whole-program transformations. I think they're two
indispensible tools for working with programming languages.
Unfortunately, they're not compatible.

The [Scope
datatype](https://hackage.haskell.org/package/bound/docs/Bound.html#t:Scope)
in `bound` is very safe. The type prevents you from creating invalid De
Bruijn terms, like `λ. 3`. This means that you can't write useful
instances of `Plated` for types which contain a `Scope`. When it comes
to choosing between `bound` and `Plated`, I choose `Plated` - because we
can use it to build functionality similar to `bound`.

Let's get some boilerplate out of the road. Here is a datatype for
lambda calculus, with De Bruijn indices (`B`), as well as free variables
(`F`). Notice that lambda abstraction (`Abs`) doesn't give a name to the
function argument, which means that only `B`s can reference them. This
is called the "locally nameless" approach.

\[Literate Haskell source\]({{ "/files/reverse-state.lhs" \|
absolute\_url }})

```haskell
{-# language DeriveGeneric #-}

import Control.Lens.Plated (Plated(..), gplate, transformM)
import GHC.Generics (Generic)

import qualified Control.Monad.RevState as Reverse
import qualified Control.Monad.State as State

data Expr
  = F String
  | B Int
  | App Expr Expr
  | Abs Expr
  deriving (Eq, Show, Generic)

instance Plated Expr where
  plate = gplate
  {-# inline plate #-}
```

The core of the `bound`-like API will be two functions:

-   `abstract :: String -> Expr -> Expr`
-   `instantiate :: Expr -> Expr -> Maybe Expr`

Let's do `abstract` first.

`abstract name expr` finds all the `F name` nodes in an `expr` and
replaces them with the appropriate De Bruijn index, then wraps the final
result in an `Abs`. The "appropriate index" is the number of `Abs`
constructors that we passed on the way.

For example, `abstract "x" (F "x")` outputs `Abs (B 0)`, because we
passed 0 `Abs` constructors to get to the `"x"`, then wrapped the final
result in an `Abs`. `abstract "y" (Abs (App (B 0) (F "y")))` outputs
`Abs (Abs (App (B 0) (B 1)))` because we passed 1 `Abs` to get to the
`"y"`, then wrapped the final result in an `Abs`.

"Do this everywhere" usually means
`[transform](https://hackage.haskell.org/package/lens/docs/Control-Lens-Plated.html#v:transform) :: Plated a => (a -> a) -> a -> a`
is appropriate. Though in this case, it doesn't give us any way to count
the number of `Abs` it passes. Instead we will use
`[transformM](https://hackage.haskell.org/package/lens/docs/Control-Lens-Plated.html#v:transformM) :: (Monad m, Plated a) => (a -> m a) -> a -> m a`
with
[State](https://hackage.haskell.org/package/mtl/docs/Control-Monad-State.html).

Here's how that looks:

```haskell
abstract_attempt_1 :: String -> Expr -> Expr
abstract_attempt_1 name = Abs . flip State.evalState 0 . transformM fun
  where
    fun :: Expr -> State.State Int Expr
    fun (F name')
      | name == name' = B <$> State.get
      | otherwise = pure $ F name'
    fun (Abs expr) = Abs expr <$ State.modify (+1)
    fun expr = pure expr
```

If you see a free variable with the name we're abstracting over, replace
it with a De Bruijn index corresponding to the number of binders we've
seen. If you see an `Abs`, increment the counter. If you see something
else, don't do anything special.

This is the right idea, but it doesn't work because the `transform`
family of functions act from the bottom up. When it sees a free variable
it can abstract over, it will replace it with `B 0`, then go upwards
through the tree, incrementing the counter. This is the *reverse* of
what we want.

Enter [Reverse
State](http://hackage.haskell.org/package/rev-state/docs/Control-Monad-Trans-RevState.html).
In reverse `State`, `get` accesses the state of the computation *after*
it, not before it. Using regular state,
`execState (modify (+1) *> modify (*2)) 0` will evaluate to `2`, because
you set the state to zero, add one, then multiply by two. Using reverse
state, the output is `2`, because you set the state to zero, multiply by
two, then add one.

This means that if we swap regular state for reverse state in
`abstract`, `get` refers to a state which is only calculated *after*
bubbling all the way to the top, and counting all the `Abs`
constructors.

So the correct code looks like this:

```haskell
abstract :: String -> Expr -> Expr
abstract name = Abs . flip Reverse.evalState 0 . transformM fun
  where
    fun :: Expr -> Reverse.State Int Expr
    fun (F name')
      | name == name' = B <$> Reverse.get
      | otherwise = pure $ F name'
    fun (Abs expr) = Abs expr <$ Reverse.modify (+1)
    fun expr = pure expr
```

The logic remains the same, except now the state transformations run
backwards.

Now for `instantiate`. `instantiate (Abs body) x` substitutes `x` into
the appropriate positions in `body`, and wraps the final result in a
`Just`. If the first argument to `instantiate` is not an `Abs`, then the
result is `Nothing`. We substitute `x` everywhere we find a `B` that
contains the number of binders we have passed.

For example, `instantiate (Abs (B 0)) (F "x")` evaluates to
`Just (F "x")`, because we found a `B 0` when we had passed zero binders
(the outer `Abs` doesn't count).
`instantiate (Abs (Abs (App (B 0) (B 1)))) (F "y")` evaluates to
`Just (Abs (App (B 0) (F "y")))`, because we found a `B 1` when we had
passed one binder. The `B 0` is not replaced because at that point, we
had passed one binder, and zero is not one.

We have the same problem as with `abstract`: counting binders proceeds
from the top down, but `transformM` works from the bottom up. We can use
reverse state again to solve this. Here's the code:

```haskell
instantiate :: Expr -> Expr -> Maybe Expr
instantiate (Abs body) x = Just $ Reverse.evalState (transformM fun body) 0
  where
    fun :: Expr -> Reverse.State Int Expr
    fun (B n) = do
      n' <- Reverse.get
      pure $
        if n == n'
        then x
        else B n
    fun (Abs expr) = Abs expr <$ Reverse.modify (+1)
    fun expr = pure expr
instantiate _ _ = Nothing
```

And there we have it: a `bound`-like API for a datatype using `Plated`.

I think there are two pressing issues when comparing this code to
`bound`: correctness and generalisation. This approach allows you to
write bogus terms, like `Abs (B 3)`, whereas `bound` does not. I'm okay
with this, because I highly value the tools `Plated` provides.
Additionally, the `bound` combinators work over any term as long as it
is a `Monad`, so `abstract` and `instantiate` only have to be written
once, whereas we haven't presented any means for generalisation of the
`Plated` approach. This is easily fixed: in a follow-up post, I'm going
to write about how we can use Backpacky Prisms to provide `abstract`
and `instantiate` as library functions.
