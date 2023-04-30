---
layout: post
title: A Practical Introduction to Monad Transformers
author: ielliott95
permalink: /a-practical-introduction-to-monad-transformers
tags:
    - programming
    - haskell
---

Monad transformers combine the functionality of two monads into one. They are often used
as a way to "flatten" nested monads, but are also used to enable interactions between
monads that, when used seperately, would be incredibly difficult to implement.


## The Task

You input a number and want to manipulate it while printing the
result each time. If there were no intermediate IO operations we could use the state monad 
with the following state changes:

```haskell
add :: Int -> State Int ()
add n = state $ \s -> ((),s + n)

subtract :: Int -> State Int ()
subtract n = state $ \s -> ((),s - n)
```

chain them together:

```haskell
manyOperations :: State Int ()
manyOperations = do
    add 1
    subtract 3
    add 5
    add 7
    subtract 22
```

then get the result:

```haskell
(_,result) = runState manyOperations 5 :: ((),Int)
```

Now let's consider how to print the state. If we want to preserve the above chaining syntax, we need
a monad where:

1. We can do stateful computations
2. Our IO functions have access variables inside the computation

This monad is called the state monad transformer.

## The Solution

The state monad transformer is defined as:

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
```

[^1]Meaning that given an initial state `s` and a state transformer `st`, we can call `runStateT st s` to get
a monad containing the state tuple. 

The real beauty (or magic, as some would say) of this monad comes from the bind function. Let's take a look
at its definition:

```haskell
(>>=) :: Monad n => StateT s n a -> (a -> StateT s n a) -> StateT s n a
m >>= k  = StateT $ \ s -> do
    ~(a, s') <- runStateT m s
    runStateT (k a) s'
```

Time to break it down. `m` is a state transformer. `k` is a function that takes a result of type `a`, and returns
a state transformer. The final state transformer, when run with an initial state, does the following:

1. Gets the result and state of running the computation in `m` with the initial state `s`
2. Passes the result of (1) to the function `k`, returning a different state transformer
3. Runs the computation created in (2) using the state returned in (1)
4. Wraps the result

This means that we will be able to keep using a simple chained sequence of monads.

How does this relate to the problem at hand?

The monad component of the state transformer allows us to execute IO operations which have access to the state
*during* the computation. Here is how the `add` and `subtract` functions can be written using the state transformer
monad:

```haskell
add :: Int -> StateT Int IO ()
add n = StateT $ \s -> do
    print (s+n)
    return ((),s+n)

subtract :: Int -> StateT Int IO ()
subtract n = StateT $ \s -> do
    print (s-n)
    return ((),s-n)
```

We can still chain them using the same syntax as before:

```haskell
manyOperations :: StateT Int IO ()
manyOperations = do
    add 1
    subtract 3
    add 5
    add 7
    subtract 22
```

and run it:

```haskell
main = runStateT manyOperations 5
-- output:
-- 6
-- 3
-- 8
-- 15
-- -7
```

[^1]: `newtype` creates a strict, isomorphic type with a single value constructor. If all that was too much, just imagine that `newtype` rearranges an existing type into a more pleasant one.
