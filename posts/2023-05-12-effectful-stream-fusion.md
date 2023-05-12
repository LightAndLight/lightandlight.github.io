---
title: Effectful Stream Fusion
permalink: /effectful-stream-fusion
tags:
- programming
---

I recently learned about [`streamly`](https://hackage.haskell.org/package/streamly), yet another
Haskell streaming library. I was surprised to find that it performs significantly better[^benches] than
[`streaming`](https://hackage.haskell.org/package/streaming), my current favourite streaming
library. I wanted to understand why, and whether I could learn anything to improve `streaming`'s
performance.

## Why not just switch to `streamly`?

I like `streaming` because I think it has an elegant way of partitioning streams, like in
[`splitAt`](https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming-Prelude.html#v:splitAt)
and
[`split`](https://hackage.haskell.org/package/streaming-0.2.3.1/docs/Streaming-Prelude.html#v:split).

I don't like that `streamly` has 3 core types to work with:
[`Unfold`s](https://hackage.haskell.org/package/streamly-core-0.1.0/docs/Streamly-Data-Unfold.html#t:Unfold),
[`Stream`s](https://hackage.haskell.org/package/streamly-core-0.1.0/docs/Streamly-Data-Stream.html#t:Stream),
and
[`Fold`s](https://hackage.haskell.org/package/streamly-core-0.1.0/docs/Streamly-Data-Fold.html#t:Fold).
I prefer an interface build around a single `Stream` type that is constructed by unfolds and
consumed by folds.

The design of the [`streamly`](https://hackage.haskell.org/package/streamly)  and
[`streamly-core`](https://hackage.haskell.org/package/streamly-core) packages go against my
aesthetics. They have dozens of modules each, most of which are prefixed with `Streamly.Internal`.

If I search `streamly-core` for the `map` function, I get 7 relevant results:

* 2 results for `map` on `Unfold`s
* 2 for `map` on `Stream`s
* 2 for `map` on `StreamK`s
* 1 for `map` on `Pipe`s

That's too much noise for my liking.

These are my preferences. I hope this doesn't come across as "`streamly` is bad".

## Why is `streamly` faster than `streaming`?

In short, a chain of `streamly` combinators compile down to a single efficient loop.

For those who are interested, here's a comparison of the sort of code generated for each library. If
you don't want to read through GHC core, feel free to skip this.

Small example:

1. Generate the numbers 1 to 10
2. Keep only the odd numbers
3. Print each remaining number and then multiply it by 2
4. Calculate the sum of the stream and print it

### `streamly`

Haskell:

```haskell
{-# NOINLINE streamlyTest #-}
streamlyTest :: IO Int
streamlyTest =
  Streamly.unfoldr
    (\n -> if n <= 10 then Just (n, n + 1) else Nothing)
    (0 :: Int)
    & Streamly.filter odd
    & Streamly.mapM (\n -> (2 * n) <$ print n)
    & Streamly.fold (Streamly.foldl' (+) 0)
```

GHC core:

```ghc-core
Rec {
-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
streamlyTest :: IO Int
streamlyTest = streamlyTest1 `cast` <Co:3>

-- RHS size: {terms: 6, types: 2, coercions: 0, joins: 0/0}
streamlyTest1 :: State# RealWorld -> (# State# RealWorld, Int #)
streamlyTest1 = \ (s :: State# RealWorld) -> $wgo7 SPEC 0# 0# s

-- RHS size: {terms: 55, types: 37, coercions: 0, joins: 0/0}
$wgo7
  :: SPEC
     -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int #)
$wgo7
  = \ (w :: SPEC)
      (ww :: Int#)
      (ww1 :: Int#)
      (w1 :: State# RealWorld) ->
      case w of { __DEFAULT ->
      case <=# ww1 10# of {
        __DEFAULT -> (# w1, I# ww #);
        1# ->
          case remInt# ww1 2# of {
            __DEFAULT ->
              case hPutStr2
                     stdout
                     (case $witos ww1 [] of { (# ww3, ww4 #) -> : ww3 ww4 })
                     True
                     w1
              of
              { (# ipv, ipv1 #) ->
              $wgo7 SPEC (+# ww (*# 2# ww1)) (+# ww1 1#) ipv
              };
            0# -> $wgo7 SPEC ww (+# ww1 1#) w1
          }
      }
      }
end Rec }
```

### `streaming`

Haskell:

```haskell
{-# NOINLINE streamingTest #-}
streamingTest :: IO Int
streamingTest =
  Streaming.unfoldr
    (\n -> pure $ if n <= 10 then Right (n, n + 1) else Left ())
    (0 :: Int)
    & Streaming.filter odd
    & Streaming.mapM (\n -> (2 * n) <$ print n)
    & Streaming.fold_ (+) 0 id
```

GHC core:

```ghc-core
-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
streamingTest :: IO Int
streamingTest = streamingTest1 `cast` <Co:3>

-- RHS size: {terms: 22, types: 53, coercions: 16, joins: 0/0}
streamingTest1 :: State# RealWorld -> (# State# RealWorld, Int #)
streamingTest1
  = \ (s :: State# RealWorld) ->
      case $s$wunfoldr (lvl12 `cast` <Co:10>) lvl of { (# ww1 #) ->
      case $wfold_loop
             ($smapM1 (lvl11 `cast` <Co:6>) (loop5 (Effect ww1))) 0# s
      of
      { (# ipv, ipv1 #) ->
      (# ipv, case ipv1 of { :> a1 ds1 -> a1 } #)
      }
      }
Rec {

-- RHS size: {terms: 21, types: 23, coercions: 0, joins: 0/0}
lvl12
  :: Int
     -> State# RealWorld -> (# State# RealWorld, Either () (Int, Int) #)
lvl12
  = \ (n :: Int) (s :: State# RealWorld) ->
      (# s,
         case n of wild { I# x ->
         case <=# x 10# of {
           __DEFAULT -> lvl3;
           1# -> Right (wild, I# (+# x 1#))
         }
         } #)

-- RHS size: {terms: 2, types: 4, coercions: 0, joins: 0/0}
lvl3 :: Either () (Int, Int)
lvl3 = Left ()

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl :: Int
lvl = I# 0#

-- RHS size: {terms: 19, types: 23, coercions: 0, joins: 0/0}
lvl11 :: Int -> State# RealWorld -> (# State# RealWorld, Int #)
lvl11
  = \ (n :: Int) (eta2 :: State# RealWorld) ->
      case hPutStr2 stdout ($fShowInt_$cshow n) True eta2 of
      { (# ipv, ipv1 #) ->
      (# ipv, case n of { I# y -> I# (*# 2# y) } #)
      }

-- RHS size: {terms: 36, types: 86, coercions: 13, joins: 0/0}
loop5 :: Stream (Of Int) IO () -> Stream (Of Int) IO ()
loop5
  = \ (str :: Stream (Of Int) IO ()) ->
      case str of wild {
        Step ds ->
          case ds of { :> a1 as ->
          case a1 of a2 { I# ipv ->
          case remInt# ipv 2# of {
            __DEFAULT -> Step (:> a2 (loop5 as));
            0# -> loop5 as
          }
          }
          };
        Effect m1 ->
          Effect
            ((\ (s :: State# RealWorld) ->
                case (m1 `cast` <Co:6>) s of { (# ipv, ipv1 #) ->
                (# ipv, loop5 ipv1 #)
                })
             `cast` <Co:7>);
        Return r1 -> wild
      }
end Rec }
```

## Stream fusion

`streamly` uses a form of *stream fusion*, which is exemplified in the paper *Stream fusion: from
lists to streams to nothing at all*[^stream-fusion-paper]. The essence of that paper is using a
"corecursive" datatype to represent (pure) streams:

```haskell
{-# LANGUAGE ExistentialQuantification #-}

data Step s a = Skip s | Yield s a | Done

data Stream a = forall s. Stream s (s -> Step s a)
```

Can I just upgrade this `Stream` type until it matches that of `streaming`?

## Fusion for `streaming`

The first thing to do is make the step function effectful:

```haskell
data Step s a = Skip s | Yield s a | Done

data Stream m a = forall s. Stream s (s -> m (Step s a))
```

This is actually the representation that powers the
[`vector`](https://hackage.haskell.org/package/vector) package
([`Data.Stream.Monadic`](https://hackage.haskell.org/package/vector-stream-0.1.0.0/docs/Data-Stream-Monadic.html#t:Stream)).
Using their monadic `Stream` type, I get the same result as `streamly` on our test above. But
here's where I get stuck: my own implementation of the monadic `Stream` type fails to fuse on
examples where `vector`'s succeeds! If I can't get my own implementation working, then I don't have
a hope of extending it to match `streaming`'s stream type.

[^benches]: <https://github.com/composewell/streaming-benchmarks#streamly-vs-streaming>

[^stream-fusion-paper]: Coutts, D., Leshchinskiy, R., & Stewart, D. (2007). Stream fusion: From
    lists to streams to nothing at all. ACM SIGPLAN Notices, 42(9), 315-326.

    <https://doi.org/10.1145/1291220.1291199>