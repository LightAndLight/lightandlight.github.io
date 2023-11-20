---
title: The Boundaries of Denotational Design
author: ielliott95
permalink: /boundaries-of-denotational-design
date: 2023-11-21T07:30:00+10
math: false
excerpt: TODO
tags:
  - programming
  - mathematics
---

* Most examples of denotational design are about importing math into computers
* That's not quite right, but it feels like the solutions still live in "math space"
  * Affine functions - https://www.youtube.com/playlist?list=PLX913KKLwrAbHhbBOd-JhHmAlr5H8Djbf
  * Partial maps, tries - http://conal.net/papers/type-class-morphisms/type-class-morphisms-long.pdf
  * Linear transformations - http://conal.net/blog/posts/reimagining-matrices
* I've had the impression that denotational design only applies to "pure functions"
  * ... or something. I don't even know. Just a feeling that you can't write "effectful"
    libraries.
  * This ends up in confusion
* As a programmer and computer user, I encounter problems involving file systems,
  network I/O, database migrations, version control systems, and so on.
  * Ideas that aren't mathematical in origin, in the same way that integers or functions are
  * These ideas and constructs are contingent on the way we currently build and use
    computing systems
  * They shouldn't be exempt from denotational design!
* Feels like there's a [draw the rest of the owl](https://www.deviantart.com/rayfan9876/art/How-to-draw-an-owl-177608203)
  problem here
  * On my mind thanks to the awesome [Statistical Rethinking](https://www.youtube.com/watch?v=cclUd_HoRlo&list=PLDcUM9US4XdMROZ57-OIRtIK0aOynbgZN&index=2)
    lectures
  * I'm pretty sure Conal's motivated to "make computers do things", too
  * But there's something missing in going from a very concrete software engineering
    problem to an idealised denotational framing
  * I want to try to draw the rest of the owl and see where we end up
    * While challenging this weird internal voice that says denotational design
      can't apply to "real software"
* I'm inspired by Conal's ["Can functional programming be liberated from the von Neumann style?"](http://conal.net/blog/posts/can-functional-programming-be-liberated-from-the-von-neumann-paradigm)
  post
  * > [..] let’s explore how to move I/O entirely out of our programming model into the
    > implementation of a denotationally simple model for whole systems.
    
Topic of discussion: file systems. Does denotational design apply here?

It seems reasonable to me to think of a file system, or a portion of one,
as a partial map from file paths to file contents:

```haskell
type PartialMap k v = k -> Maybe v

empty :: PartialMap k v  
empty = const Nothing

lookup :: k -> PartialMap k v -> Maybe v
lookup k f = f k

insert :: Eq k => k -> v -> PartialMap k v -> PartialMap k v
insert k v f = \k' -> if k == k' then Just v else f k'

delete :: Eq k => k -> PartialMap k v -> PartialMap k v
delete k f = \k' -> if k == k' then Nothing else f k'
```

```haskell
-- | @[[ Files ]] = PartialMap FilePath ByteString@
data Files

-- | @[[ empty ]] = empty@
empty :: Files
empty = _

-- | @[[ lookup k f ]] = lookup k [[ f ]]@
lookup :: FilePath -> Files -> Maybe ByteString
lookup = _

-- | @[[ insert k v f ]] = insert k v [[ f ]]@
insert :: FilePath -> ByteString -> Files -> Files
insert = _

-- | @[[ delete k f ]] = delete k [[ f ]]@
delete :: FilePath -> Files -> Files
delete = _

{- | @force path f@ creates a file in directory @path@ for each element of
the partial map denoted by @f@.

In other words, it creates a file named @path </> k@ with contents @v@
for each @k@ that has @[[ f ]] k = Just v@.
-}
force :: FilePath -> Files -> IO ()
force = _
```

I notice that what I'm trying to do seems weird.
I want `insert` on `Files` to write to the file system.
I want `lookup` on `Files` to read from the file system.
But the denotation of `Files` says nothing about file systems.
* That's a feature, not a bug. See Conal on being implementation-independent.
How can I know if it uses the file system "correctly"?
* There is a different level of correctness that's not accounted for by the denotation
* Other properties, such as resource usage
Am I making a "type error" in my reasoning?

I have an interesting implementation that I'd argue is denotationally sound.

Content warning: `unsafePerformIO`.