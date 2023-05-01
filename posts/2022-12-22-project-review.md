---
title: 2022 Project Review
author: ielliott95
permalink: /2022-project-review
date: 2022-12-27 14:25:00 +1000
tags:
    - programming
excerpt: <p>Reflections this year's hobby projects.</p>
feed_id: https://blog.ielliott.io/project-review
---

Here's a review of all[^1] the programming projects I worked on in my spare time this year.
I'm quite satisfied with the work I did. I learned a lot
and had some cool ideas that I will continue to build on. I'm glad I wrote this review, because
there's some stuff in here that I'm quite proud of that I had forgotten about. This year was a
pretty long one for me.

## `ipso`

*Ongoing*

<https://github.com/LightAndLight/ipso>

`ipso` is a scripting language that I started working on a bit over 2 years ago. My goal for this
project is to have a scripting language that I *actually enjoy* using. So far I haven't found a
language that I find satisfactory for small administrative programs; Bash and Python have no types,
and Haskell is a bit slow and old for interpreted use, for example. `ipso` is my attempt at an answer.

This year I set up a website (<https://ipso.dev>) and published my first few [releases on
GitHub](https://github.com/LightAndLight/ipso/releases).

Some of this year's features and milestones that I'm proud of:

* [REPL](https://github.com/LightAndLight/ipso/issues/170)
* [`Debug` instances for extensible records and
  variants](https://github.com/LightAndLight/ipso/pull/177) ([reference
  docs](https://ipso.dev/docs/reference.html#debugging))
* [Nested pattern matching](https://github.com/LightAndLight/ipso/issues/95)
* [Using `ipso` in a CI script](https://github.com/LightAndLight/ipso/blob/main/.github/workflows/uploadToCache)

The language itself is pretty stable now, so now my focus will be on writing standard library functions.

## `ray-tracing-in-one-weekend`

*January*

<https://github.com/LightAndLight/ray-tracing-in-one-weekend>

<img src="https://github.com/LightAndLight/ray-tracing-in-one-weekend/raw/main/render.png"
width="100%" >

An implementation of Peter Shirley's [Ray Tracing in One Weekend](https://raytracing.github.io/)
with some extra features. It was super fun. It's incredibly satisfying to go from a bunch of math to
beautiful images.

The most striking thing I learned was [Monte Carlo
integration](https://en.wikipedia.org/wiki/Monte_Carlo_integration). It's a way to compute integrals
using random numbers. Ray tracing uses it to approximate the colour of a point on a surface. Every
point on a surface has a specific, well-defined colour, and that colour can be the result of 
contributions from an extremely large number incident rays. The point's colour can be expressed as
an integral, and we use Monte Carlo integration to compute the integral with a varying level of
accuracy. For a preview render, we can use few samples, and quickly produce a noisy image. For a full
render we can use many samples, which will take longer, but will give a very accurate result.

## `sylva`

*January*

<https://github.com/LightAndLight/sylva>

"Sylva" means "forest" in Latin (according to Google Translate). I was playing with some
ideas about wikis / "document-based knowledge graphs".

There were tree things I wanted to combine:

* A web-based user interface
* Using a Git repository for versioning documents
* Preventing dead links within the "wiki"

This was just a sketch and I don't plan to do anything with it.

## `editor-vue`

*March*

<https://github.com/LightAndLight/editor-vue>

A while ago I built a toy [structural code editor](https://github.com/LightAndLight/edit-log) using
Haskell (GHCJS), and the `reflex` FRP library. I wasn't happy with the performance. I heard about
[vue.js](https://vuejs.org/) and was curious what it would be like to use it instead of `reflex`. I
rebuilt some of the code editor using `vue.js` with TypeScript, enough to get a sense of the coding
style and performance of the app. I was impressed by the performance improvements, and found
TypeScript tolerable (and much, much better than plain JavaScript).

## `nix-docs`

*March / April*

<https://github.com/LightAndLight/nix-docs>

`nix-docs` is an ongoing experiment with reference documentation for some important Nix functions. Most Nix documentation is prose paragraphs, which is pretty bad for reference docs. Reference docs
need to be skimmable, terse, and interlinked. Here's the `nix-docs` page for
`mkDerivation`: <https://blog.ielliott.io/nix-docs/mkDerivation.html>.

This year I updated the styling to match the new [NixOS](https://nixos.org/) design and wrote a documentation generator for the content (my first iteration was hand-edited HTML that I copied
from the Nixpkgs manual). 

## `ccc`

*May*

<https://github.com/LightAndLight/ccc>

`ccc` stands for [cartesian closed
category](https://ncatlab.org/nlab/show/cartesian+closed+category). I was inspired by [this podcast with Conal
Elliott](https://www.typetheoryforall.com/2022/05/09/17-The-Lost-Elegance-of-Computation-(Conal-Elliott).html),
and revisited his [compiling to categories](http://conal.net/papers/compiling-to-categories/) and
[calculating compilers categorically](http://conal.net/papers/calculating-compilers-categorically/)
papers. One important insight from "calculating compilers categorically" is that translating lambda
expressions into CCC syntax sort of "sequentialises" them. The composition operation in a category
implies an order of operations: `g ∘ f` is often read as `g` after `f`. It seems to me that CCC
syntax is closer to our [word-at-a-time](https://doi.org/10.1145/359576.359579)-style imperative CPUs.

This leads to the first idea I explored in `ccc` was: using CCC syntax as an intermediate
representation for lambda calculus. This worked out really well; I learned that the lambda to CCC
translation also performs closure conversion, which is another reason that CCC syntax is easier to
compile to imperative code.

The second idea builds on the first. Once we have a program in CCC syntax, a compiler can be defined
as a functor from CCC syntax to another cartesian closed category. I think Conal mentioned this in
the podcast episode. I wrote a messy [SSA
compiler](https://github.com/LightAndLight/ccc/blob/68d0214a778a19f04ee7c96a973749bd0d09d4d1/src/SSA.hs)
as a functor from CCC syntax arrows to "SSA builder arrows" (Haskell functions of type
`SSA -> SSA`). It was pretty straightforward because CCC syntax is sequential and closure-converted.

The last idea was to apply these techniques to
[substructural](https://en.wikipedia.org/wiki/Substructural_logic) lambda calculi (i.e. [affine](https://en.wikipedia.org/wiki/Affine_logic) and
[linear](https://en.wikipedia.org/wiki/Linear_logic) lambda calculus). Linear lambda calculus has
its own categorical syntax ([closed symmetric monoidal
category](https://ncatlab.org/nlab/show/symmetric+monoidal+category) - call it CSMC for short),
so I wrote a
[program](https://github.com/LightAndLight/ccc/blob/68d0214a778a19f04ee7c96a973749bd0d09d4d1/proofs/Linear.agda)
that translates lambda calculus to CSMC syntax, and rejects lambda calculus terms that have
non-linear variable usages. I then used the same program structure to translate lambda terms to
[semicartesian monoidal category](https://ncatlab.org/nlab/show/semicartesian+monoidal+category)
syntax, which is just CSMC syntax with a [terminal
object](https://ncatlab.org/nlab/show/terminal+object).
[That translation](https://github.com/LightAndLight/ccc/blob/68d0214a778a19f04ee7c96a973749bd0d09d4d1/proofs/Affine.agda)
allows unused variables while rejecting variable duplication, which makes it affine. The [final
translation](https://github.com/LightAndLight/ccc/blob/68d0214a778a19f04ee7c96a973749bd0d09d4d1/proofs/Unrestricted.agda)
adds a `dup : a -> a ⊗ a` arrow to the semicartesian monoidal category, which gets us back to a
cartesian closed category (but with a slightly different syntax) and unrestricted lambda calculus.

This journey lead to a style for checking lambda calculus that works for linear, affine,
and unrestricted lambda calculus. I think would be interesting to create a type checker that checks
in this style. My intuition says such a type checker might be easier to parallelise.

I also noticed that the [CCC
syntax](https://github.com/LightAndLight/ccc/blob/68d0214a778a19f04ee7c96a973749bd0d09d4d1/proofs/Unrestricted.agda#L35-L49)
I settled on is explicit about parallel computations. While composition (`f ∘ g`) can be thought of as `f` after
`g`, the tensor operator (`f ⊗ g`) can be thought of as `f` and `g` in parallel. There's a sense in
which this CCC syntax "reveals" parallelism that's inherient in the lambda calculus. I'm curious
what it would be like to write a multi-core parallel evaluator based on this.

## `march`

*June*

<https://github.com/LightAndLight/march>

I wanted to check for broken local links markdown documents, and create
a "move" command that works like `mv` but also renames links. I finished the former but not the latter.

## `bidirectional-typechecking-with-unification`

*June*

<https://github.com/LightAndLight/bidirectional-typechecking-with-unification>

This work was inspired by [an article about the limitations of unification-based type
checking](https://www.haskellforall.com/2022/06/the-appeal-of-bidirectional-type.html). It seemed to
claim that [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) / unification-based type checking is very limited, and presented a
dichotomy between bidirectional typing and unification that I don't agree with.

I wrote a Hindley-Milner-based type checker for a language with subtyping by applying bidirectional
principles. It has higher-rank polymorphism, existential types, optional record fields, and default
record fields, which are all powered by the same subtyping mechanism. Unification and instantiation
are also performed by the subtyping mechanism.

The key insight is to allow the subtyping check to transform terms. A type `A` is a subtype of `B`
when values of type `A` can be used where values of type `B` are expected. This is often written as
`A :> B`, and in code as something like `isSubtypeOf : Type -> Type -> Bool`. My type checker
returns evidence that the subtyping relation holds, which could be written as
`(a : A) :> B  ~> b`, and as a function: `isSubtypeOf : (Expr, Type) -> Type -> Maybe Expr`. The
bidirectional style means ensures that "checking" types drives subtyping. This is
all perfectly compatible with unification-based inference.

This deserves a much clearer explanation in its own blog post. I think it's a promising result
for programming language design.

## `little`

*June / July*

<https://github.com/LightAndLight/little>

`little` is my first attempt at a [Knuth-style literate
programming](https://doi.org/10.1093/comjnl/27.2.97) system. I want to write documents about code
that are also the source truth for the code. Systems like [literate Haskell](https://www.haskell.org/onlinereport/literate.html) are unsatisfying
to me because I have to present the code to the reader in the same order that the code appears
in the source file. For example, all literate Haskell articles will begin with a preamble of imports
([example article](https://blog.ielliott.io/continuations-from-the-ground-up/)). I want to present
code to the reader in a non-linear fashion, in a way that supports my explanation. I imagine that
I'd often put import declarations in an appendix, for instance.

`little doc` generates a document that I can publish on
the web, and `little code` generates the codebase that is described in the document. Another fun use
case is "self-documenting shell scripts"
([example](https://github.com/LightAndLight/little/blob/main/examples/script.lit)). Rather than
commenting a bash script, you can write a literate document that describes a bash script, and give
the document a shebang line.

`little` uses XML for its markup, so that I can use whatever "presentation" markup I want (Markdown,
LaTex, HTML, etc.). I was surprised by how "not terrible" it felt to use XML for this. I have a
strong bias against XML in general, and now that bias has gained some nuance. XML feels alright for
*markup*, that is, for extra information in documents that are mostly text which
people will consume by reading. That's what it was designed for; it's the eXtensible *Markup*
Language. What I now object to is the use of XML as a data format.
[This article](https://www.devever.net/~hl/xml) has a good heuristic for distinguishing the two uses: if
you remove all the tags from your XML document, will it still make sense to a reader? I've tried to
apply this heuristic to the syntax of `little`.

The code is pretty crappy, so if I continued to work on this I'd rewrite it. I'm optimistic about
what I created so far, though.

## `mininix`

*August*

<https://github.com/LightAndLight/mininix>

`mininix` is an attempt at understanding how Nix-style build systems work by writing a small one. It
includes a content-addressable store, a build database (using sqlite), a parallel build executor and a typed build language.

I also wanted to improve on the naming of concepts (i.e. use a better word than "derivation"), and
to keep typeability in mind from the start (Nix is very very untyped. Would types affect the build
system's design?).

One idea I'd like to explore here is a sort of "local" version of Nix. Instead of having a global
store, have a per-project store for build artifacts similar to `cabal`'s `dist[-newstyle]`
directories and
`cargo`'s `target` directory.

I'm also interested in whether we can have build systems that reuse existing package declarations.
For example, if you want to use Nix to package a Haskell project, you need to convert your
`.cabal` file to a Nix expression (or do [import from
derivation](https://nixos.wiki/wiki/Import_From_Derivation), which I fundamentally disagree with).
What if there was a way to use the `.cabal` file without the grossness of import-from-derivation?

## `top-down-hindley-milner`

*September*

<https://github.com/LightAndLight/top-down-hindley-milner>

This project shows a sort of "upside down" approach to Hindley-Milner type inference.
This work was inspired by some inaccurate type errors that `ipso` generated, and this algorithm is
my solution.

[Bidirectional type
checking](https://ncatlab.org/nlab/show/bidirectional+typechecking) separates inference from
checking, and this distinction is important in contrasting "normal" Hindley-Milner to the "top-down"
approach. Roughly speaking, Hindley-Milner constructs types through inference in a bottom-up
manner, and my algorithm refines types through checking from the top down.

In Hindley-Milner, all the work is done by inference and checking is the trivial case of
inference followed by unification with an expected type. In the "top-down" style, checking does all
the work, and inference is performed by checking against a fresh metavariable.

I want to combine this work with [the subtyping work](#bidirectional-typechecking-with-unification) I mentioned earier.

## `hover-pill`

*October*

<https://github.com/LightAndLight/hover-pill>

`hover-pill` is a game I created to learn the [Bevy](https://bevyengine.org/) game engine. You can
try an early build [here](https://blog.ielliott.io/hover-pill/). It's a 3D puzzle/platformer where
you fly around as a capsule-shaped object (I'm not a 3D artist) trying to reach the green goal square.

I haven't done any game development for years, so this project was very refreshing. Once I had
all the mechanics working, I asked my girlfriend to test the levels I designed. Each time
she completed a level, I created a slightly more difficult one. She enjoyed playing it, and I'm glad that in the end I created
something fun.

Bevy uses [`wgpu`](https://wgpu.rs/) for graphics, which combined with Rusts awesome
cross-compilation support means it was pretty easy for me to develop on my desktop (with x86_64 and Vulkan), and
then compile a WASM and WebGL version for the web. It was a pleasant surprise, coming from Haskell
and GHCJS.

This was my first time using an
[entity-component-system](https://en.wikipedia.org/wiki/Entity_component_system) framework, and I
enjoyed it. [Data-Oriented Design](https://dataorienteddesign.com/dodbook/) helped me understand the
history behind the patterns. I think there are ideas here that apply outside of game development,
but I don't know what they are yet. On example (and I think it's where I learned about the DoD book)
is [this](https://ziglang.org/download/0.8.0/release-notes.html#Reworked-Memory-Layout) explanation
of a "data-oriented" performance improvement in the Zig compiler.

## `wgpu-mandelbrot`

*October*

<https://github.com/LightAndLight/wgpu-mandelbrot>

<img src="https://github.com/LightAndLight/wgpu-mandelbrot/raw/main/images/screenshot-2.png"
width="100%" />

After [hover-pill](#hover-pill) I wanted to learn more about graphics APIs and GPU programming. I realised that computing the mandelbrot set was an
"embarrassingly parallel" problem, so it would be a good fit for GPU programming.

The mandelbrot renderer runs in realtime. It has a satisfying "blooming" effect as the iteration
count ticks up and more points are coloured. The mandelbrot
calculations are performed in a compute shader, and the colours are assigned using a
[histogram algorithm](https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set#Histogram_coloring)
on the CPU. I couldn't figure out how to do histogram colouring on the GPU.

To make sense of the WebGPU API, I created this diagram which displays all relevant (Web)GPU
resources and their relationships:

<iframe style="border: 1px solid rgba(0, 0, 0, 0.1);" width="800" height="450" src="https://www.figma.com/embed?embed_host=share&url=https%3A%2F%2Fwww.figma.com%2Ffile%2FIr3gsGIELdVBwYgVkCiHvE%2Fwebgpu%3Fnode-id%3D0%253A1%26t%3DUIEhGQEzO3ObVrhi-1" allowfullscreen></iframe>

I have a much better sense of GPU programming fundamentals, and I think the careful design of WebGPU helped.
It's higher level than Vulkan, but more explicit than OpenGL. I've done a Vulkan tutorial and forgot
almost all of it. Having learned the fundamentals `wgpu`, I think the Vulkan API would make a lot more sense
to me now.

## `hedge`

*December*

<https://github.com/LightAndLight/hedge>

`hedge` is a library that makes it easier for me to write web information systems with Haskell. I've
been developing a sense of style and a set of patterns around writing Haskell web apps,
in particular
using [servant](https://hackage.haskell.org/package/servant) and focusing on server-side rendered resources, and `hedge` is
kind of my "kitchen sink" for things that support the style.

I might create a command-line program for setting up a new project, adding endpoints, and other
forms of boilerplate I find.

I'm not sure if it will ever lead to something I could call a "framework", like
[Rails](https://rubyonrails.org/). Right now I have the sense that it would be more like a [pattern
language](https://en.wikipedia.org/wiki/A_Pattern_Language) with automated helpers.

[^1]: that made it onto GitHub