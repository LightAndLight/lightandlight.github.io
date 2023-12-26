---
title: 2023 Project Review
permalink: /2023-project-review
date: 2023-12-26T17:00:00+1000
tags:
    - programming
    - music
    - mathematics
excerpt: <p>Reflections on 2023's hobby projects.</p>
math: true
---

<div id="toc" style="float: right; max-width: 15em; white-space: normal;"><!-- generated --></div>

## Ipso

*Ongoing*

<https://ipso.dev>

<https://github.com/LightAndLight/ipso>

Ipso is a functional scripting language that I created so that I can write glue code and small scripts in a way that I enjoy.
In February I started using Ipso for some "real" scripts, so I wrote a [VSCode extension](https://github.com/LightAndLight/ipso/tree/main/vscode-ipso)
and added a bunch of features and fixes based on what I experienced.
In May I added [many new builtins](https://github.com/LightAndLight/ipso/blob/main/CHANGELOG.md#06).
Writing the VSCode extension was exciting; simple things like syntax highlighting and keyword autocompletion give the programming language a new level of "tangibility".

## Ipso scripts

*February - March*

[`git-explorer`](https://github.com/LightAndLight/git-explorer) is a prototype for a Git URI scheme I came up with.
Sometimes I take notes alongside my code and commit them to Git repository,
I want to create hyperlinks to lines of code in a file at specific commit,
in the same way that I can permalink to code on GitHub or Gitlab.
It would be cool to serve a code-aware wiki from a project's Git repo, similar to the way
[Fossil](https://www2.fossil-scm.org/home/doc/trunk/www/index.wiki)
can serve code, docs, and more from a repository.
The first step in all of this is a format for hyperlinks to Git objects.
I'll demonstrate by example, using `git-explorer` inside its own repository:

```
$ ls -a
.  ..  flake.lock  flake.nix  .git  gitex  .gitignore

$ git log HEAD~1..HEAD
commit 7f0489dab4397232cf3425fbed87b7c3636394b0 (HEAD -> main, origin/main)
Author: Isaac Elliott <isaace71295@gmail.com>
Date:   Wed May 31 11:52:11 2023 +1000

    use newer version of Ipso
    
$ gitex type git:object:7f0489dab4397232cf3425fbed87b7c3636394b0
commit

$ gitex show git:commit:7f0489dab4397232cf3425fbed87b7c3636394b0
tree 83f65fc9e11e9c8174a0822364ba411e1dbf6937
parent f880dc26e94129ebd7d688eacd9203cac0cb9964
author Isaac Elliott <isaace71295@gmail.com> 1685497931 +1000
committer Isaac Elliott <isaace71295@gmail.com> 1685497931 +1000

use newer version of Ipso

$ gitex type git:commit:7f0489dab4397232cf3425fbed87b7c3636394b0/tree
tree

$ gitex show git:commit:7f0489dab4397232cf3425fbed87b7c3636394b0/tree
100644 blob ea8c4bf7f35f6f77f75d92ad8ce8349f6e81ddba	.gitignore
100644 blob 6319f0e690d6037d6e70165c3aedbbf1a049b8b9	flake.lock
100644 blob f179ab98301cd132148e87f136566f2496e256ca	flake.nix
100755 blob ab5bac9c63cc9c8e5ca6da67ae7d8e5e58f9e68f	gitex

$ gitex show git:commit:7f0489dab4397232cf3425fbed87b7c3636394b0/tree/flake.nix | head -n 5
{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    ipso.url = "github:LightAndLight/ipso?commit=e7bf506bd8f85f00c7b00b795b587f79b5bb5d9d";
        
$ gitex uri flake.nix
git:blob:f179ab98301cd132148e87f136566f2496e256ca
git:commit:7f0489dab4397232cf3425fbed87b7c3636394b0/tree/flake.nix
```

[`timespent`](https://github.com/LightAndLight/timespent) helps me record what I've done during a work day.
When I start my work day, I `cd` to the directory that contains my time logs.
I call `timespent`, and my `EDITOR` ([Helix](https://helix-editor.com/), these days) opens today's time log file.
If the file hasn't been created yet, it's initialised from a template.
Today's time log file is located at `./YYYY-mm-dd-week/YYYY-mm-dd.txt`, where the directory has the date of this week's Monday.
I record what I've done, save and exit, and the file is automatically committed.
If I run `timespent` again, change what I've recorded, then save and exit, the changes are rebased onto the commit for today's log.

Each of these scripts is on the order of 100s of lines of Ipso code.
Writing them was super helpful for finding Ipso's pain points.
They also validated the purpose of Ipso; I enjoyed writing these scripts more than I would have if I used Bash, Python, or Haskell.

## Laurel

*March - May*

<https://github.com/LightAndLight/laurel>

Laurel is a query language experiment.
I'm perennially dissatisfied with SQL and continue to search for something better.
Laurel is an exploration inspired by [The third manifesto](https://dl.acm.org/doi/10.1145/202660.202667),
[Relational algebra by way of adjunctions](https://dl.acm.org/doi/10.1145/3236781),
and my experience with typed functional programming.

One part of the experiment is to have a single query language with multiple backends.
I implemented a REPL that can "connect" to two different backends: Postgres, and CSV files.
Here's a demo of the CSV backend:

<pre><code style="white-space: unset;">$ laurel repl
Welcome to the Laurel REPL. Type :quit to exit.
> :connect "csv" ["laurel-core/test/data/example.csv"]
connected

> :tables
table example {
  merchant : String,
  transaction type : String,
  amount : String,
  category : String
}

> :type tables
{ example : Relation { merchant : String, transaction type : String, amount : String, category : String } }

> tables.example
╭───────────────────┬───────────────────────────┬─────────────────┬───────────────────╮
│ merchant : String │ transaction type : String │ amount : String │ category : String │
├───────────────────┼───────────────────────────┼─────────────────┼───────────────────┤
│ Google            │ debit                     │ 10.00           │ tech              │
│ NAB               │ credit                    │ 5.00            │ finance           │
│ Spotify           │ debit                     │ 12.00           │ entertainment     │
│ Some Cafe         │ debit                     │ 22.00           │ eating out        │
│ Hotpot Restaurant │ debit                     │ 50.00           │ eating out        │
│ Woolworths        │ debit                     │ 38.00           │ groceries         │
│ NAB               │ credit                    │ 5.00            │ finance           │
╰───────────────────┴───────────────────────────┴─────────────────┴───────────────────╯

> :type (for row in tables.example yield row.merchant)
Relation String

> for row in tables.example yield row.merchant
╭───────────────────╮
│ _ : String        │
├───────────────────┤
│ Google            │
│ NAB               │
│ Spotify           │
│ Some Cafe         │
│ Hotpot Restaurant │
│ Woolworths        │
│ NAB               │
╰───────────────────╯

> :type (tables.example group by (\row -> row.`transaction type`))
Map String (Relation { merchant : String, transaction type : String, amount : String, category : String })

> tables.example group by (\row -> row.`transaction type`)
╭──────────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────╮
│ key : String │ value : Relation { merchant : String, transaction type : String, amount : String, category : String } │
├──────────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ credit       │ ╭───────────────────┬───────────────────────────┬─────────────────┬───────────────────╮               │
│              │ │ merchant : String │ transaction type : String │ amount : String │ category : String │               │
│              │ ├───────────────────┼───────────────────────────┼─────────────────┼───────────────────┤               │
│              │ │ NAB               │ credit                    │ 5.00            │ finance           │               │
│              │ │ NAB               │ credit                    │ 5.00            │ finance           │               │
│              │ ╰───────────────────┴───────────────────────────┴─────────────────┴───────────────────╯               │
│ debit        │ ╭───────────────────┬───────────────────────────┬─────────────────┬───────────────────╮               │
│              │ │ merchant : String │ transaction type : String │ amount : String │ category : String │               │
│              │ ├───────────────────┼───────────────────────────┼─────────────────┼───────────────────┤               │
│              │ │ Woolworths        │ debit                     │ 38.00           │ groceries         │               │
│              │ │ Hotpot Restaurant │ debit                     │ 50.00           │ eating out        │               │
│              │ │ Some Cafe         │ debit                     │ 22.00           │ eating out        │               │
│              │ │ Spotify           │ debit                     │ 12.00           │ entertainment     │               │
│              │ │ Google            │ debit                     │ 10.00           │ tech              │               │
│              │ ╰───────────────────┴───────────────────────────┴─────────────────┴───────────────────╯               │
╰──────────────┴───────────────────────────────────────────────────────────────────────────────────────────────────────╯
</code></pre>

I can also connect to to a Postgres database and import (a subset of) its schema:

```
> :connect "postgres" { host = "localhost", database = "test" }
connected

> :tables
table person {
  id : Int [PrimaryKey],
  name : String
}

table blah {
  x : Optional Int [Default(Some(0))]
}

> tables.person
╭──────────┬───────────────╮
│ id : Int │ name : String │
├──────────┼───────────────┤
│ 262145   │ alice         │
│ 262146   │ bob           │
│ 262147   │ charlie       │
╰──────────┴───────────────╯
```

This means there's potential for multi-datasource queries.

I really enjoyed implementing the tabular pretty printer. The tables look really cool!
And building the bridge between Laurel types and Postgres schemas felt like magic when I got it working.

I don't know what will happen to the typed functional version of Laurel.
I recently discovered [Datalog](https://en.wikipedia.org/wiki/Datalog), which seems a bit more elegant than the functional approach.
Next year I'll probably investigate Datalog some more.

## Blog site generator improvements

*May*

<https://blog.ielliott.io/jekyll-to-hakyll>

<https://github.com/LightAndLight/lightandlight.github.io/blob/main/site.hs>

I migrated this blog from Jekyll to Hakyll, and added a few upgrades.
Table of contents generation, heading anchors, and $\text{MathML support } (\forall a. \; a \rightarrow a)$ are the most significant features I added.

Moving my static site generator into Haskell gave me a greater feeling of agency over my site.
I started using Jekyll because it was the easiest path to get something set up on GitHub pages.
My default perspective was that I had to rely on the package authors to satisfy my needs.
Now that it's in Haskell it's something I feel like I own, and it's easier to realise that if there isn't a package that does what I
want then I can make it exist.

## Music: "Neon Rain"

*May - June*

<iframe style="margin-bottom: 1rem" title="Neon Rain by Isaac Elliott" width="100%" height="300" scrolling="no" frameborder="no" src="https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/1532726281&color=%23379392&auto_play=false&hide_related=false&show_comments=true&show_user=true&show_reposts=false&show_teaser=true&visual=true"></iframe>

I've been getting into ambient / soundtrack music over the past few years.
I kept playing around with synths, but couldn't figure out how to write a piece from start to finish.
My background is in rock and metal, which have a *lot* of constraints that I'm familiar with (forms, instrumentation, etc.).
"Neon Rain" was born from my commitment to *just make something*.
I decided on a theme&mdash;melancholy, cyberpunk&mdash;and found some pictures on [ArtStation](https://www.artstation.com)
that I found compelling.
Then I sat down and made music.
I tried to design sounds by following my sense of taste, guided by the images I'd chosen.
This required me to bypass my inner critic, who has a (n admittedly pretty whacky) desire to "be original".
I welcomed any idea that would make the music more effective instead of dismissing most ideas because they seemed cliché.
The end result is my first full ambient / soundtrack -style piece.
I made *something*, and that feels like a breakthrough.

## Music: "Resurrect"

*May - June*

<iframe style="margin-bottom: 1rem" title="Resurrect by Isaac Elliott" width="100%" height="300" scrolling="no" frameborder="no" src="https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/1536670189&color=%23379392&auto_play=false&hide_related=false&show_comments=true&show_user=true&show_reposts=false&show_teaser=true&visual=true"></iframe>

I've saved almost every [Reaper](https://www.reaper.fm) project that I've created since I started producing music in 2018.
I went back through my archives and found a project that was basically a complete song.
I thought it was cool, so I decided to mix it for release.
Even though I haven't done very much music production in the past 1-2 years, I was able to create what is overall my best mix so far.
I think the main reason for this was that I successfully used a professional song ([Slow Violence by Caligula's Horse](https://www.youtube.com/watch?v=bNlUJgP3Lwk)) as a reference mix.
[Dan Worrall](https://www.youtube.com/@DanWorrall) talked about [reference mixes as a "palate cleanser for the ears"](https://www.youtube.com/watch?v=kGW8SDFikeg),
and this really stuck with me.

## Nominal Sets

*May - July*

<https://github.com/LightAndLight/binders.rs>

<https://blog.ielliott.io/nominal-sets>

<https://blog.ielliott.io/nominal-sets-proofs>

Nominal Sets is a theory of [name binding](https://en.wikipedia.org/wiki/Name_binding).
I started writing a Rust library based on the idea, and realised that I needed to understand the theory better if I wanted judge how well my library served its purpose.
To this end, I wrote an introduction to Nominal Sets.
As a mathematical exercise I proved many theorems by hand, which I included in a separate appendix.
I feel like I leveled up my mathematical literacy.

One important tool I discovered was [my state library's online catalogue](https://www.slq.qld.gov.au/).
I felt like I had reach the limit of what I could glean from the ~3 introductory Nominal Sets papers,
and I needed to read [the book on Nominal Sets](https://www.cambridge.org/core/books/nominal-sets/80F2B0C1B78A1DC309072CCEDAA88422)
to make progress.
I didn't want to buy it, though, because it seemed too niche for my small bookshelf.
Fortunately, the Queensland state library has a full digital copy that I can read for free.
As expected, the book had what I needed to get past my mental blocks.

This is my biggest mathematical project and my biggest writing project to date.
The proofs took ~4 weeks to finish.
In the end, I concluded that the Rust library is basically okay from a mathematical point of view.
It still needs some UX and performance improvements, though.

## Hover Pill

*June*

<https://github.com/LightAndLight/hover-pill>

<div style="margin-bottom: 1rem; display: flex; flex-direction: row; align-items: center; justify-content: space-between;"><img alt="Hover Pill resting on a platform in the open sky." width="30%" src="https://github.com/LightAndLight/hover-pill/raw/main/screenshots/level.png"><img alt="The Hover Pill level editor." width="30%" src="https://github.com/LightAndLight/hover-pill/raw/main/screenshots/level-editor.png"><img alt="Hover Pill flying through an opening in a wall." width="30%" src="https://github.com/LightAndLight/hover-pill/raw/main/screenshots/other-level.png"></div>

Hover Pill is a puzzle game I created in 2022 to learn the [Bevy game engine](https://bevyengine.org/).
You fly a capsule-shaped object through obstacles to reach a goal.
This year I added a level editor, which completes the project.

You can play the web version [here](https://blog.ielliott.io/hover-pill).

## 2D visibility tests

*July*

<https://github.com/LightAndLight/2d-visibility>

I started thinking about how to make games with good agent simulations.
Visibility seems like an important primitive; there are a lot of actions that an agent should only take if they can "see" an object.
These are some experiments in calculating which objects are visible from a particular point.

<div style="display: flex; flex-direction: row; justify-content: space-between;">
<div style="width: 48%; display: flex; flex-direction: column; align-items: center;">
<video width="100%" src="https://github.com/LightAndLight/2d-visibility/raw/main/videos/exercise_2.webm" controls="controls"></video>
Exercise 1: simple line of sight
</div>
<div style="width: 48%; display: flex; flex-direction: column; align-items: center;">
<video width="100%" src="https://github.com/LightAndLight/2d-visibility/raw/main/videos/exercise_2.webm" controls="controls"></video>
Exercise 2: hiding occluded objects
</div>
</div>

<div style="display: flex; flex-direction: row; justify-content: space-between;">
<div style="width: 48%; display: flex; flex-direction: column; align-items: center;">
<video width="100%" src="https://github.com/LightAndLight/2d-visibility/raw/main/videos/exercise_3.webm" controls="controls"></video>
Exercise 3: visual feedback for occluded areas
</div>
<div style="width: 48%; display: flex; flex-direction: column; align-items: center;">
<video width="100%" src="https://github.com/LightAndLight/2d-visibility/raw/main/videos/exercise_4.webm" controls="controls"></video>
Exercise 4: hiding occluded objects
</div>
</div>

## 3D graphics fundamentals

*July - August*

<https://github.com/LightAndLight/3d-graphics-fundamentals>

<img alt="A 3D scene rendered using physically based techniques at 60FPS." width="100%" src="https://github.com/LightAndLight/3d-graphics-fundamentals/raw/main/screenshot.png">

Computer graphics has always been intimidating and difficult for me, but I keep making progress.
Last year I [implemented an offline ray tracing renderer](http://blog.ielliott.io/2022-project-review#ray-tracing-in-one-weekend)
and a [GPU-based mandelbrot fractal renderer](http://blog.ielliott.io/2022-project-review#wgpu-mandelbrot).
This year, my big graphics project was realtime physically-based 3D rendering.

There aren't any tutorials or courses for something like this.
Instead, there are examples of specific techniques spread across books, papers, blog posts, and talks, and it was up to me to piece them together.
Using only a [GPU library](https://docs.rs/wgpu/latest/wgpu/) and [linear algebra library](https://docs.rs/cgmath/latest/cgmath/), I implemented:

* Physically-based shading
* High dynamic range rendering with automatic exposure and tone mapping
* Directional and point lights
* Shadow mapping (fitted to view frustum for directional lights, omnidirectional for point lights)
* HDRI skybox

I also integrated [the egui immediate mode GUI](https://github.com/emilk/egui) so that I could add realtime debugging controls.

Like many good things, this project was *really* hard.
There were many times when I felt like there was no way to make progress because I didn't know enough.
Whenever this happened, I eventually *did* make progress by breaking the problem into smaller pieces and trying to understand each piece from first principles.

One place I got stuck in dealing with automatic exposure.
Physically-based rendering means using physically plausible values for quantities like "the amount of light visible at a pixel",
but screens generally take an 8-bit value for each pixel colour.
You have to figure out how to map your physical pixel illuminations to the range of 0 to 255.
A simple mapping like "divide every value by the max value in the scene" looks wrong, because of the [dynamic range](https://en.wikipedia.org/wiki/Dynamic_range#Photography) of the scene.
The darkest area and the lightest area can differ by many orders of magnitude.
[Tone mapping](https://en.wikipedia.org/wiki/Tone_mapping) came up a lot in discussions of HDR rendering, so I thought it was the answer.
But it's not; [exposure](https://en.wikipedia.org/wiki/Exposure_(photography)) is what I was looking for.
In particular, I wanted automatic exposure so that scenes with different brightnesses all looked good.
I couldn't find anything that really spelled out the implementation of autoexposure in a HDR renderer,
so I had to develop a physically-based intuition of what my camera was supposed to do.
Only then did the math make enough sense for me to be able to implement something.

Part of the reason I did this project was to see if I have what it takes to do computer graphics professionally.
I think this shows that I at least have potential.

## Low-level IR compiler

*August - September*

<https://github.com/LightAndLight/metis>

`metis` was supposed to be an implementation of some type systems / compilation research I'm doing,
but it ended up being a playground for implementing a low-level intermediate representation that compiles to assembly.
I implemented register allocation and branching and basic blocks with arguments, and by the time I got to calling conventions I realised how far off track I'd gone.
It was fun while it lasted, but right now I value testing the research idea more than I value learning how to reimplement LLVM.

The research idea builds on [Statically Sized Higher-kinded Polymorphism (2020)](https://blog.ielliott.io/sized-hkts).
In that project, I demonstrated higher-kinded polymorphism in a language with Rust-like monomorphisation.
Monomorphising polymorphic functions generates a multiplcative amount of code:
A function with one type argument has a monomorphisation for every type,
and a function with two type arguments has a monomorphisation for (every type) squared.
Higher-kinded polymorphism compounds the issue.

I've got [a branch](https://github.com/LightAndLight/metis/tree/use-llvm) where I've replaced all that stuff with LLVM so that I can focus on the core ideas.

## Parametric polymorphism in cartesian closed categories

*September*

<https://github.com/LightAndLight/ccc-polymorphism>

## Talk: Rust and functional programming

*September*

<https://github.com/LightAndLight/rust-and-fp>

TODO: add to talks

## Music: "Intro Theme"

*October - November*

<iframe style="margin-bottom: 1rem" title="Intro Theme by Isaac Elliott" width="100%" height="300" scrolling="no" frameborder="no" allow="autoplay" src="https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/1662315852&color=%23379392&auto_play=false&hide_related=false&show_comments=true&show_user=true&show_reposts=false&show_teaser=true&visual=true"></iframe>

## Single program web apps

*December*

<https://github.com/LightAndLight/misc>

## Denotational design experiment: incremental file processing

*November - present*

<https://github.com/LightAndLight/incremental-file-processing>