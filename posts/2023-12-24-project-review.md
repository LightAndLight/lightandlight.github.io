---
title: 2023 Project Review
permalink: /2023-project-review
date: 2023-12-25T07:25:00+1000
tags:
    - programming
    - music
excerpt: <p>Reflections on 2023's hobby projects.</p>
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

```
$ laurel repl
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
```

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
Building the bridge between Laurel types and a Postgres schema felt like magic when I got it working.

I don't know what will happen to the typed functional version of Laurel.
I recently discovered [Datalog](https://en.wikipedia.org/wiki/Datalog), which seems a bit more elegant than the functional approach.
Next year I'll probably investigate Datalog some more.

## Blog site generator improvements

*May*

<https://blog.ielliott.io/jekyll-to-hakyll>

<https://github.com/LightAndLight/lightandlight.github.io/blob/main/site.hs>

## Music: "Neon Rain"

*May - June*

<https://soundcloud.com/lightandlight/neon-rain>

## Music: "Resurrect"

*May - June*

<https://soundcloud.com/lightandlight/resurrect>

## Nominal Sets

*May - July*

<https://github.com/LightAndLight/binders.rs>

<https://blog.ielliott.io/nominal-sets>

<https://blog.ielliott.io/nominal-sets-proofs>

## Hover Pill

*June*

<https://github.com/LightAndLight/hover-pill>

## 2D visibility tests

*July*

<https://github.com/LightAndLight/2d-visibility>

## 3D graphics fundamentals

*July - August*

<https://github.com/LightAndLight/3d-graphics-fundamentals>

## Low-level IR compiler

*August - September*

<https://github.com/LightAndLight/metis>

## Parametric polymorphism in cartesian closed categories

*September*

<https://github.com/LightAndLight/ccc-polymorphism>

## Talk: Rust and functional programming

*September*

<https://github.com/LightAndLight/rust-and-fp>

TODO: add to talks

## Music: "Intro Theme"

*October - November*

<https://soundcloud.com/lightandlight/intro-theme>

## Single program web apps

*December*

<https://github.com/LightAndLight/misc>

## Denotational design experiment: incremental file processing

*November - present*

<https://github.com/LightAndLight/incremental-file-processing>