---
title: Exploring a dependency graph with Prolog
permalink: /exploring-graphs-with-prolog
date: 2025-08-02T07:00:00Z
---

<div id="toc"><!-- generated --></div>

I was working on my (currently private) [YouTube feed reader project](https://lectio.news/),
watching the dependencies build after I upgraded the GHC version,
when I noticed an odd transitive dependency: [`happy`](https://hackage.haskell.org/package/happy).
`happy` is a parser generator, and this project is a web app.
Why does my web app depend on a parser generator?

This reminded me of my recent [Haskell dependency graph exploration](https://blog.ielliott.io/playing-with-ghc-package-databases),
where I wondered whether there was a good way to query dependency graph.
It then occurred to me that [Datalog](https://en.wikipedia.org/wiki/Datalog) might be a good query language for this.
I couldn't find a canonical FOSS Datalog implementation, so I went with [GNU Prolog](https://www.gprolog.org/) instead.
Here's how it turned out.

## Setup

We're going to turn the output of `ghc-pkg dot` into a set of Prolog facts.
An edge from `a` to `b` will be encoded as a fact `depends_on(a, b)`.

<details class="shell-command" open>
<summary>
<samp>$ <kbd>cat > dot2pl.py <<EOF</kbd></samp>
</summary>
<pre><samp>$ <kbd>cat > dot2pl.py &lt;&lt;EOF
import re
import sys

def dot_to_prolog(dot_content):
    edges = re.findall(r'"(.+)"\s*->\s*"(.+)"', dot_content)

    for source, target in edges:
        print(f"depends_on('{source}', '{target}').")

line = input()
if line == "digraph {":
    pass
else:
    raise Error(f"Expected {"digraph {"}, got {line}")

while True:
    line = input()
    if line == "}":
        break
    else:
        dot_to_prolog(line)
EOF
</kbd></samp></pre>
</details>
<details><summary>(no output)</summary></details>

Where are my package databases?

<samp>$ <kbd>cabal exec &#x2D;&#x2D; bash c &apos;cat $(printenv GHC_ENVIRONMENT) | head -n 20&apos;</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
Configuration is affected by the following files:
- cabal.project
-- This is a GHC environment file written by cabal. This means you can
-- run ghc or ghci and get the environment of the project as a whole.
-- But you still need to use cabal repl $target to get the environment
-- of specific components (libs, exes, tests etc) because each one can
-- have its own source dirs, cpp flags etc.
--
clear-package-db
global-package-db
package-db /home/isaac/.local/state/cabal/store/ghc-9.8.4-4a67/package.db
package-db /home/isaac/youtube-feeds/dist-newstyle/packagedb/ghc-9.8.4
package-id youtube-feeds-atom-0.1.0.0-inplace
package-id base-4.19.2.0-32b8
package-id ghc-bignum-1.3-455a
package-id ghc-prim-0.11.0-3f63
package-id rts-1.0.2
package-id bytestring-0.12.1.0-2a7c
package-id deepseq-1.5.1.0-88f5
package-id template-haskell-2.21.0.0-bbd9
package-id ghc-boot-th-9.8.4-77b2
package-id pretty-1.1.3.6-6e96
</samp></pre>
</details>

Generate the facts.

<samp>$ <kbd>ghc-pkg &#x2D;&#x2D;package-db=/home/isaac/.local/state/cabal/store/ghc-9.8.4-4a67/package.db &#x2D;&#x2D;package-db=/home/isaac/youtube-feeds/dist-newstyle/packagedb/ghc-9.8.4 dot | python3 dot2pl.py > deps.pl</kbd></samp>
<details><summary>(no output)</summary></details>

Now add some definitions for graph manipulation.

<p class="contains-snippet">
`why_depends` is named after [`nix why-depends`](https://nix.dev/manual/nix/2.26/command-ref/new-cli/nix3-why-depends),
which is the canonical way to ask this question of installed Nix derivations.
<a class="snippet" id="why-depends-inspiration" href="#why-depends-inspiration">Part of my inspiration for this investigation is the sense that a question like `why-depends` shouldn't be baked into CLI of a tool like Nix.
Instead, there should be a language in which it's easy to ask that question and all the other questions that would never have been anticipated by the tool's authors.</a>
<a class="snippet-anchor" href="#why-depends-inspiration">#</a>
</p>

<details class="shell-command" open>
<summary>
<samp>$ <kbd>cat > graph.pl <<EOF</kbd></samp>
</summary>
<pre><samp>$ <kbd>cat > graph.pl &lt;&lt;EOF
% Identify nodes.
node(X) :- depends_on(X, _).

% Identify paths from one node to another.
why_depends(X, Y, [X, Y]) :- depends_on(X, Y).
why_depends(X, Z, [X, Y | P]) :- depends_on(X, Y), why_depends(Y, Z, [Y | P]).
EOF
</kbd></samp></pre>
</details>
<details><summary>(no output)</summary></details>

## Exploration

<samp>$ <kbd>gprolog</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>GNU Prolog 1.5.0 (64 bits)
Compiled Jul  8 2021, 09:35:47 with gcc
Copyright (C) 1999-2025 Daniel Diaz

| ?-
</samp></pre>
</details>

<samp>| ?- <kbd>[deps]. [graph].</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
compiling /home/isaac/youtube-feeds/deps.pl for byte code...
/home/isaac/youtube-feeds/deps.pl compiled, 335 lines read - 53725 bytes written, 8 ms

(2 ms) yes
compiling /home/isaac/youtube-feeds/graph.pl for byte code...
/home/isaac/youtube-feeds/graph.pl compiled, 4 lines read - 993 bytes written, 2 ms

yes
</samp></pre>
</details>

What dependencies do we have?

<samp>| ?- <kbd>setof(N, node(N), Ns).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
Ns = ['MemoTrie-0.6.11','QuickCheck-2.15.0.1','aeson-2.2.3.0','ansi-terminal-1.1.2','ansi-terminal-types-1.1','asn1-encoding-0.9.6','asn1-parse-0.9.5','asn1-types-0.3.4','async-2.2.5','attoparsec-0.14.4','base64-1.0','bifunctors-5.6.2','blaze-textual-0.2.3.1','case-insensitive-1.2.1.0','cborg-0.2.10.0','comonad-5.0.9','contravariant-1.5.5','cookie-0.5.0','crypton-1.0.1','crypton-connection-0.4.3','crypton-x509-1.7.7','crypton-x509-store-1.6.9','crypton-x509-system-1.6.7','crypton-x509-validation-1.6.13','data-default-class-0.2.0.0','data-fix-0.3.4','distributive-0.6.2.1','generics-sop-0.5.1.4','graphviz-2999.20.2.0','happy-lib-2.1.3','hashable-1.5.0.0','http-client-0.7.18','http-client-tls-0.3.6.4','http-date-0.0.11','http-media-0.8.1.1','http-semantics-0.3.0','http-types-0.12.4','http2-5.3.9','indexed-traversable-instances-0.1.2','integer-conversion-0.1.1','iproute-1.7.15','memory-0.18.0','network-control-0.1.3','network-uri-2.6.4.2','old-time-1.1.0.4','pem-0.2.4','pretty-show-1.10','prettyprinter-ansi-terminal-1.1.3','psqueues-0.2.8.0','quickcheck-state-machine-0.10.1','random-1.2.1.3','recv-0.1.0','scientific-0.3.8.0','semialign-1.3.1','semigroupoids-6.0.1','serialise-0.2.6.1','simple-sendfile-0.2.32','socks-0.6.1','sqlite-simple-0.4.19.0','streaming-commons-0.2.2.6','strict-0.5.1','temporary-1.3','text-iso8601-0.1.1','text-short-0.1.6','these-1.2.1','time-compat-1.9.8','time-manager-0.2.2','tls-2.1.6','unix-time-0.4.16','unliftio-0.2.25.0','unordered-containers-0.2.20','uri-encode-1.5.0.7','uuid-types-1.0.6','vault-0.3.1.5','vector-0.13.2.0','wai-3.2.4','warp-3.4.7','witherable-0.5','wl-pprint-text-1.2.0.2','youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0','z-happy-lib-z-backend-glr-2.1.3','z-happy-lib-z-backend-lalr-2.1.3','z-happy-lib-z-frontend-2.1.3','z-happy-lib-z-tabular-2.1.3','z-quickcheck-state-machine-z-no-vendored-treediff-0.10.1']

yes
</samp></pre>
</details>

Looks good.

I can't find `happy` in there at a glance.
Is `happy` in here?

<samp>| ?- <kbd>setof(N, (node(N), atom\_concat(happy, \_, N)), Ns).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
Ns = ['happy-lib-2.1.3']

yes
</samp></pre>
</details>

Cool. What are the names of my project's packages again?

<samp>| ?- <kbd>setof(N, Suffix^(node(N), atom_concat(&apos;youtube-feeds&apos;, Suffix, N)), Ns).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']

yes
</samp></pre>
</details>

Let's see whether the first one depends on `happy`.

<samp>| ?- <kbd>why_depends(&apos;youtube-feeds-log-0.1.0.0&apos;, &apos;happy-lib-2.1.3&apos;, Path).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
no
</samp></pre>
</details>

It doesn't. Let's check them all.

<samp>| ?- <kbd>setof(N, Suffix^(node(N), atom\_concat(&apos;youtube-feeds&apos;, Suffix, N)), Ns), member(Package, Ns), why_depends(Package, &apos;happy-lib-2.1.3&apos;, Path).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
no
</samp></pre>
</details>

Okay, that's surprising. I definitely saw `happy` being built.

Well, what depends on `happy-lib-2.1.3`?

<samp>| ?- <kbd>setof(P, depends_on(P, &apos;happy-lib-2.1.3&apos;), Ps).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
no
</samp></pre>
</details>

Hm. Okay, I think that `happy` is an *executable* required for the build of another package.

Since that didn't work out, let's play with this approach by looking for the ways I depend on `cborg-0.2.10.0`.

<samp>| ?- <kbd>setof(N, Suffix^(node(N), atom\_concat(&apos;youtube-feeds&apos;, Suffix, N)), Ns), member(Package, Ns), why_depends(Package, &apos;cborg-0.2.10.0&apos;, Path).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-oauth2-0.1.0.0'
Path = ['youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-oauth2-0.1.0.0'
Path = ['youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-server-0.1.0.0'
Path = ['youtube-feeds-server-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-server-0.1.0.0'
Path = ['youtube-feeds-server-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-server-0.1.0.0'
Path = ['youtube-feeds-server-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0','http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-server-0.1.0.0'
Path = ['youtube-feeds-server-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0','http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-server-0.1.0.0'
Path = ['youtube-feeds-server-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-server-0.1.0.0'
Path = ['youtube-feeds-server-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-youtubev3-api-0.1.0.0'
Path = ['youtube-feeds-youtubev3-api-0.1.0.0','http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-youtubev3-api-0.1.0.0'
Path = ['youtube-feeds-youtubev3-api-0.1.0.0','http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-youtubev3-api-0.1.0.0'
Path = ['youtube-feeds-youtubev3-api-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

Ns = ['youtube-feeds-log-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','youtube-feeds-server-0.1.0.0','youtube-feeds-sql-0.1.0.0','youtube-feeds-youtubev3-api-0.1.0.0']
Package = 'youtube-feeds-youtubev3-api-0.1.0.0'
Path = ['youtube-feeds-youtubev3-api-0.1.0.0','youtube-feeds-oauth2-0.1.0.0','http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'] ? ;

(2 ms) no
</samp></pre>
</details>

There's a lot of duplication because some of the project's packages depend on each other.
I think a better way to ask this question is to get all the project's direct dependencies that transitively depend on the target.

<samp>| ?- <kbd>assertz((project\_packages(X) :- setof(N, Suffix^(node(N), atom_concat(&apos;youtube-feeds&apos;, Suffix, N)), X))).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
yes
</samp></pre>
</details>

<samp>| ?- <kbd>findall(Path, (project\_packages(Packages), member(Package, Packages), depends\_on(Package, Dep), \\+ member(Dep, Packages), why_depends(Dep, &apos;cborg-0.2.10.0&apos;, Path)), Results).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
Results = [['http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'],['http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'],['http-client-tls-0.3.6.4','crypton-connection-0.4.3','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0'],['http-client-tls-0.3.6.4','tls-2.1.6','serialise-0.2.6.1','cborg-0.2.10.0']]

yes
</samp></pre>
</details>

It looks like my project only depends on `cborg-0.2.10.0` via `http-client-tls-0.3.6.4`.
Let's verify that more concisely.

<samp>| ?- <kbd>assertz((transitively\_depends\_on(X, Y) :- depends_on(X, Y))).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
yes
</samp></pre>
</details>

<samp>| ?- <kbd>assertz((transitively\_depends\_on(X, Z) :- depends\_on(X, Y), transitively\_depends_on(Y, Z))).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
yes
</samp></pre>
</details>

<samp>| ?- <kbd>setof(Dep, Packages^Package&Hat;(project\_packages(Packages), member(Package, Packages), depends\_on(Package, Dep), \\+ member(Dep, Packages), transitively\_depends\_on(Dep, &apos;cborg-0.2.10.0&apos;)), Results).</kbd></samp>
<details open>
<summary>(output)</summary>
<pre><samp>
Results = ['http-client-tls-0.3.6.4']

yes
</samp></pre>
</details>

## Thoughts and ideas

* Include build tool dependencies in the graph.

  I didn't manage to figure out how my project transitively depends on `happy` because it's probably used as a build tool somewhere.
  The GHC package database doesn't seem to store build tool information,
  so the next step would be to find all my dependencies' Cabal files and include [`build-tool-depends`](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-build-tool-depends) information in my Prolog facts.

* Create a dependency graph for all of Hackage.

  To really test the performance of this sort of query language, build a facts database for the entirety of Hackage
  by extracting Cabal files from the Hackage index.

* Query the Nix store using this approach.

  The Nix store's dependency graph was already [on my mind](#why-depends-inspiration), so why not try it out?

* Datalog & Postgres?

  I really enjoyed writing these Prolog queries. They make SQL seem clunky in comparison.
  I'm be interested in trying Datalog for some typical ["OLTP"](https://en.wikipedia.org/wiki/Online_transaction_processing) / "CRUD" applications,
  and it would be nice to avoid reinventing DBMS wheels.

  * I'd love to try a Datalog frontend for Postgres.
  * Failing that, a Datalog to SQL compiler would work.

* Standalone relational storage engines?

  It would be cool if relational databases like Postgres and SQLite had explicitly decoupled storage engines.
  My (lay) impression is that these are monolithic systems that aren't intended to multiple frontends to the same underlying storage.

  A standalone relational storage engine would be a C library that efficiently stores and searches records on disk or in memory.
  This library can then be used by a DBMS for storage management.
  Because it's a standalone library, you're no longer locked in to that particular DBMS.
  If I want a Datalog frontend, I can write it using the same storage library and have something that's compatible with existing databases.

  The pieces are probably all there to do this with something like Postgres;
  the storage format is [well-documented](https://www.postgresql.org/docs/current/storage.html) and I wouldn't be surprised if it was appropriately decoupled in the Postgres codebase.
  But I feel like the monolithic style of DBMSs discourage this kind of thinking.
  In contrast, imagine something like "[ZeroMQ](https://zeromq.org/) for database storage".
