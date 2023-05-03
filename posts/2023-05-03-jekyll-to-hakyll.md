---
title: Migrating from Jekyll to Hakyll
permalink: /jekyll-to-hakyll
tags:
- programming
---

I've just finished migrating this site from [Jekyll](https://jekyllrb.com/) to
[Hakyll](https://jaspervdj.be/hakyll/).

I used Jekyll because that's what [GitHub
Pages](https://pages.github.com/) recommended when I first set things up.

The turning point was wanting to add [MathML](https://developer.mozilla.org/en-US/docs/Web/MathML)
support. Until now, this site rendered LaTeX equations on the client side using
[MathJax](https://www.mathjax.org/). While working on a math-heavy post, I realised this was a
little bit silly. I'm using a static site generator - why not render the equations at generation
time?

I started exploring possible solutions, and found [`texmath`](https://github.com/jgm/texmath),
which is used in [Pandoc](https://pandoc.org/) to [convert TeX to
MathML](https://pandoc.org/MANUAL.html#math-rendering-in-html). No search results stood out when I
looked for ways to make Jekyll do something similar. So I figured it was time to write some Haskell.

MathML [still isn't ubiquitous](https://caniuse.com/mathml), so I added a [polyfill
script](https://github.com/LightAndLight/lightandlight.github.io/blob/2124670c349ce879a441ea01b19cbdfe42c031bf/js/mathml-polyfill.js)
based on [fred-wang/mathml-warning.js](https://github.com/fred-wang/mathml-warning.js). If you read
a math post in a web browser that has poor MathML support, you'll be asked whether you want to
improve the experience by loading external resources:

<img src="./images/mathml-warning.png" alt="Screenshot of a warning message for browsers with poor
MathML support" style="max-width: 100%" />

* spent a lot of time massaging
  * Jekyll was a lot more "just works" - seo/sitemap/feed plugins
  * I had to organise my code and make it pretty
  * Okay for me right now because I'm not working
* syntax highlighting (skylighting) is crap
* pandoc footnotes not great
  * duplicates footnotes when they're referenced multiple times. bad for citations.
  * adds a hseparator (no thanks!)