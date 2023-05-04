---
title: Why does Pandoc give me crappy syntax highlighting?
permalink: /pandoc-crappy-syntax-highlighting
tags:
- programming
---

I recently moved my site generator [from Jekyll to
Hakyll](https://blog.ielliott.io/jekyll-to-hakyll), and the syntax highlighting suffered. I wanted
to know what was happening so that I might be able to improve things. Here's what I learned.

First, a comparison. Here's how Jekyll and Hakyll highlight this Rust code: `Expr::Var(n) => Expr::Var(f(*n)),`
(newlines and comments added for readability).

Jekyll:

<style>.sourceCode.html .er { color: inherit; font-weight: inherit; }</style>

```html
<span class="nn">Expr</span>
<span class="p">::</span>
<span class="nf">Var</span>
<span class="p">(</span>
<span class="n">n</span>
<span class="p">)</span>
<!-- space -->
<span class="k">=&gt;</span>
<!-- space -->
<span class="nn">Expr</span>
<span class="p">::</span>
<span class="nf">Var</span>
<span class="p">(</span>
<span class="nf">f</span>
<span class="p">(</span>
<span class="o">*</span>
<span class="n">n</span>
<span class="p">)),</span>
```

Hakyll:

```html
<span class="pp">Expr::</span>
Var(n)<!-- space -->
<span class="op">=&gt;</span>
<!-- space -->
<span class="pp">Expr::</span>
Var(f(
<span class="op">*</span>
n))
<span class="op">,</span></span>
```

Hakyll's markup is way less granular than Jekyll's. `Expr::` marked up as a single token, and
parentheses aren't marked up as tokens. Yucky.

[Jekyll](https://jekyllrb.com/) uses [`rouge`](https://github.com/rouge-ruby/rouge) for syntax
highlighting. My [Hakyll](https://jaspervdj.be/hakyll/) generator uses [`skylighting`](https://github.com/jgm/skylighting) by way of
[Pandoc](https://pandoc.org/). When I first noticed this issue, I blamed Pandoc, hence the title.
After a bit of digging I realised that Pandoc isn't responsible for syntax highlighting itself. I
chose to keep the title in the hope that other people with a similar problem will more easily find
this article.

[Here](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-format-blaze-html/src/Skylighting/Format/HTML.hs#L22-L55)
is the "reference" for the CSS classes Pandoc uses - `pp` means "proprocessor token". Why is `Expr::` marked up as a "preprocessor token". [Wat!?](https://www.destroyallsoftware.com/talks/wat)


It's not actually a problem with Pandoc. Pandoc uses
[`skylighting`](https://hackage.haskell.org/package/skylighting) for syntax highlighting, which
contains [a repository](https://github.com/jgm/skylighting/blob/master/skylighting-core/xml/) of
[Kate syntax highlighting](https://docs.kde.org/stable5/en/kate/katepart/highlight.html)
XML files describing how to
mark up code[^1]. The [Rust XML
file](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml) 
marks up `<ident>::` as a "preprocessor token"[^2]
([`rust.xml#L39`](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml#L39),
[`rust.xml#L389`](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml#L389),
[`rust.xml#L479`](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml#L479)).

I even found an issue with the [HTML 
highlighter](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/html.xml)
while writing this post. The HTML attribute names are marked up as "error tokens", which I'm
suppressing in this article by overriding the error token class style.

[^1]: Source: <https://pandoc.org/chunkedhtml-demo/13-syntax-highlighting.html>
[^2]: See also: `dsPreprocessor` in <https://docs.kde.org/stable5/en/kate/katepart/highlight.html>