---
title: Why does Pandoc give me crappy syntax highlighting?
date: 2023-06-14
permalink: /pandoc-crappy-syntax-highlighting
tags:
- programming
---

I recently moved my site generator [from Jekyll to
Hakyll](https://blog.ielliott.io/jekyll-to-hakyll), and the syntax highlighting suffered. I wanted
to know what was happening so that I might be able to improve things. Here's what I learned.

First, a comparison. Here's how Jekyll and Hakyll use HTML to highlight this Rust code: `Expr::Var(n) => Expr::Var(f(*n)),`
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

Hakyll's markup is way less granular than Jekyll's. For example, `Expr::` is marked up as a single token, and
parentheses aren't marked up as tokens.
[Here](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-format-blaze-html/src/Skylighting/Format/HTML.hs#L22-L55)
is the "reference" for the CSS classes Pandoc uses - `pp` means "proprocessor token". Why is `Expr::` marked up as a "preprocessor token"? [Wat!?](https://www.destroyallsoftware.com/talks/wat)

When I first noticed this issue I blamed [Pandoc](https://pandoc.org/) because that's what my [Hakyll](https://jaspervdj.be/hakyll/) generator
uses for document processing.
After a bit of digging I realised that Pandoc isn't responsible for syntax highlighting itself:
it uses a package called [`skylighting`](https://github.com/jgm/skylighting) ([Hackage](https://hackage.haskell.org/package/skylighting)).
I chose to keep the title in the hope that other people with a similar problem will more easily find
this article.

So what about `skylighting` leads to the crappier syntax highlighting? If I knew that then I might be able to improve it.

`skylighting` contains [a repository](https://github.com/jgm/skylighting/blob/master/skylighting-core/xml/) of
XML files that how to mark up code[^1]. The XML format isn't exclusive to the `skylighting` project; it's the format
used by [KDE's Kate text editor](https://docs.kde.org/stable5/en/kate/kate/). The `skylighting` project actually
[uses](https://github.com/jgm/skylighting/blob/2f25c08651beee10b80242fd9c6a1c66328148d8/Makefile#L40-L48) the [original
highlighting files](https://github.com/KDE/syntax-highlighting/tree/master/data/syntax) defined by KDE.

My original complaint about `Expr::` is due to the [Rust syntax XML
file](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml) 
marking up `<ident>::` as a "preprocessor token"[^2]
([`rust.xml#L39`](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml#L39),
[`rust.xml#L389`](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml#L389),
[`rust.xml#L479`](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml#L479)).
The [Haskell syntax highlighting](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/haskell.xml)
is similarly anemic, for example marking up `name ::` in `name :: A -> B` as "other token" (instead of marking `name` as a name and `::` as an operator).
I even found an issue with the [HTML 
highlighter](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/html.xml)
while writing this post. The HTML attribute names are marked up as "error tokens", which I'm
suppressing in this article by overriding the error token class style.

[Jekyll](https://jekyllrb.com/) uses the [`rouge`](https://github.com/rouge-ruby/rouge) library for syntax highlighting, and
suffers from none of the aforementioned issues because it defines its own syntax highlighting rules ([Rust rules](https://github.com/rouge-ruby/rouge/blob/5c052c2744515981f2720b1a4ee37b1123b0bae1/lib/rouge/lexers/rust.rb#L6),
[Haskell rules](https://github.com/rouge-ruby/rouge/blob/5c052c2744515981f2720b1a4ee37b1123b0bae1/lib/rouge/lexers/haskell.rb#L6),
[HTML rules](https://github.com/rouge-ruby/rouge/blob/5c052c2744515981f2720b1a4ee37b1123b0bae1/lib/rouge/lexers/html.rb#L6)). I was
pleasantly surprised by how clean and concise `rouge`'s syntax rules look. They're written in a Ruby DSL that appears carefully
crafted and well suited to the task. The end result is files that are less than half the size of their corresponding Kate XML files,
and with much less visual clutter. 

In my opinion, the Kate XML files bundled with `skylighting` produce lower quality syntax highlighting than `rouge`. It looks like
the crappy syntax highlighting can be improved by changing the XML. If I were on a deadline then I'd create some patches and try
to get them upstreamed. But because I have the time, I'd like to indulge myself: I think this is a poor use of XML would like to
avoid contributing to it.

Begin rant.
XML is an okay markup language and a bad data format[^xml], and Kate's syntax highlighting files are an example of the XML-as-a-data-format antipattern.
It seems like the main purpose of these files is not written communication, like a page of a book.
Rather, these files store processing instructions for a syntax highlighting program.
One clue is the number of empty tags they use (e.g. [rust.xml#L372-L409](https://github.com/jgm/skylighting/blob/da282a2c521e85417c9f73116b36cbc68e01ecbf/skylighting-core/xml/rust.xml#L372-L409)).
Empty tags are only occasionally useful when marking up a document intended for written communication.
Tags normally have contents because they provide context for spans of text (e.g. bold this, italicise that).
In the Kate XML files, empty tags proliferate because they're used to represent [product types](https://en.wikipedia.org/wiki/Product_type).
End rant.

`rouge` is an improvement because it uses Ruby as its data format.
Ruby (like most other programming languages) is a decent choice for a data format because it has concise syntax for common data structures (e.g. strings, records, lists, maps)
and supports comments so that developers can store documentation alongside their data.
The main downside is that the data isn't very portable; it's easy to use from the language in which it's defined, but not so much from other languages.
If I wanted to use the `rouge` syntax highlighting definitions in Haskell then I'd need to use some subset of a Ruby parser. Seems a little funny.
The fact that Kate uses a ubiquitous format XML makes the data easier to reuse in libraries like `skylighting`.
This is important to me.

This means that I'd prefer a standardised data format for syntax highlighting descriptions.
It should support common data structures and comments.
Two candidates that come to mind are [YAML](https://yaml.org/) and [Dhall](https://dhall-lang.org/).

A data format is a choice of meta-syntax for a syntax highlighting system.
Next I'll think about semantics.
Once the semantics are nailed down, I can develop a suitable syntax by intution.

* The responsible thing to do would be to improve the XML and upstream it
* I have an ethical objection to using XML this way
  * XML is not a data format, it's a markup language https://www.devever.net/~hl/xml, https://borretti.me/article/brief-defense-of-xml
  * A syntax highlighter is not a document, like a research paper (bad fit for markup)
  * Maybe not "pure" data, like weather measurements or settings for your editor
  * A syntax highlighting spec is a program, often a finite state machine
  * XML is not a good syntax for a programming language!
    * I think this is part of why rouge seems so good in comparison to Kate's XML: Ruby has a much
      nicer syntax for this problem.
* Since I like `rouge`, can I call it from Haskell to generate the HTML I want?
  * Pro: reuse existing grammar definitions
  * Con: `rougify` doesn't do it out of the box, would have to write my own ruby script to call out to
* Reuse `rouge` by parsing the subset of Ruby used in its DSL
  * Too much effort?
  * Is this a reason to use a portable data format instead of a programming langauge DSL?
* Using textmate grammars instead (I haven't noticed any issues with VS Code's highlighting)
  * Call out to [vscode-textmate](https://github.com/microsoft/vscode-textmate)?
  * Write a Haskell-based textmate tokenizer and scrape together textmate grammars from the internet?
    * Difficulty: mapping textmate scopes to pygments tokens (common)
    
* Divide syntax highlighting into three parts
  * Rule definition syntax
    * `rouge` - ruby
    * `pygments` - python
    * Tree sitter - [sexprs - highlights query format](https://tree-sitter.github.io/tree-sitter/syntax-highlighting#highlights)
    * Kate editor - XML (Kate markup)
    * `skylighting` - XML (Kate markup)
    * Textmate - format: [textmate property list](https://macromates.com/manual/en/appendix#property-list-format), structure: [textmate language grammar](https://macromates.com/manual/en/language_grammars)
    * VS code - [JSON / YAML formatted textmate grammar](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#tokenization)
  * Rule semantics
    * `rouge`, `pygments`, Kate editor, textmate, VS code: tokenization (annotate the text span that matches a regex)
    * Tree sitter: tree pattern matching
  * Markup - syntax highlighters convert your text to a tree, with annotations in the branches
    * `pygments` - Pygments tokens
    * `rouge` - Pygments tokens
    * Tree sitter - user-defined "highlight names"
    * Kate editor - Kate [default styles](https://docs.kde.org/stable5/en/kate/katepart/highlight.html#kate-highlight-default-styles)
    * `skylighting` - Kate default styles -> Pandoc CSS classes (as far as I can tell)
    * Textmate - [textmate scopes](https://macromates.com/manual/en/language_grammars#naming_conventions)
    * VS code - [textmate scopes](https://macromates.com/manual/en/language_grammars#naming_conventions) ([reference](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#textmate-tokens-and-scopes))

[^1]: Source: <https://pandoc.org/chunkedhtml-demo/13-syntax-highlighting.html>
[^2]: See also: `dsPreprocessor` in <https://docs.kde.org/stable5/en/kate/katepart/highlight.html>
[^xml]: [XML is almost always misused](https://www.devever.net/~hl/xml) and [A Brief Defense of XML](https://borretti.me/article/brief-defense-of-xml)
    are two of my favourite articles on the topic.

