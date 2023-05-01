---
title: On Programming Language Design
author: ielliott95
permalink: /on-programming-language-design
tags:
    - programming
    - philosophy
---

In his [propositions as types
talk](https://www.youtube.com/watch?v=IOiZatlZtGU), Philip Wadler contrasts
programming languages that have been 'invented' with those that have been
'discovered'. ["And you can tell!" he exclaims, receiving giggles from the
audience](https://www.youtube.com/watch?v=IOiZatlZtGU&t=28m12s). Regardless of
how you feel about [mathematical
platonism](https://en.wikipedia.org/wiki/Philosophy_of_mathematics#Platonism),
I think Phil is pointing at a meaningful difference in the way certain ideas
arise.

--- 

[Vacuum cleaner](https://en.wikipedia.org/wiki/Vacuum_cleaner) is just not 'the
same kind of thing' as [monoid](https://en.wikipedia.org/wiki/Monoid).

On one hand, we have something that was built to accomplish a task. Vacuum cleaners
suck stuff up. Some of them have wheels and some go on your back. Some have
carpet cleaning attachments and some are adapted for tile floors. In the end, we
have an object that was made to clean, made of bits that we can point to and say
"this bit helps it suck better" or "this bit makes it easier to move" or "this
bit makes it look pretty".

On the other hand, we have something that describes how things can relate to
each other. When you say "the natural numbers with addition and 0 form a
monoid", you impose some structure onto the natural numbers. We can prove
whether or not the naturals do exhibit this structure, and then use that fact to
inform how we should think about them. We can't 'point at bits of monoid' and
say how much they contribute to some purpose.

It seems like the popular perception of programming languages falls more in the
'vacuum cleaner' camp: that a programming language is *just* something for
translating a description of computation into something machine-readable. If you
want to describe computations involving numbers and strings, then you add 'do
number stuff' and 'do string stuff' features to the language. If you find that
'X-Y-Z' is a common coding pattern, you might introduce 'W' syntax, which does
'X-Y-Z' but is easier to type.

I think that this 'features focused' development style can cause people to
ignore too much of the structure that the features contribute to
(or even that the features are *destroying* structure).
Programming language 'design' needs a lot of what goes in the 'monoid' camp. That
is, they should be treated as more of an abstract thing that gives some
structure to computation. Ignoring this aspect of development is what leads to
edge cases, unintuitive behaviour, and this general feeling of 'poor design'.

How many people have been surprised to learn that [floating point addition is not
associative](http://www.walkingrandomly.com/?p=5380)? It seems reasonable to
just expect addition to be associative. Many programming language
['wats'](https://www.destroyallsoftware.com/talks/wat) exist for a similar
reason - they are examples of a language behaving counter to our expectations.
In both these cases there are implicit 'structural contracts' that are violated.
Hidden patterns about how things relate to each other that are so common we just take
them for granted, but are not present in certain systems by design.

So I think a big part of what makes a language feel 'discovered' as opposed to
'invented' is the amount of attention paid to the structure of the thing.
'Discovered'-seeming languages have more internal consistency and fewer
'quirks', because they're not meant to *just* 'turn descriptions of computations
into binary'. They have to do this in a way that adheres to a consistent,
coherent structure of computation.
