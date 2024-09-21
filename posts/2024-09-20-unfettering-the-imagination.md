---
title: Unfettering the imagination
permalink: /unfettering-the-imagination
date: 2024-09-22
---

[`xeval`](https://github.com/LightAndLight/xeval) is a small and not particularly useful program that arose from an important thought process.
This post the result of exploring that thought process&mdash;trying to articulate it to myself&mdash;but without writing too much on the topic. 

I want to gesture at what I've been thinking via a concrete example: the evolution of mobile phone controls.
We interacted with early mobile phones by pressing a fixed set of physical buttons, and in the mid-to-late 2000s switched to touch screens.
I imagine that in the early days of that transition it was all too easy to treat the touch screen as a canvas for virtual buttons.
But "thinking in buttons" misses the potential of touch screens.

For example, I'd suggest that no amount of "thinking in buttons" will lead one to consider using a swipe gesture as an input.
To have a chance at inventing "swipe", one has to stop looking through the established "how" (buttons; discrete 2D regions with a binary input) and start from the "what" (e.g. asking for a new page to appear on the screen).
Only then does one have the freedom to imagine that dragging a finger across the display could correspond to dragging one page out of view and the next page into view.

And the *idea* of a touch screen required even more of this kind of imaginative freedom.
Moving from physical buttons to touch screens was a greater technological leap than from virtual touch screen buttons to swipe gestures,
so the idea of a touch screen would have seemed even more incredible in the absence of an implementation.
Yet this idea was still valuable 1, 5, or 10 years before it was possible to implement, because it set a direction for research.
To be able to have such ideas, one has to stop thinking so completely in terms of what is currently feasible. 

When using a computing system to achieve a goal, so much of the "how" is dictated by software.
Software is designed and created by people, which means the rules enforced by the are necessarily more malleable than, say, physics or chemistry.
You have to "click X to close the window" not because that's fundamentally how the universe works, but because people chose to make it so.
All of those concepts&mdash;clicking, virtual X button, closing, windows&mdash;were imagined by people and defined in software.

As a computer user, it's easy for me to forget this.
When I learn that "software X can't do Y", I instinctively interpret this is a statement of possibility, like "you can't build a perpetual motion machine".
In most cases this instinct is wrong, and the reason that "software X can't do Y" is because it just wasn't built that way.
I also forget that being a programmer, I'm in as good a position as any to overcome whatever software problems I recognise.
I don't have to be subject to my tools, because I can build my own.
It's weird that I have to remind myself of this, because in general I enjoy improving the process of computing, and particularly programming.
Many of my areas of interest (programming languages, version control systems, operating systems, and databases, to name a few) are related to me wanting to improve my craft.

In the same way that "thinking-in-buttons" fails to capture the full potential of touch screens,
the things we take for granted about our current computing systems could put a limit on our progress in computing.
Dreaming is the impetus to break out of the local optimum.

At this point, I think I'm ready to state the idea I've been playing with in a succinct, general form:

**I suspect there's a limit to the progress we can make from within any particular paradigm, and that technological leaps from come from inventing new paradigms.**
**These new paradigms are first imagined, irrespective of feasibility, while being grounded in our human needs and desires.**

The idea behind `xeval` came from trying this on.
I was typing a message and knew the formula for some number but didn't want to do the arithmetic in my head.
I know "how" to use my computer to help: copy the arithmetic, boot up a calculation program in another window, paste the arithmetic, ask the program to compute the answer, copy the answer, then paste the answer into the original text.
But zooming out, I saw that the "what" is much simpler: I want to evaluate the formula I just typed.

So I allowed myself to dream of better ways.
What if I could just point at any editable text and say "evaluate this"?
It felt like a good idea regardless of what my system was capable of at the time.
This time I was lucky; I was able to get pretty close to the original idea using standard tools.
But I expect that if I keep thinking in this direction, I'll come up with other good-feeling ideas that are less compatible with my current software paradigm, so I'll have to do more work to make them real.
