---
title: Conditional Probabilities and Obnoxious White Guys
author: ielliott95
permalink: /conditional-probabilities-obnoxious-white-guys
tags:
    - rationality
    - mathematics
math: true
---

In [this Twitter thread](https://twitter.com/DrEugeniaCheng/status/1124795257814691841),
Eugenia Cheng talks about how she presented at a conference for women in STEM, and was
confronted by a white guy who felt 'called out' by some of her anecdotes. Apparently, she
described interactions with obnoxious individuals in a professional setting, and noted that
they were all white guys. Ironically, the guy raised the issue in such a way that he was added
to the list of anecdotes.

Later, Dr. Cheng [clearly states her position](https://twitter.com/DrEugeniaCheng/status/1124805201452515328).

> ...people being obnoxious to me professionally are almost all white guys...

Many people take her word for it, but some (particularly white guys) are skeptical. I'm going to
quickly examine some degrees of belief in her claim through the lens of probability theory.

---

To begin, I want to explain why I have bothered to think about this. I believe Eugenia's statement;
I have no reason to think that she's mistaken or lying. But in spite of this, for some reason, I
empathised with the aforementioned white guy. Why? My unease wasn't caused by any conscious
reasoning process; it just seemed to arise of its own accord.

I've been learning that ["focusing"](https://medium.com/@ThingMaker/focusing-for-skeptics-6b949ef33a4f)
can be a helpful way to unpack these confusing feelings. I didn't go all-out focusing on this,
but throughout my inquiry I made sure to be conscious of how my reasoning interacted with the feeling.

To paint a better picture of the feeling, it was something like:

* "Tenseness"
* "Singled out"
* "I feel like I'm about to fight something"

To re-iterate: I don't think these feelings were rational, which is why I decided to keep digging. Let's
get into it. I'm going to assume basic familiarity with probability theory, and an understanding that
[probability is in the mind](https://www.lesswrong.com/posts/f6ZLxEWaankRZ2Crv/probability-is-in-the-mind).

I think Eugenia's claim is this:
$P( \text{person was white guy} \; | \; \text{interacted with obnoxious person} ) > 0.5$. In other
words: of all the obnoxious researchers she's interacted with, most are white guys. Let's look at the
conditional probability in terms of Bayes' Theorem:

$$ P(WG | O) = \frac{P(O | WG) \cdot P(WG)}{P(O)} $$

To start with, I'll plug in my estimates to show why I don't disagree with her.

I think mathematics is pretty
male-dominated, so I'm going to say $P(WG) = 0.7$. Seven in ten researchers she meets are white dudes.

Let's then say that $P(O) = 0.1$ &mdash; one in ten researchers she interacts with are jerks (am I optimistic or
pessimistic about the academic community?).

Lastly there's $P(O | WG)$: of all the white male researchers she meets, how many act
obnoxiously? I'm going to be charitable here and say (for demonstration purposes) that the white guys are
no more jerkish on average, so one in ten white male researchers she interacts with are jerks to her.
$P(O | WG) = 0.1$.

Now, compute!

$$
\begin{aligned}
~ & \frac{P(O | WG) \cdot P(WG)}{P(O)} \\\\
= & \; \frac{0.1 \cdot 0.7}{0.1} \\\\
= & \; 0.7
\end{aligned}
$$

My estimate is consistent with her statement - of all the obnoxious researchers she meets, seven in ten
would be white guys, even when assuming zero racial/gender biases.

Suppose you disagree with me. That is, your estimates are such that $P(WG | O) \le 0.5$. There are two ways
to disagree here:

1. A lower ratio $\frac{P(O | WG)}{P(O)}$. You might take
  $P(O | WG) = 0.07$, which  means $P(WG | O) = 0.49$. You might instead take
  $P(O) = 0.14$ for a similar result. Either way, you're claiming that the fragment of
  white male researchers Dr. Cheng meets are nicer on average than the general population of
  researchers she has met.

2. A lower $P(WG)$, indicating that you think Dr. Cheng interacts with relatively fewer white
   male researchers.

Running through these calculations didn't give me any closure. I agree on paper, and feel that my estimates are
appropriate. In fact, I would take $\frac{P(O | WG)}{P(O)}$ to be slightly greater than one, to account
for biases like sexism and racism. But that only means I agree more.

The idea that 'clicked' with me, that immediately resolved my inner turmoil, was this: somehow I'm implicitly
turning
$P(WG | O)$ into $P(O | WG)$. $P(O | WG)$ is the term from which stereotypes are born. If most
of the white guys you meet are jerks, then your $P(O | WG)$ is high. If you don't quotient that by the
proportion of people who are rude to you in general, then you have a gratuitous stereotype. If you do then
you're completely justified in thinking that 'white guy' and 'obnoxious' are correlated. So I think that somehow
my wires were crossed and I was subconsciously interpreting the conversation as purely a statement about
$P(O | WG)$.

I think this kind of error falls in the category of
[attribute substitution](https://en.wikipedia.org/wiki/Attribute_substitution), and I think it's pretty common.
For example in [Julia Galef's video about Bayes' Theorem](https://www.youtube.com/watch?v=BrK7X_XlGB8), she says that before
students learn Bayes' Theorem, they often give $P(B | A)$ when asked for $P(A | B)$. I
don't know how exactly this sort of thing happens &mdash; maybe I'll explore that some other time.

Anyway, I'm glad that my feelings and beliefs are now in sync on this issue.
