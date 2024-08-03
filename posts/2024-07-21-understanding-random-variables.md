---
title: Understanding Random Variables
author: ielliott95
permalink: /understanding-random-variables
date: 2023-07-21T14:20:00+10
math: true
tags:
  - mathematics
---

For some reason I find it hard to wrap my head around [random variables](https://en.wikipedia.org/wiki/Random_variable).
This post is a practise session for working with them.

## ???

I like this probability theory intro from [Introduction to Bayesian Statistics by William M. Bolstad](https://onlinelibrary.wiley.com/doi/book/10.1002/9780470181188).

I do something that has an outcome.
Even I can do the same thing repeatedly and get different outcomes each time.
The outcome of the action is *uncertain*.
Bolstad uses the term "random experiment" for this repeatable-action-with-uncertain-outcome.

While the precise outcome is uncertain, there are things that are always true about the random experiment.
The most important property is the set of possible values, called the "sample space", often written as $\Omega$.
The relative frequency of outcomes might be surprising, but there are no surprising outcome *values* because the possible values were defined up front.

A subset of $\Omega$ is called an *event*.
Each random experiment has an associated set of events $\Sigma_{\Omega} \subseteq \mathcal{P}(\Omega)$.
Events can be unioned and intersected, and the complement of an event $A$ is $\tilde{A} = \Omega \minus A$.
The set of events associated with a random experiment must be closed under union, intersection, and complement[^sigma-algebra].

Events are associated with probabilities via a function
$P \; : \; \Sigma_{\Omega} \rightarrow [0, 1]$.
To be a probability measure, $P$ can't be arbitrary.
Some properties:

* $P(\Omega) = 1$
* $P(\emptyset) = 0$
* $A \cap B = \emptyset \Rightarrow P(A \cup B) = P(A) + P(B)$
* $P(A \cup B) = P(A) + P(B) - P(A \cap B)$
* $P(\tilde{A} = 1 - P(A))$

To connect the informal understanding of an "event" with the mathematical definition here,
you can say than an event $A \in \Sigma_{\Omega}$ corresponds to performing the random experiment and observing an outcome that's an element of $A$.
It is *certain* that a random experiment's outcome is in $\Omega$, and it's *impossible* that the outcome is in $\emptyset$.

## Random variables

A random variable $X$ is a *name* for the uncertain outcome of a specific random experiment.
Not for any specific potential outcome, but all possible outcomes all at once in proportion witheir probabilities.
Implicit in the definition of a random variable is a sample space $\Omega$,
a set of events $\Sigma_{\Omega}$,
and a probability measure $P \; : \; \Sigma_{\Omega} \rightarrow [0, 1]$.

Random variables are used in a kind of set-builder notation to define events.
For random variable $X$ with sample space $\Omega$, $X = x$ is another way to write event $\{ x \}$.
If the elements of $\Omega$ are ordered, we can write $X \le x$ for $\{ \; a \in \Omega \; | \; a \le x \; \}$.

This is combined with the probability measure $P$ to give the familiar probability descriptions like $P(X = x)$ (probability that running the random experiment yields outcome $x$)
and $P(X \le x)$ (probability that running the random experiment yields outcome less than or equal to $x$).

## Properties of random variables

* Probability density function: $\text{pdf}_X(x) = P(X = x)$

* Cumulative density function: $\text{cdf}_X(x) = P(X \le x)$

* Expected value: $\text{E}[X] = \int x \cdot \text{pdf}_X(x) \, dx = \int x \cdot P(X = x) \, dx$

Because a random variable $X$ stands for an unknown and uncertain outcome drawn from
$\Omega$,
$X$ can be transformed by a function $f \; : \; \Omega \rightarrow S$,
written $Y = f(X)$.
The new sample space $S$ must have its own set of events $\Sigma_S$.

When $Y = f(X)$:

* Probability density function
  
  $$
  \begin{align*}
  \text{pdf}_{Y}(y) & = P(f(X) = y) \\
                    & = P(\{ \; x \in \Omega \; | \; f(x) = y \; \}) \\
                    & = \sum_{x \in \{ \; x \in \Omega \; | \; f(x) = y \; \}} P(x)
  \end{align*}
  $$

* Cumulative density function
  
  $$
  \begin{align*}
  \text{cdf}_{Y}(y) & = P(f(X) \le y) \\
                    & = P(\{ \; x \in \Omega \; | \; f(x) \le y \; \}) \\
  \end{align*}
  $$

* Expected value

  $$
  \begin{align*}
  \text{E}[Y] & = \int y \cdot (\sum_{x \in \{ \; x \in \Omega \; | \; f(x) = y \; \}} P(x)) \, dy \\
              & = \int \sum_{x \in \{ \; x \in \Omega \; | \; f(x) = y \; \}} y \cdot P(x)) \, dy \\
              & = \int \sum_{x \in \{ \; x \in \Omega \; | \; f(x) = y \; \}} f(x) \cdot P(x)) \, dy \\
  \end{align*}
  $$

[^sigma-algebra]: <https://en.wikipedia.org/wiki/%CE%A3-algebra>
