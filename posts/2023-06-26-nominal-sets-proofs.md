---
title: "Nominal Sets: Appendix A (Proofs)"
author: ielliott95
permalink: /nominal-sets-proofs
date: 2023-06-30T06:40:00Z
excerpt: |
  The proofs for my <a href="nominal-sets">Nominal Sets article</a>.
math: true
tags:
  - mathematics
---

Here are the proofs for my <a href="nominal-sets">Nominal Sets article</a>.
This was a personal exercise to improve my mathematical reasoning; I don't expect anyone to read it.
That said, if you do find any mistakes then please let me know!

## Proof 1

Every finite permutation can be decomposed into a sequence of swaps.
<a href="nominal-sets#proof-1-link">↩</a>

$$
\forall \pi. \; \pi = (a_1 \; \pi(a_1)) \circ ... \circ (a_n \; \pi(a_n)) \text{ where } a_i \neq \pi(a_i) \neq \pi(a_j) \neq a_j
$$

$$
\begin{array}{l}
\text{Induction on } \{ \; x \; | \; \pi(x) \neq x \; \} \text{:}
\\
\text{case } \{ \; x \; | \; \pi(x) \neq x \; \} = \emptyset
\\
\; \; \; \; \text{trivial}
\\
\text{case } \{ \; x \; | \; \pi(x) \neq x \; \} = S \cup \{a\} \text{ where } a \notin S
\\
\; \; \; \; \text{assume } \forall \pi'. \; \pi'(a) = a \land \{ \; \pi'(b) = \pi(b) \; | \; b \in S \; \}
\;
\implies
\;
\pi' = (a_1 \; \pi(a_1)) \circ ... \circ (a_n \; \pi(a_n)) 
\\
\; \; \; \; \pi
\\
\; \; \; \; = (a \; \pi(a)) \circ (a \; \pi(a)) \circ \pi
\\
\; \; \; \; \; \; \; \; \pi' = (a \; \pi(a)) \circ \pi
\\
\; \; \; \; \; \; \; \; \; \; \; \; \pi'(a) = ((a \; \pi(a)) \circ \pi)(a) = (a \; \pi(a))(\pi(a)) = a
\\
\; \; \; \; \; \; \; \; \; \; \; \; \pi'(b) = ((a \; \pi(a)) \circ \pi)(b) = (a \; \pi(a))(\pi(b)) = \pi(b) \text{ for } b \neq a
\\
\; \; \; \; = (a \; \pi(a)) \circ (a_1 \; \pi(a_1)) \circ ... \circ (a_n \; \pi(a_n)) \; (\text{inductive hypothesis})
\end{array}
$$

## Proof 2

Every name must support itself: $\{a\} \; \text{supports} \; a$.
<a href="nominal-sets#proof-2-link">↩</a>

$$
\begin{array}{c}
\begin{array}{lll}
& \forall \pi. \;
(\forall a \in \{a\}. \; \pi(a) = a) \implies \pi \cdot a = a
\\
= & \forall \pi. \; \pi(a) = a \implies \pi \cdot a = a & (\text{singleton set})
\\
= & \forall \pi. \; \pi(a) = a \implies \pi(a) = a & (\text{definition of } \cdot)
\end{array}
\\ \; \\
\forall \pi. \; \pi(a) = a \implies \pi(a) = a \;\;\;\; \text{trivial}
\end{array}
$$

## Proof 3

$\exists b. \; b \neq a \; \land \; \{b\} \; \text{supports} \; a$ is false.
<a href="nominal-sets#proof-3-link">↩</a>

$$
\begin{array}{c}
\begin{array}{lll}
& \exists b. \; b \neq a \; \land \; \{b\} \; \text{supports} \; a
\\
= & \exists b. \; b \neq a \; \land (\forall \pi. \; (\forall a \in \{b\}. \; \pi(a) = a) \implies \pi \cdot a = a)
\\
= & \exists b. \; b \neq a \; \land (\forall \pi. \; \pi(b) = b \implies \pi \cdot a = a)
\\
\end{array}
\\
\begin{array}{l}
\text{assume } \exists b. \; b \neq a \; \land (\forall \pi. \; \pi(b) = b \implies \pi \cdot a = a)
\\
\text{counterexample: } \pi = (a \; c) \; (a \neq b \neq c)
\\
\; \; \; \; (a \; c)(b) = b \implies (a \; c)(a) = a
\\
\; \; \; \; \text{but } (a \; c)(a) = c \text{ --- contradiction}
\end{array}
\end{array}
$$

## Proof 4

For names $a$ and $b$, $\{a,b\} \; \text{supports} \; a$.
<a href="nominal-sets#proof-4-link">↩</a>

$$
\begin{array}{l}
\begin{array}{lll}
& \forall \pi. \; (\forall a \in \{a, b\}. \; \pi(a) = a) \implies \pi \cdot a = a
\\
= & \forall \pi. \; \pi(a) = a \land \pi(b) = b \implies \pi \cdot a = a &
(\text{expand set})
\\
= & \forall \pi. \; \pi(a) = a \land \pi(b) = b \implies \pi(a) = a &
(\text{definition of } \cdot)
\end{array}
\\ \; \\
\forall \pi. \; \pi(a) = a \land \pi(b) = b \implies \pi(a) = a \;\;\;\;
\text{trivial}
\end{array}
$$

## Proof 5

Uniqueness of minimal supports.
<a href="nominal-sets#proof-5-link">↩</a>

$$
\begin{array}{l}
\bar{a} \; \text{supports}_{min} \; x
\land
\bar{b} \; \text{supports}_{min} \; x
\implies
\bar{a} = \bar{b}
\\ \; \\
\begin{array}{ll}
&
\bar{a} \; \text{supports}_{min} \; x
\land
\bar{b} \; \text{supports}_{min} \; x
\\
= &
(\bar{a}, x) \in
\{ (\bar{a}, x) \; | \; 
\bar{a} \in \mathcal{P}(\mathbb{A}), \;
x \in X, \;
\bar{a} \; \text{supports} \; x, \;
\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x}
\} \;
\land
\\
&
(\bar{b}, x) \in
\{ (\bar{a}, x) \; | \; 
\bar{a} \in \mathcal{P}(\mathbb{A}), \;
x \in X, \;
\bar{a} \; \text{supports} \; x, \;
\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x}
\}
\\
= &
\bar{a} \; \text{supports} \; x \; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x})
\; \land \;
\\
&
\bar{b} \; \text{supports} \; x
\; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{b} \subseteq \bar{x})
\\ \; \\
&
\bar{a} \; \text{supports} \; x
\; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{b} \subseteq \bar{x})
\implies
\bar{b} \subseteq \bar{a}
\\
&
\bar{b} \; \text{supports} \; x
\; \land \;
(\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies
\bar{a} \subseteq \bar{x})
\implies
\bar{a} \subseteq \bar{b}
\\ \; \\
&
\bar{a} \subseteq \bar{b} \land \bar{b} \subseteq \bar{a} \implies a = b
\end{array}
\end{array}
$$

## Proof 6

The identity function is supported by the empty set.
<a href="nominal-sets#proof-6-link">↩</a>

$$
\begin{array}{l}
\forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x. \; \pi \cdot id(\pi^{-1} \cdot x) = id(x)
\\
\iff \; \forall \pi. \; \forall x. \; \pi \cdot id(\pi^{-1} \cdot x) = x
\\ \; \\
\pi \cdot id(\pi^{-1} \cdot x) = \pi \cdot \pi^{1} \cdot x = x
\end{array}
$$

## Proof 7

$\text{cmp}(a, b) = a \stackrel{?}{=} b$ is supported by the empty set.
<a href="nominal-sets#proof-7-link">↩</a>

$$
\begin{array}{l}
\forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; \pi \cdot \text{cmp}(\pi^{-1}
\cdot (x, y)) = \text{cmp}(x, y)
\\
\iff \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; \pi \cdot \text{cmp}(\pi^{-1}
\cdot x, \pi^{-1} \cdot y) = \text{cmp}(x, y)
\\
\iff \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; \pi \cdot ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = (x \stackrel{?}{=} y)
\\
\iff \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \forall x, y. \; ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = (x \stackrel{?}{=} y) \;\;\;\; (\text{booleans contain no names})
\\
\iff \forall \pi. \; \forall x, y. \; ((\pi^{-1}
\cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = (x \stackrel{?}{=} y)
\\ \; \\
\text{case } x = y
\\
\; \; \; \; \iff ((\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = \text{true}
\\
\; \; \; \; (\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)
\\
\; \; \; \; = (\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot x) \; (x = y)
\\
\; \; \; \; = \text{true}
\\ \; \\
\text{case } x \neq y
\\
\; \; \; \; \iff ((\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)) = \text{false}
\\
\; \; \; \; (\pi^{-1} \cdot x) \stackrel{?}{=} (\pi^{-1} \cdot y)
\\
\; \; \; \; = x' \stackrel{?}{=} y' \text{ where } x' \neq y' \;\;\;\; (\text{injectivity of } \pi^{-1})
\\
\; \; \; \; = \text{false}
\end{array}
$$

## Proof 8

$a$ and $b$ must be in the support of $\text{iffy}(x) = \text{if } a \stackrel{?}{=} x \text{ then } b
\text{ else } x$.
<a href="nominal-sets#proof-8-link">↩</a>

### When $a$ is missing

$$
\begin{array}{l}
\forall \pi. \; (\forall a \in \{ b \}. \; \pi(a) = a) \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\\
= \forall \pi. \; \pi(b) = b \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\\ \; \\
\text{can't prove for all } \pi, x \text{. counterexample: } \pi = (a \;
n), \; x = a
\\
\begin{array}{ll}
(a \; n) \cdot \text{iffy}((a \; n) \cdot a) & = (a \; n) \cdot \text{iffy}(n) = (a \; n) \cdot n = a
\\
\text{iffy}(a) & = b
\end{array}
\end{array}
$$

### When $b$ is missing

$$
\begin{array}{l}
\forall \pi. \; (\forall x \in \{ a \}. \; \pi(x) = x) \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\\
= \forall \pi. \; \pi(a) = a \implies \forall x. \; \pi \cdot \text{iffy}(\pi^{-1}
\cdot x) = \text{iffy}(x)
\\ \; \\
\text{can't prove for all } \pi, x \text{. counterexample: } \pi = (b \;
n), \; x = a
\\
\begin{array}{ll}
(b \; n) \cdot \text{iffy}((b \; n) \cdot a) & = (b \; n) \cdot \text{iffy}(a) = (b \; n) \cdot b = n
\\
\text{iffy}(a) & = b
\end{array}
\end{array}
$$

## Proof 9

Swapping fresh names does nothing.
<a href="nominal-sets#proof-9-link">↩</a>

$$
\begin{array}{l}
a \; \# \; x \land b \; \# \; x \implies (a \; b) \cdot x = x
\\ \; \\
a \; \# \; x \land b \; \# \; x
\\
\iff a \notin \text{support}_{min}(x) \land b \notin \text{support}_{min}(x)
\\
\iff a \notin \bar{x} \land b \notin \bar{x} \text{ for some } \bar{x} \text{ where } \bar{x} \; \text{supports} \; x \land (\forall \bar{y}. \; \bar{y} \; \text{supports} \; x \implies \bar{x} \subseteq \bar{y})
\\ \; \\
\bar{x} \; \text{supports} \; x
\\
\iff \forall \pi. \; (\forall a \in \bar{x}. \; \pi(a) = a) \implies \pi \cdot x = x
\\ \; \\
\pi = (a \; b)
\\
\forall c \in \bar{x}. \; (a \; b)(c) = c \; (a \notin \bar{x} \land b \notin \bar{x} \implies \forall c \in \bar{x}. \; a \neq c \land b \neq c)
\\
\therefore \; (a \; b) \cdot x = x
\end{array}
$$

## Proof 10

Freshness "distributes" across functions.
<a href="nominal-sets#proof-10-link">↩</a>

$$
\begin{array}{l}
a \; \# \; f \land a \; \# \; x \implies a \; \# \; f(x)
\\ \; \\
a \; \# \; f
\\
\iff a \notin \text{support}_{min}(f)
\\
\iff a \notin \bar{a}
\text{ for some } \bar{a} \text{ where } \bar{a} \; \text{supports} \; f \land (\forall \bar{b}. \; \bar{b} \; \text{supports} \; f \implies \bar{a} \subseteq \bar{b})
\\ \; \\
\bar{a} \; \text{supports} \; f
\\
\iff \forall \pi. \; (\forall a \in \bar{a}. \; \pi(a) = a) \implies \pi \cdot f = f
\\ \; \\
a \; \# \; x
\\
\iff a \notin \text{support}_{min}(x)
\\
\iff a \notin \bar{b}
\text{ for some } \bar{b} \text{ where } \bar{b} \; \text{supports} \; x \land (\forall \bar{a}. \; \bar{a} \; \text{supports} \; x \implies \bar{b} \subseteq \bar{a})
\\ \; \\
\bar{a} \; \text{supports} \; x
\\
\iff \forall \pi. \; (\forall a \in \bar{b}. \; \pi(a) = a) \implies \pi \cdot x = x
\\ \; \\
(\bar{a} \cup \bar{b}) \; \text{supports} \; f(x)
\\
\iff \forall \pi. \; (\forall a \in (\bar{a} \cup \bar{b}). \; \pi(a) = a) \implies \pi \cdot f(x) = f(x)
\\
\pi \cdot f(x)
\\
= (\pi \cdot f)(\pi \cdot x)
\\
= f(\pi \cdot x) \; ((\forall a \in (\bar{a} \cup \bar{b}). \; \pi(a) = a) \implies (\forall a \in \bar{a}. \; \pi(a) = a) \text{, } \bar{a} \; \text{supports} \; f)
\\
= f(x) \; ((\forall a \in (\bar{a} \cup \bar{b}). \; \pi(a) = a) \implies (\forall a \in \bar{b}. \; \pi(a) = a) \text{, } \bar{b} \; \text{supports} \; x)
\\ \; \\
a \notin (\bar{a} \cup \bar{b}) \; (a \notin \bar{a} \land a \notin \bar{b})
\\ \; \\
\text{given } \bar{c} \text{ where } \bar{c} \; \text{supports} \; f(x) \land (\forall \bar{b}. \; \bar{b} \; \text{supports} \; f(x) \implies \bar{c} \subseteq \bar{b})
\\
(\bar{a} \cup \bar{b}) \; \text{supports} \; f(x) \land a \notin (\bar{a} \cup \bar{b})
\\
\implies \bar{c} \subseteq (\bar{a} \cup \bar{b}) \land a \notin (\bar{a} \cup \bar{b}) \; (\bar{c} \text{ minimal})
\\
\implies a \notin \bar{c}
\\
\iff a \; \# \; f(x)
\end{array}
$$

## Proof 11

The support of name binding: $\text{support}(\langle a \rangle x) = \text{support}(x) - \{ a \}$.
<a href="nominal-sets#proof-11-link">↩</a>

$$
\begin{array}{l}
(\text{support}(x) - \{ a \}) \; \text{supports}_{min} \; \langle a \rangle x
\\
\iff ((\text{support}(x) - \{ a \}) \; \text{supports} \; \langle a \rangle x) \land (\forall \bar{b}. \; \bar{b} \; \text{supports} \; \langle a \rangle x \implies (\text{support}(x) - \{ a \}) \subseteq \bar{b})
\end{array}
$$

### Support

$$
\begin{array}{l}
(\text{support}(x) - \{ a \}) \; \text{supports} \; \langle a \rangle x
\\
\iff \forall \pi. \; (\forall b \in (\text{support}(x) - \{ a \}). \; \pi(b) = b) \implies \pi \cdot (\langle a \rangle x) = \langle a \rangle x
\\ \; \\
\pi \cdot (\langle a \rangle x)
\\
= \langle \pi(a) \rangle \pi \cdot x
\\
\; \; \; \; \forall b. \; b \; \# \; (a, x, \pi(a), \pi \cdot x) \implies (a \; b) \cdot x = (\pi(a) \; b) \cdot (\pi \cdot x)
\\
\; \; \; \; (\pi(a) \; b) \cdot \pi \cdot x
\\
\; \; \; \; \text{case } \pi(a) = a
\\
\; \; \; \; \; \; \; \; = (a \; b) \cdot \pi \cdot x
\\
\; \; \; \; \; \; \; \; = (a \; b) \cdot x \; (\pi(a) = a \land (\forall b \in (\text{support}(x) - \{a\}). \; \pi(b) = b) \implies \forall b \in \text{support}(x). \; \pi(b) = b)
\\
\; \; \; \; \text{case } \pi(a) \neq a
\\
\; \; \; \; \; \; \; \; \; \; \; \; \pi = (a \; \pi(a)) \circ (b_1 \; \pi(b_1)) \circ ... \circ (b_n \; \pi(b_n)) \; (\text{A.1})
\\
\; \; \; \; \; \; \; \; \; \; \; \; \; \; \; \; \text{ for all } b_i \in \mathbb{A} - \text{support}(x) - \{a\} \text{ where } \pi(b_i) \notin \text{support}(x) \land \pi(b_i) \neq \pi(b_j) \neq b_j \neq b_i \neq a \neq \pi(a)
\\
\; \; \; \; \; \; \; \; = (\pi(a) \; b) \cdot (a \; \pi(a)) \cdot x \; (b_i \; \# \; x \land \pi(b_i) \; \# \; x \text{A.9})
\\
\; \; \; \; \; \; \; \; = ((\pi(a) \; b) \circ (a \; \pi(a))) \cdot x
\\
\; \; \; \; \; \; \; \; = ((a \; b) \circ (\pi(a) \; b)) \cdot x \; (\text{A.21})
\\
\; \; \; \; \; \; \; \; = (a \; b) \cdot (\pi(a) \; b) \cdot x
\\
\; \; \; \; \; \; \; \; \; \; \; \; \text{assume } \pi(a) \in \text{support}(x)
\\
\; \; \; \; \; \; \; \; \; \; \; \; \implies \pi(\pi(a)) = \pi(a) \; ((\pi(a) \neq a \implies \pi(a) \in (\text{support}(x) - \{a\})) \land \forall b \in (\text{support}(x) - \{a\}). \; \pi(b) = b)
\\
\; \; \; \; \; \; \; \; \; \; \; \; \implies \pi(a) = a \text{ --- contradiction}
\\
\; \; \; \; \; \; \; \; \; \; \; \; \therefore \pi(a) \; \# \; x
\\
\; \; \; \; \; \; \; \; = (a \; b) \cdot x \; (\pi(a) \; \# \; x \land b \; \# \; x \implies (\pi(a) \; b) \cdot x = x \text{ --- A.9})
\\
= \langle a \rangle x
\end{array}
$$

### Minimality

$$
\begin{array}{l}
\forall \bar{b}. \; \bar{b} \; \text{supports} \; \langle a \rangle x \implies (\text{support}(x) - \{a\}) \subseteq \bar{b}
\\ \; \\
\bar{b} \; \text{supports} \; \langle a \rangle x
\\
\iff \forall \pi. \; (\forall b \in \bar{b}. \; \pi(b) = b) \implies \pi \cdot \langle a \rangle x = \langle a \rangle x
\\
\iff \forall \pi. \; (\forall b \in \bar{b}. \; \pi(b) = b) \implies \langle \pi \cdot a \rangle (\pi \cdot x) = \langle a \rangle x
\\ \; \\
\end{array}
$$

TODO: finish proof

## Proof 12

$a \; \# \; \langle b \rangle x \iff a = b \lor a \; \# \; x$ &nbsp; <a href="nominal-sets#proof-12-link">↩</a>

### Forward

$$
\begin{array}{l}
\text{assume } a \; \# \; \langle b \rangle x
\\
\text{case } a = b
\\
\; \; \; \; a = b
\\
\text{case } a \neq b
\\
\; \; \; \; a \; \# \; x \iff a \notin \text{support}(x) - \{b\}
\\
\; \; \; \; \implies a \notin \text{support}(x)
\\
\; \; \; \; \iff a \; \# \; x
\end{array}
$$

### Backward

$$
\begin{array}{l}
\text{assume } a = b \lor a \; \# \; x
\\
\text{case } a = b
\\
\; \; \; \; \text{support}(\langle b \rangle x)
\\
\; \; \; \; = \text{support}(x) - \{b\}
\\
\; \; \; \; = \text{support}(x) - \{a\}
\\
\; \; \; \; a \notin \text{support}(x) - \{a\}
\\
\; \; \; \; \iff a \; \# \; \langle b \rangle x
\\
\text{case } a \; \# \; x
\\
\; \; \; \; \text{support}(\langle b \rangle x)
\\
\; \; \; \; = \text{support}(x) - \{b\}
\\
\; \; \; \; a \; \# \; x \iff a \notin \text{support}(x)
\\
\; \; \; \; \implies a \notin \text{support}(x) - \{b\}
\\
\; \; \; \; \iff a \; \# \; \langle b \rangle x
\end{array}
$$

## Proof 13

The interchangeability of "some fresh" and "any fresh". <a href="nominal-sets#proof-13-link">↩</a>

$$
\begin{array}{c}
\exists b. \; b \; \# \; (a, x, a', x') \land (a \; b) \cdot x = (a' \; b) \cdot x'
\\
\iff
\\
\forall b. \; b \; \# \; (a, x, a', x') \implies (a \; b) \cdot x = (a' \; b) \cdot x'
\end{array}
$$

### Forward

$$
\begin{array}{l}
\text{assume } \exists b. \; b \; \# \; (a, x, a', x') \land (a \; b) \cdot x = (a' \; b) \cdot x'
\\
\text{assume } \forall b'. \; b' \; \# \; (a, x, a' x')
\\
(a \; b) \cdot x = (a' \; b) \cdot x'
\\
\iff (b \; b') \cdot (a \; b) \cdot x = (b \; b') \cdot (a' \; b) \cdot x'
\\
\iff ((b \; b') \circ (a \; b)) \cdot x = ((b \; b') \circ (a' \; b)) \cdot x'
\\
\iff ((a \; b') \circ (b \; b')) \cdot x = ((a' \; b') \circ (b \; b')) \cdot x' \; (\text{A.21})
\\
\iff (a \; b') \cdot (b \; b') \cdot x = (a' \; b') \cdot (b \; b') \cdot x'
\\
\iff (a \; b') \cdot x = (a' \; b') \cdot x' \; (b \; \# \; x' \land b' \; \# \; x' \text{ --- A.9})
\end{array}
$$
  
### Backward

$$
\begin{array}{l}
\text{assume } \forall b. \; b \; \# \; (a, x, a', x') \implies (a \; b) \cdot x = (a' \; b) \cdot x'
\\
\exists b'. \; b' \; \# \; (a, x, a', x') \; (\text{"choose-a-fresh-name"})
\\
\land
\\
(a \; b') \cdot x = (a' \; b') \cdot x' \; (\text{original assumption})
\end{array}
$$

## Proof 14

Equivariant functions are supported by the empty set.
<a href="nominal-sets#proof-14-link">↩</a>

$$
\begin{array}{l}
(\forall \pi, x. \; f (\pi \cdot x) = \pi \cdot f(x)) \implies \{\} \; \text{supports}_{min} \; f
\\ \; \\
\{\} \; \text{supports}_{min} \; f 
\\
\iff \{\} \; \text{supports} \; f \land (\forall \bar{x}. \; \bar{x} \; \text{supports} \; x \implies \{\} \subseteq \bar{x})
\\
\iff \{\} \; \text{supports} \; f \; (\forall \bar{x}. \; \{\} \subseteq \bar{x} \text{ trivial})
\\ \; \\
\{\} \; \text{supports} \; f
\\
\iff \forall \pi. \; (\forall a \in \{\}. \; \pi(a) = a) \implies \pi \cdot f = f
\\
\forall x. \; (\pi \cdot f)(x)
\\
= \pi \cdot f(\pi^{-1} \cdot x)
\\
= \pi \cdot \pi^{-1} f(x) (f \text{ equivariant})
\\
= f(x)
\end{array}
$$

## Proof 15

The identity function is equivariant.
<a href="nominal-sets#proof-15-link">↩</a>

$$
\text{id}(\pi \cdot x) = \pi \cdot x = \pi \cdot \text{id}(x)
$$

## Proof 16

The composition of two equivariant functions is equivariant.
<a href="nominal-sets#proof-16-link">↩</a>

$$
\begin{array}{lll}
& f(g(\pi \cdot x))
\\
= & f(\pi \cdot g(x)) & (\text{equivariance of } g)
\\
= & \pi \cdot f(g(x)) & (\text{equivariance of } f)
\end{array}
$$

## Proof 17

$\text{Nom}$ has a terminal object, which is the singleton set. <a
href="nominal-sets#proof-17-link">↩</a>

The singleton set $\{*\}$ has the trivial permutation action $\pi \cdot * = *$.

For every nominal set $X$, there is a single function $\mathbb{1}_X \; : \; X \rightarrow \{*\}$:

$$
\begin{array}{l}
\mathbb{1}_X \; : \; X \rightarrow \{*\}
\\
\mathbb{1}_X(x) = *
\end{array}
$$

$\mathbb{1}_X$ is equivariant:

$$
\begin{array}{l}
\mathbb{1}_X(\pi \cdot x)
\\
= *
\\
= \pi \cdot *
\\
= \pi \cdot \mathbb{1}_X(x)
\end{array}
$$

## Proof 18

Introduction and elimination of pairs is equivariant.
<a href="nominal-sets#proof-18-link">↩</a>

$$
\begin{array}{l}
\text{fst} \; : \; X \times Y \rightarrow X
\\
\text{fst}(x, y) = x
\\ \; \\
\text{fst}(\pi \cdot (x, y)) =
\text{fst}(\pi \cdot x, \pi \cdot y) =
\pi \cdot x =
\pi \cdot \text{fst}(x, y)
\\ \; \\
\text{snd} \; : \; X \times Y \rightarrow Y
\\
\text{snd}(x, y) = y
\\ \; \\
\text{snd}(\pi \cdot (x, y)) =
\text{snd}(\pi \cdot x, \pi \cdot y) =
\pi \cdot y =
\pi \cdot \text{snd}(x, y)
\\ \; \\
\text{pair} \; : \; (Z \rightarrow X) \times (Z \rightarrow Y) \rightarrow (Z \rightarrow X \times Y)
\\
\text{pair}(f, g) = \lambda x. \; (f(x), g(x))
\\ \; \\
\text{pair}(\pi \cdot (f, g))
\\
= \text{pair}(\pi \cdot f, \pi \cdot g)
\\
= \lambda x. \; ((\pi \cdot f)(x), (\pi \cdot g)(x))
\\
= \lambda x. \; (\pi \cdot f(\pi^{-1} \cdot x), \pi \cdot g(\pi^{-1} \cdot x))
\\
= \lambda x. \; \pi \cdot (f(\pi^{-1} \cdot x), g(\pi^{-1} \cdot x))
\\
= \lambda x. \; \pi \cdot (\lambda y. \; (f(y), g(y))) (\pi^{-1} \cdot x)
\\
= \lambda x. \; \pi \cdot \text{pair}(f, g) (\pi^{-1} \cdot x)
\\
= \pi \cdot \text{pair}(f, g)
\end{array}
$$

## Proof 19

Introduction and elimination of coproducts is equivariant.
<a href="nominal-sets#proof-19-link">↩</a>

$$
\begin{array}{l}
\text{in}_L \; : \; X \rightarrow X + Y
\\
\text{in}_L(x) = (\text{L}, x)
\\ \; \\
\text{in}_L(\pi \cdot x) = (\text{L}, \pi \cdot x) = \pi \cdot (\text{L}, x) = \pi \cdot \text{in}_L(x)
\\ \; \\
\text{in}_R : Y \rightarrow X + Y
\\
\text{in}_R(y) = (\text{R}, y)
\\ \; \\
\text{in}_R(\pi \cdot y) = (\text{R}, \pi \cdot y) = \pi \cdot (\text{R}, y) = \pi \cdot \text{in}_R(y)
\\ \; \\
\text{match} \; : \; (X \rightarrow Z) \times (Y \rightarrow Z) \rightarrow X + Y \rightarrow Z
\\
\text{match}(f, g) = (\lambda (\text{L}, x). \; f(x)) \; | \; (\lambda (\text{R}, x). \; g(x))
\\ \; \\
\text{match}(\pi \cdot (f, g))
\\
= \text{match}(\pi \cdot f, \pi \cdot g)
\\
= (\lambda (\text{L}, x). \; (\pi \cdot f)(x)) \; | \; (\lambda (\text{R}, x). \; (\pi \cdot g)(x))
\\
= (\lambda (\text{L}, x). \; \pi \cdot f(\pi^{-1} \cdot x)) \; | \; (\lambda (\text{R}, x). \; \pi
\cdot g(\pi^{-1} \cdot x))
\\
= \lambda y. \; ((\lambda (\text{L}, x). \; \pi \cdot f(\pi^{-1} \cdot x)) \; | \; (\lambda (\text{R}, x). \; \pi
\cdot g(\pi^{-1} \cdot x)))(y)
\\
= \lambda y. \; ((\lambda (\text{L}, x). \; \pi \cdot f(x)) \; | \; (\lambda (\text{R}, x). \; \pi
\cdot g(x)))(\pi^{-1} \cdot y)
\\
= \lambda y. \; \pi \cdot ((\lambda (\text{L}, x). \; f(x)) \; | \; (\lambda (\text{R}, x). \; g(x)))(\pi^{-1} \cdot y)
\\
= \lambda y. \; \pi \cdot \text{match}(f,g)(\pi^{-1} \cdot y)
\\
= \pi \cdot \text{match}(f,g)
\end{array}
$$

## Proof 20

Finitely supported functions between nominal sets are exponential objects.
<a href="nominal-sets#proof-20-link">↩</a>

Firstly, not all functions are finitely supported, which means that in general $A \rightarrow B$
(for nominal sets $A$ and $B$) is
not itself a nominal set. The set of finitely supported functions $A \rightarrow_{\text{fs}} B$ is a
nominal set.

$$
\begin{array}{l}
B^A = A \rightarrow_{\text{fs}} B
\\ \; \\
\text{eval} \; : \; B^A \times A \rightarrow_{Nom} B
\\
\text{eval}(f, x) = f(x)
\\ \; \\
\lambda f \; : \; X \rightarrow_{Nom} B^A
\\
\lambda f(x) = f'_x
\\ \; \;
\text{where}
\\
\; \; \; \; f'_x \; : \; A \rightarrow_{\text{fs}} B
\\
\; \; \; \; f'_x(a) = f(x, a)
\end{array}
$$

$\text{eval}$ is equivariant:

$$
\begin{array}{l}
\text{eval}(\pi \cdot (f, x))
\\
= \text{eval}(\pi \cdot f, \pi \cdot x))
\\
= (\pi \cdot f)(\pi \cdot x)
\\
= \pi \cdot f(\pi^{-1} \cdot \pi \cdot x)
\\
= \pi \cdot f(x)
\\
= \pi \cdot \text{eval}(f, x)
\end{array}
$$

$\lambda f$ is equivariant:

$$
\begin{array}{l}
\lambda f(\pi \cdot x)
\\
= f'_{(\pi \cdot x)}
\\
\; \; \; \; \forall a. \; f(\pi \cdot x, a)
\\
\; \; \; \; = f(\pi \cdot x, \pi \cdot \pi^{-1} \cdot a)
\\
\; \; \; \; = \pi \cdot f(x, \pi^{-1} \cdot a)
\\
= \pi \cdot f'_x
\\
= \pi \cdot \lambda f(x)
\end{array}
$$

Universal property:

$$
\begin{array}{l}
\forall (f \in X \times A \rightarrow_{Nom} B).
\;
\exists ! (\lambda f \in X \rightarrow_{Nom} B^A).
\;
\text{eval} \circ (\lambda f \times \text{id}) = f
\\ \; \\
\text{given } f \; : \; X \times A \rightarrow_{Nom} B
\\ \; \\
(\text{eval} \circ (\lambda f \times \text{id}))(x, a)
\\
= \text{eval}((\lambda f \times \text{id})(x, a))
\\
= \text{eval}(\lambda f(x), \text{id}(a))
\\
= \text{eval}(\lambda f(x), a)
\\
= \lambda f(x)(a)
\\
= f(x, a)
\end{array}
$$

## Proof 21

Swapping can "commute" with a permutation. (Used in <a href="#proof-22">A.22</a>)

$$
\begin{array}{c}
\pi \circ (a \; b) = (\pi(a) \; \pi(b)) \circ \pi
\\ \; \\
(\pi \circ (a \; b))(x)
\\
= \pi((a \; b)(x))
\\ \; \\
\begin{aligned}
& \text{case } x = a
\\
& = \pi((a \; b)(a))
\\
& = \pi(b)
\\
& = (\pi(a) \; \pi(b))(\pi(a))
\\
& = (\pi(a) \; \pi(b))(\pi(x))
\\
& = ((\pi(a) \; \pi(b)) \circ \pi)(x)
\end{aligned}
\begin{aligned}
& \text{case } x = b
\\
& = \pi((a \; b)(b))
\\
& = \pi(a)
\\
& = (\pi(a) \; \pi(b))(\pi(b))
\\
& = (\pi(a) \; \pi(b))(\pi(x))
\\
& = ((\pi(a) \; \pi(b)) \circ \pi)(x)
\end{aligned}
\\
\begin{aligned}
& \text{case } x \neq a \land x \neq b
\\
& = \pi((a \; b)(x))
\\
& = \pi(x)
\\
& = (\pi(a) \; \pi(b))(\pi(x)) \; (x \neq a \land x \neq b \implies \pi(x) \neq \pi(a) \land \pi(x) \neq \pi(b) \text{ --- } \pi \text{ injective})
\\
& = ((\pi(a) \; \pi(b)) \circ \pi)(x)
\end{aligned}
\end{array}
$$

## Proof 22

$[\mathbb{A}]({-})$ is right adjoint to the functor ${}- * \; \mathbb{A}$ arising from the following
nominal set: $X * \mathbb{A} = \{ \; (x, a) \; | \; x \in X, a \; \# \; x  \;\}$. <a href="nominal-sets#proof-22-link">↩</a>

$$
\begin{array}{l}
{}- * \; \mathbb{A} \dashv [\mathbb{A}]({-}) 
\\ \; \\
\text{bind} \; : \;
(X * \; \mathbb{A} \rightarrow_{Nom} Y) \rightarrow 
X \rightarrow_{Nom} [\mathbb{A}](Y)
\\
\text{bind}(f)(x) = \langle a \rangle f(x, a) \text{ for some } a \; \# \; x
\\ \; \\
\text{bind}^{-1} \; : \; 
(X \rightarrow_{Nom} [\mathbb{A}](Y)) \rightarrow
X * \; \mathbb{A} \rightarrow_{Nom} Y
\\
\text{bind}^{-1}(f)(x, a) = f(x) \; @ \; a
\\
\; \; \text{where}
\\
\; \; \; \; -{} \; @ -{} \; : \; [\mathbb{A}]X * \mathbb{A} \rightarrow X
\\
\; \; \; \; @(\langle a \rangle x, a') = (a \; a') \cdot x
\end{array}
$$

<br>

$$
\begin{array}{l}
\text{bind}(\text{bind}^{-1}(f))(x)
\\
= \langle a \rangle \; \text{bind}^{-1}(f)(x, a)
\text{ for some } a \; \# \; x
\\
= \langle a \rangle \; (f(x) \; @ \; a)
\\
\; \; \; \; f(x) \; @ \; a \text{ requires } a \; \# \; f(x)
\\
\; \; \; \; \; \; \; \; a \; \# \; x \land a \; \# \; f \; (f \text{ equivariant -- A.14}) \implies a \; \# \; f(x) \; (\text{A.10})
\\
\; \; \; \; \text{let } \langle a' \rangle x' = f(x)
\\
= \langle a \rangle (\langle a' \rangle x' \; @ \; a)
\\
= \langle a \rangle \; (a' \; a) \cdot x'
\\ \; \\
\; \; \; \; a \; \# f(x)
\\
\; \; \; \; \iff a \; \# \; \langle a' \rangle x'
\\
\; \; \; \; \iff a = a' \lor a \; \# \; x'
\\ \; \\
\; \; \; \; \text{case } a = a'
\\
\; \; \; \; \; \; \; \; \langle a \rangle \; (a' \; a) \cdot x'
\\
\; \; \; \; \; \; \; \; = \langle a' \rangle \; (a' \; a') \cdot x'
\\
\; \; \; \; \; \; \; \; = \langle a' \rangle x'
\\
\; \; \; \; \; \; \; \; = f(x)
\\ \; \\
\; \; \; \; \text{case } a \; \# \; x'
\\
\; \; \; \; \; \; \; \; \forall b. \; b \; \# \; (a, (a' \; a) \cdot x', a', x') \implies (a \; b) \cdot (a' \; a) \cdot x' = (a' \; b) \cdot x'
\\
\; \; \; \; \; \; \; \; \iff \forall b. \; b \; \# \; (a, (a' \; a) \cdot x', a', x') \implies ((a \; b) \circ (a' \; a)) \cdot x' = (a' \; b) \cdot x'
\\
\; \; \; \; \; \; \; \; \iff \forall b. \; b \; \# \; (a, (a' \; a) \cdot x', a', x') \implies ((a' \; b) \circ (a \; b)) \cdot x' = (a' \; b) \cdot x' \; (\pi \circ (a \; b) = (\pi(a) \; \pi(b)) \circ \pi \text{ --- A.21})
\\
\; \; \; \; \; \; \; \; \iff \forall b. \; b \; \# \; (a, (a' \; a) \cdot x', a', x') \implies (a' \; b) \cdot (a \; b) \cdot x' = (a' \; b) \cdot x'
\\
\; \; \; \; \; \; \; \; \iff \forall b. \; b \; \# \; (a, (a' \; a) \cdot x', a', x') \implies (a' \; b) \cdot x' = (a' \; b) \cdot x' \; (a \; \# \; x' \land b \; \# \; x' \implies (a \; b) \cdot x' = x' \text{ --- A.9})
\\
\; \; \; \; \; \; \; \; \iff \exists b. \; b \; \# \; (a, (a' \; a) \cdot x', a', x') \land (a' \; b) \cdot x' = (a' \; b) \cdot x' \; (a \; \# \; x' \land b \; \# \; x' \implies (a \; b) \cdot x' = x' \; (\text{A.13})
\\ \; \\
\; \; \; \; \; \; \; \; \langle a \rangle \; (a' \; a) \cdot x'
\\
\; \; \; \; \; \; \; \; = \langle a' \rangle x'
\\
\; \; \; \; \; \; \; \; = f(x)
\end{array}
$$

<br>

$$
\begin{array}{l}
\text{bind}^{-1}(\text{bind}(f))(x, a) \text{ where } a \; \# \; x
\\
= \text{bind}(f)(x) \; @ \; a
\\
\; \; \; \; \text{bind}(f)(x) \; @ \; a \text{ requires } a \; \# \; \text{bind}(f)(x)
\\
\; \; \; \; a \; \# \; x \land a \; \# \; \text{bind}(f) \; (\text{bind(f)} \text{ equivariant -- A.14}) \implies a \; \# \; \text{bind}(f)(x) \; (\text{A.10})
\\
= (\langle a' \rangle f(x, a') \text{ for some } a' \; \# \; x) \; @ \; a \\
= (a' \; a) \cdot f(x, a')
\\
= f((a' \; a) \cdot x, (a' \; a) \cdot a') \; (f \; \text{equivariant})
\\
= f((a' \; a) \cdot x, a)
\\
= f(x, a) \; (a' \; \# \; x \land a \; \# \; x \implies (a' \; a) \cdot x = x \text{ --- A.9})
\end{array}
$$

## Proof 23

$[\mathbb{A}]({-})$ is left adjoint to this functor:
$R(Y) = \{ \; f \; | \; f \in Y^{\mathbb{A}}, \; \forall a. \; a \; \# \; f(a) \;
\}$. <a href="nominal-sets#proof-23-link">↩</a>

$$
\begin{array}{l}
[\mathbb{A}](-{}) \dashv R
\\ \; \\
\text{unbind} \; : \; (X \rightarrow_{Nom} R(Y)) \rightarrow [\mathbb{A}] X \rightarrow_{Nom} Y
\\
\text{unbind}(f)(\langle a \rangle x) = f(x)(a)
\\ \; \\
\text{unbind}^{-1} \; : \; ([\mathbb{A}] X \rightarrow_{Nom} Y) \rightarrow X \rightarrow_{Nom} R(Y)
\\
\text{unbind}^{-1}(f)(x) = \lambda a. \; f(\langle a \rangle x)
\end{array}
$$

<br>

$$
\begin{array}{l}
\text{unbind}^{-1} \text{ requires } \forall a. \; a \; \# \; (\lambda a'. \; f(\langle a' \rangle x))(a)
\\ \; \\
\forall a. \; a \; \# \; (\lambda a'. \; f(\langle a' \rangle x))(a)
\\
\iff \forall a. \; a \; \# \; f(\langle a \rangle x)
\\
a \; \# \; f \; (f \text{ equivariant -- A.14}) \land a \; \# \; \langle a \rangle x \; (a = b \implies a \; \# \; \langle b \rangle x \text{ --- A.12}) \implies a \; \# \; f(\langle a \rangle x) \; (\text{A.10})
\end{array}
$$

<br>

$$
\begin{array}{l}
\text{unbind}(\text{unbind}^{-1}(f))(\langle a \rangle x)
\\
= \text{unbind}^{-1}(f)(x)(a)
\\
= (\lambda a'. \; f(\langle a' \rangle x))(a)
\\
= f(\langle a \rangle x) \; (\beta \text{-equivalence})
\end{array}
$$

$$
\begin{array}{l}
\text{unbind}^{-1}(\text{unbind}(f))(x)
\\
= \lambda a. \; \text{unbind}(f)(\langle a \rangle x)
\\
= \lambda a. f(x)(a)
\\
= f(x) \; (\eta \text{-equivalence})
\end{array}
$$
