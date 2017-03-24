---
layout: post
permalink: /why-LINQ-is-broken/
title: Why LINQ (well, C#) is Broken
author: ielliott95
tags:
  - programming
  - haskell
---

LINQ is a system that provides a flexible query interface for .NET languages.
It allows a user to write queries over arbitrary data using an in-built
SQL-like syntax. This syntactic sugar is mapped to method calls at compile time,
so any data structure that implements the correct methods can be used with LINQ.

The essential methods for enabling LINQ support are `Select` and `SelectMany`,
implemented as extension methods. They have the following types:
{% highlight c# %}
SomeData<B> Select<A,B>(this SomeData<A> a, Func<A,B> f)
SomeData<B> SelectMany<A,B>(this SomeData<A> a, Func<A,SomeData<B>> f)
SomeData<C> SelectMany<A,B,C>(this SomeData<A> a, Func<A,SomeData<B>> f, Func<A,B,C> g) // Overloaded to reduce levels of nesting
{% endhighlight %}

With implementations of these three methods, it is possible to write a query
expression such as:
{% highlight c# %}
SomeData<A> myA = ...;
SomeData<B> myB = ...;
Func<A,B,C> f = ...;
SomeData<C> myC = from a in myA
                  from b in myB
                  select f(a,b);
{% endhighlight %}
which will be compiled to something like:
`SomeData<C> output = justWord.SelectMany(a => myB, (a, b) => f(a, b));`

Readers who are familiar with Haskell or similar functional languages will
notice that `Select` is `fmap`, `SelectMany` is `>>=` and the
`from .. in .. select` syntax is equivalent to Monad comprehensions. The above
code would be written in Haskell as follows:
{% highlight haskell %}
myA = ...
myB = ...
f a b = ...
myC = do
  a <- myA
  b <- myB
  return $ f a b
{% endhighlight %}

LINQ was designed to bring Monad comprehensions to C#. And it does. Almost.

Consider our query from earlier:
{% highlight c# %}
...
SomeData<C> myC = from a in myA
                  from b in myB
                  select f(a,b);
{% endhighlight %}
This seems like a common pattern. We don't want to write this code over and
over, so we abstract `myA`, `myB` and `f` and make the query into a method.

{% highlight c# %}
SomeData<C> CombineWith<A,B,C>(SomeData<A> myA, SomeData<B> myB, Func<A,B,C> f)
{
    return from a in myA from b in myB select f(a,b);
}
{% endhighlight %}

Now say we define a new data type to use with LINQ, call it `OtherData<A>`, and
implement `Select` and `SelectMany` appropriately. We also want to implement
`CombineWith` because `from .. in .. from .. in .. select ..` is still a common
pattern that we want to avoid writing:
{% highlight c# %}
OtherData<C> CombineWith<A,B,C>(OtherData<A> myA, OtherData<B> myB, Func<A,B,C> f)
{
    return from a in myA from b in myB select f(a,b);
}
{% endhighlight %}
There is a pattern emerging. For every data type that we want to use with LINQ,
one must reimplement all LINQ-specific methods specifically for that type.

This is an issue because it grossly violates DRY (don't repeat yourself).
A well-written program should not have duplicated code - it makes maintenance
more laborious and increases the chance of bugs.

So in an effort to save ourselves time, we should abstract over this common
pattern. We require a function that specifies
> for all generic classes `F<???>` implementing `Select` and `SelectMany`, given
an instance of  `F` containing `A`s, another instance of `F` containing `B`s,
and a `Func<A,B,C`, return an `F` containing `C`s

It turns out that it's actually impossible to write this method in C#. I'd like
to write something like
`F<C> CombineWith<F<?>,A,B,C>(F<A> myA, F<B> myB, Func<A,B,C> f)`, but C# only
allows abstraction over non-generic types.

To add a little more weight to this revelation, let's imagine if we could
not abstract over the contents of a list ie. the method
`List<A> Sort<A>(List <A> input)` cannot be expressed in this language. Due to
this limitation, we would have to create a new list class every time we needed
a different element type inside the list, then reimplement `Sort` for each new
class.
> `ListOfInt.Sort`
`ListOfBool.Sort`
`ListOfSomeData.Sort`
...

This is again a terrible violation of the "don't repeat yourself" principle.
You write `n` implementations of `Sort`, where `n` is the number of sortable
classes. Imagine that each implementation used the
[proven incorrect version of TimSort](http://envisage-project.eu/proving-android-java-and-python-sorting-algorithm-is-broken-and-how-to-fix-it/).
If you wanted to implement the correct version, you would have to update `n`
methods.

Also consider the implementation of
`List<B> Map<A,B>(List<A> input, Func<A,B> f)` in a generic-less language. You
would have to write a different method for each inhabitant of `A` and `B`
> `ListOfInt.MapToListOfInt`
`ListOfInt.MapToListOfBool`
`ListOfInt.MapToListOfSomeData`
`ListOfBool.MapToListOfBool`
...

You write `n^2` `Map` methods where `n` is the number of of mappable classes.

More generally, in this generic-less language, you write `O(n^m)` where `m` is
the sum of should-be-generic inputs and should-be-generic outputs, and `n` is
the number of should-be-generic classes.

This exponential growth of redundant nonsense also applies to our `CombineWith`
issue. For every LINQ-able class, you have to write a separate implementation
of `CombineWith`, even though it's exactly the same code!

Haskell (and other sane functional languages) uses a concept called "Higher
Kinded Types" to address this problem. Every type has a "kind" (denoted `*`). In
C#, every type must have kind `*`. Higher-kinded types are functions from kinds
to kinds. Given data declaration that has a single type variable, say
`Maybe a = Just a | Nothing`, we say that `Maybe` has kind `* -> *`, which means
that it is a higher-kinded type that takes a type of kind `*` and returns a type
of kind `*`. In C#, every type must have kind `*` ie. if you have a defined the
class `List<A>` then you get a compile error if you refer to `List` without
the type argument.

Let's take another look the Haskell implementation of `CombineWith`:
{% highlight haskell %}
combineWith :: Monad m => m a -> m b -> (a -> b -> c) -> m c
combineWith myA myB f = do
  a <- myA
  b <- myB
  return $ f a b
{% endhighlight %}
In this function, and the definition of the Monad typeclass (read: interface)`m`
implicitly has kind `* -> *`. This function will work for any type that is
an instance of Monad (read: implements the Monad interface). In Haskell, this
code only needs to be written once. The cost of implementation and maintenance
of a group of functions has gone from O(n^m) to O(1).

Now you might say, "Well, I don't use LINQ like that. I only use it for
`IEnumerable` things". This is akin to a user of our imaginary generic-less
language saying "Well, I don't use Sort like that. I only sort lists of
integers". It is agreed that a language without generics is counter to
productivity. It follows that a language without higher-kinded types is also
counter to productivity.
