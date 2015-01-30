---
layout: post
title: Introduction to Monad Transformers
permalink: /introduction-to-monad-transformers/
---

Monad transformers combine the functionality of two monads into one. They are often used
as a way to "flatten" nested monads, but are also used to enable interactions between
monads that, when used seperately, would be incredibly difficult to implement.

Example case: You input a number and want to indefinitely manipulate it while printing the
result each time. If there were no intermediate IO operations we could use the state monad 
with the following state changes:

{% highlight haskell %}
add :: Int -> State Int ()
add n = state $ \x -> ((),x + n)

subtract :: Int -> State Int ()
subtract n = state $ \x -> ((),x - n)
{% endhighlight %}

chain them together:

{% highlight haskell %}
manyOperations :: State Int ()
manyOperations = do
    add 1
    subtract 3
    add 5
    add 7
    subtract 22
{% endhighlight %}

then get the result:

{% highlight haskell %}
initialValue = 5 :: Int
(_,result) = runState manyOperations initialValue :: ((),Int)
{% endhighlight %}

But if we want to print the value each time an operation occurs, things get *very* tricky. There is
no way we can redefine our operation functions to print values *while suppporting the same chaining
interface*.[^1] This is where monad transformers come in.

[^1]: Seriously, try it.
