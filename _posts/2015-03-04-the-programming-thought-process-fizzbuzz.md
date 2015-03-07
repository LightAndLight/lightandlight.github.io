---
layout: post
title: "The Programming Thought Process: Fizzbuzz"
permalink: /the-programming-thought-process-fizzbuzz/
tags:
    - programming
---

With so many programming languages and frameworks at our disposal, it is
too easy to believe that knowledge of many tools is the defining characteristic
of a good programmer. However, many experienced programmers will assert that
it isn't the languages you know, but your ability to solve problems that 
defines you as a programmer.

In this article I will attempt to explain some of the instinctual problem
solving techniques that experienced programmers use. Our problem will be 
"fizzbuzz"; a notorious yet straightforward problem used to separate programmers
from non-programmers in job interviews. Its specification:

> Write a program that prints the numbers from 1 to 100. But for multiples of 
> three print “Fizz” instead of the number and for the multiples of five print 
> “Buzz”. For numbers which are multiples of both three and five print “FizzBuzz”.

The first step in creating an answer is to examine the specification text
for keywords that you can translate into code. Consider the words "from
1 to 100". If we want to access every number from 1 to 100, then the best
approach is to use a loop. All the other instructions apply to each individual 
number, so the loop will contain this logic. `for` and `while` loops are 
equally valid ways to complete the task, however I'll use a for loop for this 
example as it looks much cleaner.

{% highlight python %}
for i in range(1,101): # range(a,b) has range a <= i < b
    # do some things
{% endhighlight %}

The next few lines of the spec outline three conditions which could change what
will be printed. These can all be expressed using "if-else" statements due to 
their boolean nature. Additionally, if the none of the conditions are 
satisfied, then the number is to be printed. This "default" behaviour can be
specified in the "else" section of the statement.

When there are multiple "if-else" statements checking the same variable, it's
best to use "elif" statement for all the options instead of nested "if-else".

{% highlight python %}
for i in range(1,101):
    if multiple_of_three(i):
        print("Fizz")
    elif multiple_of_five(i):
        print("Buzz")
    elif multiple_of_three_and_five(i):
        print("FizzBuzz")
    else:
        print(i)
{% endhighlight %}

Here I've used some placeholder functions to express the divisibility of the 
number, but how how should they be implemented? The simplest answer is to use
the modulus operator (`%`). `a % b` calculates the remainder of `a / b`, so the
three multiple functions could be replaced by `i % 3 == 0`, `i % 5 == 0` and 
`i % 3 == 0 and i % 5 == 0`:

{% highlight python %}
for i in range(1,101):
    if i % 3 == 0:
        print("Fizz")
    elif i % 5 == 0:
        print("Buzz")
    elif i % 3 == 0 and i % 5 == 0:
        print("FizzBuzz")
    else:
        print(i)
{% endhighlight %}

If one didn't know of the modulus operator, however, the same functionality
could be created using arithmetic:

A number `a` is a multiple of another number `b` if `a / b` has no remainder.
There exists integers `q` and `r` such that `a = q * b + r`. If `a` is a 
multiple of `b` then `a / b  = q`, otherwise `a / b = q + r / b`. This means 
that for all `r`, `q = floor(a / b)`. Thus if `a - b * floor(a / b) = 0` then
`a` is a multiple of `b`:

{% highlight python %}
from math import floor # import the floor function from Python's math module

def multiple_of(a,b):
    # if a is a multiple of b
    return (a - (b * floor(a/b)) == 0)

for i in range(1,101):
    if multiple_of(i,3):
        print("Fizz")
    elif multiple_of(i,5):
        print("Buzz")
    elif multiple_of(i,3) and multiple_of(i,5):
        print("FizzBuzz")
    else:
        print(i)
{% endhighlight %}

The next step is testing the code. For simple programs this can be done by
running the code and looking at the output. 

{% highlight bash %}
$ python fizzbuzz.py
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
Fizz
...
{% endhighlight %}

The output of the program lacks any mentions of "FizzBuzz", printing "Fizz" 
instead. This is a clue that the problem lies in the condition evalution.
The numbers divisible by both three and five are evaluated as just being
divisible by three. To fix this, either

* Move the "FizzBuzz" condition so it is evaluated first; or;
* Add extra parameters to the "Fizz" and "Buzz" conditions to clarify that they
should not trigger when a number is divisible by both three and five.

{% highlight python %}

...

# 1. changing condition evaluation order
if i % 3 == 0 and i % 5 == 0:
    print("FizzBuzz")
elif i % 3 == 0: # numbers divisible by both three and five will never reach this condition
    print("Fizz")
elif i % 5 == 0:
    print("Buzz")
else:
    print(i)
{% endhighlight %}

{% highlight python %}

...

# 2. clarifying logic
if i % 3 == 0 and not i % 5 == 0: # we want to print fizz for numbers that are divisible by three and NOT divisible by five
    print("Fizz")
elif i % 5 == 0 and not i % 3 == 0: # the opposite is true here
    print("Buzz")
elif i % 3 == 0 and i % 5 == 0:
    print("FizzBuzz")
else:
    print(i)
{% endhighlight %}

Either way, the program now functions correctly.

Now, I can imagine that some people would have questions like "How would I
recognise that loops would be useful in this?" or "How do I know to use if-else
statements for this problem?" There are three answers to these kinds of 
questions:

1. Know your tools.

   Knowledge of languages and tools does not define you as as programmer, but
   this knowledge does influence how effectively you can solve a problem using a
   given language. Strong knowledge of language features will give you an
   indication of which tasks are easy or difficult using that language, and
   and help you use full potential of the language to complete the task.

2. Practise.

   If you only read and never practise you will never reach your full potential.
   Practise is essential in reinforcing learning. 

3. Get feedback.

   Learning is much easier when you have someone more experienced to guide you.
   As well as practising on your own, get your work reviewed by someone who 
   knows more than you. They will easily be able to point inefficient or 
   redundant code. Additionally, you need to take note of the tips they give you
   and then practise integrating them when you work. If you don't take advice to
   heart then you will never improve.

In summary, when attempting programming problems you need to:

* Identify keywords
* Translate them to code
* Test the code
* Look for clues regarding any errors
* Change the code based on any clues you found
* Rinse and repeat

Learn your tools, use the tools and get feedback on your work to ensure contant
improvement.

Good luck.
