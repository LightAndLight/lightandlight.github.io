---
layout: post
title: Search and Replace in Multiple Files
author: ielliott95
permalink: /search-and-replace-in-multiple-files/
tags:
    - programming
    - tools
---

{% highlight bash %}
$ sed -i "s/pattern/replacement/g" FILES
{% endhighlight %} 

## The Story
Last semester I had to write a static website by hand with no templating
resulting in a lot of duplicated code across multiple pages. I had already 
finished most of the project when I realised that the main page of the 
project should be named `index.html` instead of `home.html`. I renamed the
file, but that left me with countless references to "home.html" that needed
to be changed, and I wanted to change them all at once. Enter `sed`.

`sed` allows the user to write programs which operate on streams of text.
It is run using the syntax

{% highlight bash %}
$ sed OPTIONS.. [SCRIPT] [FILENAME..]
{% endhighlight %}

To search and replace using `sed` we use the `s` command of the form 
`s/regex/replacement/flags`. Our `sed` script would become
`s/home\.html/index.html/g`. The `.` needs to be escaped because `.` on its own
matches any character in regex. The `g` flag means to replace every occurrence
of the pattern, instead of just the first.

By default, `sed` will only write the altered text to `stdout`, so we need to
use the `-i` flag to make the alterations inside the source file.

The final command is now

{% highlight bash %}
$ sed -i "s/home\.html/index.html/g" *.html
{% endhighlight %} 

which will apply the sed program to all the HTML files in the directory. Easy!
