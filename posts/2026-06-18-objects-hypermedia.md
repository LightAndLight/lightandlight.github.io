---
title: Exploring objects through hypermedia
permalink: /objects-hypermedia
date: 2026-06-18T03:15:00
tags:
  - programming
---

## Outline

* The Web is object-oriented in the same sense as Smalltalk.

* It's built out of URLs (object references) and HTTP requests (messages).

  Roughly, a HTTP request's method names the message, and the request's body provides the arguments.

* If everything is working properly, you don't get to arbitrarily manipulate a web server;
  you send messages that the server undersands.

* Algorithms rely on assumptions about the behaviours of the objects involved.

  An algorithm that uses numbers will fall apart if an arithmetic operation suddenly defies expectation,
  such as "every fifth addition performs a multiplication".
  The operations should also respect our intuitions about other parts of the system.
  e.g. the "addition" operation shouldn't cause the system's next email to be sent in all-caps.

* In the object-oriented paradigm, the set of messages that an object accepts and the coherence conditions
  relating these messages is called a "protocol". (A bit confusing for this discussion,
  because in the context of the Web, "protocol" refers to *transfer protocol*)

* The Domain Name System lets us extend the web and publish our own objects.

* There's nothing [in the URL](https://www.w3.org/DesignIssues/Axioms.html#opaque)
  that tells about its referent's protocol. How do we actually use the object?

* If the object is to be used in an algorithm, then its protocol has to be
  communicated out-of-band. The algorithm encodes assumptions about the
  object's behaviour.

  * This is what people are doing when they write JSON/XML web APIs.
    I call these "web RPC systems".

* What about dynamic/free-form/non-algorithmic use? Imagine every interaction
  on the Web required you to read API documentation and then run a `curl`.

* A better idea: we agree on a well-known message that you can send the object
  to get it to disclose itself to you. It describes some of the messages that
  are available to you based on its current state.

* Even better idea: the available messages and their arguments are translated
  into a visual, interactive medium. Instead of scanning a list of
  description-message pairs and typing the correct invocation into the terminal,
  you trigger the appropriate message by interacting with the graphical element
  that represents it.

* This approach is called "hypermedia"
