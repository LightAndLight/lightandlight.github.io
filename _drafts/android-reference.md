---
layout: post
permalink: /android-reference/
title: Android Reference
---

This post mainly serves as a way to really drill in the core Android
development principles, as I'm soon commencing work on an app which may last
for months. I'm sure most of it is going to be paraphrasing the Android
Developer guides, but it'll be a helpful exercise to record it all.

## The Manifest File

The manifest file is an XML file containing meta-data about an app. First and
foremost it specifies app permissions, the minimum API level, the hardware and
software features used and any external libraries that are used.

In addition to this, every activity that the app uses must be recorded here.
Data such as the activity name, description and icon can also be declared.

### Links

[Activity XML Attributes](http://developer.android.com/guide/topics/manifest/activity-element.html)

## User Interface

### Vocabulary Clarification

`View`: Something that you interact with or *view* (commonly known as a widget)

`ViewGroup`: A container which defines the layout of multiple `View`s

### Creation

You can create `View`s and `ViewGroup`s in an activity, or you can define the
hierarchy in an XML file (I'm not sure yet, but I think the former may be
helpful if you want to generate some different `View`s without changing
activities. I'll keep it in mind).

### Responding to events

A `View` that can trigger an even will have a corresponding attribute
(ie. `Button` has `onClick`). This attribute's value can then be set to the
name of a method in hosting activity. The method must

- Be public
- Return void
- Take one argument, of type `View` (the calling view will be passed to it)

### Links
[Default Android widgets and layouts](http://developer.android.com/reference/android/widget/package-summary.html)

## Resources

### In Code

The `R` class provides access to IDs for all the resources in the `res` folder.
For each resource of `type` and `name`, its ID can be accessed at `R.type.name`. 
For example, the ID for the resource

{% highlight xml %}
<string name="a_string">My String</string>
{% endhighlight %}

will be accessable at `R.string.a_string`. Many methods take these IDs as
argments to access the resources.

### In XML

Resources can be referenced from XML files using the syntax `@type/name`.

Sometimes you will want to reference an element that you can't name, like a
`View`. In this case, you can use the `android:id` attribute to manually
assign an ID. This attribute can either reference an already defined ID:
`@id/assigned_id` or it can create one on the spot: `@+id/assigned_id`.
