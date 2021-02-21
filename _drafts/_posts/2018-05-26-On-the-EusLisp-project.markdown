---
layout: post
title:  "On the EusLisp project"
categories: EusLisp
---

### On the beginning of the project

Before talking about EusLisp in more depth, it will be well to talk about my project in particular,
so that you, the reader, may have some better understanding of my biases. As said on [the previous post]({{  'euslisp/2018/05/25/First-Steps-On-Euslisp.html' | relative_url }}), I got to know of EusLisp when looking for GSoC projects, on the JSK page.
For the reasons mentioned on that post, the project caught my interest but, to be more specific,
what caught my interest was a proposed project to make EusLisp more Common Lisp compliant. The first
step to do that would be to have an idea of how non-compliant EusLisp is relative to Common Lisp, so, with a little research,
I got to [ansi-test][ansi] and thought it would be a good idea to make use of it. Ansi-test is a
reasonably complete and well-made test suite built just for testing Common Lisp compliance (and it is
said that the projects which made use of it have caught "non-intended behaviors" to the mounts), so
I would try to use it.

Trying to make a quick read trough the files of the project (the ansi-test project, that is), I got to the
understanding that Common Lisp is much more different from Scheme than I first thought, so I'd
better invest some time on learning some of it. I wanted (and needed) to go quick, so I started with [Ansi
Common Lisp][ansi-cl], by Paul Graham. My immediate intention was to get acquainted with the
language syntax and grammar, so that reading Common Lisp code wouldn't be like Latin to me, so I first read it quickly, paying
some, but not too much, court to some of the more subtle details and examples the book had. That was
enough for me to understand a great deal of Common Lisp programs I found and to give a try to
starting to find some errors and, who knows, correct some that ocurred when using ansi-test.

But, although I did need to do and show some work on the project I had proposed, I also needed (as I
still need) to deepen my knowledge on the language so as to gain more understanding and
be able to do better work in the future. In this context, knowledge is much like a machine that one
needs not only to maintain, but to improve. To have a good and adequate machine, however, takes time
and work, so I had to find a compromise: I cannot stop studying and researching, as well as I
cannot stop using what I've studied and researched so far to keep the project going. My primary choice, after Ansi
Common Lisp was [On Lisp][on-lisp], also by Paul Graham. This one I'm reading with more care and calm,
by the pieces.

Now, that digression (I'm sorry if it was too big a digression) was to say that I am far from a
specialist, and that anything I say here should be taken with a grain of salt. I will try to be as
accurate as possible on my statements, but you shouldn't take my word for it, as I'm learning as it goes (and, by all
means, if you find anything objectionable here, please report it! :D ).


### On the Project


Where I come from we have a saying that "quem não se comunica se estrumbica". You probably don't
understand what I mean, but never mind. The point is that one of the reasons for me to start
this blog series (but not the most important reason, mind you) is to use it as a means of comunication with those great guys at the JSK lab, at
the University of Tokyo, who are willing to help me on this project. After giving it some thought, I found that this
blogging format would be a most convenient one for the kind of communication I'm willing to do (better, than Gist, for
instance, which I've tried before) first because it is more organized, then because it is more
accessible (so it might help other people in a similar situation, which is also one of the project
objectives), which forces me to (at least try to) write more clearly. It has the caveat that I cannot do a
blog post a day and, at times, maybe even one a week will seem too much, as it purposefully deals
with a more complete kind of work (instead of silly errors, for instance), but I'll make an effort
to make it work.

Now, I would rather not lose the time of those forementioned great guys with my ramblings, so will
make use of the following convention. The line just before a more technical part of a text will be
followed by two solid lines, like so

---
---

and the line just after such a part will be followed by one solid line, like so

---

so that, if you are after the technical parts, you are invited to use those signals as a guidance.

#### On the basics

As of now, there is a great wealth of material on EusLisp (some of which, in japanese, but well..
that's life). You can find a nice tutorial covering the basics (translated from [this one][ja-tut] by [Affonso][affonso])
[here][en-tut], and more information [here][Eus] and [here][jskeus]. In particular, the (english) manual is [here][manual-en].
There is a more complete japanese version of the manual, but the difference has mostly to do with
the kind of things you may find in a regular robotics textbook. You can find some very interesting
documentation on the `/doc` folder of the [EusLisp GitHub][Eus] as well.

One thing you might notice, by quickly scanning trough those pages, is that EusLisp is very integrated with [ROS][ros].
In fact, there are many (not to say all) EusLisp packages developed with ROS in mind. The thing is
that EusLisp was built to deal with robots, so making it integrated with ROS makes the work easier.
If you are willing to work with EusLisp, you might invest some time on learning some ROS basics as
well.



[ansi]:        https://common-lisp.net/project/ansi-test/
[ansi-cl]:     http://www.paulgraham.com/acl.html
[on-lisp]:     http://www.paulgraham.com/onlisp.html
[ja-tut]:      http://euslisp-tutorial.readthedocs.io/ja/latest/
[en-tut]:      http://euslisp-tutorial.readthedocs.io/en/latest/
[affonso]:     https://github.com/Affonso-Gui/
[Eus]:         https://github.com/euslisp/EusLisp
[jskeus]:      https://github.com/euslisp/jskeus
[manual-en]:   https://github.com/euslisp/EusLisp/raw/master/doc/latex/manual.pdf
[ros]:         http://www.ros.org/
