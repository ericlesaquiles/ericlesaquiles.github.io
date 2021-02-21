---
layout: post
title:  "End of GSoC Project"
categories: EusLisp
---

The GSoC project is officially coming to its end (I should send the
last details until August 15th, at most). At the beginning of the GSoC
project, [Kei Okada][k-okada] said (if I heard correctly, which might
not be the case given the poor connection we had at the video call) he
wanted the project to be joyful for me. It sure was, in many
different, sometimes unexpected, ways. I learnt a lot about
programming, robotics, projects and myself in particular. Much of that
learning would be useful even if a turn up a budhist monk in Tibet,
which is a good.

That surprising learning experience, however, came at the price of failure. At
this project I've made so many stupid mistakes (of which my mentors
have seen a few) that would make some people faint. As could be
guessed, that has had a negative effect on the walking of the
project. I did much less than I intended and wanted to do at the
beginning.

#### Brief historic
----

In brief, the project was about the Common Lisp
compatibility of EusLisp. In order to have a reasonable idea of the
compatibility, we've made use of the [Ansi Test][ansi-test]
framework, which was intended as a framework for testing the different
Common Lisp's implementations' compliance to the Common Lisp
standard.

That framework was meant to test reasonably complete
implementations, so it makes use of many a Common Lisp features. For
us, that meant a problem, because even in order to run the tests, we
needed a infrastructure which we didn't have at the beginning, and
which had to be either supplied or replaced by something else or
somehow rendered unnecessary. The tests in their vanilla form were
unable to run, and had to be adapted, which was initally done by
Affonso (at the beginning, I just barely knew of EusLisp and of Common
Lisp, mind you). Some adaptions come in form of new functions,
macros of packages, but some require a deeper more complicated
structure.

Anyway, after we were able to run  a limited quantity of tests, we
would need a good way to keep track of them. There are a total of about
20.000 tests, so there is no way we would do that by hand, and it was needed to
automate it. That bit was easy, I just created a simple script which would
fetch the test results from a file and update an `.org` REPORTS file (with reports)
with the result of that test (which would mark which tests passed and
the percentage of passed tests by file.. those files may be found in
the `reports/` folder of [the project][eus-test]). The idea is that the more 
tests got marked on that file, the more Common Lisp compliant EusLisp
is. Of course, it is not that simple, as there are tests which don't
pass out of a lack of infrastructure (to make the test run) rather
than lack of the feature, but that picture seems mostly accurate, overall.

Except when one wants to update the reports, the tests are usually run
in pieces. That is, the programmer selects the feature to be tested
and the test cases, and run them. Trying to understand the
issues that there were with the tests (as, occasionaly, the addition of a
test case causes the tests to crash) and trying to make more tests
runnable (without crashing) were a good part of the work. Another part
was on adding features (compatibility forms, which can be seen as the
`eus-*` in the `auxiliary/` folder of [the project][eus-test]). Certainly, for a
little bit more experienced programmer than me, all that could be done
much quicker and with more quality (Affonso, for instance, who is a
much better programmer than I am, was able to do much more when he
did), but well.. Anyway, some of the problems encountered and
countered are listed in the `REPORT.org` file in `reports/`. *For the
GSoC fellows, if you want to see the part of the work due to me, you
may look at these [ansi commits](https://github.com/ericlesaquiles/ansi-test/commits/gsoc2018).
It is just a little confusing, but you'll certainly have no difficulty
figuring me out in there.*

### Pitfalls
---

It might be well to note some pitfalls for the me in the future (but other
people might ocasionally find it useful as well).

1. **Bias**: being a student of applied mathematics, having a "research bias"
in general not only hasn't hurt me much, but has most often been very
useful. I am to this day a proponent of the idea that one should have
a firm grasp of what is to be done before doing it. However, that leads
to the chance of inactivity or of "having a firm grasp on something"
which might not be of much use in the foreseable future. If one looks only for personal satisfaction
(which many mathematicians do, btw) that is fine. If, however, one
wants code that is to run in a robot in the space of a few months
(which could, arguably, provide greater personal satisfaction),
some care should be taken.

     Prior to this project, I've done one, at the
University, about Constraint Logic Programming. It was a one-year
project, so I could calmly feel the flavor of what I was doing, check
what kind of interesting ideas there were on the subject,
acknowledging that most of those would be of no use for me at that
time, and make use of the few ones that could be of some use.
Programming with Lisp has much space for a great variety of ideas, but
it was mostly a mistake to go too deep on some of the most isoteric
ones instead of focusing on what I had to (indeed, had proposed to)
do. That was perhaps most blatant with the multiple-values issue, when I started
looking for "continuations-stuff", which couldn't be reasonably
implemented (and hardly understood, due to lack of documentation), but
was true as well up to the end, when I was to implement the conditions system
for EusLisp, but ended up making something too complicated which
doesn't work well enough (indeed, I'm not sure as yet what to take out
of it. probably, will have to cut loads of code before submitting).

2. **Know the tools of the trade**:
If you're commited to doing something on a different field than the
one you're used to, it is important to have a good knowledge of the
tools of the trade. Doing great stuff requires adequate tools, and
knowing how to use those tools well is essential[^1]. I did know that, but
didn't give it its due attention. I found, for instance the emacs as a most
adequate tool for the programming and set out to learn how to use
it. Which I did, but then stopped.

    I would have to stop at some point,
of course, but not all "stopping points" are equally adequate and, had I learnt a
little more emacs more at the beginning, I could have saved much time
afterwards. This point is similar to the previous one, except that
this one refers, so to speak, to more stablished pathways (in other
words, this one refers to a more local framing, whereas the previous
one to a more global framing.. having a grasp on the global framing is
important if you want to think, whereas having a grasp on the local one is
important if you want to walk).

	  By tools I
don't mean only "physical" tools, but abstract ones as well. I could probably
have progressed much more by reading more EusLisp code and trying to
understand it at the onset than at the end. Apart from that, perhaps I could say that
knowing how to walk better gives you time to think more leisurely.

3. **Mind your confusion:** It is true that when I had proposed this project I knew it would be
a problem to find time for it and for the University classes and for the
other projects I had
already commited to (of those projects I will have something to say more in the future). And it was true, that was indeed a
problem. However, come to think about it, I don't think it was an
essential one. I did have to pay a closer attention to my schedule
(and I could improve that part), but the most important issue was the
usage of the time alloted for the tasks I had proposed myself.

      Time is, as I see it, the most precious asset we have as humans, and the
      most common way of losing it is by confusion. 
     Distress and tiredness don't help either, but they are a
     hindrance only insofar as they contribute to mental confusion (if
     you feel tired but calm, I'd say you're probably mostly fine,
     though probably somewhat slower). Don't get me wrong, I don't
     mean to say confusion is a bad thing per si. Most stupid persons
     are throughly confident and rarely confused. Confidence has more
     to do with an internal narrative, and one which is, more often than
     not, fanciful. In spite of that, it is of my opinion that
      confusion, when it appears, should be treated (not necessarily
      eliminated, mind you) as a matter of the greatest importance,
      because it can squander one's productivity (among other things
      as well) without one realizing it. Obvious as it might seem to
      you, it seems to me a rather subtle point, most often difficult
      to diagnose except with much attention. I realize, and this
      comes with great sadness, that I've spent a good part of my time
      in confusion. 

### Closing Remarks
---

The people at Google ask us, the students participating in GSoC, to
make some kind of report on what has been done. I thought about doing
it, but figured it wouldn't be very useful. It is mostly technical
stuff about which, if you feel like knowing more about, you may follow the links
above and, otherwise, having me talk about it would be, at best,
tedious (it would be more useful to have several posts, with the "best
parts", but I kind of have that already, and will expand on it, as
noted in other posts).

Instead, as I'm not sure whether the guys at JSK are aware of
how much this project has added to my learning experience, I will use
this space to compliment them, and let them be aware that it has
indeed been quite significant. But not only on the technical and soft
learning part. In particular, if I will go more into robotics (which I
intend to do), it will be mostly due to this project. I am very thankful.


<span style="color: #fffff; font-family: Babas; font-size: .75em;">
obs.: sorry i'm taking so long to make new posts (and, in particular,
for those euslisp-related ones, promised a long time ago), but we'll have them soon
</span>


---
---
---
---
[^1]: A professor of mine once used the analogy of either going to war with a pocket knife or with a war tank as a means of comparing different tools. Gauß is said to have gone to war with a pocket knife, and killed many enemies.

[eus-test]:	    https://github.com/euslisp/ansi-test
[euslisp]:	    https://github.com/euslisp/euslisp
[k-okada]:	    https://github.com/k-okada
[ansi-test]:	    https://common-lisp.net/project/ansi-test/