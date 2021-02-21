---
layout: post
title:  "First steps on Euslisp"
categories: EusLisp
---

### Motivic introduction to EusLisp

I have recently been acquainted with [EusLisp][Eus][^1], when looking at the projects page for this
years's GSoC. Having Lisp in the name immediatelly got my interest, so I thought I should give it a
look. It is a somewhat peculiar language whose development started in the eighties by the hands of [Toshihiro Matsui][Matsui], to deal with robotics applications. Being a project in Japan, it should not come as a surprise that the language of choice was a Lisp-based one, as they're known for their enthusiasm for higher level programming (such as functional and logical),
but it was a pleasant surprise to see it developed with robotics in mind (but the surprise might be
due to my ingenuity).

The rational for the language scheme is a simple one, bottom-up: it was realized that list
processing capabilities was important to a nice robotics implementation and, apart from that, that it would be
interesting to have some object orientation, as nobody has, as far as I know (this could be only due to my ignorance),
thought of a better way of modelling objects than as objects
 (with properties (or "slots") and methods). It turns out that
the object orientation in EusLisp is not really our standard bread and butter object orientation model that we see walking down
the streets everyday, but we will get there soon enough. Most importantly, its nice "geometrical abilities"
(geometric modelling abilities, that is) are mostly based on this object system (as was intended).

An important point about EusLisp is that it is not an island, and tries to have some compatibility
with Common Lisp (mostly due to the advantages that usually come when adhering to some standard, such as code-sharing)
but fails in some important ways. In particular,

* the object system is quite different (for instance, it allows only single inheritance);
* it is dynamically scoped, which means.. no closures;
* it doens't allow for multiple return functions, which might be more of a hindrance than could
appear at first sight.

Another thing of note is that, as EusLisp is mostly used at the [laboratory of robotics at the University of
Tokyo][jsk] (they have some interesting projects, by the way, this [IRT][irt] one for instance),
no one has bothered to implement some functions or macros described by the Common Lisp specification, but which are not used by them (if they are not implemented because they are not needed, or the other way around, is a question for the philosopher).
However, in spite of the notable differences between EusLisp and Common Lisp, no one really knows
(up to now, that is), how close siblings they are. It would be an interesting information to have at hand,
because it would

* yield some confidence (or lack thereof) to some EusLisp user wanting to make use of a specific Common Lisp
library, or the other way around;
* allow, if it be wanted, to bring them (the two languages) closer more easily (as with two people unknown to each
other, but which might have lots in common).

As might be noted, "to bring them closer" could have two interpretations: to make EusLisp more like
Common Lisp, or the other way around (implementing EusLisp features in Common Lisp). Either way, it
would be interesting, as was mentioned, to have a better ideia of how these languages are similar.

### On the Motif

All that is well and dandy, but why would I be interested? First because it is Lisp related, which
would be mildly interesting per se, second because it is robotics-related, which, together with the first
note, makes it of even greater interest. I participated in a robotics team while on high-school, but
it was really low-level robotics (nothing related to AI or computer vision, or things of the kind).
But yet, I find the area of robotics in general of great interest (although it might not be
recognized at first sight from my choice of University course, namely Applied Mathematics, instead of Engeneering,
for instance, but a proper explanation for that will deserve a post of its own), and I've had a wish
to do something about that interest. But there is a third reason for that interest,
namely that the project was on [GSoC][gsoc] (google summer of code). And it was interesting because,
amongst the suggested sub-projects (I'm counting EusLisp as a project), there was one to make EusLisp
more compliant to Common Lisp. For me, who was not (or rather, isn't) too familiar with many of other
technicalities, this seemed a nice opportunity get closer to a field of interest (namely, robotics),
by means of a tool of interest (namely, Lisp), by running a project in (however slightly) tangential to another field of
interest (namely, linguistics) and, yet, possibly, to make some money to fund an intended summer
school I would like to go to the next summer.

Not too confident about it, due to my lack of technical expertise, I submitted a proposal for the project and, surprisingly enough, was accepted.
That presented itself to me as a great challenge due to several reasons, namely:

* I didn't have any experience in EusLisp or Common Lisp until recently (my Lisp experiences are mostly derived
from Scheme);
* The GSoC time-table starts on the middle of my semester which goes until July and comes back at
August, which presents a time difficulty;
* Apart from the usual classes I take on University, there is also a number of projects I've
compromissed myself with (which I will talk about later on, in other posts), which presents another time
difficulty;
* My equipment sometimes does not work properly, which is annoying.

I've never been famous due to my time management skills, so I'm having to improve that bit. Apart
from that, the rest is study and work, which I should know how to do at this point of the
championship, but which isn't neccessarily easy and has its caveats.

On the next post, I will talk more about EusLisp as a language and the project together with some of
its first difficulties and how they're overcome.

---
---
---
---
[^1]: If you're wondering what does the "Eus" part of "EusLisp" stand for, it is "Etl, Umezono, Sakura-mura", where "Etl" stand for "Electrotechnical Laboratory", the place in which Matsui-san did the work.

[Eus]:         https://github.com/euslisp/EusLisp
[Matsui]:      http://staff.aist.go.jp/t.matsui/
[jsk]:         http://www.jsk.t.u-tokyo.ac.jp/index.html
[IRT]:         http://www.jsk.t.u-tokyo.ac.jp/research/irt/index.html
[gsoc]:        https://summerofcode.withgoogle.com/
