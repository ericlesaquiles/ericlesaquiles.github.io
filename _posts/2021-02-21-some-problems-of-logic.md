---
title: On Some Problems of Logic
author: Éricles Lima
date: <2021-02-21 dom>
categories: [Logic]
tags: [Applied Math]
pin: false
usemathjax: true
---

# Are you just solving logic problems?

When one mentions "logic", I'd wager for most people what comes to mind are
those "fun little logic problems", that are usually word problems
where one has to "use logic" to solve a situation. And here, "use
logic" usually denotes that no special knowledge or acquaintance with
special theories of special fields are required - and then, upon
grasping the problem, usually stated in our good old ordinary
language, proceed by "pure reasoning" to solve it. 

Also, most "educated people" would have an idea that, well, you could
formalize the procedure, exchange words for symbols and then perhaps
automate at least some parts of the ordeal, but all that with no
fundamental change in understanding - that is, those symbols are
supposed to only formalize our natural languge anyway, so that is no
really big difference.

And then with that sort of understanding, people may perhaps be a
little surprise when hearing there is such a thing as "research in
logic" - for weren't the fundamental principles known since, like
Aristotle? Or, they might think, that's all really about solving logic
problems - peharps some more complicated and tricky ones than the ones
they're used to.

That isn't the case though. Although some logic puzzles of that kind can prove
quite important, it just happens that there are plenty of
fundamental problems in logic that are unsolved - in that there is not
consensus. 

This post is a (very) brief review on a selection of those problems -
but I do intend to come back to some problems more in depth later.

# What is logic, or rather, what is logic supposed to be?

This is a contentious question, but in most generality, logic can
regarded as being about **truth preservationg**. That is, if you have
some statements[^statements], you'd like to perform some operations on
those statements such that their truth is preserved ~ that is, if you
started with some true statements, you want to be sure that the
outcome of those operations will be also true, given the semantics of
the operations.

The said operations usually take the form of connectives like "*and*",
"*or*", "*not*", and a few others. 

That might seem a bit too vague to work with though. Philosophers and
logicians usually like to say, that **logic is about validity** of
statements, irrespective of their content. So, for example, if *a is
false* and *b* is any proposition, *a and b* is *false* independent on
whether *a* is about some friday night program or about some deep
aspect of the universe - one could say **logic doesn't care** what the
contents are.

And then the bussiness of logic, so to speak, would be to come with
appropriate rules and semantics for such "operations" (calling the
such as "and" and "or" operations might be contentious, but never mind
that).

And why would one be interested in that? Well, it sure looks like a
truth preservation apparatus would be quite a useful one. 

One would usually prefer to act according to some truth than to some
falsity, so, in the least, logic could provide us with some kind of *norm*
for separating some adequate actions from some inadequate actions, in
some way, doesn't it? Let's try to look into that a bit more closely.

# What is the normative status of logic, if any?

There is this idea that in order to be deemed rational beings we
think, or ought to think, according to the laws of logic. Or, at the very least, logic is
supposed to somehow guide our thoughts - "otherwise, of what use would
it be?", you could say. That's what many people, including logicians
and philosophers would say - some would go so far as saying (I'm
looking about you, Frege) that "for a thought to be a thought at all,
it must be constrained by the laws of logic". That is to say that
***logic is normative for rational thought***, meaning that is somehow
*norms* how reason is to take place.

But, is it so? If you think it is, then you've got a some of
problems in hand. To name a few:

1. In logic, it's usually held that if everytime *a* is true *b* is
   true, one can infer "a entails b" (written $$a \vDash b$$)
   - But one would hardly accept an argument such as "$$2 + 2 = 4$$,
     therefore Earth is a planet", although it satisfies that premise;

2. In logic, it's usually held that from an inconsistent set of
   propositions, one can infer anything (this is called the **"law of
   explosion"**, *ex falso quodlibet*, "from contradiction, anything
   follows") 
   - But, no matter how poor in maths one is, something like $$2 + 2 =
   5$$ and $$2 + 2 = 4$$ and $$4 \neq 5$$, therefore "Bolsonaro is a
   good politician" would hardly be deemed acceptable;

2. In logic, it's usually understood that, if "a entails c" (written
   $$a \vDash c$$), then "a and b entails c" ($$a,b \vDash c$$) where
   *a*, *b* and *c* are propositions - that means that by coming with
   new propositions, one can perhaps infer more propositions, but not
   less, and is called *monotonicity*;
   - But in life we could have, for instance, a = "I have two eyes", c
     = "I may be able to see", and have $$a \vDash c$$. But then, if
     I add b = "my eyes are blind", it's not the case that $$a, b
     \vDash c$$ - that is, **reasoning isn't, and isn't supposed to
     be, monotonic**;
	 
4. If you, in good faith, write a book on (say) Ornithology, then you
   (supposedly) have good reasons to believe that any given sentence
   in the book is true; however, you, quite reasonably, add to the
   preface "in a work of this magnitude, it's hardly possible not to
   incur in error at some point, for whathever reason; in case you
   find some error, feel free to let us know"
   - That is, you have enough reason to believe any given sentence
	 found in the book is correct, but not enough reason to believe
     *all* sentences found therein are correct. This is known as the
	 **preface paradox**.

There are many other such problems of course. To be sure, for any of
those problems, you can probably find a solution good enough, in the
realms of logic. For instance, there are plenty of logics that don't
have *explosion*, so maybe you could pick one of those; likewise,
there are such *non-monotonic logics*, which could also be of use.

But then, you can hardly deal with *all* problems that arise that way in a
uniform manner, and any solution that you find to one of them usually
create other problems further down.

> But well, is logic supposed to deal with that kind of thing anyway? If
not, why we'd care about it in the first place?

One possible answer is "logic hasn't really anything much to do with
reasoning - or, at least, not more than any other discipline", as
argued most notably by Gilbert Harman (see his [Change in
View](http://fitelson.org/probability/harman_change_in_view.pdf).

Another related answer is that *logic isn't quite supposed to deal with
reasoning* directly, but one can still come up with so-called
***"bridge principles"***, principles that purport to, well, bridge
logic and reasoning. That is something that John MacFarlane tries to
set up in his paper [In What Sense (If Any) Is Logic Normative
forThought?](https://www.johnmacfarlane.net/normativity_of_logic.pdf).

But this is still an open problem. Most people, it seems, would be
inclined to say that "yes, logic is normative". But how so? And which
logic? Would you go with some non-classical logic, such as FDE (as
[perhaps some buddhist would be inclined to
accept](https://aeon.co/essays/the-logic-of-buddhist-philosophy-goes-beyond-simple-truth))? 

Either way, there seems to be plenty of problems to solve there - and
I will probably come back to this in another post.

# Is logic exceptional? How so?

The previous discussion touches on another problem, this one possibly a
bit more philosophical. If you subscribe to logic as a foundation for
how we think rationally (although you might be starting to doubt
that), or, even if not entirely that, at least for (say) the methods
of sciences, then *you might be inclined to see logic as exceptional* in
some way. 

That is, if it's on the foundation, it better be a strong one. But
what is securing the foundations? "Well," you might say, "it's
supposed to secure itself". Which is a sensible enough answer.

But, if you think about sciences, such as biology,
physics, chemistry and whatnot, you can see all of them has their
methods, which change and evolve with time, as to make use of better
understanding of "how things work". 

Underlying that is supposedly a set of principles that is, again,
supposedly, unchanged, namely, the principles of logic - our secure
foundations. For (say) a physicist to decide on holding to or dropping
a theory, she should do so *logically*, *in some sense* - that's how
science is supposed to evolve.

In spite of that, we know the conception of logic varied greatly over
time - at least from Aristotle onwards in the westerner part, or at
least from Nagarjuna in the Indian subcontinent. The nowadays
mainstream *classical logic* is in many ways different from
aristotelian logic, and took its current shape only recently.

But then it would seem to be just like another science: one has
theories, carries experiments (for some appropriate sense of
"experiments"), and then hold on to, or drop said theories according
to the outcomes of the experiments. In principle, it wouldn't seem
unfair to lay the development of logic (whathever that is) in that
manner (but it does raises problems, to which I will come
shortly). But then it looks just like the development of any other
science.

One possible reply would be to say

> "You see, the understanding of logic might have changed, but the
> fundamental underlying principles have been, and still are, the same, and
> unchanged. With that in mind, logic could be said to be *exceptional* to
> other disciplines, in that we can hopefully find said fundamental
> underlying principles without the need to do *material
> experiments* - and those make up what we could rightly call 
> **'the one true logic'** or, not to incur in circularity, the
> 'correct logic'."

That is to say that there is a "ultralogic", and after finding it, by whichever
considerations, one need look no further: those are the foundations.
And there seemingly are good reasons to follow that line.

But another view is that there actually is no such "ultralogic", and
that what one actually needs are various *logics*, one for each use:

- so, you could prefer using something like classical logic for, say,
  mathematics, but some paraconsistent logic for when one need to deal
  with real contradictions (about contraditions, see for instance,
  Priest's [Can contradictions be
  true?](https://grahampriest.net/?ddownload=1063))
  
This view is known as **pluralist view about logic**: there are
possibly more than *"one true logic"*, and it lends easily to *anti-exceptionalism*. 

But you could be anti-exceptionalist about logics whilst holding there is
only "one true logic" 
(though [some suggest that, well, not
quite](https://ojs.victoria.ac.nz/ajl/article/view/4865)), all you
need is to say "logic is like any other science".

That is an open question, philosophically speaking. Hopefully I was
able to impart some of its significance.

By the way, apart from
*anti-exceptionalist*/*exceptionalist*/*pluralism*/*non-pluralism*
about logics, there is also the view (not held by many, as far as I
know) of ***nihilism about logic***, with the idea roughly as follows

> "It makes no sense to talk of pluralism about logic, for logic is
> supposed to be universal, and hence it should apply in every
> situation, as a "truth preserving thing". There, however, doesn't
> seem to be a logic that applies in every situation. Hence, there
> doesn't seem to exist something worth the name of *logic*.

I won't go much into that last option, but if you're interested, you
might want to check [this](https://www3.nd.edu/~cfranks/nihilism.pdf),
or [this nice lecture by Gillian Russel](https://www.youtube.com/watch?v=J12yz8J2m-M).

# Why study logic(s)

It turns out the study of logic (or of *logics*, if you're a
pluralist) can reveal many subtleties that would hardly be apparent
otherwise. As Graham Priest said in a lecture, "logic acts like a
magnifying glass, allowing us to see in details structures we didn't
know existed".
