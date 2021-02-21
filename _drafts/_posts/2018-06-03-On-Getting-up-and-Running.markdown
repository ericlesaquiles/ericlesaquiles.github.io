---
layout: post
title:  "On Getting up and Running"
categories: EusLisp
---

This is the first of a series of posts on which I will talk (mostly) about the usage of EusLisp and
its features. There will be another series on which I will say more about its internal workings
(coming soon). This first series will be mostly Lisp (with some notes about "system maintenance" (for lack
of a better word) and ROS, specially in the beginning), whereas the second will be mostly C (the
language that EusLisp's internal organs are built on). These posts will also reflect some of
my tinkering and digging, as well as my difficulties with the system but, apart from that, it'll be mostly stuff that's
somewhere in the documentations. Hopefully, after this series, you will have most of the information
needed to use the EusLisp system and to tinker with it at your own will (it must be said though, reading ROS' and EusLisp's manual is
still advised, of course).

### Notes on the installation

In order to install the software, you should first think about how you want to use it. As of right
now (as said before) much of the EusLisp project (and correlate projects by [JSK][jsk-ros-pkg]) is
made geared towards ROS. If you intend to use EusLisp with ROS, there is an installation procedure. However,
the EusLisp language per se exists independently of ROS, and it is possible to have it
like that (independently), in which case another installation procedure should be adopted. From here
on, the second option is not to be followed, as I will deal only with the ROS-related
installation (unless explicitly noted otherwise).

Before beginning on this project, I actually used Arch Linux but, as you can see [here][jskeus], it is recommended to use Ubuntu 14.04 (as of this writing), with the Indigo version of ROS for working on the project, so I switched to Ubuntu.
It was the Ubuntu 16.04 (with the [Kinetic][kinetic] version of ROS) I switched to, however, but it all went fine. I also installed the [Indigo][indigo] version
in another computer, and it also went fine. I've recently installed on Debian Jesse with no major
issues as well[^1]. As it turns out, there is an [Arch Linux package][ros-arch] as well for the kinetic version, which seems to
work well. As it seems then, if you use a Linux version other than Ubuntu, the system should mostly work, only
with perhaps some more work. If you are afraid to try it, maybe you could choose to be careful
instead. It is said that ROS assumes the default configuration, so it could yield unexpected issues
for non-default ones (by what I've heard, the assumed default is: using bash and python 2 on
Ubuntu), but I've not had problems with that as yet (as it seems to me, that could give problem only on a
few exotic circunstances. What those circunstances are, I am not aware of).
I should note though that, although Ubuntu runs significantly slower (than Arch) for me,
I'm not too sad about it, because it buys time in some other ways.

One final note on the installation is that you might be better off by looking at the [ROS
instructions][ros-install] prior to looking at the jskeus instructions, as they explain better the
ROS side (as should be expected, by the way), which may come of use if you are new to this. They
have some nice ROS tutorials as well, to which maybe you should give a try.

### Running

Just to be sure that all went well, you should try running the tutorial commands at
[jskeus][jskeus]. There might happen unexpected issues, in which case you should feel free to [report
it][issues]. By the way, after a quick glance, you should note a number of japanese writings. It is
much fun trying to understand what they mean, so you might want to give it a try :)

### Command line tools

#### EusLisp

After the systems seems up and running, I advise you to look at [this tutorial][eus-tut][^2]. It will
give you, among other things, some important information about packages used in the development
process, in particular [wstool][wstool] and [catkin-tool][catkin-tool] (you should follow the links
for more details about those. A short introduction is given below). Afterwards, the said tutorial introduces you to the command line
tools `eusgl`, `irteusgl` and `roseus`. Besides those, there are `eus`, `eusx` and `euscomp` as well. A more in
depth explanation about them may come in handy.

Those tools come in different levels of functionality:

* `eus0` which consists of only C-coded subprograms.
* `eus1` is loaded with compiled lisp programs under the "l" directory.
* `eus2` is eus1 + compiled compiler
* `eusg` is eus2 + compiled geometric packages
* `eus`  is eusg + xwindow, but does not connect to Xserver.
* `eusx` is a symbolic link to "eus", and tries to connect to Xserver.

The reason for eus to be made into small pieces is to ease troubleshooting and regeneration ([lisp readme][eusr]. Apart from those, there are also

* `euscomp` is usually a symbolic link to `eus` that, when invoked with a number of filenames,
compiles them. It may receive a number of command line arguments as well, and for information about
those you should look at the manual;
* `eusgl` is an EusLisp version that interfaces with OpenGL;
* `irteusgl` is an `eusgl` with the `irteus` expansions (meaning it has some more features);
* `roseus` is the [`eus` package][roseus] for interfacing with ROS.

If you're planning to use ROS, you would probably be making heavy use
of `roseus`, which comes in a different package (you may find it in [roseus]).

#### ROS

You will probably be using a few of ROS command line tools, so it's a good idea to become acquainted with some of them.
It follows a brief introduction to a few important ones. For information on some other commands, you
may refer to [ROS command line tools][ros-cmd].

* `catkin` is a build tool (it aids you to build the software) that comes bundled with ROS. By means of it, you can create a [`catkin_workspace`][catkin],
that is, a folder in which one mdifies, builds and installs `catkin` packages;
* `wstool` is a program to work with various version control systems (it lets you work with git, mercurial, bazaar, among
other version control systems. You probably don't want to worry about whether the codes you are getting come from mercurial or git, for
instance, and `wstool` lets you do just that (not worry);
* `rosdep` helps you get a project's dependendies;

### On running EusLisp

I would like to note that, on EusLisp interpreter, you can ommit the first parentheses pair (i.e. write something like `eus$ + 1 2`). For some
reason, I don't know of any other Lisp implementation doing that. Apart from that, EusLisp does not support readline, which means it is not easy to work with it
barebones on the Unix shell (it doesn't offer commands history, for instance). But then, even if it was, it would still be better to use it through emacs, so there is no reason to be sad about that.
If you want to use the command line, a simple `M-x shell` should do it. There is an [EusLisp major mode][major-mode] as well, in case might wonder. If you're not familiar with emacs, this is a good chance to change that.

### On EusLisp internals

The language is developed since the eighties, and has some good history on its back. For one thing, if you look at [the github repo][euslisp],
you will find many interesting things. For instance, as of right now, the language does not have the loop macro out of the box. The reason that's a
problem is that ansi-test makes heavy use of that macro, so many tests would fail due to its lack.

The loop macro is a funny thing because, in spite of its power, it has a [polemic][paul-loop] history. Most lisp implementations nowadays use a derivative of
MIT loop macro (but CLISP is one that doesn't) from the eighties that nobody would dare lay fingers on, as it would probably crash it (that MIT loop macro is hacky). Then, I
set up
researching how to implement it. There is a modern loop macro implementaton that's roughly explained [here][loop-macro]. It is a nice one. Makes use of combinator parsing, but relies too much on lexical scoping (closure), for instance, which would make it somewhat difficult to port to EusLisp. If you're wondering what is combinator parsing, I wondered the same (it's an interesting subject about which I cannot say much right now, but plan on doing a post about it on the future).  Of course, it would be much faster to just copy someone else's loop macro and use it, which is what one of the members of the JSK Lab did.

What nobody realized (including me, until later) is that, as a matter of fact, EusLisp did already have a loop macro (it is on [this][loop-folder] folder).
That is funny, because there are files in use in that folder (such as `unittest.l`) but, in spite of that, no clue was had that there was a loop
macro lurking in there. A look at the `lib` folder, as well as a look into the `doc` folder will present us many pleasant surprises. We will talk
about that (and then with more code) next time. I will talk about some basic tutorials as well. See you then.


---
---
---

By the way, if you're curious about the truck driver strike's outcome, it's mostly over. There is still risk of a new strike, due to agruopecuary's
efforts to un-make the agreement (one could wonder "why would they do that?", it seems they have different priorities).

**EDIT:** I'm sorry for those who read this post earlier, as it had many gross mistakes. That made
me get to the opinion that people shouldn't write prose when they're sick.
---
---
---
---
[^1]: If you're wondering the reason for me having installed the system on so many different machines, the thing is that my actual computer (the Ubuntu 16) doesn't work at times (or rather, works, but with severe limitations and annoyances), so I borrowed another notebook from my advisor, so that I could go on working. It was, however, an old notebook, and it started showing network and hard drive issues shortly thereafter. Then, she borrowed me another notebook (this one with Debian), younger than the previous one, which just works.
[^2]: Thank you [Affonso][affonso] for the translation!

[jsk-ros-pkg]:      https://github.com/jsk-ros-pkg/
[jskeus]:           https://github.com/euslisp/jskeus
[ros-arch]:         https://wiki.archlinux.org/index.php/ROS
[ros-install]:      http://www.ros.org/install/
[issues]:           https://github.com/euslisp/jskeus/issues
[eus-tut]:          http://euslisp-tutorial.readthedocs.io/en/latest/
[affonso]:          https://github.com/Affonso-Gui/
[wstool]:           http://wiki.ros.org/wstool
[catkin-tool]:      https://catkin-tools.readthedocs.io/en/latest/
[roseus]:	    http://wiki.ros.org/roseus/Tutorials
[roseus-code]:	    https://github.com/jsk-ros-pkg/jsk_roseus
[catkin]:	    http://wiki.ros.org/catkin/workspaces
[major-mode]:	    https://github.com/iory/euslisp-mode
[euslisp]:	    https://github.com/iory/euslisp-mode
[paul-loop]:	    http://www.paulgraham.com/loop.html
[loop-macro]:	    https://www.youtube.com/watch?v=ZJr81DtSwUc
[loop-folder]:	    https://github.com/euslisp/EusLisp/tree/master/lib/llib
[kinetic]:          http://wiki.ros.org/kinetic
[indigo]:           http://wiki.ros.org/indigo
[ros-cmd]:          http://wiki.ros.org/ROS/CommandLineTools
[eusr]:		    https://github.com/euslisp/EusLisp/tree/master/lisp
