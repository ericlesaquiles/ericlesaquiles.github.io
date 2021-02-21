---
layout: post
title:  "On Hacking EusLisp - Part I"
categories: EusLisp
---

The good thing about having the source of the language one work with is
one can hack it - extend it, shrink it, make it do what
comes to the mind. Of course, that may be thought as
"the bad thing" about having the source of the language as well, but where I
come from we tend to look at liberty with good eyes.

By now, you should be aware the EusLisp's source lies on
[EusLisp's github][euslisp]. We will need a way of running that code
smoothly if we want to hack it. The simplest and most robust way
(though, in some sense, not the least complex) of doing
this I could find is by means of [jskeus][jskeus]. Just follow the steps:

* Clone the jskeus repository;
* In the jskeus repository, run `make` (you will probably need `sudo`).

If all goes well (which it might not, in particular if you
lack some library, in which case you should pay attention to
the log messages so as to figure out what to do), you should have
properly installed the EusLisp version from [EusLisp
github][euslisp]. A hacker as you are, you'd probably want to install
your own version. To do so, you may run `make GIT_EUSURL=<your_url>
GIT_EUSBRANCH=<your_branch>` (the default values for `GIT_EUSURL` and
`EUS_BRANCH` are `https://github.com/euslisp/euslisp` and `master`, respectively)[^1].

### The code

Now we have some idea on how to run it, let's look at the
code. It is on the `lisp` folder of your EusLisp directory. Take a look at the `c` code, on the `c`
folder. If you are to write code for EusLisp, you are expected to be
familiarized, at least, with the contents of `eus.h`, so you might want
to start by there (and by its companion, `eus.c`).

Those codes were written mostly on the 80s and 90s, so it has a whole
different flavor. On my part, I found the style strange at first, as I
do when reading, say, Tolstoy, but I'd soon get used to it. Overall, my
feel is as if reading a book by a wizard of yonder and of yore, whose main spells
I had either never learnt or learnt and forgotten, so it feels
refreshing.

Now, in order to better understand the code, as well as what to do
with it (if anything), I will run you trhough a series of
tutorial-like examples, one at a time, starting from the simplest. I
will try not to bore you with too many details, but if you think I'm
skimming too much, making it incomprehensible, do feel free to reach me
out and will be glad to try to help you (but make no promise on that).

### Defining a function

To start with, let's try to define a c function visible to EusLisp
(that is, a function callable just as any other from EusLisp language,
but which is written in c). That is useful not only as a didactically
, but practically as well, as, depending on what you want
to do, it might be way more convenient to do it in c than in EusLisp
(though, we hope, that won't happen too often).

EusLisp c functions must accept three arguments:

* A context pointer, `ctx`;
* An integer to stand for the number of arguments to the function, `n`;
* The arguments block, `argv`.

Here, "context" means the thread context, consisting of a (c) stack,
a binding stack as well as frame pointers, which chain blocks like
`lambda`, `block`, `flet` and `let`.
It is important for those arguments to have those names, as the
c-macros in `eus.h` rely on that. As you might have figured
out, the file in which you are to make your function must include `eus.h`.
There is an example on the manual on how to define EusLisp functions
in C. The one they show is one on making the average of
numbers, but I don't find that interesting enough. Instead, I'm to
look at `SORT`, a function that sorts. Being present in EusLisp as
a shell to quicksort, it is simple enough so I don't have
to explain you the agorithm, as everybody should have an ideia of what
it's supposed to od, but it's
complicated enough to show us some interesting uses of interesting
spells. Let's look at it by parts:

```
pointer SORT(ctx,n,argv)
register context *ctx;
register int n;
pointer argv[];
{ register pointer seq,work;
  pointer *xsp;
  register int i,width;
  ckarg2(2,3);
  seq=argv[0];
  if (seq==NIL) return(NIL);

#if THREADED
  mutex_lock(&qsort_lock);
#endif
	...
```
Firstly we see - no surprise - what was just said: three arguments,
with their forementioned names[^2]. You should realize as well that
we have many `pointer`s around. You should have an intuitive
understanding of what a `pointer` is, and to look at `eus.h` to have
a more concrete one. You should realize as well that, apart from
that olden c style, the name of the function is in uppercase. You
should probably stick to this convention.

Afterwards, we make some checking (the "ck" part of things like
`ckarg2` stands for "check") on the arguments[^3] and issue a
`mutex_lock`. You should realize the original language implementors
have put a good enphasis on parallelism, so there is a good deal
of code (including functions, macros, and the such) to deal with
thread-related issues and features. If you plan to use EusLisp in a stand-alone
manner, that might be useful. If, however, you intend to use it with
ROS, since the ROS system is (at least, the last time I checked)
one process - single thread (so you may have many processes, with one
thread each, instead of many threads from a few processes), that
shouldn't make much of a difference, so I won't discuss it much for
now (if you know what a mutex is, it should be clear to you what that
part means, and, if you don't, a quick research should make it so).

Going on:

```
	...
  qsortctx=ctx;
  COMPAR=argv[1];
  if (n==3) COMPKEY=argv[2]; else COMPKEY=0;
  if (islist(seq)) {	/*sort list destructively*/
    n=0;
    work=seq;
    xsp=ctx->vsp;
    while (islist(work)) { ckpush(ccar(work)); work=ccdr(work); n++;}
    COMPTYPE=ELM_FIXED;
    qsort(xsp,n,sizeof(pointer),(int (*)())compar);
    work=seq;
    for (i=0; i<n; i++) { pointer_update(ccar(work),*xsp++); work=ccdr(work);}
    ctx->vsp-=n;}
	...
```
You should be able to guess what most of the constructions that appear
in there do (yes, ccdr means what you think it does, a `cdr`). Some of them could, however,
seem somewhat obscure. Yet, you could guess what they mean by looking
at a few examples (meaning a quick [ag][argentum] trhough the code
could help). To give you a hand:

* Doing a quick search, the `vsp` field in `context` seems to be
commonly used to check whether your stack is ok (I think of it as
standing for "value stack pointer") and, in order cases, it seems to
act kind of like  a stack. We pass it to xsp so as not to mess with it,
and then update it before leaving.

If you are to order some set, you need an order relation, which, in
this case, is represented by the function `compar` (which will follow
in a moment, hold on). It makes use of `COMPTYPE` so as to have an idea of
what comparing it is to do (I mention it here so that you're not left
wondering what that strange thing is for).

Going on with the code:

```
  else if (isvector(seq)) {
    COMPTYPE=elmtypeof(seq);
    if (COMPTYPE==ELM_CHAR || COMPTYPE==ELM_BYTE) width=1;
    else if (COMPTYPE==ELM_BIT || COMPTYPE==ELM_FOREIGN) error(E_NOVECTOR);
    else width=sizeof(eusinteger_t);
    qsort(seq->c.vec.v,vecsize(seq),width,(int (*)())compar);}
#if THREADED
  mutex_unlock(&qsort_lock);
#endif
  return(seq);}
```

I think this part is clear enough without further explaining. Let's
then quicckly look at `compar`[^4]:
```
static pointer	COMPAR,COMPKEY;
static int 	COMPTYPE;
context *qsortctx;

int compar(x,y)
pointer *x, *y;
{ pointer xx,yy,result;
  eusfloat_t *fx,*fy;
  numunion nu;

  switch (COMPTYPE) {
    case ELM_CHAR: case ELM_BYTE:
		xx= makeint(*(char *)x); yy= makeint(*(char *)y); break;
    case ELM_INT: xx=makeint((eusinteger_t)(*x)); yy=makeint((eusinteger_t)(*y)); break;
    case ELM_FLOAT:
		fx=(eusfloat_t *)x; fy=(eusfloat_t *)y;
		xx=makeflt(*fx); yy=makeflt(*fy); break;
    default: xx= *x; yy= *y;}
  if (COMPKEY) {
    xx=call1(qsortctx,COMPKEY,xx);
    (*qsortctx->vsp++=((pointer)xx)); // vpush
    yy=call1(qsortctx,COMPKEY,yy);
    (*qsortctx->vsp++=((pointer)yy)); // vpush
  }
  result=call2(qsortctx,COMPAR,xx,yy);
  if (COMPKEY) {
    (*(--(qsortctx->vsp))); // vpop
    (*(--(qsortctx->vsp))); // vpop
  }
  if (result==NIL) return(1); else return(-1);}
```

You will realize that there are various ways for us to deal with
numbers. They were needed because EusLisp code is intended to run
consistently in many kinds of systems and, sometimes, it is desirable
for the functions to deal well with more than one kind of number. In
particular, there is `numunion`. You could have found it strange we
define a variable that is not used, but the thing is it is, by the
macros at `eus.h` (just to enphasize: you'd want to be acquainted with
that file's content).

You will find `makeint` in eus.c. Besides
deciding about whether to make an ordinary integer or a big one
(bigint), it stamps a flag bit on it which is used to discriminate
pointers floats and integers. If you're in doubt what `makeflt` means,
"flt" stands for "float" (instead of "flet", as one could think).

Now, at last, but not least, we need to "defun" that function, for
EusLisp. In our case, that is done in `sequence.c` (the file where
`SORT` is defined):

```
void sequence(ctx,mod)
register context *ctx;
pointer mod;
{
  QIDENTITY=defun(ctx,"IDENTITY",mod,IDENTITY);
  QIDENTITY=QIDENTITY->c.sym.spefunc;
  defun(ctx,"SUBSEQ",mod,SUBSEQ);
  defun(ctx,"COPY-SEQ",mod,COPYSEQ);
  defun(ctx,"REVERSE",mod,REVERSE);
  defun(ctx,"NREVERSE",mod,NREVERSE);
  defun(ctx,"CONCATENATE",mod,CONCATENATE);
  defun(ctx,"COERCE",mod,COERCE);
  defun(ctx,"MAP",mod,MAP);
  defunpkg(ctx,"RAW-FILL",mod,FILL,syspkg);
  defunpkg(ctx,"RAW-POSITION",mod,POSITION,syspkg);
  defunpkg(ctx,"RAW-FIND",mod,FIND,syspkg);
  defunpkg(ctx,"RAW-COUNT",mod,COUNT,syspkg);
  defunpkg(ctx,"UNIVERSAL-REMOVE",mod,UNIREMOVE,syspkg);
  defunpkg(ctx,"RAW-REMOVE-DUPLICATES",mod,REMOVE_DUPLICATES,syspkg);
  defunpkg(ctx,"RAW-DELETE",mod,DELETE,syspkg);
  defunpkg(ctx,"RAW-SUBSTITUTE",mod,SUBSTITUTE,syspkg);
  defunpkg(ctx,"RAW-NSUBSTITUTE",mod,NSUBSTITUTE,syspkg);
  defunpkg(ctx,"VECTOR-REPLACE",mod,VECREPLACE,syspkg);
  defun(ctx,"SORT",mod,SORT);
  defun(ctx,"LENGTH",mod,LENGTH);
  defun(ctx,"ELT",mod,ELT);
  defun(ctx,"SETELT",mod,SETELT);
  }
```
It is on the line just before LENGTH and just after
VECTOR-REPLACE. `defun` takes as arguments the contex in which you are
defining the function, the name of the function, the module in which
it is to be and the function itself. The module is, roughly speaking
something that helps ease the compilation, linking and dependency
checking in chunks (we will say more about that later). You see there
is a related `defunpkg` as well, we will touch on it at a later
time. To close it up, you might want to add an appropriate line in
`eus_proto.h` (you can see one for `SORT` there) and in `l/eusstart.l`
 in `l/exports.l`,
so as to make  the function visible.

Now, hopefully, you should have some rough idea on what to do to make
functions.

### Closing remarks

As you might have seen, EusLisp is a reasonably big project, with enough files
and lines per file that you could get lost or waste a big amount of
time just navigating those. I then suggest, if you don't know it
already, to learn how to make use of `ctags`. It could save you a good
amount of time and comes in many flavors, so you may find your
favorite.


---
---
---
---
[^1]: Be aware that you can [use git clone on a local repository][local-url].
[^2]: The register keyword hints the compiler to place the variable in a register - a quicker memory. As far as I know, it is irrelevant for modern compilers.
[^3]: You should look at eus.h for a definition of `ckarg2`.
[^4]: You should note, that piece of code should come before the previous function.


[jskeus]:           https://github.com/euslisp/jskeus
[euslisp]:	    https://github.com/euslisp/euslisp
[local-url]:	    https://mirrors.edge.kernel.org/pub/software/scm/git/docs/git-clone.html#URLS
[argentum]:	    https://github.com/ggreer/the_silver_searcher
