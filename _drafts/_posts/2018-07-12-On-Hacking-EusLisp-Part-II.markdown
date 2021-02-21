---
layout: post
title:  "On Hacking EusLisp - Part II"
categories: EusLisp
---

To fix the basic, we will be doing something a little bit different. This time, we will look at a special form implementation,
[`macrolet`, by Affonso][macrolet]. Just as a reminder, `macrolet` is
supposed to act like `flet`, except that instead of creating local
functions, it creates local macros. Affonso's `macrolet`
implementation is much like the one of EusLisp `flet`. It goes roughly as
follows:

* Define a `makemacrolet` function, which creates a macro for that `fletframe`;
* Define a `MACROLET` function, which calls `makemacrolet` for each
  macro to be defined.

It follows his `makemacrolet`:
```
struct fletframe *makemacrolet(ctx,nm,def,scp,link)
register context *ctx;
pointer nm,def;
struct fletframe *scp,*link;
{ register struct fletframe *ffp=(struct fletframe *)(ctx->vsp);
  register pointer p;
  int i;
  for (i=0; i<sizeof(struct fletframe)/sizeof(pointer); i++)
    vpush(makeint(0));
  ffp->name=nm;
  ffp->fclosure=cons(ctx,MACRO,def);
  ffp->scope=scp;
  ffp->lexlink=link; ffp->dynlink=ctx->fletfp;	/*dynlink is not used*/
  ctx->fletfp=ffp;
  return(ffp);}
```

Understanding the precise mechanics of how it works might take a few
more posts (not much due to its complexities, but rather due to my
difficulty in writing briefly), but if we just read it, what happens
shouldn't be too much of a mistery.

What we need first is to create a `fletframe`, whose name is yielded
by `nm`, with a lisp macro defined by `def` and scope `scp`. That
`fletframe is lexically linked to, well, wathever it is lexically
linked to. It is this bit that will make the definitions local. There
could be a dynamical link as well, but it is not used, since the
definitionss are intended as local. Those observations might become
clearer if we look at:

```
pointer MACROLET(ctx,arg)
register context *ctx;
register pointer arg;
{ register pointer macs, mac;
  register struct fletframe *ffp=ctx->fletfp;
  pointer result;
#ifdef SPEC_DEBUG
  printf( "MACROLET:" ); hoge_print(arg);
#endif
  GC_POINT;
  macs=ccar(arg);
  while (iscons(macs)) {
    mac=ccar(macs); macs=ccdr(macs);
    makemacrolet(ctx,ccar(mac),ccdr(mac),ffp,ctx->fletfp);}
  result=progn(ctx,ccdr(arg));
  ctx->fletfp=ffp;
  return(result);}
```

Before I forget, that weird `GC_POINT` there in the middle
is related to the garbage collection (I should tell you more about it
later).

Perhaps that "whathever" lexical context have become clearer now: it
is the `fletframe` of the thread context (or, if you have only one thread,
it is just _the_ `fletframe`). I remind you that we have that kind of
lexical frame so that we can chain them (so, for instance, you see
those two frames in the `fletframe` struct[^1]).

What, besides that, is going on in that `MACROLET` function? We can see the while
loop there, which we let making macros, as many as we
need. Afterwards, we rightly set the thread's flet frame and do a
progn on the remaining forms on the macrolet form, and return.

Maybe one can say there really are only two _important_ misteries here[^2]
the real mistery here, are about the mechanics of those frames, which
I explained  so poorly, and the origin of that `arg` thing, which
yields us almost all we want[^3]. About the poor frame explanation, I
will bid you to accept my apologies, as I don't think making a much
more detailed explanation would be of too great use at this point
(but, rather, later, with a more proper example).

The `arg` part, however, comes from it being a special form. As you
should remember, c-written EusLisp functions receive three
arguments. Special forms, however, receive two arguments only, namely
our familiar context, without which all is meaningless, and a list of
forms - the forms given to the special form.

To close it up, we might want to edit `l/eusstart.l` and
`l/exports.l`, just as before, to make the addition made visible. In
addition, you should consider adding a documentation session as well,
as Affonso has done nicely (you might check it at the link on the top
of this post).

---
---
---
---
[^1]: Which you can find in `eus.h`.
[^2]: As usual, that, mostly for psychological reasons, disregards those _real_ important ones we on our daily lifes.
[^3]: Same as [^2].

[jskeus]:           https://github.com/euslisp/jskeus
[euslisp]:	    https://github.com/euslisp/euslisp
[local-url]:	    https://mirrors.edge.kernel.org/pub/software/scm/git/docs/git-clone.html#URLS
[argentum]:	    https://github.com/ggreer/the_silver_searcher
[macrolet]:	    https://github.com/euslisp/EusLisp/pull/282
