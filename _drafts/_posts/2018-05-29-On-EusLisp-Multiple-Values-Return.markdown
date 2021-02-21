---
layout: post
title:  "On EusLisp Multiple Values Return"
categories: EusLisp
---


This is an unusual week in that there is a national truck driver strike that is making almost every
store with little, if any, supply. That has many incoveniences, but there is at least one convenience,
namely that it forced the stopping of classes, which means I'll be able to focus much more on the project, as
well as have more time to study. Hopefully, the strikers will have won what they want as well[^1].

Anyway, as [already mentioned]({{ 'euslisp/2018/05/25/First-Steps-On-Euslisp.html' | relative_url }}),
there is a handful of specially important incompatibilities of EusLisp with Common Lisp (of which,
maybe the most striking one is the object system). I plan on dealing (and writing about) each one of
them more appropriately, one at a time. Now is the time of the multiple value system (or lack
thereof).

### Multiple values

EusLisp doesn't support multiple values return (ostensibly due to efficiency reasons, but I'll come back to this shortly) and Common Lisp does.
Multiple values returns are not (in general) widely used, it must be said, but they were specified
in order to allow for a more efficient passing of values, when needed (it should be more efficient
than returning a list, for instance, although, as the specification does not specify how it should
be implemented, a Common Lisp implementation may implement its multiple value return as just that, a
list).

A look at [CLtL][cltl] (you may begin on page 190, if you have the document) gives us a glimpse of how the multiple values
"structure" (so to speak) should work. It is a somewhat extensive specification, telling us of what
a handful of functions, macros and special forms are supposed to do. Besides that, it specifies how multiple
values should be handled in various circunstances (so that, for instance, if a function returns more values than
what the caller can handle, the superfluous ones should be discarded and, if a function returns less
values than the caller wished for, the extra values are filled with nil). That gives us a hint that,
in order to make EusLisp fully Common Lisp compliant in respect to
multiple values, more than a simple superficial work would have to be done.

It should be noted that the multiple values structure on Common Lisp is distinctive from Scheme,
for instance, in that Scheme specifies only two functions (namely,
`values` and `call-with-values`) and an error if there is a mismatch between expected number of
arguments and received ones (ostensibly, to avoid the use poor programming practices). In spite of that, it
can be seen that there are only two really fundamental constructs for multiple values in Common
Lisp, namely `values` and `multiple-value-call`, which can be used to implement the others. One
could reasonably argue that implementing those two constructs in EusLisp, then, would largely make it.

### Continuations

Now, when researching about how I could, perhaps, best implement multiple values for EusLisp I stumbled
over two documents, [this one][cmucl] on the "Design of CMU Lisp" and [this one][mvrs] on
"An Efficient Implementation of Multiple Return Values in Scheme", both of which make use of (some
sort of) continuations. As I try to follow a philosophy of being gentle to the non-expert (such as myself),
and as it may serve as a reference, it follows a brief
overview of continuations and its styles. For a somewhat more complete introduction, you may refer
to [this page][scheme-book] (it is an excerpt of a pretty decent book, by the way).

In a nutshell, a continuation is an abstraction of "the computation that is left to be done". Say,
for instance, that you are interested on evaluating the expression `(+ 1 2)`, we can, then, isolate
the following continuations:

* the one waiting for the value of "+";
* the one waiting for the value of "1";
* the one waiting for the value of "2".

In that expression, the continuation of "1" is `(+ _ 2)`, where `_` is an empty slot, indicating it
expects a value. In languages such as Scheme and Ruby continuations are first class citizens, which
means that one is able to attribute it for a variable and do all sorts of things one is usually able
to do to variables (in Scheme, it is done by `call/cc`, but the utility of such a construct [has been
questioned][no-call/cc]). In that kind of language, one would then be able to, for instance,
attribute to a variable `k` the forementioned continuation `(+ _ 2)`, which expects one value `v`
and does `(+ v 2)`.

In spite of what one might feel led to think by the previous simple example,
continuations are useful for many sorts of things, so it may come as a relief to know that, when a
construct such as `call/cc` is not available to make continuations first class, one can usually
emulate it with a `lambda` expression, in what is known as continuation passing style, or CPS for
short. The idea is simple: if you want to make use of continuations, abstract them and pass them as
parameters. For instance, the following code (adapted from [here][scheme-book])

```
(labels ((f (lambda (x) (cons 'a x)))
         (g (lambda (x) (cons 'b (f x))))
         (h (lambda (x) (g (cons 'c x)))))
  (cons 'd (h '())))
```

yields `(d b a c)`. On this code, f conses the symbol b to x and returns the result to the continuation of g.
We can turn this into CPS by making such implicit continuations explicit functions, such as:

```
(labels ((f (lambda (x k) (#'k (cons 'a x))))
         (g (lambda (x k)
             (f x #'(lambda (v) (#'k (cons 'b v))))))
         (h (lambda (x k) (g (cons 'c x) #'k))))
  (h '() #'(lambda (v) (cons 'd v))))
```

That sort of code seems more complicated at first, but it helps if one reads k as "the continuation
of ...", as that's what it should represent (though, after that, it still is more complicated than the
initical code).

More complicated at it seems, CPS may help us, among other things, to deal with communication between procedures (by
means of success/failure continuations, for instance), with multithreading and, what is our focus now, with
multiple values.

As mentioned before, Kent Dybvyg has [this article][mvrs] relating to a multiple values
implementation for Scheme. It, however, makes use of (at least seemingly so) predefined constructs,
such as `mv-call`, `mv-values` and `mv-let`, with no greater hint as to how they might be
implemented. He has, however, another implementation in [this other excerpt][scheme-book-cont],
which, when translated to Common Lisp, would look like the following:

```
(defvar magic (cons 'multiple 'values))

(defun magicp (x)
  (and (consp x) (eq (car x) magic)))

(defun values (&rest args)
  (if (and (not (null args)) (null (cdr args)))
      (car args)
      (cons magic args)))

(defun call-with-values (producer consumer)
  (let ((x producer))
    (if (magicp x)
        (apply #'consumer (cdr x))
        (funcall #'consumer x))))
```

Here we define `call-with-values`, because that is the specified by the Scheme standard, but it is
equivalent to the ones specified by Common Lisp in the sense that one can be used to make the other,
and the other way around (so I'll not be paying much heed to this right now). Apart from that,
remember that one of the reasons for the idea of multiple values to exist in the first place is
efficiency, and this code does little to be efficient.

### The implicit continuations way

This was to say that if we want multiple values, we should probably give some more thought to the process.
Remember I mentioned [this document][cmucl] at the beginning. It gives a rough explanation about the CMUCL
compiler (whose successor is our beloved [SBCL][sbcl]). By what we can see on the document, the multiple values
is dealt with by what they call Implicit Continuation Representation. "It behaves similarly to CPS, but its
representation is different enough that they are not interchangeable." (I'm paraphrasing the
document). The thing is, strange as it might sound, continuations are a notion people use to [create
compilers][ml-comp].


---
---
---
---


[no-call/cc]:               http://okmij.org/ftp/continuations/against-callcc.html
[cltl]:                     http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html
[cmucl]:                    https://common-lisp.net/project/cmucl/doc/CMUCL-design.pdf
[mvrs]:                     https://www.cs.indiana.edu/~dyb/pubs/mrvs.pdf
[scheme-book]:              https://www.scheme.com/tspl4/further.html#g63
[scheme-book-cont]:         https://www.scheme.com/tspl4/control.html#g104
[sbcl]:		            	    http://www.sbcl.org/
[ml-comp]:                  https://www.amazon.com/Compiling-Continuations-Andrew-W-Appel/dp/052103311X/ref=sr_1_8/141-4592538-3932161?ie=UTF8&qid=1528073807&sr=8-8&keywords=andrew+appel

[^1]: The strike is on its 9th day and the president has announced a deal. But some strikers seem to disagree with that. I'll issue an update when it's over.
