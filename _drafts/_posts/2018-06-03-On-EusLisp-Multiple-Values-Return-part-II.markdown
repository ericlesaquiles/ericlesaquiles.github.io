---
layout: post
title:  "On EusLisp Multiple Values Return-part II"
categories: EusLisp
---

The ending of the [last post]({{ 'euslisp/2018/05/29/On-EusLisp-Multiple-Values-Return.html' | relative_url}})
might have seemed a little bit too abrupt (to be fair, it was). The thing is that before finishing
it, I had a meeting with [Kei Okada][okada] and [Yuki Furuta][furuschev] to talk about the project
and it was realized (i.e. they made me realize) that efficiency is not so much of an issue as
implementation time and complexity is being. Which means that, instead of going for a nicely fit,
almost platonic, multiple values interface, I should pick an "alright" one that is simple and make it
quick (actually that corresponds, in this context, to a most efficient implementation, if you think
about it). So I kind of dropped that lengthier explanation/implementation, with "continuations" and stuff (to be
precise, the CMUCL is not reaally a continuation in the tradicional sense, but it has the idea) for
now (but rest in peace, as I plan to make another post about that in the future).


Now, it will be handy to keep in mind that, according to [CLtL][cltl], in spite of the seemingly
great quantity of forms for dealing with multiple values, there are actually only two specified as
special-forms, namely `multiple-value-call` and `multiple-value-prog1`, by means of which the others
can be implemented. Of course, we need forms to produce multiple values as well. There actually is
only essentialy one of those (namely `values`).

With that in mind, you might recall that, in forementioned previous post, we had the following code
(the code is mostly repeated here, with a few additions, for reference):

```
(defun my-list-values (&rest args)
  "Takes a multiple-arguments return of values and make it a - simple - list"
  (if (magicp (car args))
      (cdr (car args))
      (car args)))

(defun my-values (&rest args)
  (if (and (not (null args)) (null (cdr args)))
      (car args)
      (cons magic args)))

(defun my-values-list (list)
  "The values contained in list are returned as multiple values"
  (apply #'my-values list))

(defun call-with-values (producer consumer)
  "Producer is a no-arguments function whose return values are fed to consumer"
  (let ((x (funcall producer)))
      (if (magicp x)
          (apply consumer (cdr x))
          (funcall consumer x))))'))
```

Those definitions were actually geared towards multiple values for the Scheme language, instead of Common Lisp. There are a
few notable differences between the two, the most obvious one being the different forms they
provide (one cannot find `call-with-values` in an ordinary Common Lisp code, for instance), but the
most important one being the lack of some flexibility in the following sense. In Common Lisp multiple values
setting, if a "consumer" form (the form which will receive arguments) receives more arguments
than it expects, the remaining ones should be silently discarded and, otherwise, if it receives
less arguments than it expects, the others should be filled with `nil` (to make it clear: this is only valid to
arguments passed trough multiple values. If such a mismatch occurs by normal means, an error is
raised when it should be). Scheme, instead, provides an error if there is arity mismatch.

---
---

Before trying to deal with such important issues, let's make a change to the previous code, so that it looks
a bit more like Common Lisp.

It is a first try on implementing the `multiple-value-call` (which, recall,
is supposed to be a special form). That function receives a function *fun* as the first argument and any number of
forms as other arguments. Those forms are evaluated and their return values (all their return values, that is)
are passed as arguments to *fun*. It is somewhat similar to `call-with-values`, except that the
*producer* argument in call-with-values (remember, call-with-values is called as `(call-with-values producer
consumer)`) is supposed to be a no-arguments function. The idea here is to execute each of the forms
in a *lambda* (the argument to `call-with-values`), have their return values accumulated, and pass
the accumulation to the function, as shown below (the indentation was impaired so that the definition could better fit on screen):

```
(defmacro my-multiple-value-call (fun &rest forms)
  "Reunites the return values for each form in form and applies fun to them"
  (progn
    (setq is-multiple-values-on t)
    `(call-with-values #'(lambda()
                     (let ((form-values (list ,@forms)))
                       (progn
                         (setq is-multiple-values-on nil)
                         (my-values-list
                          (loop for el in form-values append (my-list-values
                                                                  (if (consp el)
                                                                      el
                                                                    (list el))))))))
                             ,fun)))
```

It should be noted that one *important* rule is not being followed here, namely the *flexibility* of
a producer being able to send a different quantity than the one expected by the producer. By using
`my-values`, the consumer *must* expect as many values as the quantity given to `my-values`. This is
a shame, because the main reason for all this is to provide compatibility with Common Lisp, and
Common Lisp code makes frequent use of only the first value of a multiple values function. We could
make one way out of this, for this one case in which only the first value returned by a function is used,
if we make use of the compromise that multiple values are to be dealt with only by means of those
functions (or macros made out of those functions), creating a variable that tells us us whether a call to
values is being made by `my-multiple-value-call`. If it isn't, `value` should return at most one
value[^2].

```
(defvar is-multiple-value-call-on nil)

(defun my-values (&rest args)
  (if  (or (and (not (null args))(null (cdr args)))
           (not is-multiple-value-call-on))
      (car args)
      (cons magic args)))

(defun my-multiple-value-call (fun &rest forms)
  (progn
    (setq is-multiple-value-call-on t)
    (call-with-values #'(lambda()
           (my-values-list
            (loop for el in forms append (my-list-values (funcall (eval el))))))
          fun)
  (setq is-multiple-value-call-on nil)))

(defmacro my-multiple-value-call (fun &rest forms)
  "Reunites the return values for each form in form and applies fun to them"
  (progn
    (setq is-multiple-values-on t)
    `(call-with-values #'(lambda()
                     (let ((form-values (list ,@forms)))
                       (progn
                         (setq is-multiple-values-on nil)
                         (my-values-list
                          (loop for el in form-values append (my-list-values
                                                                  (if (consp el)
                                                                      el
                                                                    (list el))))))))
                             ,fun)))
```

To be fair, that would be enough for most use cases. In spite of that, that implementation has several issues.
Firstly, it is so hacky that makes me want to cry (it was a hacky thing from the onset, but not this much).
Secondly, this raises sinchronization issues (the variable `is-multiple-value-call-on` should
probably make use of a semaphor). Being taught not to
cry since an infant, I would be ready to gloss over the first issue, but not the second one. In
principle, `values` could be used a lot, so making it a semaphor-dependent thing could create too
small a bottleneck. One should better find an alternative[^1].


Amongst the first places to look for is the "ELisp lisp" implementation, the Emacs language. Elisp
shares some likeness to EusLisp, returns only one value as well, and provides a Common Lisp
compatibility layer. The multiple-values part is documented [here][emacs-mv]. It reads, at the end:
> Since a perfect emulation is not feasible in Emacs Lisp, this package opts to keep it as simple
> and predictable as possible.

Meaning it has only a very limited multiple-values structure. Not a very heart-warming prospect.
At first, I made the puerile mistake of thinking this was a minor
issue that could be quickly dealt with. After being able to think about it for sometime
while preparing food and taking shower and in other moments of leisure (when I'm usually able to have better ideas), I became increasingly convinced that that was not the case.
If you come to think about it, given that we want the multiple values interface to supply `nil`
or to silently discard exceeding values when (and only when) it is adequate to do so, it needs to
have information about the arity of the consumer. If the environment does not yet supply that
information (which it doesn't, as far as I know) it should get it by other means. The best way to
cleanly deal with that I could think of is by making a special-form that yields the function's
arity (together with information about whether it makes use of features like `&rest`). That could be
dealt with by making the arity of the function part of its *type*, which could be checked at
runtime my `multiple-value-call`, in order to take the appropriate action.
Dealing with types can, however, be a complicated business and might raise a number of issues.
If one puts the tip of the finger in one of those issues, it is likely to have the
whole hand dirtied with mud. But if it is the best way, it is the best way (or maybe it isn't, but time
runs).

---

It's somewhat shameful that this multiple values thing is taking so much time (even more so given
that I said it would be quick and simple). I've lost too much time trying to dig into EusLisp and
Common Lisp specifications (as well as looking for what people have done) to find a simpler way that
doesn't involve messing up with other modules (even though the fact that `multiple-value-call` is
specified as a special form should ring a bell that it might not be possible).


---
---
---
---
[^1]:  **EDIT:** Actually, as it turns out, sinchronization wouldn't be too much of an issue, as ROS makes use mostly of single threaded processes. But yet, the code is very hacky.
[^2]: **EDIT:** The code had to be slightly modified from the first version. For it to be used as a
proper multiple-values interface, it would need a few other modifications as well, in particular the
replacing of the loop macro in the multiple-value-call definition.

[okada]:                    https://github.com/k-okada
[furuschev]:                https://github.com/furushchev
[cltl]:                     http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html
[emacs-mv]:                 https://www.gnu.org/software/emacs/manual/html_node/cl/Multiple-Values.html
[ooe]:                      http://goldenboy.wikia.com/wiki/Kintaro_O
