---
title: The Little Haskeller
description: This is a translation of the code from _The Little MLer_ into Haskell.
tags: haskell
---

This is a translation of the code from _The Little MLer_ into Haskell.
It's divided into sections by chapter, and numbered according to the Q&A
numbers in the text of each chapter.

![Some caption](/img/PetrolBomberMural.jpg)

It's currently in quite an early state, and rather rough and ready. I
intend to continue adding translations as I go through the book myself,
clean up things as I learn more and receive comments, and perhaps do a
big final cleanup at the end. Please feel free to mail me any comments
you have, preferably including the revision number of the document above.
I would appreciate feedback from both Haskell experts and novices who
are using this.

 > $\mathsf{add} = \lambda{}x^{\mathsf{Nat}}. \lambda{}y^{\mathsf{Nat}}.
 > \mathsf{R} x (\lambda{}z^{\mathsf{Nat}}. \lambda{}w^{\mathsf{Nat}}.
 > \mathsf{S}z) y$

This is "literate Haskell," which means that the actual code is
surrounded by blank lines and preceeded by a ">" symbol on each line. If
this file is saved as a ".lhs" file, it can be read directly into the
ghci or Hugs interpreters (using the ":l" command), and you can play
with the definitions.


Building Blocks
---------------

Another literate Haskell post:

I've tried a few times to read various documents on the web about
Monad Transformers in Haskell. I think that in almost every case the
authors are trying to show how clever they are rather than explaining
their use. If I had just seen the simplest possible examples of Monad
Transformers at work I would have been able to figure out what was going
on and that would have given me enough information to bootstrap myself
into Monad Transforming enlightenment.

So to save other people the trouble I went through I'm providing you
with examples of Monad Transformers at work. I'm not even going to
explain in detail how they work, they're close to self-evident once the
main features are pointed out.

> import Control.Monad.State
> import Control.Monad.Identity

Firstly here are two examples of the use of the State Monad. This
code isn't intended as a tutorial on the State Monad so I won't
explain how they work.

> test1 = do
>             a <- get
>             modify (+1)
>             b <- get
>             return (a,b)

> test2 = do
>             a <- get
>             modify (++"1")
>             b <- get
>             return (a,b)

> go1 = evalState test1 0
> go2 = evalState test2 "0"

Note how evalState 'unwraps' the State Monad and gives you back the
answer at the end.

So the question is: how do I combine both of these states into one
so that I can update or read either the integer or the string state
at will? I could cheat and make a new state of type (Int,String)
but that isn't a general solution.

The idea is that you use a Monad Transformer. A Monad Transformer
if like a layer of onion peel. You start with the Identity monad
and then use Monad Transformers to add layers of functionality.
So to get a two-state monad you take the Identity and add two
layers of stateness to it. To get the answer at the end you need
to unwrap the State layers and then unwrap the Identity layer too.

When you're inside your 'do' expression you need a way to choose
which Monad you're talking to. Ordinary Monad commands will
talk to the outermost layer and you can use 'lift' to send your
commands inwards by one layer.

The following should now be self-evident:

> test3 = do
>     modify (+ 1)
>     lift $ modify (++ "1")
>     a <- get
>     b <- lift get
>     return (a,b)

> go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"

Note that we use evalStateT to unwrap the State layer instead
of evalState. evalStateT is what you use to unwrap one layer
of the Monad. evalState is what you use when your entire Monad
is just the State monad. When State is a layer, rather than
the whole thing, it's called StateT. (Try :type go3 in ghci.)

What if you want to combine IO and State? For various reasons
the IO monad must form the innermost core of your onion so
there's no need to wrap an IO layer around the Identity,
you can just start with IO. And when you unwrap you don't
need to unwrap the IO layer because the Haskell compiler does
that for you. (Now you can see why you can't layer IO, you're
not supposed to unwrap IO as it's the job of the code that
calls main at the top level to do that.)

So here we go:

> test5 = do
>     modify (+ 1)
>     a <- get
>     lift (print a)
>     modify (+ 1)
>     b <- get
>     lift (print b)

> go5 = evalStateT test5 0

At this point you might be suspicious that IO is being handled
slightly differently from State. So here's a StateT layer
wrapped around State:

> test7 = do
>     modify (+ 1)
>     lift $ modify (++ "1")
>     a <- get
>     b <- lift get
>     return (a,b)

> go7 = evalState (evalStateT test7 0) "0"

You can probably safely ignore my comments and crib the code above
directly for your own use.

And credit where credit's due: I found the following link to be
more helpful than any other in trying to figure this stuff out
http://www.cs.nott.ac.uk/~nhn/MGS2006/LectureNotes/lecture03-9up.pdf
(But I still felt that this link was obfuscating.)

You may now be wondering where test4 and test6 are. So am I.

Update: I just figured out that you don't always need to use the
'lift' operation. Some of the Monad Transformers have been defined
so that the commands will automatically be passed into the inner
Monad (and further) if needed. I think it's slightly confusing to
write code this way, however.
