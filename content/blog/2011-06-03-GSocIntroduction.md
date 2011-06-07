---
title: GSoC 2011: Parallelising cabal-install
description: First post about my GSoC 2011 project
tags: haskell, gsoc
---

This summer, I will be working on parallelising
[cabal-install](http://www.haskell.org/cabal/) under the aegis of the Google
Summer of Code program. The aim of the
[project](http://socghop.appspot.com/gsoc/project/google/gsoc2011/refold/31001)
is to make Cabal utilise multiple threads for running the build process (a-la
`make -j`). This means that Cabal will be able to make use of those shiny
multi-core processors many developers now own. While initially the unit of
granularity will be the package, in the second phase of the project I plan to
add support for building even single modules in parallel (which will require
interaction with `ghc --make`). Until my patches are accepted into the main
repo, they'll live [on Darcden](http://darcsden.com/23Skidoo) (not much there
yet!).

![Thread communication in the prototype](http://dissocial.st/img/GSoC2011-Design.png)

While I haven't yet done much work on modifying the cabal-install proper, I've
produced a small [prototype](https://gist.github.com/982483) that illustrates my
approach to the problem. The prototype program consists of several threads which
communicate via
[Chans](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent-Chan.html#t:Chan). There
are several worker threads, which compile the packages
([threadDelay](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html#v:threadDelay)
is used to simulate actual operations). A single control thread maintains the
package graph and assigns tasks to the worker threads. A single logger thread
prints out messages received from the worker threads. A single install thread
installs the packages into the target directory (this is done serially, but can
also be parallelized if deemed safe).

After the install thread installs a package, it notifies the controller thread,
which then updates the package graph and adds new tasks for the worker threads
(if possible). The control thread terminates when the last package has been
installed (which leads to the termination of all other threads).

I still have exams until the 7th of June, but I've already posted [my first
patch](http://hackage.haskell.org/trac/hackage/ticket/849). Though not directly
related to the project, it helped me to smooth out some wrinkles in the workflow
and get accustomed to the Darcs way of doing things.
