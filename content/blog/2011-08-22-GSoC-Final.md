---
title: 'Parallelising cabal-install: Results'
description: Post about results of my GSoC 2011 project
tags: haskell, gsoc
---

The [parallel
cabal-install](http://www.google-melange.com/gsoc/project/google/gsoc2011/refold/31001)
GSoC project has reached the stage where it can be useful to a wider
audience. This post describes the current state of the code, provides a guide to
installing & using it, and outlines the roadmap for the near future.

Building
--------

The parallel version of `cabal-install` is available from [this Darcsden
repository](http://darcsden.com/23Skidoo/Cabal). Since this branch contains
changes to both cabal-install and the Cabal library itself, you are advised to
use [cabal-dev](http://hackage.haskell.org/package/cabal-dev) for building:

    $ darcs get http://darcsden.com/23Skidoo/Cabal
    $ cd Cabal
    $ cabal-dev install cabal/ cabal-install/

This will install the new `cabal` executable to `./cabal-dev/bin/cabal`. You can
then move it to the location of your choosing - for example, replacing the
`cabal` executable you have in `$PATH` (don't forget to do a backup!).

Running
-------

To install packages from Hackage in parallel, just run `cabal install -jN`,
where `N` is the number of "jobs", or worker threads. You will see something
like the following:

    $ cabal install -j2 transformers quickcheck
    Resolving dependencies...
    [1] Downloading QuickCheck-2.4.1.1...
    [2] Downloading transformers-0.2.2.0...
    [2] Configuring transformers-0.2.2.0...
    [1] Configuring QuickCheck-2.4.1.1...
    [2] Building transformers-0.2.2.0...
    [2] Preprocessing library transformers-0.2.2.0...
    [1] Building QuickCheck-2.4.1.1...
    [1] Preprocessing library QuickCheck-2.4.1.1...
    [2] [ 1 of 21] Compiling Data.Functor.Product ( Data/Functor/Product.hs,
    dist/build/Data/Functor/Product.o )

    ...

    [2] [21 of 21] Compiling Control.Monad.Trans.Writer.Strict (
    Control/Monad/Trans/Writer/Strict.hs,
    dist/build/Control/Monad/Trans/Writer/Strict.o )
    [install] Installing library in
    /home/cabal-test/.cabal/lib/transformers-0.2.2.0/ghc-7.0.4
    [install] Registering transformers-0.2.2.0...

    ...

    [1] [13 of 13] Compiling Test.QuickCheck.All ( Test/QuickCheck/All.hs,
    dist/build/Test/QuickCheck/All.o )
    [install] Installing library in
    /home/cabal-test/.cabal/lib/QuickCheck-2.4.1.1/ghc-7.0.4
    [install] Registering QuickCheck-2.4.1.1...

Here, all output prefixed with `[1]` and `[2]` comes from the worker threads,
and everything prefixed with `[install]` comes from the install thread
(installation step is serialised to avoid races).

Additionally, you can play with the [`+RTS -N`
option](http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/using-smp.html#parallel-options),
though it shouldn't produce a noticeable speed difference.

A good way to stress the parallel install code is to back up your `~/.ghc` and
`~/.cabal` directories and then run `cabal install world`:

    # Backing up old data:
    $ mv ~/.ghc ~/.ghc.bak
    $ mv ~/.cabal ~/.cabal.bak

    # Recreating the ~.cabal directory:
    $ cabal update
    $ cp ~/.cabal.bak/world ~/.cabal/

    # Rebuilding the world (N = number of concurrent jobs):
    $ cabal install -jN world

If you encounter any bugs, please drop me a line in the comments or submit a bug
report at the [Cabal bug
tracker](http://hackage.haskell.org/trac/hackage/newticket).

Future work
-----------

First of all, the parallel patches need to be merged into the mainline
repository, which may take some time.

Next, there are possibilities for improvement. Current version of the code works
at the package granularity (a package is always built by a single thread), which
doesn't work so well for giant packages like
[Agda](http://hackage.haskell.org/package/Agda) and
[Darcs](http://hackage.haskell.org/package/darcs). We can get better speedup by
ditching `ghc --make` and instead compiling each module separately with `ghc
-c`; the dependency graph can be extracted with `ghc -M`. This should be also
integrated with the `build` command.

Third, there is the question of other compilers (e.g. Hugs). At the moment, only
GHC is supported by the parallel install code, but it should be possible to
adapt it to work with other compilers. This is not considered a top priority,
though.

Acknowledgements
----------------

Thanks to my mentor, [Johan Tibell](http://blog.johantibell.com), for code
reviews and helpful comments; and to Google for sponsoring my work during the
summer.
