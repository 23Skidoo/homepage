---
title: An Introduction to Cabal sandboxes
description: Description of the new Cabal feature I implemented during GSoC 2012.
tags: haskell, gsoc
---

This post describes sandboxes, a new `cabal` feature that will be present in the
1.18 release. Though 1.18 is still not out yet, you can already experiment with
the new features by building `cabal` from Git. This post is mainly aimed at
people new to sandboxes -- if you have already used `cabal-dev`, feel free to
skip the introductory sections.

Building Cabal from git
-----------------------

The recommended method of bootstrapping the Git version of the `cabal` tool is
by using `cabal-dev`. Assuming you already have a previous version of `cabal`
installed:

    $ cabal install cabal-dev
    $ git clone git://github.com/haskell/cabal.git
    $ cd cabal/cabal-install
    $ cabal-dev add-source ../Cabal
    $ cabal-dev install

That's all! Now you have the latest version of the `cabal` tool installed under
`/path/to/cabal/cabal-install/cabal-dev/bin`.

What are sandboxes and why are they needed?
-------------------------------------------

If you have used Haskell for some time, you've probably heard the expression
"Cabal hell". It refers to the fact that installing a new package with `cabal
install` can break existing packages on your system.

The reason for this behaviour is *destructive reinstalls*. As of this writing,
Cabal doesn't support installing *multiple instances* of the same version of a
single package (but note that installing *multiple versions* of the same package
is completely fine). So how does this affect you, the user?

Imagine that you have installed the library `foo`, which depends on
`bar-1.0`, which in turn depends on `baz` (any version):

![](/e/img/sandboxes-pic-0.png "foo-1.0 -> bar-1.0 -> baz-1.0;")

Now you have decided to install `quux`, which depends on `bar-1.0` and
`baz-2.0`. Since you have only `baz-1.0` installed, you need to install
`baz-2.0` and recompile `bar-1.0` against it:

![](/e/img/sandboxes-pic-1.png "quux-1.0 -> bar-1.0; quux-1.0 -> baz-2.0; bar -> baz-2.0;")

But since Cabal allows you to have only a single instance of `bar-1.0`
installed, the package `foo-1.0` is now broken since it depends on an instance
of package `bar-1.0` that was removed! Cue much weeping and gnashing of teeth:

![](/e/img/sandboxes-pic-2.png "foo-1.0 -> ???; baz-1.0;")

While we know what is the right way to fix this issue (see the "future work"
section), getting there will take time, and sandboxes present a relatively
low-cost interim solution. The idea is to build packages in isolated
environments ("sandboxes") with sandbox-local package databases, thus minimising
the risk of such conflicts as described above. Sandboxes are also useful when
your package depends on patched or unreleased versions of libraries.

Usage
-----

Using Cabal sandboxes is simple!

TODO

Advanced usage
--------------

TODO: `sandbox --snapshot`, `hc-pkg`, shared sandboxes, multiple compilers

For the users of cabal-dev
--------------------------

The sandbox feature gives you basically the same functionality as
`cabal-dev`, but integrated with the `cabal` tool itself. Here's a
handy cheatsheet for the users of `cabal-dev`:

|-------------------------------------------|------------------------------|---------------------------------------|
| Action                                    | `cabal-dev`                  | `cabal sandbox`                       |
|-------------------------------------------| -----------------------------|---------------------------------------|
| Initialise a sandbox                      | `cabal-dev $ANY_COMMAND`     | `cabal sandbox init`                  |
| Delete the sandbox                        | `rm -rf ./cabal-dev`         | `cabal sandbox delete`                |
| Link a source directory from the sandbox  | `N/A`                        | `cabal sandbox add-source`            |
| Make a package available in the sandbox   | `cabal-dev add-source`       | `cabal sandbox add-source --snapshot` |
| Build the current package                 | `cabal-dev build`            | `cabal build`                         |
| Install a package into the sandbox        | `cabal-dev install $PKGNAME` | `cabal install $PKGNAME`              |
| Any other standard `cabal` command        | `cabal-dev $COMMAND`         | `cabal $COMMAND`                      |
| Install dependencies of a package         | `cabal-dev install-deps`     | `cabal install --only-dependencies`   |
| Run sandbox-local ghci                    | `cabal-dev ghci`             | `cabal repl`                          |
| Sandbox-restricted `ghc-pkg`              | `cabal-dev ghc-pkg`          | `cabal sandbox hc-pkg`                |
| Path to the sandbox directory             | `./cabal-dev`                | `./.cabal-sandbox`                    |
|-------------------------------------------|------------------------------|---------------------------------------|

One important difference is that `add-source` adds a link to a source directory
instead of making a source snapshot available for install. The add-source
packages are reinstalled each time the sandboxed package is built. To get
`cabal-dev`'s behaviour, use `cabal add-source --snapshot`.

Another difference is that sandboxes are constrained.

Future work
-----------

Smarter updating of add-source deps

Make sandboxes the default

Purely functional backend
