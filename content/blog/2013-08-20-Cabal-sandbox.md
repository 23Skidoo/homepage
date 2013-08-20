---
title: An Introduction to Cabal sandboxes
description: Description of the new Cabal feature I implemented during GSoC 2012.
tags: haskell, gsoc
---

This post describes sandboxes, a new feature of `cabal` that will be present in
the 1.18 release. Sandboxes allow to build packages in isolation by creating a
private package environment for each package. If you are familiar with Python's
virtualenv or Ruby's RVM, this is a Haskell analogue. Though 1.18 is still not
out yet, you can already experiment with the new features by building `cabal`
from Git. This post is mainly aimed at people new to sandboxes -- if you have
already used `cabal-dev`, feel free to
[skip the introductory sections](#for-the-users-of-cabal-dev).

Building Cabal from git
-----------------------

Assuming you already have a previous version of `cabal` installed:

    $ git clone git://github.com/haskell/cabal.git /path/to/cabal
    $ cd /path/to/cabal
    $ cabal install Cabal/ cabal-install/

That's all! Now you have the latest version of the `cabal` tool installed under
`~/.cabal/bin`.

Alternatively, if you have only GHC (but not `cabal`) installed:

    $ git clone git://github.com/haskell/cabal.git /path/to/cabal
    $ cd /path/to/cabal/Cabal
    $ runhaskell Setup.hs configure
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs install
    $ cd ../cabal-install
    $ sh bootstrap.sh

What are sandboxes and why are they needed?
-------------------------------------------

If you have used Haskell for some time, you've probably heard the expression
"Cabal hell". It refers to the fact that installing a new package with `cabal
install` can break existing packages on your system.

The reason for this behaviour is *destructive reinstalls*. As of this writing,
Cabal doesn't support having *multiple instances* of the same version of a
single package installed simultaneously (but note that installing *multiple
versions* of the same package is completely fine). So how does this affect you,
the user?

Imagine that you have installed the library `foo`, which depends on
`bar-1.0`, which in turn depends on `baz` (any version):

![](/e/img/sandboxes-pic-0.png "foo-1.0 -> bar-1.0 -> baz-1.0;")

After some time you then decide to install `quux`, which depends on `bar-1.0`
and `baz-2.0`. Since you have only `baz-1.0` installed, you need to install
`baz-2.0` and recompile `bar-1.0` against it:

![](/e/img/sandboxes-pic-1.png "quux-1.0 -> bar-1.0; quux-1.0 -> baz-2.0; bar -> baz-2.0;")

But since Cabal allows you to have only a single instance of `bar-1.0`
installed, the package `foo-1.0` is now broken since it depends on an instance
of package `bar-1.0` that was removed! Cue much weeping and gnashing of teeth:

![](/e/img/sandboxes-pic-2.png "foo-1.0 -> ???; baz-1.0;")

While we know what is the right way to fix this issue (see the
["future work"](#future-work) section below), getting there will take time, and
sandboxes present a relatively low-cost interim solution. The idea is to build
each package in an isolated environment ("sandbox") with a sandbox-local package
database. Because sandboxes are per-project, we can constrain them to be
internally consistent and simply prohibit such conflicts as described above.

Besides alleviating the "Cabal hell" problem, sandboxes are also useful when
your package depends on patched or unreleased libraries.

Usage
-----

Using sandboxes is simple: if you already know how to use the `cabal` tool to
build your packages, you only need to learn a few additional commands. To
initialise a fresh sandbox in the current directory, run `cabal sandbox
init`. All subsequent commands (such as `build` and `install`) from this point
will use the sandbox.

    $ cd /path/to/my/haskell/library
    $ cabal sandbox init                   # Initialise the sandbox
    $ cabal install --only-dependencies    # Install dependencies into the sandbox
    $ cabal build                          # Build your package inside the sandbox

It can be useful to make a source package available for installation in the
sandbox - for example, if your package depends on a patched or an unreleased
version of a library. This can be done with the `cabal sandbox add-source`
command - think of it as "local Hackage". If an add-source dependency is later
modified, it is reinstalled automatically.

    $ cabal sandbox add-source /my/patched/library # Add a new add-source dependency
    $ cabal install --dependencies-only            # Install it into the sandbox
    $ cabal build                                  # Build the local package
    $ $EDITOR /my/patched/library/Source.hs        # Modify the add-source dependency
    $ cabal build                                  # Modified dependency is automatically reinstalled

Normally, the sandbox settings (such as optimisation level) are inherited from
the main Cabal config file (`$HOME/cabal/config`). Sometimes, though, you need
to change some settings specifically for a single sandbox. You can do this by
creating a `cabal.config` file in the same directory with your
`cabal.sandbox.config` (which was created by `sandbox init`). This file has the
same syntax as the main Cabal config file.

    $ cat cabal.config
    documentation: True
    constraints: foo == 1.0, bar >= 2.0, baz
    $ cabal build                                  # Uses settings from the cabal.config file

When you have decided that you no longer want to build your package inside a
sandbox, just delete it:

    $ cabal sandbox delete                       # Built-in command
    $ rm -rf .cabal-sandbox cabal.sandbox.config # Alternative manual method


Advanced usage
--------------

The default behaviour of the `add-source` command is to track modifications done
to the added dependency and reinstall the sandbox copy of the package when
needed. Sometimes this is not desirable: in these cases you can use `add-source
--snapshot`, which disables the change tracking. In addition to `add-source`,
there are also `list-sources` and `delete-source` commands.

Sometimes one wants to share a single sandbox between multiple packages. This
can be easily done with the `--sandbox` option:

    $ cd /path/to/shared-sandbox
    $ cabal sandbox init
    $ cd /path/to/package-a
    $ cabal sandbox init --sandbox /path/to/shared-sandbox
    $ cd /path/to/package-b
    $ cabal sandbox init --sandbox /path/to/shared-sandbox

Using multiple different versions of GHC simultaneously is also supported, via
the `-w` option:

    $ cabal sandbox init
    $ cabal install --only-dependencies -w /path/to/ghc-1 # Install dependencies for both compilers
    $ cabal install --only-dependencies -w /path/to/ghc-2
    $ cabal configure -w /path/to/ghc-1                   # Build with the first compiler
    $ cabal build
    $ cabal configure -w /path/to/ghc-2                   # Build with the second compiler
    $ cabal build

It can be occasionally useful to run the `ghc-pkg` tool on the sandbox package
DB directly (for example, you may need to unregister some packages). The command
`cabal sandbox hc-pkg` is a convenient wrapper for `ghc-pkg` that runs it with
the appropriate `--package-conf` argument:

    $ cabal -v sandbox hc-pkg list
    Using a sandbox located at /path/to/.cabal-sandbox
    'ghc-pkg' '--global' '--no-user-package-conf'
        '--package-conf=/path/to/.cabal-sandbox/i386-linux-ghc-7.4.2-packages.conf.d'
        'list'
    [...]


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

Another difference is that sandboxes are constrained to be consistent - that is,
destructively reinstalling a package (like in the introduction example) is not
allowed. Installing multiple versions of a package is still fine.

Future work
-----------

In the future, we want to
[make hermetic builds the default](http://blog.johantibell.com/2012/03/cabal-of-my-dreams.html) -
that is, the build system should work as if all build artifacts were rebuilt
anew each time. Ideally, this feature would be built on top of a purely
functional [Nix](http://nixos.org/nixos/)-like package DB, which would allow to
share build artifacts between different builds without worrying about the
destructive update problem outlined in the introduction. Unfortunately, this is
a large and a non-trivial project, although some work
[has already been done in this direction](http://www.youtube.com/watch?v=h4QmkyN28Qs). A
possible interim solution is to run each build in its own sandbox.

There is also a number of relatively minor UI issues with sandboxes which were
postponed until the next release (1.20). For more details on this, see the
[Cabal bug tracker](https://github.com/haskell/cabal/issues?labels=&milestone=21&page=1&state=open).

Last but not least, we're really interested in your feedback on this feature,
especially in how well it works on large-scale projects.

Acknowledgements
----------------

Thanks to Johan Tibell, Duncan Coutts and Andres Löh for code reviews and
guidance, and to Google for paying me for working on this project during last
summer. Thanks to Rogan Creswick for writing the original `cabal-dev` tool,
which this work builds upon.
