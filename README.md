Building from source
====================

Prerequisites
-------------

To build the compose tool from source you need GHC (at least version 7.10.1)
and the cabal-install tool. Both may be provided by your package manager,
alternatively see [www.haskell.org/downloads][haskell.org/downloads] for
downloads and instructions.

Building
--------

*   It is recommended to build the package in an isolated environment by
    creating a cabal sandbox (otherwise all packages will be installed to your
    user package database). Make sure you create the sandbox in the directory
    that contains the `compose.cabal` file:

        cabal sandbox init

*   To build the compose tool, execute the following command in the project's
    root directory (the directory containing the `compose.cabal` file):

        cabal install

    The executable will be written to `.cabal-sandbox/bin/compose`.

Usage
=====

To compose all modules of a Prism model `my_model.prism`, run:

    compose my_model.prism

The result will be written to stdout. Using the `-o` option, the resulting
model can be written to a file instead:

    compose my_model.prism -o out.prism

In case you only want to compose a subset of all modules of a model, you can
provide a list of modules:

    compose my_model.prism process1 process2

