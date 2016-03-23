Building from source
====================

Prerequisites
-------------

Download and install the `stack` tool from [Stackage](www.stackage.org).

If you do not have a recent version of GHC on your system (at least version
7.10), run

    stack setup

in the project directory (containing the `stack.yaml` file) to install the
compiler.

Building
--------

To build the tool, run the following command in the project's root directory
(containing the `stack.yaml` file):

    stack build

Optionally, you may run

    stack install

to copy the `profeat` binary to ~/.local/bin.

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

If there is only a single module left after composition, it is possible to move
the global variables into this module using the `--merge-globals` option:

    compose my_model.prism -o out.prism --merge-globals

