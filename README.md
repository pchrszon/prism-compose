# prism-compose

This tool performs the parallel composition of
[PRISM](www.prismmodelchecker.org) on the PRISM language level.

## Usage

To compose all modules of a PRISM model `my_model.prism`, run:

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


## Building from source

### Prerequisites

Download and install the `stack` tool from [haskellstack.org](www.haskellstack.org).

### Building

To build the tool, run the following command in the project's root directory
(containing the `stack.yaml` file):

    stack build

Optionally, you may run

    stack install

to copy the `compose` binary to ~/.local/bin.
