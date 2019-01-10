# OFord

This an implementation of Ford-Fulkerson Algorithm, in Ocaml Language.

It can also resolve Circulation Demand Problem given an input in two type of format.

- You can use the link below to create a readable input file graphicaly)

 https://www-m9.ma.tum.de/graph-algorithms/flow-ford-fulkerson/index_en.html#tab_tg

## Dependencies

This project use OUnit fort testing pursose only you can install it with this command

`opam install ounit`

of course you need to have `OCaml` and `Opam` installed

## Tests

### To run existing tests

`./src/test1`  for Max Flow problem

`./src/test2` for Circulation demand: this one print paths that can be used and the flow of each one

### Create New Tests

To add new test here are steps:

1. Add the new graph file into the `in` folder
2. Add the file name to the files variable in `test.ml` file
3. Add an element (<span style="color = blue;">source</span>, **source**, **right flow value**) to the paths variable and follow the compile test steps below.

### Compile own tests

`ocamlfind ocamlc -o test -package oUnit -linkpkg -g graph.ml gfile.ml algo.ml test.ml`

## Build

To build the project run command

`ocamlbuild ftest.native`  and then `./ftest.native infile source sink outfile` to run the executable


