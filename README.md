# rfg-cli

## Description

rfg is a command line allowing you to generate random data through a tree of
folders and files randomly distributed.

Size of the file are determined randomly and number of files/folders into a
folder are too.

The idea of the project was not only to create a randomly made tree of files
and directories, but also to check if the data are conform to the generated
ones. For that when we generate them we save the size and md5sum of each
generated files along with their path. It is then after possible to run a
`check` command and verify the integrity.

This was originally conceived to check that data backup system was resilient to
disc failure.

This then allow you to create TB of randomly generated data and check their
safety when simulating disc failures.

## System dependencies

rfg uses `md5sum` program to create checksums.

## Compiling and Language

The language used is OCaml 4.02 and uses ocamlbuild for building.

Once the repository downloaded you need the following dependencies from OPAM :
> opam install base64 hex ocamlfind

The other dependencies are standard packages of OCaml (threads and unix).

### Compile

At the root of the depot run :
> ocamlbuild -use-ocamlfind rfg.native

This will build rfg.native application that you can run :
> ./rfg.native --help
