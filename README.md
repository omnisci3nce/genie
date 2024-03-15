# genie

> We let the genie out of the bottle, but you get to keep the 3 wishes. Simple, GUI, OCaml.

A simple, functional style GUI framework. 

All third-party dependencies are licensed under their own license.

## Platform Support

**Currently Linux is the only supported target! This is a _highly_ WIP library and so cross-platform support while absolutely planned will not be here for a while.**

## Contributing

Right now you need to pin my fork of ocaml-bindgen manually with

- `opam pin bindgen git+https://github.com/omnisci3nce/ocaml-bindgen.git` but this will be fixed imminently
- run `opam install bindgen` to install the fork
- run `dune build` to make sure everything installed correctly
- run `dune exec examples/basic/main.exe.` to run the example