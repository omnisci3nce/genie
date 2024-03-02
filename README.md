# genie

> We let the genie out of the bottle, but you get to keep the 3 wishes. Simple, GUI, OCaml.

## Contributing

Right now you need to pin my fork of ocaml-bindgen manually with

- `opam pin bindgen git+https://github.com/omnisci3nce/ocaml-bindgen.git` but this will be fixed imminently
- run `opam install bindgen` to install the fork
- run `dune build` to make sure everything installed correctly
- run `dune exec examples/basic/main.exe.` to run the example