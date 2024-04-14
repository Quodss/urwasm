# urwasm
WebAssembly interpreter suite for Urbit

Continuation of my work on [UWasm](https://github.com/Quodss/wasm-hackathon). Copy the files from `desk` directory to a new Urbit desk and commit them. Run tests with:
```
-test /=wasm=/tests ~
```
or try running your own Wasm modules.

State of the project:

- [X] Complete Wasm interpreter specification in Hoon
- [X] Parsing of both binary and text Wasm file formats (the latter is done via Wasm calls, so it's slow without jets)

Next:
- [X] Language for Invocation of Assembly (Lia) specification
- [ ] Lia interpreter in Hoon:
  - [ ] Lia-to-Wasm compiler
    - [X] Hoon code compiles
    - [X] Wasm implementation of GC and alloc typechecked
    - [X] Compiler runs and outputs valid Wasm
    - [ ] Test the allocator
  - [X] Wasm binary encoder
  - [ ] Lia end-to-end interpeting function (jet target)
- [ ] Jet of Lia interpreter:
  - [ ] Lia jet
  - [ ] Ensure determinism of the Wasm runtime in C 
  - [ ] Wasm validation in Hoon, Lia validation in Hoon and C
- [ ] Operationalization:
  - [ ] Caching of Lia state
  - [ ] Unit tests
  - [ ] Lia text format
    - [ ] Lexer
    - [ ] Type inference