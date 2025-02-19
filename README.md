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
- [X] Stage 1 complete! Jetted execution of Wasm without state preservation
  - suitable for parsers, compession algorithms, etc

Next:
- [X] Language for Invocation of Assembly (Lia) specification
- [X] Lia monad in Hoon:
- [X] Jet of Lia monad reducer:
- [ ] Operationalization:
  - [ ] Caching of Lia state
  - [ ] Unit tests