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
- [ ] Language for Invocation of Assembly (Lia) specification
- [ ] Lia interpreter in Hoon
- [ ] Jet of Lia interpreter