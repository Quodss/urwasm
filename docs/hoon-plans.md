## Roadmap for WebAssembly Interpreter in Hoon

At this moment (~2023.11.27) `urwasm` is a working demo: it can run some simple programs shown in `rust` directory of the repo. This project was hastily constructed during the Hackathon, and now it needs some good polishment and sometimes a reconstruction.

Firstly, we need to choose some specification version and implement it first, and then update the package if necessary. For now each newer version is a strict superset of any previous version and also backward compatible.

Then, for each component:

1. The parser needs to be partially rewritten and extended to accomodate the entirety of `.wasm` binary format spec. I don't think adding `.wat` support is necessary, and also I want to complile a `.wat->.wasm` transformer from Rust and run this transformer on Urbit instead of having to write `.wat` parser myself. Bootstrapping FTW!
2. The validator needs to be written. Right now module type correctness is dynamically verified at runtime, which creates unnecessary overhead both in computation and development. Validation would happen right after parsing, before Wasm AST is returned
3. Interpreter needs to be rewritten for clarity and to utilize structural sharing of the global state of the module. Right now the entire memory buffer gets copied, lightly edited and pasted for each block instance, function call and loop iteration.
    * In addition, floating point ops correctness must be verified — AFAIK Wasm's floating point rounding mode differs from Hoon's `++rs` and `++rd` default settings — would it be enough to change the settings?
    * Also need to add handling of sections that are missing at the moment: most important one is Import section.

With those things done, phase 1 from [the bounty description](https://urbit.org/grants/wasm-nock) will be complete. 