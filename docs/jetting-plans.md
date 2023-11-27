## Roadmap for WebAssembly Interpreter jetting

At this moment (~2023.11.27) jet is practically nonexistent: all I have is a Arvo-to-wasm3 interface via a fake jet of a function in `hoon.hoon`. Once the Hoon interpreter is complete for a given spec version, jetting it should be a higher priority than adding some new features IMO. But we shall see.

0. Add `++sew` jet.

    `++sew` is the opposite of `++cut`: it splices a donor atom into a recipient atom. `urwasm` uses it as a function to load data into the memory buffer, which is modeled as a single atom. To my (and not only my) surprise `++sew` is not jetted and it uses `++add` to splice the atom, leading to a `log(e)` time complexity (`e` is the recipient atom). Jetting `++sew` would be a nice exercise before moving to writing other jets.

1. Simple jet
    
   Next step would be to turn the Arvo-wasm3 interface into a properly registered and validated jet.

   Concerns:
   
   * What is a proper way to validate the jet? Just throw in a bunch of modules and see if the interpreters don't diverge?
   * How to deal with nondeterministic operation `memory.grow`? It tries to grow the memory buffer, returning -1 on failure and previous size on success. How to guarantee that the both Hoon interpreter and jet act the same way? Same concern goes with stack size limit in wasm3, should I add the same limit in the Hoon interpreter?

2. High-order jetting interface

    In `simple_debts.rs` example we need to call multiple functions (move the stack pointer, allocate memory for three vectors) before we can call our main solving function. Translating intermediate state of Wasm interpreter from C to Hoon representation and back is unfeasible. Instead I'll develop a simple scripting Language for Invocation of Assembly (Lia), that would specify the algorithm that is going to be performed by the interpreter. An example:

    ```
    # Lia (Language for Invocation of Assembly) scripting language
    # a script must start with at least one import line and the import
    # must have at least one wasm-module
    #
    # "add_to_stack_pointer", "malloc" and "process" are functions exported
    # by the wasm module
    #
    import wasm-module *bin-wasm            # expose namespace
    import atom string0, i32 len0           # imports must have explicit types
    retptr = add_to_stack_pointer(-16)      # implicit type of input and output
                                            # from type section
    ptr0 = malloc(len0, 1) 
    memory[0].write(string0, ptr0, len0, 1) # memory[i].write takes 4 atoms 
    process(retptr, ptr0, len0)
    i32 r0 = memory[0].read(retptr, 4, 1)   # explicit type, casts atom to i32
    i32 r1 = memory[0].read(retptr+4, 4, 1) # '4' is implicitly typed as i32,
                                            # addition operator upcasts to i64
                                            # or convert to fn if necessary
    return memory[0].read(r0, r1, 1)        # `return` expression is 'return' + expression
                                            # that returns an atom
    ```