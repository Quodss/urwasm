::  Lia (Language for invoking Assembly) types
::
::    Lia is a DSL used to batch operations on a Wasm module together
::    to make jetting efficient. State of a given Wasm module is
::    formally defined as a history of actions performed on
::    an instantiated module. Lia interpreter in Hoon transform
::    this representation into an informal state by replaying the actions,
::    and returns a list of Lia values, which are Wasm values except
::    for funcref or bytearray slices.
::
::    Since ++lia would not return the entire informal state, that
::    informal state does not have to be computed in Nock: ++lia could
::    call a Wasm runtime implementation in C to obtain the final
::    informal state, and then convert C values to nouns to return.
::
::    But of course, ++lia cannot call Wasm runtime in C directly, it
::    can only run Nock. So ++lia calls to Wasm interpreter in Nock,
::    and is jetted with a Lia interpreter in C, which in turn calls
::    a Wasm runtime in C.
::
::    Why do we have to deal with that many layers? Because current Nock
::    interpreters do not support optimizations like lazy copying of atoms
::    or atom paging, so each call to a jetted Wasm interpreter would incur
::    copying and allocation of the entire state of a Wasm module, which
::    creates prohibitive overhead for memory-heavy computations. Besides,
::    each function invocation of a jetted interpreter would demand serialization
::    of the state of the module before code execution, and deserialization
::    when the result is returned.
::
::    Formally, ++lia would rerun the whole sequence of actions every time we
::    interact with the module (invoke another function, read/write). Think
::    of this as representing Urbit state as a product of Nock(event-log, L) by 
::    storing the log and not the product of Nock evaluation. A sufficiently
::    smart jet would store the informal state of the module as a cache,
::    tagged with a noun with the input parameters. Since Wasm execution
::    is determenistic on Urbit, this noun will be a sufficient description
::    of the cache. Cache storage and reclamation strategies are
::    mere implementation details, albeit important ones.
::
::    Lia is a stack machine that operates on a space of Lia values, represented
::    with 32-bit atom indices. Instantiation of the module is implicit. If
::    the underlying Wasm runtime returns a trap (determenistic crash), Lia will
::    also trap and not return anything. If the Wasm interpreter blocks on
::    an unresolved import, Lia will attempt to resolve it using its store of import
::    resolutions and external function definitions provided by the caller. If
::    the store is empty, then Lia would block, returning a request for
::    an external call resolution. This trick allows us to enrich import functions'
::    domain with octs. Note that external functions provided to Wasm map lists of
::    numeric values onto themselves, but the blocks may contain other values such
::    as octs. Variable space of external functions' execution environment is separate
::    from the main Lia environment.
::    Reads from the linear memory produce octs. Arithmetic operations specify type,
::    and may accept octs as an input. In that case Lia will attempt to convert octs
::    to an operand with a proper size, trapping on overflow.
::
/-  *engine
::
|%
+$  idx  @F
+$  value
  $%  $<(%ref coin-wasm)
      [%octs octs]
  ==
::
+$  action  (list op)
+$  op
  $%
    [%get =idx]
    [%set =idx]
    [%run name=cord]
    [%add type=num-type]
    [%sub type=num-type]
    [%cut offset=@ len=@]
    [%read ptr=@ len=@]
    [%writ ptr=@ len=@]
    [%if true=(list op) false=(list op)]
    [%for i=idx from=@s to=@s step=@s body=(list op)]
  ==
::
+$  ext-func
  $:  params=(list ?(num-type vec-type))
      results=(list ?(num-type vec-type))
      code=(list op)
  ==
::
--