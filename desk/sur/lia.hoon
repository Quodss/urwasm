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
::
::    Reads from the linear memory produce octs. Arithmetic operations specify type,
::    and may accept octs as an input. In that case Lia will attempt to convert octs
::    to an operand with a proper size, trapping on overflow.
::
/-  sur=engine
|%
::  Noun code definition for Lia stack machine
::
++  line
  |%
  +$  value-type  ?(num-type:sur vec-type:sur %octs)
  +$  block-type  (pair (list value-type) (list value-type))
  +$  ext-func-type  %+  pair
                       (list ?(num-type:sur vec-type:sur))
                       (list ?(num-type:sur vec-type:sur))
  +$  ext-func  [type=ext-func-type body=(list op)]
  +$  idx  @F
  +$  value
    $%  $<(%ref coin-wasm:sur)
        [%octs octs]
    ==
  ::
  +$  action  [type=block-type body=(list op)]
  +$  op
    $%
      [%get type=value-type =idx]  ::  compiler: numerical values are dereferenced and pushed onto the stack, for octs idx is pushed onto the stack
      [%set type=?(num-type:sur vec-type:sur) =idx]
      [%let type=value-type]  ::  set type of space index; compiler: noop
      [%run name=cord]
      [%run-ext name=term]
      [%add type=num-type:sur]
      [%sub type=num-type:sur]
      :: [%cut ~]  
      [%read p=idx]  ::  consumes ptr and len
      [%writ p=idx]  ::  consumes ptr, offset and len
      [%block type=block-type body=(list op)]
      [%if type=block-type true=(list op) false=(list op)]
      [%loop type=block-type body=(list op)]
      [%br label=@]
      [%br-if label=@]
      :: [%lit p=value]
      [%const p=$<(%ref coin-wasm:sur)]
      [%len =idx]
      [%nop ~]
      [%drop ~]
      :: [%yeet ~]
      :: [%octs p=idx]  ::  data and len
      [%read-octs-i p=idx type=?(%i32 %i64)]  ::  offset, len -> octs to int
      [%read-octs-f p=idx type=?(%f32 %f64)]  ::  offset      -> octs to float
    ==
  ::
  ::
  +$  state
    $:
      space=(list value)  ::  storage of values
      stack=(pole value)  ::  operational stack
      store=store:sur     ::  Wasm store
    ==
  ::
  +$  input
    $:  =^module:sur
        code=(list action)
        shop=(list (list value))
        ext=(map (pair cord cord) ext-func)
        import=(map term block-type)
        diff=(each (list action) (list value))
    ==
  ::
  +$  result
    $%  [%0 out=(list value)]
        [%2 ~]
    ==
  ::
  --
::  AST definition
::
++  tree
  |%
  +$  script
    $:  input=(list name)
        code=block
        return=(list op)
    ==
  ::
  +$  value-type  ?(num-type:sur vec-type:sur %octs)
  +$  block-type  (pair (list value-type) (list value-type))
  +$  block  [type=block-type body=(list phrase)]
  +$  phrase  [names=(list name) =op]
  +$  name  @tas
  +$  op
    $@  name
    $%
      [%let p=(list name)]
      [%run p=cord q=(list op)]
      [%run-ext p=term q=(list op)]
      [%cut octs=op offset=op len=op]
      [%read offset=op len=op]
      [%writ octs=op offset=op]
      [%lit p=value:line]
      [%len octs=op]
      [%octs dat=op len=op]
      [%add type=num-type:sur p=op q=op]
      [%sub type=num-type:sur p=op q=op]
      [%if test=op true=block false=block]
      [%while test=op body=block]
      [%break ~]
      :: [%yield p=(list op)]
    ==
  --
--