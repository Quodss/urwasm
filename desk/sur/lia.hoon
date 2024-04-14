::  Lia (Language for invoking Assembly) types
::
::    Lia is a DSL used to batch operations on a Wasm module together
::    to make jetting efficient.  State of a given Wasm module is
::    formally defined as a history of actions performed on
::    an instantiated module.  Lia interpreter in Hoon transform
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
::    can only run Nock.  So ++lia calls to Wasm interpreter in Nock,
::    and is jetted with a Lia interpreter in C, which in turn calls
::    a Wasm runtime in C.
::
::    Why do we have to deal with that many layers? Because current Nock
::    interpreters do not support optimizations like lazy copying of atoms
::    or atom paging, so each call to a jetted Wasm interpreter would incur
::    copying and allocation of the entire state of a Wasm module, which
::    creates prohibitive overhead for memory-heavy computations.  Besides,
::    each function invocation of a jetted interpreter would demand serialization
::    of the state of the module before code execution, and deserialization
::    when the result is returned.
::
::    Formally, ++lia would rerun the whole sequence of actions every time we
::    interact with the module (invoke another function, read/write to the memory
::    buffer).  Think of this as representing Urbit state as a product of
::    ock(event-log, L) by storing the log and not the product of Nock evaluation.
::    A sufficiently smart jet would store the informal state of the module as a cache,
::    tagged with a noun with the input parameters.  Since Wasm execution
::    is deterministic on Urbit, this noun will be a sufficient description
::    of the cache.  Cache storage and reclamation strategies are
::    mere implementation details, albeit important ones.
::
::    Lia is a stack machine that operates on a space of Lia values, represented
::    with 32-bit atom indices.  Instantiation of the module is implicit.  If
::    the underlying Wasm runtime returns a trap (deterministic crash), Lia will
::    also trap and not return anything.  If the Wasm interpreter blocks on
::    an unresolved import, Lia will attempt to resolve it using its store of import
::    resolutions and external function definitions provided by the caller.  If
::    the store is empty, then Lia would block, returning a request for
::    an external call resolution.  This trick allows us to enrich import functions'
::    domain with octs.  Note that external functions provided to Wasm map lists of
::    numeric values onto themselves, but the blocks may contain other values such
::    as octs.  Variable space of external functions' execution environment is separate
::    from the main Lia environment.
::
::    Reads from the linear memory produce octs.  Arithmetic operations in linearized
::    form of Lia code specify type.
::
::    Some behaviors are not defined in Lia.  Some of them would be prevented
::    by the structure of the Lia AST in |tree and text format parser, all of
::    them must be caught by a Lia validator:
::      - A value in space cannot be edited while that value (or a reference
::      to it) is on the stack;
::      - %get'ting an unassigned space value is forbidden;
::      - %set'ting a space index with a value of a type that differs from 
::        the type of that space index is forbidden. Type of space index
::        is defined with %let;
::    Some desired behaviors:
::      - if a space value is initialized with a %let, it is given a default
::        value of that type (default Wasm value are given by the Wasm spec,
::        default value for octs is [0 0])
::
/-  sur=engine
|%
::  Noun code definition for Lia stack machine
::
++  line
  |%
  +$  value-type  ?(num-type:sur vec-type:sur %octs)
  +$  block-type  (pair (list value-type) (list value-type))
  +$  numtype  ?(num-type:sur vec-type:sur)
  +$  ext-func-type  (pair (list numtype) (list numtype))
  +$  ext-func  [type=ext-func-type body=(list op)]
  +$  idx  @D
  +$  value
    $%  $<(%ref coin-wasm:sur)
        [%octs octs]
    ==
  ::
  +$  action  [type=block-type body=(list op)]
  +$  op
    $~  [%nop ~]
    $%
      [%get type=value-type =idx]  ::  compiler: numerical values are dereferenced and pushed onto the stack, for octs idx is pushed onto the stack
      [%set type=value-type =idx]
      [%let type=value-type =idx]  ::  set type of space index; compiler: set default value
      [%run name=cord]
      [%run-lia name=term target=(list idx)]  ::  execute Lia import and write results to target
      [%read p=idx]  ::  consumes ptr and len
      [%writ p=idx]  ::  consumes ptr, offset and len
      [%cut from=idx to=idx]  ::  consumes offset and len
      [%octs to=idx]  ::  consumes data and len
      [%if type=block-type true=(list op) false=(list op)]
      [%loop type=block-type body=(list op)]
      [%br label=@]
      [%br-if label=@]
      [%len =idx]
      [%read-octs-i p=idx type=?(%i32 %i64)]  ::  offset, len -> octs to int
      [%read-octs-f p=idx type=?(%f32 %f64)]  ::  offset      -> octs to float
      instr-num:sur
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
        diff=(each action (list value))
    ==
  ::
  +$  result
    $%  [%0 out=(list value)]
    ::
        $:  %1
            $%
              [%king name=term in=(list value)]
              [%serf req=request:sur]
        ==  ==
    ::
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
        type=script-type
        =code
        return=(list name)
    ==
  ::
  +$  input
    $:  =^module:sur
        code=(list script)
        shop=(list (list value:line))
        ext=(map (pair cord cord) ext-func)
        import=(map term script-type)
        diff=(each script (list value:line))
    ==
  ::
  +$  ext-func
    $:  input=(list name)
        type=ext-type
        =code
        return=(list name)
    ==
  ::
  +$  value-type  ?(num-type:sur vec-type:sur %octs)
  +$  script-type  (pair (list value-type) (list value-type))
  +$  ext-type  ext-func-type:line
  +$  code  (list phrase)
  +$  phrase
    $%
      [%op names=(list (pair name value-type)) =op]
      [%let p=name q=value-type]
      [%if test=op true=code false=code]
      [%while test=op body=code]
      [%break ~]
      [%read to=name offset=op len=op]
      [%writ from=name offset=op len=op]
      [%octs to=name dat=op len=op]
      [%cut from=name to=name offset=op len=op]
      [%run-lia p=term q=(list op) r=(list (pair name value-type))]
    ==
  +$  name  @tas
  +$  op
    $~  [%zero %const %i32 0]
    $%
      [%name p=name q=value-type]
      [%run p=cord q=(list op)]
      [%len from=name]
      [%zero p=instr-num-zero:sur]
      [%one p=instr-num-one:sur q=op]
      [%two p=instr-num-two:sur q=op r=op]
      [%read-octs-i from=name type=?(%i32 %i64) off=op len=op]
      [%read-octs-f from=name type=?(%f32 %f64) off=op]
    ==
  --
--