::  WebAssembly parser
::
/-  sur=wasm
/+  op-map=parser-op-map
/+  simd-map=parser-simd-map
|%
::  ++main: parsing function. Extends octstream with
::  leading zeros, then applies ++module:r parsing rule.
::
++  main
  |=  wasm=octs
  =,  sur
  ^-  module
  =|  out=module
  =/  bytes=tape  (trip q.wasm)
  ::  add leading zeros
  ::
  =.  bytes
    %+  weld  bytes
    ;;  tape
    (reap (sub p.wasm (lent bytes)) '\00')
  (scan bytes module:r)
  ::
  ::  |r: core with parsing rules.
  ::  Parsing rules often use the same name as the types
  ::  in /sur/wasm/hoon. Whenever you need a type make sure
  ::  to point at the structure file. For example,
  ::  num-type in |r refers to the parsing rule, which
  ::  returns a noun with the type num-type:sur
  ::
++  r
  |%
  ::  ++womp: returns a mold of a product of rule
  ::
  ++  womp
    |*  rul=rule
    $_  =+  vex=(rul)
    ?>  ?=(^ q.vex)
    p.u.q.vex
  ::  ++bild: connects an edge with a rule and a rule-
  ::  producing gate, returning the result of the
  ::  rule produced by slamming the gate with
  ::  the result of the first rule
  ::
  ++  bild
    |*  [vex=edge gat=_=>(rule |*(* *rule))]
    ?~  q.vex
      vex
    %.  [vex (gat p.u.q.vex)]
    (comp |*([a=* b=*] b))
  ::  ++bonk: rule modifier. Applies two rules to a nail,
  ::  failing if either failed or if the rules returned
  ::  different continuation. Returns the result of 
  ::  the second rule otherwise.
  ::
  ++  bonk
    |*  [tet=rule fes=rule]
    |=  tub=nail
    ^+  (fes)
    =+  try=(tet tub)
    ?~  q.try  try
    =+  zen=(fes tub)
    ?~  q.zen  zen
    ?:  =(q.u.q.try q.u.q.zen)
      zen
    (fail tub)
  ::  ++feel: rule modifier. Tests equality of
  ::  the parsing result with a given noun
  ::
  ++  feel
    |*  [a=* sef=rule]
    |=  tub=nail
    =+  vex=(sef tub)
    ?~  q.vex  vex
    ?:  =(a p.u.q.vex)
      vex
    [p=p.vex q=~]
  ::  ++u-n: parse uN integer as an atom
  ::
  ++  u-n
    |=  n-bits=@
    =*  this  $
    %+  knee  *@
    |.  ~+
    ;~  pose
      ::  multiple byte case
      ::
      ?:  (lte n-bits 7)  fail
      %+  cook
        |=  [n=@ m=@]
        %+  add
          (mul 128 m)
        (sub n 128)
      ;~  plug
        (shim 128 255)
        this(n-bits (sub n-bits 7))
      ==
      ::  single byte case
      ::
      (cook ,@ (shim 0 (dec (bex (min n-bits 7)))))
    ==
  ::  ++s-n: parse sN integer as a @s
  ::
  ++  s-n
    |=  n-bits=@
    =*  this  $
    %+  knee  *@s
    |.  ~+
    ;~  pose
      ::  single byte: positive
      ::
      (cook (cury new:si &) (shim 0 (dec (bex (min (dec n-bits) 6)))))
      ::  single byte: negative
      ::
      %+  cook
        |=  n=@
        =,  si
        (dif (new & n) --128)
      ;~  simu
        (shim 64 127)
        (shim (sub 128 (min 128 (bex (dec n-bits)))) 127)
      ==
      ::  multiple bytes
      ::
      ?:  (lte n-bits 7)  fail
      %+  cook
        |=  [n=@s m=@s]
        =,  si
        (sum (dif n --128) (pro --128 m))
      ;~  plug
        (cook (cury new:si &) (shim 128 255))
        this(n-bits (sub n-bits 7))
      ==
    ==
  ::
  ++  u32  (u-n 32)
  ++  u64  (u-n 64)
  ++  f32
    %+  cook
      |=  =(list @)
      ;;  @rs
      %+  can  3
      (turn list (lead 1))
    (stun [4 4] next)
  ::
  ++  f64
    %+  cook
      |=  =(list @)
      ;;  @rd
      %+  can  3
      (turn list (lead 1))
    (stun [8 8] next)
  ::  ++vec: parse .wasm vector of rule
  ::
  ++  vec
    |*  rul=rule
    ;~  bild
      u32
      |=  n=@
      (stun [n n] rul)
    ==
  ::
  ++  name     (cook crip (vec prn))
  ++  vec-u32  (vec u32)
  ++  num-type
    %+  cook  num-type:sur
    ;~  pose
      (cold %i32 (just '\7f'))
      (cold %i64 (just '\7e'))
      (cold %f32 (just '\7d'))
      (cold %f64 (just '\7c'))
    ==
  ::
  ++  vec-type  (cold %v128 (just '\7b'))
  ::
  ++  ref-type
    %+  cook  ref-type:sur
    ;~  pose
      (cold %extn (just '\6f'))
      (cold %func (just '\70'))
    ==
  ::
  ++  valtype
    %+  cook  valtype:sur
    ;~(pose num-type vec-type ref-type)
  ::
  ++  func-type
    %+  cook  func-type:sur
    ;~  pfix
      (just '\60')
      ;~(plug (vec valtype) (vec valtype))
    ==
  ::
  ++  limits
    %+  cook  limits:sur
    ;~  pose
      ;~(plug (cold %flor (just '\00')) u32)
      ;~(plug (cold %ceil (just '\01')) u32 u32)
    ==
  ::
  ::  Instruction and expression parsing rules
  ::
  ++  expr
    %+  knee  *expression:sur
    |.  ~+
    ;~(sfix (star instr) end)
  ::
  ++  expr-pair
    %+  knee  [*expression:sur *expression:sur]
    |.  ~+
    ;~  plug
      (star instr)
      ;~  pose
        (cold ~ end)
        (ifix [else end] (star instr))
      ==
    ==
  ::
  ++  end        (just '\0b')
  ++  else       (just '\05')
  ++  const-i32  (just '\41')
  ++  const-i64  (just '\42')
  ++  const-f32  (just '\43')
  ++  const-f64  (just '\44')  
  ++  block-op   (just '\02')
  ++  loop-op    (just '\03')
  ++  if-op      (just '\04')
  ::
  ++  instr
    ;~  pose
      ;~(pfix (just '\fc') fc)  ::  0xfc and 0xfd prefixes
      ;~(pfix (just '\fd') fd)
      select-vec
      instr-zero
      instr-one
      instr-two
      br-table
      block
      loop
      if
    ==
  ::
  ++  select-vec
    %+  cook  instruction:sur
    ;~  pfix
      (just '\1c')
      %+  stag  %select
      %+  stag  %~
      (vec valtype)
    ==
  ::
  ++  br-table
    %+  cook  handle-br-table
    ;~(plug (just '\0e') vec-u32 u32)
  ::
  ++  instr-zero  (sear op-map next)
  ++  instr-one
    %+  cook  instruction:sur
    ;~  pose
      %+  sear  handle-one-arg-i32
      ;~(plug next u32)
    ::
      %+  cook  handle-const-i32
      ;~(plug const-i32 (s-n 32))
    ::
      %+  cook  handle-const-i64
      ;~(plug const-i64 (s-n 64))
    ::
      %+  cook  handle-const-f32
      ;~(plug const-f32 f32)
    ::
      %+  cook  handle-const-f64
      ;~(plug const-f64 f64)
    ::
      ;~  pfix
        (just '\d0')
        (stag %ref-null ref-type)
      ==
    ::
      ;~  pfix
        (just '\d2')
        (stag %ref-func u32)
      ==
    ::
    ==
  ::
  ++  instr-two
    %+  sear  handle-two-args-i32
    ;~(plug next u32 u32)
  ::
  ++  block
    %+  cook  handle-block
    ;~(pfix block-op ;~(plug block-type expr))
  ::
  ++  loop
    %+  cook  handle-loop
    ;~(pfix loop-op ;~(plug block-type expr))
  ::
  ++  if
    %+  cook  handle-if
    ;~  pfix
      if-op
      ;~(plug block-type expr-pair)
    ==
  ::
  ++  block-type
    %+  cook  block-type:sur
    ;~  pose
      (cold [~ ~] (just '\40'))
      ;~(plug (easy ~) (cook get-valtype next) (easy ~))
      (cook abs:si (s-n 33))
    ==
  ::
  ::  All handle-X functions must return `instruction` type
  ::
  ::
  ++  handle-one-arg-i32
    |=  [op=char arg=@]
    ^-  (unit instruction:sur)
    ?+  op  ~
      %0xc   `[%br arg]
      %0xd   `[%br-if arg]
      %0x10  `[%call arg]
      %0x20  `[%local-get arg]
      %0x21  `[%local-set arg]
      %0x22  `[%local-tee arg]
      %0x23  `[%global-get arg]
      %0x24  `[%global-set arg]
      %0x25  `[%table-get arg]
      %0x26  `[%table-set arg]
      %0x3f  `[%memory-size ?>(=(arg 0) %0)]
      %0x40  `[%memory-grow ?>(=(arg 0) %0)]
    ==
  ::
  ::
  ++  handle-two-args-i32
    |=  [op=char arg1=@ arg2=@]
    ^-  (unit instruction:sur)
    ?+  op  ~
        %0x11
      ?>  =(arg2 0)
      `[%call-indirect arg1 %0x0]
    ::
        %0x28
      `[%load %i32 [arg1 arg2] ~ ~]
    ::
        %0x29
      `[%load %i64 [arg1 arg2] ~ ~]
    ::
        %0x2a
      `[%load %f32 [arg1 arg2] ~ ~]
    ::
        %0x2b
      `[%load %f64 [arg1 arg2] ~ ~]
    ::
        %0x2c
      `[%load %i32 [arg1 arg2] `%8 `%s]
    ::
        %0x2d
      `[%load %i32 [arg1 arg2] `%8 `%u]
    ::
        %0x2e
      `[%load %i32 [arg1 arg2] `%8 `%s]
    ::
        %0x2f
      `[%load %i32 [arg1 arg2] `%16 `%u]
    ::
        %0x30
      `[%load %i64 [arg1 arg2] `%8 `%s]
    ::
        %0x31
      `[%load %i64 [arg1 arg2] `%8 `%u]
    ::
        %0x32
      `[%load %i64 [arg1 arg2] `%16 `%s]
    ::
        %0x33
      `[%load %i64 [arg1 arg2] `%16 `%u]
    ::
        %0x34
      `[%load %i64 [arg1 arg2] `%32 `%s]
    ::
        %0x35
      `[%load %i64 [arg1 arg2] `%32 `%u]
    ::
        %0x36
      `[%store %i32 [arg1 arg2] ~]
    ::
        %0x37
      `[%store %i64 [arg1 arg2] ~]
    ::
        %0x38
      `[%store %f32 [arg1 arg2] ~]
    ::
        %0x39
      `[%store %f64 [arg1 arg2] ~]
    ::
        %0x3a
      `[%store %i32 [arg1 arg2] `%8]
    ::
        %0x3b
      `[%store %i32 [arg1 arg2] `%16]
    ::
        %0x3c
      `[%store %i64 [arg1 arg2] `%8]
    ::
        %0x3d
      `[%store %i64 [arg1 arg2] `%16]
    ::
        %0x3e
      `[%store %i64 [arg1 arg2] `%32]
    ==
  ::
  ++  handle-br-table
    |=  [op=char vec=(list @) i=@]
    ^-  instruction:sur
    ?>  ?=(%0xe op)
    [%br-table vec i]
  ::
  ++  handle-block
    |=  [type=block-type:sur body=expression:sur]
    ^-  $>(%block instruction:sur)
    [%block type body]
  ::
  ++  get-valtype
    |=  byte=@
    ^-  valtype:sur
    ?+  byte  ~|(`@ux`byte !!)
      %0x7f  %i32
      %0x7e  %i64
      %0x7d  %f32
      %0x7c  %f64
    ==
  ++  handle-loop
    |=  [type=block-type:sur body=expression:sur]
    ^-  instruction:sur
    [%loop type body]
  ::
  ++  handle-if
    |=  $:  type=block-type:sur
            body-true=expression:sur
            body-false=expression:sur
        ==
    ^-  instruction:sur
    [%if type body-true body-false]
  ::
  ++  handle-const-f64
    |=  [op=char i=@rd]
    ^-  instruction:sur
    [%const %f64 i]
  ::
  ++  handle-const-f32
    |=  [op=char i=@rs]
    ^-  instruction:sur
    [%const %f32 i]
  ::
  ++  handle-const-i32
    |=  [op=char i=@s]
    ^-  instruction:sur
    =;  i-unsigned=@
      [%const %i32 i-unsigned]
    =,  si
    ?:  (syn i)
      +:(old i)
    (sub (bex 32) +:(old i))
  ::
  ++  handle-const-i64
    |=  [op=char i=@s]
    ^-  instruction:sur
    =;  i-unsigned=@
      [%const %i64 i-unsigned]
    =,  si
    ?:  (syn i)
      +:(old i)
    (sub (bex 64) +:(old i))
  ::  ++fc: 0xFC extension parser
  ::
  ++  fc
    |^
    %+  cook  instruction:sur
    ;~  pose
      zero-args
      one-arg
      two-args
    ==
    ::
    ++  zero-args  (sear handle-zero u32)
    ++  one-arg    (sear handle-one ;~(plug u32 u32))
    ++  two-args   (sear handle-two ;~(plug u32 u32 u32))
    ++  handle-zero
      |=  op=@
      ^-  (unit instruction:sur)
      ?.  (lte op 7)  ~
      :-  ~
      :*
        %trunc
      ::  Type
      ::
        ?:((lte op 3) %i32 %i64)
      ::  Source type
      ::
        `?:(=(0 (mod (div op 2) 2)) %f32 %f64)
      ::  Mode
      ::
        `?:(=(0 (mod op 2)) %s %u)
      ::
        &  ::  saturated
      ==
    ::
    ++  handle-one
      |=  [op=@ arg=@]
      ^-  (unit instruction:sur)
      ?+  op  ~
        %9   `[%data-drop arg]
        %11  `[%memory-fill ?>(?=(%0 arg) arg)]
        %13  `[%elem-drop arg]
        %15  `[%table-grow arg]
        %16  `[%table-size arg]
        %17  `[%table-fill arg]
      ==
    ::
    ++  handle-two
      |=  [op=@ arg1=@ arg2=@]
      ^-  (unit instruction:sur)
      ?+  op  ~
        %8   `[%memory-init arg1 ?>(?=(%0 arg2) arg2)]
        %10  `[%memory-copy ?>(?=(%0 arg1) arg1) ?>(?=(%0 arg1) arg1)]
        %12  `[%table-init arg1 arg2]
        %14  `[%table-copy arg1 arg2]
      ==
    ::
    --
  ::  ++fd: 0xFD extension parser
  ::
  ++  fd
    |^
    ::  Opcode and immediate parameters
    ::
    ;~  pose
      (sear memarg ;~(plug u32 u32 u32))
      (sear mem-lane ;~(plug u32 ;~(plug u32 u32) next))
      (cook const ;~(pfix (feel 12 u32) (stun [16 16] next)))
      (cook shuffle ;~(pfix (feel 13 u32) (stun [16 16] next)))
      (sear lane ;~(plug u32 next))
      (sear simd-map u32)
    ==
    ::
    ++  memarg
      |=  [op=@ mem=[@ @]]
      ^-  (unit instruction:sur)
      =;  =(unit instr-vec:sur)
        ?~  unit  ~
        `[%vec u.unit]
      ?+  op  ~
        %0   `[%load mem ~]
        %1   `[%load mem ~ %8 %extend %s]
        %2   `[%load mem ~ %8 %extend %u]
        %3   `[%load mem ~ %16 %extend %s]
        %4   `[%load mem ~ %16 %extend %u]
        %5   `[%load mem ~ %32 %extend %s]
        %6   `[%load mem ~ %32 %extend %u]
        %7   `[%load mem ~ %8 %splat]
        %8   `[%load mem ~ %16 %splat]
        %9   `[%load mem ~ %32 %splat]
        %10  `[%load mem ~ %64 %splat]
        %92  `[%load mem ~ %32 %zero]
        %93  `[%load mem ~ %64 %zero]
        %11  `[%store mem]
      ==
    ::
    ++  mem-lane
      |=  [op=@ mem=[@ @] l=@]
      ^-  (unit instruction:sur)
      =;  =(unit instr-vec:sur)
        ?~  unit  ~
        `[%vec u.unit]
      ?+  op  ~
        %84  `[%load-lane mem %8 l]
        %85  `[%load-lane mem %16 l]
        %86  `[%load-lane mem %32 l]
        %87  `[%load-lane mem %64 l]
        %88  `[%store-lane mem %8 l]
        %89  `[%store-lane mem %16 l]
        %90  `[%store-lane mem %32 l]
        %91  `[%store-lane mem %64 l]
      ==
    ::
    ++  const
      |=  =(list @)
      ^-  instruction:sur
      :^  %vec  %const  %v128
      (can 3 (turn list (lead 1)))
    ::
    ++  shuffle
      |=  =(list @)
      ^-  instruction:sur
      [%vec %shuffle list]
    ::
    ++  lane
      |=  [op=@ l=@]
      ^-  (unit instruction:sur)
      =;  =(unit instr-vec:sur)
        ?~  unit  ~
        `[%vec u.unit]
      ?+  op  ~
        %21  `[%extract %i8 l %s]
        %22  `[%extract %i8 l %u]
        %23  `[%replace %i8 l]
        %24  `[%extract %i16 l %s]
        %25  `[%extract %i16 l %u]
        %26  `[%replace %i16 l]
        %27  `[%extract %i32 l %u]
        %28  `[%replace %i32 l]
        %29  `[%extract %i64 l %u]
        %30  `[%replace %i64 l]
        %31  `[%extract %f32 l %u]
        %32  `[%replace %f32 l]
        %33  `[%extract %f64 l %u]
        %34  `[%replace %f64 l]
      ==
    --
  ::
  ::  Section rules
  ::
  ::  Type section
  ::
  ++  type-section
    %+  cook  type-section:sur
    (vec func-type)
  ::  Import section
  ::
  ++  import-section
    %+  cook  import-section:sur
    (vec import)
  ::
  ++  import
    %+  cook  import:sur
    ;~  plug
      name
      name
      import-desc
    ==
  ::
  ++  import-desc
    ;~  pose
      import-func
      import-tabl
      import-memo
      import-glob
    ==
  ::
  ++  import-func  ;~(plug (cold %func (just '\00')) u32)
  ++  import-tabl  ;~(plug (cold %tabl (just '\01')) ref-type limits)
  ++  import-memo  ;~(plug (cold %memo (just '\02')) limits)
  ++  import-glob
    ;~  plug
      (cold %glob (just '\03'))
      valtype
      con-var
    ==
  ::
  ++  con-var
    ;~  pose
      (cold %con (just '\00'))
      (cold %var (just '\01'))
    ==  
  ::  Function section
  ::
  ++  function-section
    %+  cook  function-section:sur
    (vec u32)
  ::  Table section
  ::
  ++  table-section
    %+  cook  table-section:sur
    (vec table)
  ::
  ++  table  ;~(plug ref-type limits)
  ::  Memory section
  ::
  ++  memory-section
    %+  cook  memory-section:sur
    (vec limits)
  ::  Global section
  ::
  ++  global-section
    %+  cook  global-section:sur
    (vec global)
  ::
  ++  global
    %+  cook  global:sur
    ;~  plug
      valtype
      con-var
      ;~(sfix instr end)
    ==
  ::  Export section
  ::
  ++  export-section
    %+  cook  export-section:sur
    (vec export)
  ::
  ++  export
    ;~  plug
      name
      ;~  pose
        (cold %func (just '\00'))
        (cold %tabl (just '\01'))
        (cold %memo (just '\02'))
        (cold %glob (just '\03'))
      ==
      u32
    ==
  ::  Start section
  ::
  ++  start-section
    %+  cook  start-section:sur
    (punt u32)
  ::  Element section
  ::
  ++  elem-section
    %+  cook  elem-section:sur
    (vec elem)
  ::
  ++  elem
    ;~  pose
      elem-0
      elem-1
      elem-2
      elem-3
      elem-4
      elem-5
      elem-6
      elem-7
    ==
  ::
  ++  elem-kind  (just '\00')
  ++  elem-0
    %+  cook  handle-elem-0  ::  write handle functions
    ;~  pfix  (just '\00')
      ;~(plug expr (vec u32))
    ==
  ::
  ++  elem-1
    %+  cook  handle-elem-1
    ;~  pfix  (just '\01')
      ;~(plug elem-kind (vec u32))
    ==
  ::
  ++  elem-2
    %+  cook  handle-elem-2
    ;~  pfix  (just '\02')
      ;~(plug u32 expr elem-kind (vec u32))
    ==
  ::
  ++  elem-3
  %+  cook  handle-elem-3
  ;~  pfix  (just '\03')
    ;~(plug elem-kind (vec u32))
  ==
  ::
  ++  elem-4
    %+  cook  handle-elem-4
    ;~  pfix  (just '\04')
      ;~(plug expr (vec expr))
    ==
  ::
  ++  elem-5
    %+  cook  handle-elem-5
    ;~  pfix  (just '\05')
      ;~(plug ref-type (vec expr))
    ==
  ::
  ++  elem-6
    %+  cook  handle-elem-6
    ;~  pfix  (just '\06')
      ;~(plug u32 expr ref-type (vec expr))
    ==
  ::
  ++  elem-7  
    %+  cook  handle-elem-7
    ;~  pfix  (just '\07')
      ;~(plug ref-type (vec expr))
    ==
  ::
  ++  handle-elem-0
    |=  [e=expression:sur y=(list @)]
    ^-  elem:sur
    ?>  ?=([$>(%const instruction:sur) ~] e)
    :+  %func
      %+  turn  y
      |=  y=@
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      [%ref-func y]
    [%acti 0 i.e]
  ::
  ++  handle-elem-1
    |=  [et=@ y=(list @)]
    ^-  elem:sur
    :+  ?+  et  ~|(%unrecognized-elem-kind !!)
          %0x0  %func
        ==
      %+  turn  y
      |=  y=@
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      [%ref-func y]
    [%pass ~]
  ::
  ++  handle-elem-2
    |=  [x=@ e=expression:sur et=@ y=(list @)]
    ^-  elem:sur
    ?>  ?=([$>(%const instruction:sur) ~] e)
    :+  ?+  et  ~|(%unrecognized-elem-kind !!)
          %0x0  %func
        ==
      %+  turn  y
      |=  y=@
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      [%ref-func y]
    [%acti x i.e]
  ::
  ++  handle-elem-3
    |=  [et=@ y=(list @)]
    ^-  elem:sur
    :+  ?+  et  ~|(%unrecognized-elem-kind !!)
          %0x0  %func
        ==
      %+  turn  y
      |=  y=@
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      [%ref-func y]
    [%decl ~]
  ::
  ++  handle-elem-4
    |=  [e=expression:sur el=(list expression:sur)]
    ^-  elem:sur
    ?>  ?=([$>(%const instruction:sur) ~] e)
    :+  %func
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      ?>  ?=([[%ref-func *] ~] ex)
      i.ex
    [%acti 0 i.e]
  ::
  ++  handle-elem-5
    |=  [et=ref-type:sur el=(list expression:sur)]
    ^-  elem:sur
    :+  et
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      ?>  ?=([[%ref-func *] ~] ex)
      i.ex
    [%pass ~]
  ::
  ++  handle-elem-6
    |=  [x=@ e=expression:sur et=ref-type:sur el=(list expression:sur)]
    ^-  elem:sur
    ?>  ?=([$>(%const instruction:sur) ~] e)
    :+  et
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      ?>  ?=([[%ref-func *] ~] ex)
      i.ex
    [%acti x i.e]
  ::
  ++  handle-elem-7
    |=  [et=ref-type:sur el=(list expression:sur)]
    ^-  elem:sur
    :+  et
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(?(%ref-func %ref-null) instruction:sur)
      ?>  ?=([[%ref-func *] ~] ex)
      i.ex
    [%decl ~]
  ::  Code section
  ::
  ++  code-section
    %+  cook  code-section:sur
    (vec code)
  ::
  ++  code  (bonk (vec next) ;~(pfix u32 func))
  ++  func
    ;~  plug
      (cook handle-locals (vec locals))
      expr
    ==
  ::
  ++  locals  ;~(plug u32 valtype)
  ::  ++handle-locals: concatenate locals declaration
  ::
  ++  handle-locals
    |=  l=(list [n=@ v=valtype:sur])
    ^-  (list valtype:sur)
    ?~  l  ~
    %+  weld  (reap n.i.l v.i.l)
    $(l t.l)
  ::  Data section
  ::
  ++  data-section
    %+  cook  data-section:sur
    (vec data)
  ::
  ++  data
    %+  cook  handle-data
    ;~    pose
        ;~  plug
          (cold %acti (just '\00'))
          ;~(sfix instr end)
          (cook to-octs (vec next))
        ==
    ::
        ;~  plug
          (cold %pass (just '\01'))
          (cook to-octs (vec next))
        ==
    ::
        ;~    plug
            (cold %acti (just '\02'))
            ;~    pfix
                u32
                ;~  plug
                  ;~(sfix instr end)
                  (cook to-octs (vec next))
    ==  ==  ==  ==
  ::
  ++  to-octs
    |=  =tape
    ^-  octs
    :-  (lent tape)
    %+  can  3
    (turn tape (lead 1))
  ::
  ++  handle-data
    |=  $=  p
        $%  [%acti off=instruction:sur b=octs]
            [%pass b=octs]
        ==
    ^-  data:sur
    ?:  ?=(%pass -.p)  p
    ?>  ?=($>(%const instruction:sur) off.p)
    p
  ::  
  ++  datacnt-section
    %+  cook  datacnt-section:sur
    (punt u32)
  ::
  ++  module
    %+  cook  module:sur
    ;~  pfix
      magic
      version
      (ifix [customs customs] module-contents)
    ==
  ::
  ++  module-contents
    ;~  (glue customs)
      (check 1 type-section *type-section:sur)
      (check 2 import-section *import-section:sur)
      (check 3 function-section *function-section:sur)
      (check 4 table-section *table-section:sur)
      (check 5 memory-section *memory-section:sur) 
      (check 6 global-section *global-section:sur)
      (check 7 export-section *export-section:sur)
      (check 8 start-section *start-section:sur)
      (check 9 elem-section *elem-section:sur)
      (check 12 datacnt-section *datacnt-section:sur)
      (check 10 code-section *code-section:sur)
      (check 11 data-section *data-section:sur)
    ==
  ::
  ++  check
    |*  [id=@ sec=rule def=*]
    ;~  pose
    ::  section present
    ::
      ;~  pfix
        (just `@`id)
        (bonk (vec next) ;~(pfix u32 sec))
      ==
    ::  section missing
    ::
      (easy `(womp sec)`def)
    ==
  ::
  ++  customs
    %-  star
    ;~  plug
      (just '\00')
      (vec next)
    ==
  ::
  ++  magic  (jest '\00asm')
  ++  version
    ;~  plug
      (just '\01')
      (just '\00')  ::  leading zeros shenanigans
      (just '\00')
      (just '\00')
    ==
  ::
  --  :: |r
--