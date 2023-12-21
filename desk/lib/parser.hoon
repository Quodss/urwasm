::  WebAssembly parser
::
/-  sur=wasm
|%
::
::  ++main: parsing function. Extends octstream with
::  leading zeros, then applies ++module:r parsing rule.
::
++  main
  |=  wasm=octs
  =,  sur
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
      (cook |=(n=@ (new:si & n)) (shim 0 (dec (bex (min (dec n-bits) 6)))))
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
        (cook |=(n=@ (new:si & n)) (shim 128 255))
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
      %+  fuse  (reap 4 1)
      list
    (stun [4 4] next)
  ::
  ++  f64
    %+  cook
      |=  =(list @)
      ;;  @rd
      %+  can  3
      %+  fuse  (reap 8 1)
      list
    (stun [8 8] next)
  ::
  ++  fuse                        ::  from ~paldev
    |*  [a=(list) b=(list)]
    ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
    ?~  a  ~
    ?~  b  ~
    :-  [i.a i.b]
    $(a t.a, b t.b)
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
  ++  name     (vec prn)
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
  ++  expr-else
    %+  knee  *expression:sur
    |.  ~+
    ;~(sfix (star instr) else)
  ::
  ++  end        (just '\0b')
  ++  else       (just '\05')
  ++  const-i32  (just '\41')
  ++  const-i64  (just '\42')
  ++  const-f32  (just '\43')
  ++  const-f64  (just '\44')
  ++  br-table   (just '\0e')
  ++  block-op   (just '\02')
  ++  loop-op    (just '\03')
  ++  if-op      (just '\04')
  ::
  ++  instr       ::  XX  TODO 
    ;~  pose      ::  parse nesting instruction's types correctly
      instr-zero
      instr-one
      instr-two
      if-else
      block
      loop
      if
    ==
  ::
  ++  instr-zero  (cook handle-zero-args (mask mask-zero))
  ++  instr-one
    ;~  pose
      %+  cook  handle-one-arg-i32
      ;~(plug (mask mask-one-i32) u32)
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
    ==
  ::
  ++  instr-two
    ;~  pose
      %+  cook  handle-two-args-i32
      ;~(plug (mask mask-two-i32) u32 u32)
    ::
      %+  cook  handle-br-table
      ;~(plug br-table vec-u32 u32)
    ==
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
    ;~(pfix if-op ;~(plug block-type expr))
  ::
  ++  if-else
    %+  cook  handle-if-else
    ;~  pfix
      if-op
      ;~(plug block-type expr-else expr)
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
  ++  mask-zero
    ^~
    %+  skim  (gulf '\00' '\ff')
    |=  op=char
    ?=(bin-opcodes-zero-args:sur op)
  ::
  ++  handle-zero-args
    |=  op=char
    ^-  instruction:sur
    ?+  op  ~|(`@ux`op !!)
      %0x0   [%unreachable ~]
      %0x1   [%nop ~]
      %0xf   [%return ~]
      %0x1a  [%drop ~]
      %0x1b  [%select ~]
      %0xa7  [%wrap ~]
      %0xb6  [%demote ~]
      %0xbb  [%promote ~]
    ::
        eqz-opcodes:sur
      :-  %eqz
      ?-  op
        %0x45  %i32
        %0x50  %i64
      ==
    ::
        eq-opcodes:sur
      :-  %eq
      ?-  op
        %0x46  %i32
        %0x51  %i64
        %0x5b  %f32
        %0x61  %f64
      ==
    ::
        ne-opcodes:sur
      :-  %ne
      ?-  op
        %0x47  %i32
        %0x52  %i64
        %0x5c  %f32
        %0x62  %f64
      ==
    ::
        lt-opcodes:sur
      :-  %lt
      ?-  op
        %0x48  [%i32 `%s]
        %0x49  [%i32 `%u]
        %0x53  [%i64 `%s]
        %0x54  [%i64 `%u]
        %0x5d  [%f32 ~]
        %0x63  [%f64 ~]
      ==
    ::
        gt-opcodes:sur
      :-  %gt
      ?-  op
        %0x4a  [%i32 `%s]
        %0x4b  [%i32 `%u]
        %0x55  [%i64 `%s]
        %0x56  [%i64 `%u]
        %0x5e  [%f32 ~]
        %0x64  [%f64 ~]
      ==
    ::
        le-opcodes:sur
      :-  %le
      ?-  op
        %0x4c  [%i32 `%s]
        %0x4d  [%i32 `%u]
        %0x57  [%i64 `%s]
        %0x58  [%i64 `%u]
        %0x5f  [%f32 ~]
        %0x65  [%f64 ~]
      ==
    ::
        ge-opcodes:sur
      :-  %ge
      ?-  op
        %0x4e  [%i32 `%s]
        %0x4f  [%i32 `%u]
        %0x59  [%i64 `%s]
        %0x5a  [%i64 `%u]
        %0x60  [%f32 ~]
        %0x66  [%f64 ~]
      ==
    ::
        clz-opcodes:sur
      :-  %clz
      ?-  op
        %0x67  %i32
        %0x79  %i64
      ==
    ::
        ctz-opcodes:sur
      :-  %ctz
      ?-  op
        %0x68  %i32
        %0x7a  %i64
      ==
    ::
        popcnt-opcodes:sur
      :-  %popcnt
      ?-  op
        %0x69  %i32
        %0x7b  %i64
      ==
    ::
        add-opcodes:sur
      :-  %add
      ?-  op
        %0x6a  %i32
        %0x7c  %i64
        %0x92  %f32
        %0xa0  %f64
      ==
    ::
        sub-opcodes:sur
      :-  %sub
      ?-  op
        %0x6b  %i32
        %0x7d  %i64
        %0x93  %f32
        %0xa1  %f64
      ==
    ::
        mul-opcodes:sur
      :-  %mul
      ?-  op
        %0x6c  %i32
        %0x7e  %i64
        %0x94  %f32
        %0xa2  %f64
      ==
    ::
        div-opcodes:sur
      :-  %div
      ?-  op
        %0x6d  [%i32 `%s]
        %0x6e  [%i32 `%u]
        %0x7f  [%i64 `%s]
        %0x80  [%i64 `%u]
        %0x95  [%f32 ~]
        %0xa3  [%f64 ~]
      ==
    ::
        rem-opcodes:sur
      :-  %rem
      ?-  op
        %0x6f  [%i32 %s]
        %0x70  [%i32 %u]
        %0x81  [%i64 %s]
        %0x82  [%i64 %u]
      ==
    ::
        and-opcodes:sur
      :-  %and
      ?-  op
        %0x71  %i32
        %0x83  %i64
      ==
    ::
        or-opcodes:sur
      :-  %or
      ?-  op
        %0x72  %i32
        %0x84  %i64
      ==
    ::
        xor-opcodes:sur
      :-  %xor
      ?-  op
        %0x73  %i32
        %0x85  %i64
      ==
    ::
        shl-opcodes:sur
      :-  %shl
      ?-  op
        %0x74  %i32
        %0x86  %i64
      ==
    ::
        shr-opcodes:sur
      :-  %shr
      ?-  op
        %0x75  [%i32 %s]
        %0x76  [%i32 %u]
        %0x87  [%i64 %s]
        %0x88  [%i64 %u]
      ==
    ::
        rotl-opcodes:sur
      :-  %rotl
      ?-  op
        %0x77  %i32
        %0x89  %i64
      ==
    ::
        rotr-opcodes:sur
      :-  %rotr
      ?-  op
        %0x78  %i32
        %0x8a  %i64
      ==
    ::
        abs-opcodes:sur
      :-  %abs
      ?-  op
        %0x8b  %f32
        %0x99  %f64
      ==
    ::
        neg-opcodes:sur
      :-  %neg
      ?-  op
        %0x8c  %f32
        %0x9a  %f64
      ==
    ::
        ceil-opcodes:sur
      :-  %ceil
      ?-  op
        %0x8d  %f32
        %0x9b  %f64
      ==
    ::
        floor-opcodes:sur
      :-  %floor
      ?-  op
        %0x8e  %f32
        %0x9c  %f64
      ==
    ::
        trunc-opcodes:sur
      :-  %trunc
      ?-  op
        %0x8f  [%f32 ~ ~]
        %0x9d  [%f64 ~ ~]
        %0xa8  [%i32 `%f32 `%s]
        %0xa9  [%i32 `%f32 `%u]
        %0xaa  [%i32 `%f64 `%s]
        %0xab  [%i32 `%f64 `%u]
        %0xae  [%i64 `%f32 `%s]
        %0xaf  [%i64 `%f32 `%u]
        %0xb0  [%i64 `%f64 `%s]
        %0xb1  [%i64 `%f64 `%u]
      ==
    ::
        nearest-opcodes:sur
      :-  %nearest
      ?-  op
        %0x90  %f32
        %0x9e  %f64
      ==
    ::
        sqrt-opcodes:sur
      :-  %sqrt
      ?-  op
        %0x91  %f32
        %0x9f  %f64
      ==
    ::
        min-opcodes:sur
      :-  %min
      ?-  op
        %0x96  %f32
        %0xa4  %f64
      ==
    ::
        max-opcodes:sur
      :-  %max
      ?-  op
        %0x97  %f32
        %0xa5  %f64
      ==
    ::
        ::  copysign-opcodes:sur
    ::
        extend-opcodes:sur
      :-  %extend
      ?+  op  !!
        %0xac  [%i64 %32 %s]
        %0xad  [%i64 %32 %u]
        %0xc0  [%i32 %8 %s]
      ==
    ::
        convert-opcodes:sur
      :-  %convert
      ?+  op  !!
        %0xba  [%f64 %i64 %u]
      ==
        reinterpret-opcodes:sur
      :-  %reinterpret
      ?+  op  !!
        %0xbf  [%f64 %i64]
      ==
    ::
    ==
  ::
  ++  mask-one-64
    ^-  (list char)
    :~  '\42'
        '\44'
    ==
  ++  mask-one-i32
    ^~
    %+  skim  (gulf '\00' '\ff')
    |=  op=char
    ?&  ?=(bin-opcodes-one-arg:sur op)
        !(~(has in (silt mask-one-64)) op)
        !=(op '\43')
        !=(op '\41')
    ==
  ::
  ++  handle-one-arg-i32
    |=  [op=char arg=@]
    ^-  instruction:sur
    ?+  op  ~|(`@ux`op !!)
      %0xc   [%br arg]
      %0xd   [%br-if arg]
      %0x10  [%call arg]
      %0x20  [%local-get arg]
      %0x21  [%local-set arg]
      %0x22  [%local-tee arg]
      %0x23  [%global-get arg]
      %0x24  [%global-set arg]
      %0x40  [%memory-grow %0x0]
      %0x41  [%const %i32 arg]
    ==
  ::
  ++  mask-two-i32
    ^~
    %+  skim  (gulf '\00' '\ff')
    |=  op=char
    ?&  ?=(bin-opcodes-two-args:sur op)
        !=(op '\0e')
    ==
  ::
  ++  handle-two-args-i32
    |=  [op=char arg1=@ arg2=@]
    ^-  instruction:sur
    ?+  op  ~|(`@ux`op !!)
        %0x11
      ?>  =(arg2 0)
      [%call-indirect arg1 %0x0]
    ::
        %0x28
      [%load %i32 [arg1 arg2] ~ ~]
    ::
        %0x29
      [%load %i64 [arg1 arg2] ~ ~]
    ::
        %0x2b
      [%load %f64 [arg1 arg2] ~ ~]
    ::
        %0x2c
      [%load %i32 [arg1 arg2] `%8 `%s]
    ::
        %0x2d
      [%load %i32 [arg1 arg2] `%8 `%u]
    ::
        %0x2f
      [%load %i32 [arg1 arg2] `%16 `%u]
    ::
        %0x31
      [%load %i64 [arg1 arg2] `%8 `%u]
    ::
        %0x33
      [%load %i64 [arg1 arg2] `%16 `%u]
    ::
        %0x34
      [%load %i64 [arg1 arg2] `%32 `%s]
    ::
        %0x35
      [%load %i64 [arg1 arg2] `%32 `%u]
    ::
        %0x36
      [%store %i32 [arg1 arg2] ~]
    ::
        %0x37
      [%store %i64 [arg1 arg2] ~]
    ::
        %0x39
      [%store %f64 [arg1 arg2] ~]
    ::
        %0x3a
      [%store %i32 [arg1 arg2] `%8]
    ::
        %0x3b
      [%store %i32 [arg1 arg2] `%16]
    ::
        %0x3e
      [%store %i64 [arg1 arg2] `%32]
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
    |=  [type=block-type:sur body=expression:sur]
    ^-  instruction:sur
    [%if type body ~]
  ::
  ++  handle-if-else
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
  ::
  ::  ++section-bytes: break up module bytes into sections
  ::
  ::  A section is
  ::    - a one-byte section id, from 0 to 12
  ::    - u32 size of the contents in bytes,
  ::    - contents, whose structure is dependent on the section id.
  ::  Last two pieces can be reinterpreted as a vector of bytes,
  ::  or (vec next) in terms of parsing rules.
  ::
  ++  section-bytes
    %+  cook  (list tape)
    %-  star
    ;~(plug (shim 0 12) (vec next))
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
      (vec prn)
      (vec prn)
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
      (vec prn)
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
    ?>  ?=([[%const coin-wasm:sur] ~] e)
    :+  %func
      %+  turn  y
      |=  y=@
      ^-  $>(%const instruction:sur)
      [%const %ref %func y]
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
      ^-  $>(%const instruction:sur)
      [%const %ref %func y]
    [%pass ~]
  ::
  ++  handle-elem-2
    |=  [x=@ e=expression:sur et=@ y=(list @)]
    ^-  elem:sur
    ?>  ?=([[%const coin-wasm:sur] ~] e)
    :+  ?+  et  ~|(%unrecognized-elem-kind !!)
          %0x0  %func
        ==
      %+  turn  y
      |=  y=@
      ^-  $>(%const instruction:sur)
      [%const %ref %func y]
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
      ^-  $>(%const instruction:sur)
      [%const %ref %func y]
    [%decl ~]
  ::
  ++  handle-elem-4
    |=  [e=expression:sur el=(list expression:sur)]
    ^-  elem:sur
    ?>  ?=([[%const coin-wasm:sur] ~] e)
    :+  %func
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(%const instruction:sur)
      ?>  ?=([[%const *] ~] ex)
      i.ex
    [%acti 0 i.e]
  ::
  ++  handle-elem-5
    |=  [et=ref-type:sur el=(list expression:sur)]
    ^-  elem:sur
    :+  et
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(%const instruction:sur)
      ?>  ?=([[%const *] ~] ex)
      i.ex
    [%pass ~]
  ::
  ++  handle-elem-6
    |=  [x=@ e=expression:sur et=ref-type:sur el=(list expression:sur)]
    ^-  elem:sur
    ?>  ?=([[%const coin-wasm:sur] ~] e)
    :+  et
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(%const instruction:sur)
      ?>  ?=([[%const *] ~] ex)
      i.ex
    [%acti x i.e]
  ++  handle-elem-7
    |=  [et=ref-type:sur el=(list expression:sur)]
    ^-  elem:sur
    :+  et
      %+  turn  el
      |=  ex=expression:sur
      ^-  $>(%const instruction:sur)
      ?>  ?=([[%const *] ~] ex)
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
    (fuse (reap (lent tape) 1) tape)
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
    ::
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