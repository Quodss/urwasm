::  .wasm -> AST transformer
::  (main:wasm-to-ast wasm-file=@) returns AST representation of the module
::
::::  /hoon/wasm-to-ast/lib
  ::
/-  *wasm
|%
++  main
  |=  wasm=@
  ^-  module
  =/  wasm-bytes=(list @ux)  (rip 3 wasm)
  ?>  ~|  'malformed-wasm-header'
      .=  (scag 8 wasm-bytes)
      ^~  %+  weld
            `(list @ux)`[0x0 ((list @ux) "asm")]  ::  binary magic
          `(list @ux)`~[0x1 0x0 0x0 0x0]          ::  Wasm version
  ::
  ::  Wasm binary is a vector of [section-code=@ux section-size=@ux section-bytes=(list @ux)]
  ::
  =.  wasm-bytes  (slag 8 wasm-bytes)
  =|  out=module
  |-  ^-  module
  ?~  wasm-bytes  out
  =/  section-code=@ux  i.wasm-bytes
  =^  section-size=@  t.wasm-bytes  (snip-u-n t.wasm-bytes 32)
  =/  section-bytes=(list @ux)  (scag section-size t.wasm-bytes)
  ::  zero-padding
  ::
  =?  section-bytes  !=(section-size (lent section-bytes))
    (weld section-bytes (reap (sub section-size (lent section-bytes)) 0x0))
  =.  out
    ?+  section-code  out  ::  ~&("skipped {<section-code>}" out)
    ::  each section is found only once in the binary file
    ::
      %0x1  out(type-section (get-type-section section-bytes))
      %0x3  out(function-section (get-function-section section-bytes))
      %0x4  out(table-section (get-table-section section-bytes))
      %0x5  out(memory-section (get-memory-section section-bytes))
      %0x6  out(global-section (get-global-section section-bytes))
      %0x7  out(export-section (get-export-section section-bytes))
      %0x9  out(elem-section (get-elem-section section-bytes))
      %0xa  out(code-section (get-code-section section-bytes))
      %0xb  out(data-section (get-data-section section-bytes))
    ==
  $(wasm-bytes (slag section-size t.wasm-bytes))
::
++  snip-u-n
  |=  [bytes=(list @ux) n=@]
  ^-  [@u (list @ux)]
  ?~  bytes  !!
  ?:  (lth i.bytes 128)
    ?>  (lth i.bytes (bex n))
    [i.bytes t.bytes]
  ?>  (gth n 7)
  =/  [m=@u rest-bytes=(list @ux)]  $(bytes t.bytes, n (sub n 7))
  :_  rest-bytes
  (add (sub i.bytes 128) (mul 128 m))
::
++  snip-s-n
  !:
  |=  [bytes=(list @ux) n=@]
  ^-  [@s (list @ux)]
  =,  si
  ?~  bytes  !!
  ?:  (lth i.bytes 64)
    ?>  (lth i.bytes (bex (dec n)))
    [(new & i.bytes) t.bytes]
  ?:  (lth i.bytes 128)
    ?>  |((gth n 8) (gte i.bytes (sub 128 (bex (dec n)))))
    [(dif (new & i.bytes) --128) t.bytes]
  ?>  (gth n 7)
  =/  [m=@s rest-bytes=(list @ux)]  $(bytes t.bytes, n (sub n 7))
  :_  rest-bytes
  (sum (new & (sub i.bytes 128)) (pro --128 m))
::
++  get-type-section
  |=  bytes=(list @ux)
  ^-  type-section
  ?:  =(~ bytes)  *type-section
  =|  out=type-section
  =^  num-types=@  bytes  (snip-u-n bytes 32)
  =/  type-bytes=(list @ux)  bytes
  |-  ^-  type-section
  ?:  =(~ type-bytes)
    ?>  =(num-types (lent out))
    (flop out)
  =^  =func-type  type-bytes
    (get-func-type type-bytes)
  $(out [func-type out])
::
++  get-func-type
  |=  bytes=(list @ux)
  ^-  [func-type (list @ux)]
  ?>  =((snag 0 bytes) %0x60)
  =.  bytes  (slag 1 bytes)
  =|  out=func-type
  =^  num-params=@  bytes  (snip-u-n bytes 32)
  =.  params.out  (turn (scag num-params bytes) get-valtype)
  =^  num-results=@  bytes  (snip-u-n (slag num-params bytes) 32)
  =.  results.out  (turn (scag num-results bytes) get-valtype)
  ?>  =(num-results (lent results.out))
  ?>  =(num-params (lent params.out))
  :-  out
  (slag num-results bytes)
::
++  get-valtype
  |=  byte=@
  ^-  valtype
  ?+  byte  ~|(`@ux`byte !!)
    %0x7f  %i32
    %0x7e  %i64
    %0x7d  %f32
    %0x7c  %f64
  ==
::
++  get-function-section
  |=  bytes=(list @ux)
  ^-  function-section
  ?:  =(~ bytes)  *function-section
  =^  num-functions=@  bytes  (snip-u-n bytes 32)
  =/  out=function-section  ((list @) bytes)
  ?>  =(num-functions (lent out))
  out
::
++  get-table-section
  |=  bytes=(list @ux)
  ^-  table-section
  |^
  (scan ;;(tape bytes) table-section-rule)
  ::
  ++  table-section-rule  (vec table-rule)
  ++  table-rule
    %+  cook  table
    ;~  plug
      (cold %funcref (just '\70'))
      ;~(pose lim-0 lim-1)
    ==
  ::
  ++  lim-0
    ;~  pfix
      (just '\00')
      ;~(plug (i-n 32) (easy ~))
    ==
  ::
  ++  lim-1
    ;~  pfix
      (just '\01')
      ;~(plug (i-n 32) (easy ~) (i-n 32))
    ==
  ::
  --
::
++  get-memory-section
  |=  bytes=(list @ux)
  ^-  memory-section
  |^
  (scan ;;(tape bytes) memory-section-rule)
  ::
  ++  memory-section-rule  (vec mem-rule)
  ++  mem-rule
    %+  cook  mem
    ;~(pose lim-0 lim-1)
  ::
  ++  lim-0
    ;~  pfix
      (just '\00')
      ;~(plug (i-n 32) (easy ~))
    ==
  ::
  ++  lim-1
    ;~  pfix
      (just '\01')
      ;~(plug (i-n 32) (easy ~) (i-n 32))
    ==
  ::
  --
::
++  get-global-section
  |=  bytes=(list @ux)
  ^-  global-section
  |^
  (scan ;;(tape bytes) global-section-rule)
  ::
  ++  global-section-rule  (vec global-rule)
  ++  global-rule
    %+  cook  global
    ;~  plug
      (cook get-valtype (mask "\7c\7d\7e\7f"))
    ::
      ;~  pose
        (cold %const (just '\00'))
        (cold %mut (just '\01'))
      ==
    ::
      expression-end
    ==
  --
::
++  get-export-section
  |=  bytes=(list @ux)
  ^-  export-section
  ?:  =(~ bytes)  *export-section
  =|  out=export-section
  =^  num-exports=@  bytes  (snip-u-n bytes 32)
  =/  exports-bytes=(list @ux)  bytes
  ?:  =(0 num-exports)
    ?>  =(~ exports-bytes)
    out
  |-  ^-  export-section
  ?:  =(~ exports-bytes)
    ?>  =(0 num-exports)
    (flop out)
  =^  length-name=@  exports-bytes  (snip-u-n exports-bytes 32)
  =/  name=@t
    %-  crip
    ;;  tape
    (scag length-name exports-bytes)
  =/  export-desc-byte=@ux  (snag length-name exports-bytes)
  :: ?>  =(export-desc-byte %0x0)
  :: =^  export-index-byte=@  exports-bytes  (snip-u-n (slag +(length-name) exports-bytes) 32)
  :: %=  $
  ::   out  [[name %func export-index-byte] out]
  ::   num-exports    (dec num-exports)
  :: ==
  ?+    export-desc-byte  !!
      %0x0
     =^  export-index-byte=@  exports-bytes  (snip-u-n (slag +(length-name) exports-bytes) 32)
     %=  $
       out  [[name %func export-index-byte] out]
       num-exports    (dec num-exports)
     ==
  ::
      %0x2
    =^  export-index-byte=@  exports-bytes  (snip-u-n (slag +(length-name) exports-bytes) 32)
     %=  $
       out  [[name %memory export-index-byte] out]
       num-exports    (dec num-exports)
     ==
  ==
::
++  get-elem-section
  |=  bytes=(list @ux)
  ^-  elem-section
  |^
  (scan ;;(tape bytes) elem-section-rule)
  ::
  ++  elem-section-rule  (vec elem-rule)
  ++  elem-rule
  %+  cook  elem
  ;~  plug
    (just '\00')
    expression-end
    vec-i-32
  ==
  --
::
++  get-code-section
  |=  bytes=(list @ux)
  ^-  code-section
  ?:  =(~ bytes)  *code-section
  =|  out=code-section
  =^  num-codes=@  bytes  (snip-u-n bytes 32)
  =/  codes-bytes=(list @ux)  bytes
  |-  ^-  code-section
  ?:  =(~ codes-bytes)
    ?>  =(num-codes 0)
    (flop out)
  =^  one-code-length=@  codes-bytes  (snip-u-n codes-bytes 32)
  =/  one-code-bytes=(list @ux)
    (scag one-code-length codes-bytes)
  ?>  =(one-code-length (lent one-code-bytes))
  %=  $
    num-codes    (dec num-codes)
    codes-bytes  (slag one-code-length codes-bytes)
    out          [(get-code one-code-bytes) out]
  ==
::
++  get-code
  |=  bytes=(list @ux)
  ^-  code
  ?:  =(~ bytes)  *code
  |^
  =|  out=code
  =^  locals-number=@  bytes  (snip-u-n bytes 32)
  =^  locals=(list valtype)  bytes  (handle-locals locals-number bytes)
  out(expression (parse-instructions bytes), locals locals)
  ::
  ++  handle-locals
    |=  [locals-number=@ bytes=(list @ux)]
    ^-  [(list valtype) (list @ux)]
    ?:  =(locals-number 0)
      [~ bytes]
    =^  valtype-number=@  bytes  (snip-u-n bytes 32)
    ?~  bytes  !!
    =/  v=valtype  (get-valtype i.bytes)
    =^  rest-locals=(list valtype)  t.bytes
      $(locals-number (dec locals-number), bytes t.bytes)
    :_  t.bytes
    (weld (reap valtype-number v) rest-locals)
  ::
  --
::
++  get-data-section
  |=  bytes=(list @ux)
  ^-  data-section
  |^
  (scan ;;(tape bytes) data-section-rule)
  ::
  ++  data-section-rule  (vec data-rule)
  ++  data-rule
    %+  cook  data
    ;~  pose
      ;~(plug (cold %active (just '\00')) expression-end (cook to-atoms (vec next)))
      ;~(plug (cold %passive (just '\01')) (cook to-atoms (vec next)))
    ==
  ::
  ++  to-atoms
    |=  a=(list char)
    ^-  [@ @]
    :-  (lent a)
    %+  can  3
    (fuse (reap (lent a) 1) a)
  ::
  --
::
++  parse-instructions
  |=  bytes=(pole @ux)
  ^-  (list instruction)
  (scan ;;(tape bytes) expression-end)
::  Functional parser utils for expression parsing
::
::  ++i-n: parse n-bit unsigned integer 
::
++  i-n  ::  parse u32 -> (i-n 32)
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
::  ++bild: if an edge `vex` reflects a failure, fail,
::  otherwise connect `vex` with a rule built from the parsing result
::  slammed against the gate `gat`
::
++  bild
  |*  [vex=edge gat=_=>(rule |*(* *rule))]
  ?~  q.vex
    vex
  %.  [vex (gat p.u.q.vex)]
  (comp |*([a=* b=*] b))
::  parse i32 vector
::
++  vec-i-32
  %+  cook  ,(list @)
  ;~  bild
    (i-n 32)
    |=  n=@
    (stun [n n] (cook ,@ (i-n 32)))
  ==
::
++  vec           ::  vec-i-32 == (vec (i-n 32))
  |*  rul=rule
  ;~  bild
    (i-n 32)
    |=  n=@
    (stun [n n] rul)
  ==
::
++  limits-parser
  %+  cook  limits
  ;~
    pose
    ;~(plug (just '\00') (i-n 32))
    ;~(plug (just '\01') (i-n 32) (i-n 32))
  ==
::
:: ++  memory-section-parser
::   %+  cook  memory-section
::   ;~
::     pfix
::     (just '\05')
::     (vec limits-parser)
::   ==
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
++  f32
  %+  cook
    |=  =(list @)
    ;;  @rs
    %+  can  3
    %+  fuse  (reap 4 1)
    list
  (stun [4 4] next)
::
++  fuse                                                ::  from ~paldev
  |*  [a=(list) b=(list)]
  ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
  ?~  a  ~
  ?~  b  ~
  :-  [i.a i.b]
  $(a t.a, b t.b)
::
::  parse an expression that ends with `end` (0xb)
::
++  expression-end
  %+  knee  *(list instruction)
  |.  ~+
  ;~(sfix (star instr) end)
::  parse an expression that ends with `else` (0x5)
::
++  expression-else
  %+  knee  *(list instruction)
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
::  parse an instruction
::
++  instr
  ;~  pose
    instr-zero
    instr-one
    instr-two
    if-else
    block
    loop
    if
  ==
::
::  Instruction parsers
::
++  instr-zero  (cook handle-zero-args (mask mask-zero))
++  instr-one
  ;~  pose
    %+  cook  handle-one-arg-i32
    ;~(plug (mask mask-one-i32) (i-n 32))
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
    ;~(plug (mask mask-two-i32) (i-n 32) (i-n 32))
  ::
    %+  cook  handle-br-table
    ;~(plug br-table vec-i-32 (i-n 32))
  ==
::
++  block
  %+  cook  handle-block
  ;~(pfix block-op ;~(plug next expression-end))
::
++  loop
  %+  cook  handle-loop
  ;~(pfix loop-op ;~(plug next expression-end))
::
++  if
  %+  cook  handle-if
  ;~(pfix if-op ;~(plug next expression-end))
::
++  if-else
  %+  cook  handle-if-else
  ;~(pfix if-op ;~(plug next expression-else expression-end))
::
::  All handle-X functions must return `instruction` type
::
++  mask-zero
  ^~
  %+  skim  (gulf '\00' '\ff')
  |=  op=char
  ?=(bin-opcodes-zero-args op)
::
++  handle-zero-args
  |=  op=char
  ^-  instruction
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
      eqz-opcodes
    :-  %eqz
    ?-  op
      %0x45  %i32
      %0x50  %i64
    ==
  ::
      eq-opcodes
    :-  %eq
    ?-  op
      %0x46  %i32
      %0x51  %i64
      %0x5b  %f32
      %0x61  %f64
    ==
  ::
      ne-opcodes
    :-  %ne
    ?-  op
      %0x47  %i32
      %0x52  %i64
      %0x5c  %f32
      %0x62  %f64
    ==
  ::
      lt-opcodes
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
      gt-opcodes
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
      le-opcodes
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
      ge-opcodes
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
      clz-opcodes
    :-  %clz
    ?-  op
      %0x67  %i32
      %0x79  %i64
    ==
  ::
      ctz-opcodes
    :-  %ctz
    ?-  op
      %0x68  %i32
      %0x7a  %i64
    ==
  ::
      popcnt-opcodes
    :-  %popcnt
    ?-  op
      %0x69  %i32
      %0x7b  %i64
    ==
  ::
      add-opcodes
    :-  %add
    ?-  op
      %0x6a  %i32
      %0x7c  %i64
      %0x92  %f32
      %0xa0  %f64
    ==
  ::
      sub-opcodes
    :-  %sub
    ?-  op
      %0x6b  %i32
      %0x7d  %i64
      %0x93  %f32
      %0xa1  %f64
    ==
  ::
      mul-opcodes
    :-  %mul
    ?-  op
      %0x6c  %i32
      %0x7e  %i64
      %0x94  %f32
      %0xa2  %f64
    ==
  ::
      div-opcodes
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
      rem-opcodes
    :-  %rem
    ?-  op
      %0x6f  [%i32 %s]
      %0x70  [%i32 %u]
      %0x81  [%i64 %s]
      %0x82  [%i64 %u]
    ==
  ::
      and-opcodes
    :-  %and
    ?-  op
      %0x71  %i32
      %0x83  %i64
    ==
  ::
      or-opcodes
    :-  %or
    ?-  op
      %0x72  %i32
      %0x84  %i64
    ==
  ::
      xor-opcodes
    :-  %xor
    ?-  op
      %0x73  %i32
      %0x85  %i64
    ==
  ::
      shl-opcodes
    :-  %shl
    ?-  op
      %0x74  %i32
      %0x86  %i64
    ==
  ::
      shr-opcodes
    :-  %shr
    ?-  op
      %0x75  [%i32 %s]
      %0x76  [%i32 %u]
      %0x87  [%i64 %s]
      %0x88  [%i64 %u]
    ==
  ::
      rotl-opcodes
    :-  %rotl
    ?-  op
      %0x77  %i32
      %0x89  %i64
    ==
  ::
      rotr-opcodes
    :-  %rotr
    ?-  op
      %0x78  %i32
      %0x8a  %i64
    ==
  ::
      abs-opcodes
    :-  %abs
    ?-  op
      %0x8b  %f32
      %0x99  %f64
    ==
  ::
      neg-opcodes
    :-  %neg
    ?-  op
      %0x8c  %f32
      %0x9a  %f64
    ==
  ::
      ceil-opcodes
    :-  %ceil
    ?-  op
      %0x8d  %f32
      %0x9b  %f64
    ==
  ::
      floor-opcodes
    :-  %floor
    ?-  op
      %0x8e  %f32
      %0x9c  %f64
    ==
  ::
      trunc-opcodes
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
      nearest-opcodes
    :-  %nearest
    ?-  op
      %0x90  %f32
      %0x9e  %f64
    ==
  ::
      sqrt-opcodes
    :-  %sqrt
    ?-  op
      %0x91  %f32
      %0x9f  %f64
    ==
  ::
      min-opcodes
    :-  %min
    ?-  op
      %0x96  %f32
      %0xa4  %f64
    ==
  ::
      max-opcodes
    :-  %max
    ?-  op
      %0x97  %f32
      %0xa5  %f64
    ==
  ::
      ::  copysign-opcodes
      extend-opcodes
    :-  %extend
    ?+  op  !!
      %0xac  [%i64 %32 %s]
      %0xad  [%i64 %32 %u]
      %0xc0  [%i32 %8 %s]
    ==
  ::
      convert-opcodes
     :-  %convert
     ?+  op  !!
       %0xba  [%f64 %i64 %u]
     ==
      reinterpret-opcodes
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
  ?&  ?=(bin-opcodes-one-arg op)
      !(~(has in (silt mask-one-64)) op)
      !=(op '\43')
      !=(op '\41')
  ==
::
++  handle-one-arg-i32
  |=  [op=char arg=@]
  ^-  instruction
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
  ?&  ?=(bin-opcodes-two-args op)
      !=(op '\0e')
  ==
::
++  handle-two-args-i32
  |=  [op=char arg1=@ arg2=@]
  ^-  instruction
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
  ^-  instruction
  ?>  ?=(%0xe op)
  [%br-table vec i]
::
++  handle-block
  |=  [blocktype-index=@ body=(list instruction)]
  ^-  instruction
  :-  %block
  :_  body
  ?:  ?=(%0x40 blocktype-index)  ~
  ~[(get-valtype blocktype-index)]
::
++  handle-loop
  |=  [blocktype-index=@ body=(list instruction)]
  ^-  instruction
  [%loop ~ body]
::
++  handle-if
  |=  [blocktype-index=@ body=(list instruction)]
  ^-  instruction
  :-  %if
  :_  [body ~]
  ?:  ?=(%0x40 blocktype-index)  ~
  ~[(get-valtype blocktype-index)]
::
++  handle-if-else
  |=  $:  blocktype-index=@
          body-true=(list instruction)
          body-false=(list instruction)
      ==
  ^-  instruction
  :-  %if
  :_  [body-true body-false]
  ?:  ?=(%0x40 blocktype-index)  ~
  ~[(get-valtype blocktype-index)]
::
++  handle-const-f64
  |=  [op=char i=@rd]
  ^-  instruction
  [%const %f64 i]
::
++  handle-const-f32
  |=  [op=char i=@rs]
  ^-  instruction
  [%const %f32 i]
::
++  handle-const-i32
  |=  [op=char i=@s]
  ^-  instruction
  =;  i-unsigned=@
    [%const %i32 i-unsigned]
  =,  si
  ?:  (syn i)
    +:(old i)
  (sub (bex 32) +:(old i))
  
::
++  handle-const-i64
  |=  [op=char i=@s]
  ^-  instruction
  =;  i-unsigned=@
    [%const %i64 i-unsigned]
  =,  si
  ?:  (syn i)
    +:(old i)
  (sub (bex 64) +:(old i))
  
::
--