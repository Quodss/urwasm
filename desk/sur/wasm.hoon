::  urwasm AST definition and binary opcode classification
::
::    The first part mirrors Structure chapter of WebAssembly Core Specification
::    https://www.w3.org/TR/2022/WD-wasm-core-2-20220419/syntax/index.html.
::    The second part is based on binary format specification.
::
::::  /hoon/wasm/sur
  ::
|%
::  Types
::
+$  num-type  ?(%i32 %i64 %f32 %f64)
+$  vec-type  %v128
+$  ref-type  ?(%extn %func)  ::  externref and funcref
+$  valtype
  $~  %i32
  $?  num-type
      vec-type
      ref-type
  ==
::
::  $coin-wasm: type-annotated value
::
+$  coin-wasm
  $~  [%i32 *@]
  $%  [num-type @]
      [vec-type @]
      [%ref $%([%null ref-type] [%func @])]  ::  add external referenecs
  ==
::
+$  limits
  $%  [%flor p=@]      ::  min only
      [%ceil p=@ q=@]  ::  min and max
  ==
::
::  Instructions
::
+$  memarg
  $+  memarg
  [align=@ offset=@]
::
+$  instruction
  $%
    ::  Control instructions
    ::
    [%unreachable ~]
    [%nop ~]
    [%block result-type=(list valtype) body=expression]  ::  XX update type field of nesting instructions
    [%loop result-type=(list valtype) body=expression]
    $:  %if
        result-type=(list valtype)
        branch-true=expression
        branch-false=expression
    ==
  ::
    [%br label=@]
    [%br-if label=@]
    [%br-table label-vec=(list @) label-default=@]
    [%return ~]
    [%call func-id=@]
    [%call-indirect type-id=@ table-id=%0x0]
    :: [%end ~]   ::  %end and %else should be removed, since nesting is expressed
    :: [%else ~]  ::  with noun structure 
    ::  Parametric instructions
    ::
    [%drop ~]
    [%select ~]
    ::  Variable instructions
    ::
    [%local-get index=@]
    [%local-set index=@]
    [%local-tee index=@]
    [%global-tee index=@]
    [%global-get index=@]
    [%global-set index=@]
    ::  Memory instructions
    ::
    $:  %load
        type=valtype
        m=memarg
        n=(unit ?(%8 %16 %32))
        mode=(unit ?(%s %u))
    ==
    ::
    $:  %store
        type=valtype
        m=memarg
        n=(unit ?(%8 %16 %32))
    ==
    ::
    [%memory-size mem-id=%0x0]
    [%memory-grow mem-id=%0x0]
    ::  Numeric instructions
    ::
    [%const p=coin-wasm]
    ::::  comparison:
    ::::
    [%eqz type=?(%i32 %i64)]
    [%eq type=valtype]
    [%ne type=valtype]
    [%lt type=valtype mode=(unit ?(%s %u))]
    [%gt type=valtype mode=(unit ?(%s %u))]
    [%le type=valtype mode=(unit ?(%s %u))]
    [%ge type=valtype mode=(unit ?(%s %u))]
    ::::  arithmetics:
    ::::
    [%clz type=?(%i32 %i64)]
    [%ctz type=?(%i32 %i64)]
    [%popcnt type=?(%i32 %i64)]
    [%add type=valtype]
    [%sub type=valtype]
    [%mul type=valtype]
    [%div type=valtype mode=(unit ?(%s %u))]
    [%rem type=?(%i32 %i64) mode=?(%s %u)]
    [%and type=?(%i32 %i64)]
    [%or type=?(%i32 %i64)]
    [%xor type=?(%i32 %i64)]
    [%shl type=?(%i32 %i64)]
    [%shr type=?(%i32 %i64) mode=?(%s %u)]
    [%rotl type=?(%i32 %i64)]
    [%rotr type=?(%i32 %i64)]
    [%abs type=?(%f32 %f64)]
    [%neg type=?(%f32 %f64)]
    [%ceil type=?(%f32 %f64)]
    [%floor type=?(%f32 %f64)]
    ::
    $:  %trunc
        type=valtype
        source-type=(unit ?(%f32 %f64))
        mode=(unit ?(%s %u))
    ==
    ::
    [%nearest type=?(%f32 %f64)]
    [%sqrt type=?(%f32 %f64)]
    [%min type=?(%f32 %f64)]
    [%max type=?(%f32 %f64)]
    [%copysign type=?(%f32 %f64)]
    [%wrap ~]
    [%extend type=?(%i32 %i64) source=?(%8 %16 %32) mode=?(%s %u)]
    [%convert type=?(%f32 %f64) source-type=?(%i32 %i64) mode=?(%s %u)]
    [%demote ~]
    [%promote ~]
    [%reinterpret type=valtype source-type=valtype]
    ::  XX add SIMD stuff, check completeness
  ==  ::  $instruction
::
+$  expression  (list instruction)
::
::  Modules
::
::  $module: represents `module` type from WebAssembly specification,
::  but in a format closer to the binary representation, e.g. separate
::  code and function sections. This is done to simplify parsing.
::
+$  module
  $:
    =type-section
    =import-section
    =function-section
    =table-section
    =memory-section
    =global-section
    =export-section
    =start-section
    =elem-section
    =datacnt-section
    =code-section
    =data-section
  ==
::
::  Definitions of sections
::
::  Type section
::
+$  type-section
  $+  type-section
  (list func-type)
::
+$  func-type
  $:  params=(list valtype)
      results=(list valtype)
  ==
::  Import section
::
+$  import-section
  $+  import-section
  (list import)
::
+$  import
  $:  mod=tape
      name=tape
      $=  desc
      $%
        [%func id=@]
        [%tabl table]
        [%memo l=limits]
        [%glob v=valtype m=?(%con %var)]  ::  constant or variable
  ==  ==
::  Function section
::
+$  function-section
  $+  function-section
  (list type-id=@)
::
::  Table section
::
+$  table-section  (list table)
+$  table  (pair ref-type limits)
::
::  Memory section
::
+$  memory-section  (list limits)
::
::  Global section
::
+$  global-section  (list global)
::  valtype, mutability and init value.
::  We use a single constant instruction as opposed to a
::  (list instruction) since there is no global value type
::  that would take multiple constant values
::
+$  global
  $:  v=valtype
      m=?(%con %var)
      i=$>(?(%const %global-get) instruction)
  ==
::
::  Export section
::
+$  export-section
  $+  export-section
  (list export)
::
+$  export  [name=tape =export-desc]
::
+$  export-desc
  $%  [%func i=@]
      [%tabl i=@]
      [%memo i=@]
      [%glob i=@]
  ==
::
::  Start section
::
+$  start-section  (unit @)
::
::  Element section
::
+$  elem-section  (list elem)
::  The constant instructions are going to contain
::  %ref reference constants. The offset in %acti
::  active mode `off` and an element of `i` list 
::  of expressions are a single instruction because
::  they yield a single value
::
+$  elem
  $:  t=ref-type
      i=(list $>(%const instruction))
      $=  m
      $%  [%pass ~]
          [%decl ~]
          [%acti tab=@ off=$>(%const instruction)]
  ==  ==
::
::  Code section
::
+$  code-section
  $+  code-section
  (list code)
::
+$  code
  $:  locals=(list valtype)
      =expression
  ==
::
::  Data section
::
+$  data-section  (list data)
::  memid is implied to be 0
::
+$  data
  $%
    [%acti off=$>(%const instruction) b=octs]
    [%pass b=octs]
  ==
::
::  Data count section
::
++  datacnt-section  (unit @)
::
::  Binary opcode classification
::
+$  opcode  $?  bin-opcodes-zero-args
                bin-opcodes-one-arg
                bin-opcodes-two-args
                bin-opcodes-blocks
                pseudo-opcode
            ==
::
+$  bin-opcodes-zero-args
  $?
::  trap  nop   return  drop   select  wrap  demote  promote
    %0x0  %0x1  %0xf    %0x1a  %0x1b  %0xa7  %0xb6   %0xbb
::    
    eqz-opcodes  eq-opcodes  ne-opcodes  lt-opcodes  gt-opcodes  le-opcodes
    ge-opcodes  clz-opcodes  ctz-opcodes  popcnt-opcodes  add-opcodes
    sub-opcodes  mul-opcodes  div-opcodes  rem-opcodes  and-opcodes  or-opcodes
    xor-opcodes  shl-opcodes  shr-opcodes  rotl-opcodes  rotr-opcodes
    abs-opcodes  neg-opcodes  ceil-opcodes  floor-opcodes  trunc-opcodes
    nearest-opcodes  sqrt-opcodes  min-opcodes  max-opcodes  copysign-opcodes
    extend-opcodes  convert-opcodes  reinterpret-opcodes
  ==
::
+$  pseudo-opcode  ?(%0x5 %0xb)  ::  else, end
+$  bin-opcodes-one-arg
  $?
::  br    br_if  call  local.get  local.set  local.tee  global.get  global.set  
    %0xc  %0xd   %0x10  %0x20     %0x21      %0x22      %0x23       %0x24
::
    const-opcodes
    %0x3f  ::  memory.size
    %0x40  ::  memory.grow
  ==
::
+$  bin-opcodes-two-args
  $?
    %0xe   ::  br_table
    %0x11  ::  call_indirect
    load-opcodes
    store-opcodes
  ==
::
+$  bin-opcodes-blocks
  $?
    %0x2  ::  block
    %0x3  ::  loop
    %0x4  ::  if
  ==
::
+$  const-opcodes
  $?
    %0x41  ::  i32
    %0x42  ::  i64
    %0x43  ::  f32
    %0x44  ::  f64
  ==
::
+$  load-opcodes
  $?
    %0x28  ::  i32
    %0x29  ::  i64
    %0x2a  ::  f32
    %0x2b  ::  f64
    %0x2c  ::  i32 8 s
    %0x2d  ::  i32 8 u
    %0x2e  ::  i32 16 s
    %0x2f  ::  i32 16 u
    %0x30  ::  i64 8 s
    %0x31  ::  i64 8 u
    %0x32  ::  i64 16 s
    %0x33  ::  i64 16 u
    %0x34  ::  i64 32 s
    %0x35  ::  i64 32 u
  ==
::
+$  store-opcodes
  $?
    %0x36  ::  i32
    %0x37  ::  i64
    %0x38  ::  f32
    %0x39  ::  f64
    %0x3a  ::  i32 8
    %0x3b  ::  i32 16
    %0x3c  ::  i64 8
    %0x3d  ::  i64 16
    %0x3e  ::  i64 32
  ==
::
+$  eqz-opcodes  ?(%0x45 %0x50)              ::  i32, i64
+$  eq-opcodes   ?(%0x46 %0x51 %0x5b %0x61)  ::  i32, i64, f32, f64
+$  ne-opcodes   ?(%0x47 %0x52 %0x5c %0x62)  ::  i32, i64, f32, f64
+$  lt-opcodes
  $?
    %0x48  ::  i32 s
    %0x49  ::  i32 u
    %0x53  ::  i64 s
    %0x54  ::  i64 u
    %0x5d  ::  f32
    %0x63  ::  f64
  ==
::
+$  gt-opcodes
  $?
    %0x4a  ::  i32 s
    %0x4b  ::  i32 u
    %0x55  ::  i64 s
    %0x56  ::  i64 u
    %0x5e  ::  f32
    %0x64  ::  f64
  ==
::
+$  le-opcodes
  $?
    %0x4c  ::  i32 s
    %0x4d  ::  i32 u
    %0x57  ::  i64 s
    %0x58  ::  i64 u
    %0x5f  ::  f32
    %0x65  ::  f64
  ==
::
+$  ge-opcodes
  $?
    %0x4e  ::  i32 s
    %0x4f  ::  i32 u
    %0x59  ::  i64 s
    %0x5a  ::  i64 u
    %0x60  ::  f32
    %0x66  ::  f64
  ==
::
+$  clz-opcodes  ?(%0x67 %0x79)              ::  i32, i64
+$  ctz-opcodes  ?(%0x68 %0x7a)              ::  i32, i64
+$  popcnt-opcodes  ?(%0x69 %0x7b)           ::  i32, i64
+$  add-opcodes  ?(%0x6a %0x7c %0x92 %0xa0)  ::  i32, i64, f32, f64
+$  sub-opcodes  ?(%0x6b %0x7d %0x93 %0xa1)  ::  i32, i64, f32, f64
+$  mul-opcodes  ?(%0x6c %0x7e %0x94 %0xa2)  ::  i32, i64, f32, f64
+$  div-opcodes
  $?
    %0x6d  ::  i32 s
    %0x6e  ::  i32 u
    %0x7f  ::  i64 s
    %0x80  ::  i64 u
    %0x95  ::  f32
    %0xa3  ::  f64
  ==
::
+$  rem-opcodes
  $?
    %0x6f  ::  i32 s
    %0x70  ::  i32 u
    %0x81  ::  i64 s
    %0x82  ::  i64 u
  ==
::
+$  and-opcodes  ?(%0x71 %0x83)  ::  i32, i64
+$  or-opcodes   ?(%0x72 %0x84)  ::  i32, i64
+$  xor-opcodes  ?(%0x73 %0x85)  ::  i32, i64
+$  shl-opcodes  ?(%0x74 %0x86)  ::  i32, i64
+$  shr-opcodes
  $?
    %0x75  ::  i32 s
    %0x76  ::  i32 u
    %0x87  ::  i64 s
    %0x88  ::  i64 u
  ==
::
+$  rotl-opcodes   ?(%0x77 %0x89)  ::  i32, i64
+$  rotr-opcodes   ?(%0x78 %0x8a)  ::  i32, i64
+$  abs-opcodes    ?(%0x8b %0x99)  ::  f32, f64
+$  neg-opcodes    ?(%0x8c %0x9a)  ::  f32, f64
+$  ceil-opcodes   ?(%0x8d %0x9b)  ::  f32, f64
+$  floor-opcodes  ?(%0x8e %0x9c)  ::  f32, f64
+$  trunc-opcodes
  $?
    %0x8f  ::  f32
    %0x9d  ::  f64
    %0xa8  ::  f32 -> i32 s
    %0xa9  ::  f32 -> i32 u
    %0xaa  ::  f64 -> i32 s
    %0xab  ::  f64 -> i32 u
    %0xae  ::  f32 -> i64 s
    %0xaf  ::  f32 -> i64 u
    %0xb0  ::  f64 -> i64 s
    %0xb1  ::  f64 -> i64 u
  ==
::
+$  nearest-opcodes   ?(%0x90 %0x9e)  ::  f32, f64
+$  sqrt-opcodes      ?(%0x91 %0x9f)  ::  f32, f64
+$  min-opcodes       ?(%0x96 %0xa4)  ::  f32, f64
+$  max-opcodes       ?(%0x97 %0xa5)  ::  f32, f64
+$  copysign-opcodes  ?(%0x98 %0xa6)  ::  f32, f64
+$  extend-opcodes
  $?
    %0xac  ::  i32 -> i64 s
    %0xad  ::  i32 -> i64 u
    %0xc0  ::  i8  -> i32 s
    %0xc1  ::  i16 -> i32 s
    %0xc2  ::  i8  -> i64 s
    %0xc3  ::  i16 -> i64 s
    %0xc4  ::  i32 -> i64 s  ??  same as 0xac???
  ==
+$  convert-opcodes
  $?
    %0xb2  ::  i32 s -> f32
    %0xb3  ::  i32 u -> f32
    %0xb4  ::  i64 s -> f32
    %0xb5  ::  i64 u -> f32
    %0xb7  ::  i32 s -> f64
    %0xb8  ::  i32 u -> f64
    %0xb9  ::  i64 s -> f64
    %0xba  ::  i64 u -> f64
  ==
::
+$  reinterpret-opcodes
  $?
    %0xbc  ::  f32 -> i32
    %0xbd  ::  f64 -> i64
    %0xbe  ::  i32 -> f32
    %0xbf  ::  i64 -> f64
  ==
--