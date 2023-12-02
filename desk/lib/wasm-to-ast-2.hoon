::  WebAssembly parser
::
/-  sur=wasm
/+  handle=handle-operators
|%
::
::  ++section-ord: gate for comparing section id order correctness.
::  id=0 (custom section) can be anywhere, rest of the sections are
::  ordered as ~[1 2 3 4 5 6 7 8 9 12 10 11]
::
++  section-ord
  |=  [a=@ b=@]
  ^-  ?
  ?:  |(=(a 0) =(b 0))  %.y
  ?.  |(=(a 12) =(b 12))
    (gth b a)
  ?.  =(b 12)
    |(=(b 10) =(b 11))
  ?:  =(a 12)  %.n
  !$(a b, b a)
::  ++main: parsing function. Extends octstream with
::  leading zeros, then applies ++module:r parsing rule.
::
++  main
  =,  sur
  |=  wasm=octs
  =|  out=module
  =/  bytes=tape  (trip q.wasm)
  =.  bytes  %+  weld  bytes
             ;;  tape
             (reap (sub p.wasm (lent bytes)) '\00')
  ::  Wasm binary magic & binary version
  ::
  ?>  =((scag 8 bytes) "\00asm\01\00\00\00")
  =/  section-bytes=(list tape)  (scan (slag 8 bytes) section-bytes:r)
  =+  last-id=0
  |-  ^-  module
  ?~  section-bytes  out
  ?>  ?=(^ i.section-bytes)
  ?>  (section-ord last-id i.i.section-bytes)
  =.  out
    ?+  i.i.section-bytes  ~|(%unrecognized-section-code !!)
      %0   out
      %1   out(type-section (scan t.i.section-bytes type-section:r))
      %2   out(import-section (scan t.i.section-bytes import-section:r))
      %3   out(function-section (scan t.i.section-bytes function-section:r))
      %4   out(table-section (scan t.i.section-bytes table-section:r))
      %5   out(memory-section (scan t.i.section-bytes memory-section:r))
      %6   out(global-section (scan t.i.section-bytes global-section:r))
      %7   out(export-section (scan t.i.section-bytes export-section:r))
      %8   out(start-section (scan t.i.section-bytes start-section:r))
      %9   out(elem-section (scan t.i.section-bytes elem-section:r))
      %12  out(datacnt-section (scan t.i.section-bytes datacnt-section:r))
      %10  out(code-section (scan t.i.section-bytes code-section:r))
      %11  out(data-section (scan t.i.section-bytes data-section:r))
    ==
  $(last-id i.i.section-bytes, section-bytes t.section-bytes)
  ::
  ::  |r: core with parsing rules
  ::
++  r
  |%
  ::  ++bild: connexts an edge with a rule and a rule-
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
  ::  ++vec: parse .wasm vector of rule
  ::
  ++  vec
    |*  rul=rule
    ;~  bild
      u32
      |=  n=@
      (stun [n n] rul)
    ==
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
  ::
  ++  func-type
    %+  cook  func-type:sur
    ;~  pfix
      (just '\60')
      ;~(plug (vec valtype) (vec valtype))
    ==
  ::
  ++  valtype
    %+  cook  valtype:sur
    ;~(pose num-type vec-type ref-type)
  ::
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
  ++  ref-type
    %+  cook  ref-type:sur
    ;~  pose
      (cold %extn (just '\6f'))
      (cold %func (just '\70'))
    ==
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
  ++  u32  (u-n 32)
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
  ::
  ++  limits
    %+  cook  limits:sur
    ;~  pose
      ;~(plug (cold %flor (just '\00')) u32)
      ;~(plug (cold %ceil (just '\01')) u32 u32)
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
      instruction
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
  ::
  ++  start-section
    %+  cook  start-section:sur
    u32
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
      ;~(plug expression-end (vec u32))
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
      ;~(plug u32 expression-end elem-kind (vec u32))
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
      ;~(plug expression-end (vec expression-end))
    ==
  ::
  ++  elem-5
    %+  cook  handle-elem-5
    ;~  pfix  (just '\05')
      ;~(plug ref-type (vec expression-end))
    ==
  ::
  ++  elem-6
    %+  cook  handle-elem-6
    ;~  pfix  (just '\06')
      ;~(plug u32 expression-end ref-type (vec expression-end))
    ==
  ::
  ++  elem-7  
    %+  cook  handle-elem-7
    ;~  pfix  (just '\07')
      ;~(plug ref-type (vec expression-end))
    ==
  ::
  ++  code-section   !!
  ++  data-section   !!
  ++  datcnt-section  !!
  --  :: |r
--