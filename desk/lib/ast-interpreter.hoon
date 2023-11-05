::  Web Assembly AST interpreter
::
::::  /hoon/ast-interpreter/lib
  ::
/-  *wasm
/+  handle=handle-operators
|%
::  +make-export-map: turns export-section into a map [name=@t =export-desc]
::
++  make-export-map
  |=  =export-section
  =|  out=(map @t export-desc)
  |-  ^-  (map @t export-desc)
  ?~  export-section  out
  =,  i.export-section
  %=  $
    out  (~(put by out) name export-desc)
    export-section  t.export-section
  ==
::  +find-func-id: find func-id from a name of an exported function
::
++  find-func-id
  |=  [name=@t =module]
  ^-  @
  =,  module
  =/  =export-desc  (~(got by (make-export-map export-section)) name)
  ?>  ?=(%func -.export-desc)
  i.export-desc
::  +get-types: turn a (list coin-wasm) into a list of types of coins
::
++  get-types
  |=  a=(list coin-wasm)
  ^-  (list valtype)
  ?~  a  ~
  [type.i.a $(a t.a)]
::  +mint:  reverse of +get-types
::
++  mint
  |=  a=(list valtype)
  ^-  (list coin-wasm)
  ?~  a  ~
  :_  $(a t.a)
  ^-  coin-wasm
  ?-  i.a
    %i32  [i.a *@]
    %i64  [i.a *@]
    %f32  [i.a *@rs]
    %f64  [i.a *@rd]
  ==
::  +instantiate: prepare module
::
++  instantiate
  |=  =module
  =,  module
  ::  This is where globally available structures are defined: globals, memory,
  ::  imports, start function application etc. Returns core for module handling
  ::
  =+  :*  buffer=0
          n-pages=`@`?:(?=(@ memory-section.module) 0 min.i.memory-section.module)
          max-pages=`(unit @)`?:(?=(@ memory-section.module) `0 max.i.memory-section.module)
          table=`(list @)`~  ::  XX mayb incorrect? deal with this later
          globals=`(list coin-wasm)`~
      ==
  =*  state-global  -
  ::  instantiate table
  ::
  =?  table  ?=(^ elem-section.module)
    ?>  ?=(@ t.elem-section.module)  ::  quick hack for an elem-section w/ one element
    =*  elem  i.elem-section.module
    ?>  ?=([[%const [%i32 @]] ~] offset.elem)  ::  XX the DIRTIEST hack, to be fixed
    (weld (reap n.p.i.offset.elem 0) y.elem)
  ::  instantiate globals
  ::
  =?  globals  ?=(^ global-section.module)
    ?>  ?=(@ t.global-section.module)  ::  quick hack for a global-section w/ one element
    =*  global  i.global-section.module
    ?>  ?=([[%const [%i32 @]] ~] e.global)  ::  XX the DIRTIEST hack, to be fixed
    ~[[%i32 n.p.i.e.global]]
  ::  load data
  ::
  =?  buffer  ?=(^ data-section.module)
    |-  ^-  @
    =*  data  i.data-section.module
    ?>  ?=(%active -.data)
    ?>  ?=([[%const [%i32 @]] ~] offset.data)  ::  XX the DIRTIEST hack, to be fixed
    =.  buffer  (sew bloq=3 [n.p.i.offset.data size=len.b.data array.b.data] buffer)
    ?:  ?=(@ t.data-section.module)  buffer
    $(data-section.module t.data-section.module)
  ::  call-trace for debugging
  ::
  =+  call-trace=`(list @)`~
  ::
  |%
  ++  this  .
  ++  call-id
    |=  [func-id=@ input-params=(list coin-wasm)]
    ^-  [(list coin-wasm) _this]
    :: ~&  [func-id input-params]
    =.  call-trace  [func-id call-trace]
    :: ~&  [(flop call-trace) input-params]
    =/  =func-type  (snag (snag func-id function-section) type-section)
    =/  =code  (snag func-id code-section)
    =/  expression  expression.code
    ::  Type check:
    ::
    ?>  =(params.func-type (get-types input-params))
    ::  Initialize locals:
    ::
    =/  locals=(list coin-wasm)  (weld input-params (mint locals.code))
    :: =/  out=stack  return:reduce:(hwasm-instance func-type locals expression)
    =/  reduced  reduce:(hwasm-instance func-type locals expression)
    =/  out=stack  return:reduced
    ::  Assert: no branch or branch with label 0 or return
    ::
    ?>  |(?=(~ p.out) ?=([%return ~] u.p.out) =(0 i.u.p.out))
    ::  Type check
    ::
    :: This doesn't work, there are left-over operands on the stack. Fix later with validation
    ::
    ?.  =((get-types q.out) results.func-type)
      ~|  [(get-types q.out) results.func-type]
      ~|  q.out
      ~|  "in func-id={<func-id>}"
      !!
    =.  call-trace  +.call-trace
    :: ~&  "end {<func-id>}"
    [q.out this(state-global state-global:reduced)]
    :: ~?  =(func-id 23)  q.out
    :: [(scag (lent results.func-type) q.out) this(state-global state-global:reduced)]
  ::  +hwasm-instance: core for expression computation
  ::
  ++  hwasm-instance
    |=  $:  expr-type=func-type
            locals=(list coin-wasm)
            expression=(list instruction)
        ==
    =|  s=stack
    |%
    ++  this  .
    ++  return
      ^-  stack
      s
    ::
    ++  reduce
      |-  ^+  this
      ::  terminate when no instructions left or navigating br
      ::
      ?:  |(=(~ expression) !=(p.s ~))  this  ::  TMI
      ::  else, apply instruction
      ::
      =.  this  (apply-instruction (snag 0 expression))
      $(expression (slag 1 expression))
    ::
    ++  apply-instruction
      |=  =instruction
      ^+  this
      !.
      ?+  instruction  ~|("missing instruction: {<instruction>}" !!)
      ::
          [%unreachable ~]
        ~|  "Trap"
        !!
      ::
          [%select ~]
        ?>  ?=([which=coin-wasm val2=coin-wasm val1=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        ?>  =(%i32 type.which)
        this(q.s [?:(!=(0 n.which) val1 val2) rest])
      ::
          [%drop ~]
        ?>  ?=([coin-wasm rest=*] q.s)
        =,  q.s
        this(q.s rest)
      ::
          [%global-get index=@]
        =,  instruction
        this(q.s [(snag index globals) q.s])
      ::
          [%global-set index=@]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        ?>  =(type.a type:(snag index globals))
        this(q.s rest, globals (snap globals index a))  ::  mutability check should be done at validation, and the typecheck moved there too
      ::
          [%local-get index=@]
        =,  instruction
        this(q.s [(snag index locals) q.s])
      ::
          [%local-set index=@]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        ?>  =(type.a type:(snag index locals))
        this(q.s rest, locals (snap locals index a))
      ::
          [%local-tee index=@]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        ?>  =(type.a type:(snag index locals))
        this(locals (snap locals index a))
      ::  ATTENTION: operands are inversed
      ::
          [%add type=valtype]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(add:handle type a b) rest])
      ::
          [%ne type=valtype]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(ne:handle type a b) rest])
      ::
          [%call func-id=@]
        =,  instruction
        =/  subfunc-type=func-type
          (snag (snag func-id function-section) type-section)
        =/  input-values=(list coin-wasm)
          %-  flop  ::  reversed order of operands in the stack
          (scag (lent params.subfunc-type) q.s)
        ?>  =((get-types input-values) params.subfunc-type)
        =+  [out instance]=(call-id func-id input-values)
        %=    this
            q.s
          %+  weld
            :: (scag (lent results.subfunc-type) (flop out))
            (flop out)
          (slag (lent params.subfunc-type) q.s)
        ::
          state-global  state-global:instance
        ==
      ::
          [%call-indirect type-id=@ table-id=%0x0]
        ?>  ?=([func-table-index=coin-wasm rest=*] q.s)
        =,  instruction
        =,  q.s
        =/  subfunc-type=func-type
          (snag type-id type-section)
        =/  input-values=(list coin-wasm)
          %-  flop  ::  reversed order of operands in the stack
          (scag (lent params.subfunc-type) rest)
        ?>  =((get-types input-values) params.subfunc-type)
        =+  [out instance]=(call-id (snag n.func-table-index table) input-values)
        %=    this
            q.s
          %+  weld
            :: (scag (lent results.subfunc-type) (flop out))
            (flop out)
          (slag (lent params.subfunc-type) rest)
        ::
          state-global  state-global:instance
        ==
      ::
          [%const p=coin-wasm]
        =,  instruction
        this(q.s [p q.s])
      ::
          [%lt type=valtype mode=(unit ?(%s %u))]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        =+  result=(lt:handle type mode a b)
        :: ~&  [a b result]
        this(q.s [result rest])
      ::
          [%gt type=valtype mode=(unit ?(%s %u))]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(gt:handle type mode a b) rest])
      ::
          [%ge type=valtype mode=(unit ?(%s %u))]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(ge:handle type mode a b) rest])
      ::
          [%le type=valtype mode=(unit ?(%s %u))]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(le:handle type mode a b) rest])
      ::
          [%shr type=?(%i32 %i64) mode=?(%s %u)]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(shr:handle type mode a b) rest])
      ::
          [%shl type=?(%i32 %i64)]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(shl:handle type a b) rest])
      ::
          [%if *]
        ?>  ?=([a=[type=%i32 n=@] rest=*] q.s)
        =,  q.s
        =,  instruction
        ?>  =(%i32 type.a)
        =/  if-instance
          ?.  =(n.a 0)
            reduce:this(s *stack, expression branch-true)
          reduce:this(s *stack, expression branch-false)
        =/  br=(unit branch)
          ?~  p.s.if-instance  ~
          ?:  ?=([%return ~] u.p.s.if-instance)  `[%return ~]
          ?:  =(0 i.u.p.s.if-instance)  ~
          `[%target (dec i.u.p.s.if-instance)]
        =?  q.s.if-instance  =(~ br)
          =+  out=(scag (lent result-type) q.s.if-instance)
          ?>  =(result-type (get-types out))
          out
        :: ~?  &(=(~ br) !=(~ q.s.if-instance))
          :: q.s.if-instance
        %=  this
          p.s     br
          q.s     (weld q.s.if-instance rest)
          locals  locals.if-instance
          state-global  state-global.if-instance
        ==
      ::
          [%sub type=valtype]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(sub:handle type a b) rest])
      ::
          [%mul type=valtype]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(mul:handle type a b) rest])
      ::
          [%div type=valtype mode=(unit ?(%s %u))]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        =+  result=(div:handle type mode a b)
        this(q.s [result rest])
      ::
          [%or type=?(%i32 %i64)]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(or:handle type a b) rest])
      ::
          [%xor type=?(%i32 %i64)]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(xor:handle type a b) rest])
      ::
          [%rotl type=?(%i32 %i64)]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(rotl:handle type a b) rest])
      ::
          [%and type=?(%i32 %i64)]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(and:handle type a b) rest])
      ::
          [%eq type=valtype]
        ?>  ?=([b=coin-wasm a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(eq:handle type a b) rest])
      ::
          [%block *]  ::  [%block result-type=(list valtype) body=(list instruction)]
        =,  instruction
        =/  block-instance  reduce:this(s *stack, expression body)
        =/  br=(unit branch)
          ?~  p.s.block-instance  ~
          ?:  ?=([%return ~] u.p.s.block-instance)  `[%return ~]
          ?:  =(0 i.u.p.s.block-instance)  ~
          `[%target (dec i.u.p.s.block-instance)]
        =?  q.s.block-instance  =(~ br)
          =+  out=(scag (lent result-type) q.s.block-instance)
          ?>  =(result-type (get-types out))
          out
        %=  this
          p.s     br
          q.s     (weld q.s.block-instance q.s)
          locals  locals.block-instance
          state-global  state-global.block-instance
        ==
      ::
          [%return ~]
        this(p.s `[%return ~])
      ::
          [%popcnt type=?(%i32 %i64)]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(popcnt:handle type a) rest])
      ::
          [%ctz type=?(%i32 %i64)]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(ctz:handle type a) rest])
      ::
          [%wrap ~]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        ?>  =(%i64 type.a)
        this(q.s [[%i32 (mod n.a ^~((bex 32)))] rest])
      ::
          [%extend type=?(%i32 %i64) source=?(%8 %16 %32) mode=?(%s %u)]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(extend:handle type source mode a) rest])
      ::
          [%eqz type=?(%i32 %i64)]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(eqz:handle type a) rest])
      ::
          [%clz type=?(%i32 %i64)]
        ?>  ?=([a=coin-wasm rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [(clz:handle type a) rest])
      ::
          [%br label=@]
        =,  instruction
        this(p.s `[%target label])
      ::
          [%br-if label=@]
        ?>  ?=([a=[type=%i32 n=@] rest=*] q.s)
        =,  instruction
        =,  q.s
        ?.  =(n.a 0)
          this(p.s `[%target label], q.s rest)
        this(q.s rest)
      ::
          [%br-table *]  ::  [%br-table label-vec=(list @) label-default=@]
        ?>  ?=([a=[type=%i32 n=@] rest=*] q.s)
        =,  instruction
        =,  q.s
        ?:  (gte n.a (lent label-vec))
          this(p.s `[%target label-default], q.s rest)
        this(p.s `[%target (snag n.a label-vec)], q.s rest)
      ::
          [%loop ~ body=*]
        =,  instruction
        |-  ^+  this
        =/  loop-instance  reduce:this(s *stack, expression body)
        ?:  ?&  ?=(^ p.s.loop-instance)
                ?=([%target @] u.p.s.loop-instance)
                =(0 i.u.p.s.loop-instance)
            ==
          %=  $
            locals        locals.loop-instance
            state-global  state-global.loop-instance
          ==
        =/  br=(unit branch)
          ?~  p.s.loop-instance  ~
          ?:  ?=([%return ~] u.p.s.loop-instance)  `[%return ~]
          `[%target (dec i.u.p.s.loop-instance)]
        %=  this
          p.s           br
          q.s           (weld q.s.loop-instance q.s)
          locals        locals.loop-instance
          state-global  state-global.loop-instance
        ==
      ::
          [%store %i32 *]
        ?>  ?=([content=[type=%i32 n=@] addr=[type=%i32 n=@] rest=*] q.s)
        =,  q.s
        =,  instruction
        =/  i=@  (add n.addr offset.m)
        :: ?>  (lth (add i 4) (mul 65.536 n-pages))  ::  let's not care about memory size for now
        =;  [size=@ to-put=@]
          this(q.s rest, buffer (sew bloq=3 [i size to-put] buffer))
        :: =-  ~&([instruction n.addr `@t`->] -)
        ?~  n.instruction
          [4 n.content]
        [(div u.n.instruction 8) (mod n.content (bex u.n.instruction))]
      ::
          [%store %i64 *]
        ?>  ?=([content=[type=%i64 n=@] addr=[type=%i32 n=@] rest=*] q.s)
        =,  q.s
        =,  instruction
        =/  i=@  (add n.addr offset.m)
        :: ?>  (lth (add i 8) (mul 65.536 n-pages))  ::  let's not care about memory size for now
        =;  [size=@ to-put=@]
          this(q.s rest, buffer (sew bloq=3 [i size to-put] buffer))
        ?~  n.instruction
          [8 n.content]
        [(div u.n.instruction 8) (mod n.content (bex u.n.instruction))]
      ::
          [%load %i32 *]
        ?>  ?=([addr=[type=%i32 n=@] rest=*] q.s)
        :: ~&  [instruction addr.q.s]
        =,  q.s
        =,  instruction
        =/  i=@  (add n.addr offset.m)
        :: ?>  (lth (add i 4) (mul 65.536 n-pages))
        =;  loaded=@
          this(q.s [[%i32 loaded] rest])
        :: =-  ~&([instruction n.addr `@t`-] -)
        ?~  n.instruction
          (cut 3 [i 4] buffer)
        ?>  ?=(^ mode.instruction)
        ?-    u.mode.instruction
            %u
          (cut 3 [i (div u.n.instruction 8)] buffer)
        ::
            %s
          %+  si-to-complement:handle  32
          %+  complement-to-si:handle  u.n.instruction
          (cut 3 [i (div u.n.instruction 8)] buffer)
        ==
      ::
          [%load %i64 *]
        ?>  ?=([addr=[type=%i32 n=@] rest=*] q.s)
        :: ~&  [instruction addr.q.s]
        =,  q.s
        =,  instruction
        =/  i=@  (add n.addr offset.m)
        :: ?>  (lth (add i 4) (mul 65.536 n-pages))
        =;  loaded=@
          this(q.s [[%i64 loaded] rest])
        ?~  n
          (cut 3 [i 8] buffer)
        ?>  ?=(^ mode.instruction)
        ?-    u.mode.instruction
            %u
          (cut 3 [i (div (need n.instruction) 8)] buffer)
        ::
            %s
          %+  si-to-complement:handle  64
          %+  complement-to-si:handle  (need n.instruction)
          (cut 3 [i (div (need n.instruction) 8)] buffer)
        ==
      ::
          [%memory-grow *]
        this(n-pages +(n-pages), q.s [[%i32 n-pages] q.s])
      ::
          [%convert *]  ::  [%convert type=?(%f32 %f64) source-type=?(%i32 %i64) mode=?(%s %u)]
        ?>  ?=(%f64 type.instruction)
        ?>  ?=(%i64 source-type.instruction)
        ?>  ?=(%u mode.instruction)
        ?>  ?=([a=[type=%i64 n=@] rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [[%f64 (sun:rd n.a)] rest])
      ::
          [%reinterpret *]  ::  [%convert type=?(%f32 %f64) source-type=?(%i32 %i64) mode=?(%s %u)]
        ?>  ?=(%f64 type.instruction)
        ?>  ?=(%i64 source-type.instruction)
        ?>  ?=([a=[type=%i64 n=@] rest=*] q.s)
        =,  q.s
        =,  instruction
        this(q.s [[%f64 ;;(@rd n.a)] rest])
      ==
    --
  --
::
--