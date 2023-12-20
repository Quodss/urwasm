::  Instruction definitions
::
::  This lib defines instructions as functions
::  local-state -> local-state, as well as exposes
::  the gates to the +apply arm of the interpreter engine.
::
::  To add a r-ary numerical instruction (an instruction
::  that only involves numerical stack values):
::    1. Register the instruction in respective r-ary-num
::    type;
::    2. Add an entry to the respective map m in r-ary:fetch
::    3. Define an arm for that instruction. The resulting
::    gate must take an instruction, producing a gate which
::    takes r vals and returns a single val to be pushed on
::    the stack
::
::  Addition of non-numerical instructions, i.e. instructions
::  that read and/or write to fields other than the stack or
::  involve references is slightly more general and requires 
::  writing a gate $-(local-state local-state). Look at
::  the examples in set:fetch
::
::::  /hoon/op-def/lib
  ::
/-  *engine
::
|%
++  complement-to-si
  |=  [base=@ n=@]
  ^-  @s
  =.  n  (mod n (bex base))
  =/  sign=?  (lth n (bex (dec base)))
  %+  new:si  sign
  ?:  sign  n
  (sub (bex base) n)
::
++  si-to-complement
  |=  [base=@ s=@s]
  ^-  @
  ?:  (syn:si s)
    +:(old:si s)
  (sub (bex base) +:(old:si s))
::
++  coin-to-val
  |=  c=coin-wasm
  ^-  val
  ?:  ?=(%ref -.c)
    c
  +.c
::
++  val-to-coin
  |=  [v=val eg=coin-wasm]
  ^-  coin-wasm
  ?@  v
    ?<  ?=(%ref -.eg)
    ;;  coin-wasm  ::  the compiler isn't convinced that the 
    [-.eg v]       ::  expression below is a coin-wasm, why? :(
  ?>  ?=(%ref -.eg)
  v
::
::  |kind: instruction classes
::
++  kind
  |%
  +$  nullary
    $?  %unreachable
        %return
        %memory-grow
        %drop
    ==
  ::
  +$  get  ?(%global-get %local-get)
  +$  set  ?(%global-set %local-set %global-tee %local-tee)
  +$  branch  ?(%br %br-if %br-table)
  +$  unary-num
    $?
      %popcnt  %ctz  %wrap  %extend
      %eqz  %clz  %convert  %reinterpret
    ==
  ::
  +$  binary-num
    $?
      %add  %ne  %lt  %gt  %ge  %le  %shr  %shl
      %sub  %mul  %div  %or  %xor  %rotl  %and
      %eq
    ==
  ::
  --
::  +fetch-gate: turn $instruction to a gate to transform
::  the local state
::
++  fetch-gate
  |=  i=$<(?(%call %loop %call-indirect %block %if) instruction)
  ^-  $-(local-state local-state)
  ?+    -.i  ~|("missing instruction: {<i>}" !!)
      nullary:kind  (null:fetch i)
      %load         (load:fetch i)
      %store        (store:fetch i)
      %const        (const:fetch i)
      get:kind      (get:fetch i)
      set:kind      (set:fetch i)
      branch:kind   (branch:fetch i)
      %select       select:fetch
  ::
      unary-num:kind
    |=  l=local-state
    ^-  local-state
    ?>  ?=([a=@ rest=*] va.stack.l)
    =,  va.stack.l
    l(va.stack [((unar:fetch i) a) rest])
  ::
      binary-num:kind
    |=  l=local-state
    ^-  local-state
    ?>  ?=([b=@ a=@ rest=*] va.stack.l)
    =,  va.stack.l
    l(va.stack [((bina:fetch i) a b) rest])
  ::
  ==
::
::  |fetch: core with instruction definitions
::
++  fetch
  |%
  ++  select
    |=  l=local-state
    ^-  local-state
    ?>  ?=([which=@ val2=val val1=val rest=*] va.stack.l)
    =,  va.stack.l
    %=    l
        va.stack
      [?.(=(0 which) val1 val2) rest]
    ==
  ++  null
    =-  |=  i=instruction
        (~(got by m) ;;(@tas -.i))
    ^~
    ^=  m
    ^-  (map @tas $-(local-state local-state))
    |^
    %-  my
    :~
      unreachable+unreachable
      return+return
      memory-grow+memory-grow
      drop+drop
    ==
    ::
    ++  unreachable
      |=  l=local-state
      ^-  local-state
      l(br.stack [%trap ~])
    ::
    ++  return
      |=  l=local-state
      ^-  local-state
      l(br.stack [%retr ~])
    ::
    ++  memory-grow
      |=  l=local-state
      ^-  local-state
      %=  l
        va.stack  [n-pages.mem.store.l va.stack.l]
        n-pages.mem.store  +(n-pages.mem.store.l)
      ==
    ::
    ++  drop
      |=  l=local-state
      ^-  local-state
      l(va.stack +.va.stack.l)
    --
  ::
  ++  load
    |=  i=instruction
    ?>  ?=(%load -.i)
    |=  l=local-state
    ^-  local-state
    ?>  ?=([addr=@ rest=*] va.stack.l)
    =,  va.stack.l
    =/  index=@  (add addr offset.m.i)
    =;  loaded=@
      l(va.stack [loaded rest])
    ?~  n.i
      ?+  type.i  !!
        %i32  (cut 3 [index 4] buffer.mem.store.l)
        %i64  (cut 3 [index 8] buffer.mem.store.l)
      ==
    ?>  ?=(^ mode.i)
    ?-    u.mode.i
        %u
      (cut 3 [index (div u.n.i 8)] buffer.mem.store.l)
    ::
        %s
      %+  si-to-complement  ?+(type.i !! %i32 32, %i64 64)
      %+  complement-to-si  u.n.i
      (cut 3 [index (div u.n.i 8)] buffer.mem.store.l)
    ==
  ::
  ++  store
    |=  i=instruction
    ?>  ?=(%store -.i)
    |=  l=local-state
    ^-  local-state
    ?>  ?=([content=@ addr=@ rest=*] va.stack.l)
    =,  va.stack.l
    =/  index=@  (add addr offset.m.i)
    =;  [size=@ to-put=@]
      %=    l
          va.stack  rest
          buffer.mem.store
        (sew bloq=3 [index size to-put] buffer.mem.store.l)
      ==
    ?~  n.i
      :_  content
      ?+(type.i !! %i32 4, %i64 8)
    [(div u.n.i 8) (mod content (bex u.n.i))]
  ::
  ++  const
    |=  i=instruction
    ?>  ?=(%const -.i)
    |=  l=local-state
    ^-  local-state
    l(va.stack [(coin-to-val p.i) va.stack.l])
  ::
  ++  get
    |=  i=instruction
    ?>  ?=(get:kind -.i)
    |=  l=local-state
    ^-  local-state
    =;  got=val
      l(va.stack [got va.stack.l])
    ?-  -.i
      %local-get   (snag index.i locals.l)
      %global-get  (coin-to-val (snag index.i globals.store.l))
    ==
  ::
  ++  set
    |=  i=instruction
    ?>  ?=(set:kind -.i)
    ~!  -.i
    |=  l=local-state
    ^-  local-state
    ?>  ?=([a=val rest=*] va.stack.l)
    =,  va.stack.l
    ?-    -.i
        %local-set
      l(locals (snap locals.l index.i a), va.stack rest)
    ::
        %local-tee
      l(locals (snap locals.l index.i a))
        %global-set
      %=    l
          va.stack  rest
          globals.store
        ::  turn a to coin-wasm based on example in globals,
        ::  and replace the value in globals
        ::
        %^  snap  globals.store.l  index.i
        %+  val-to-coin  a
        (snag index.i globals.store.l)
      ==
    ::
        %global-tee
      %=    l
          globals.store
        ::  turn a to coin-wasm based on example in globals,
        ::  and replace the value in globals
        ::
        %^  snap  globals.store.l  index.i
        %+  val-to-coin  a
        (snag index.i globals.store.l)
      ==
    ==
  ::
  ++  branch
    |=  i=instruction
    ?>  ?=(branch:kind -.i)
    |=  l=local-state
    ^-  local-state
    ?-    -.i
        %br  l(br.stack [%targ label.i])
        %br-if
      ?>  ?=([a=@ rest=*] va.stack.l)
      =,  va.stack.l
      ?.  =(a 0)
        l(br.stack [%targ label.i], va.stack rest)
      l(va.stack rest)
    ::
        %br-table
      ?>  ?=([a=@ rest=*] va.stack.l)
      =,  va.stack.l
      ?:  (gte a (lent label-vec.i))
        l(va.stack rest, br.stack [%targ label-default.i])
      l(va.stack rest, br.stack [%targ (snag a label-vec.i)])
    ==
  ::  +unar: unary gate fetcher
  ::
  ++  unar
    =-  |=  i=instruction
        ?>  ?=(unary-num:kind -.i)
        ^-  $-(@ @)
        ~+
        ((~(got by m) ;;(@tas -.i)) i)
    ^~
    ^=  m
    ^-  (map @tas $-(instruction $-(@ @)))
    |^
    %-  my
    :~
      popcnt+popcnt  ctz+ctz  wrap+wrap
      extend+extend  eqz+eqz  clz+clz  convert+convert
      reinterpret+reinterpret
    ==
    ::
    ++  popcnt
      |=  *
      |=  v=@
      ^-  @
      =+  counter=0
      |-  ^-  @
      ?:  =(v 0)  counter
      $(v (div v 2), counter (add counter (mod v 2)))
    ::
    ++  ctz
      |=  i=instruction
      ?>  ?=(%ctz -.i)
      =/  counter-max=@  ?-(type.i %i32 32, %i64 64)
      |=  v=@
      ^-  @
      =+  counter=0
      |-  ^-  @
      ?:  =(counter counter-max)  counter
      ?:  =(1 (mod v 2))  counter
      $(v (div v 2), counter +(counter))
    ::
    ++  wrap
      |=  *
      |=  v=@
      ^-  @
      (mod v ^~((bex 32)))
    ::
    ++  extend
      |=  i=instruction
      ?>  ?=(%extend -.i)
      =/  base=@  ?-(type.i %i32 32, %i64 64)
      |=  v=@
      ^-  @
      ?-  mode.i
        %u  (mod v base)
        %s  (si-to-complement base (complement-to-si source.i v))
      ==
    ::
    ++  eqz
      |=  *
      |=  v=@
      ^-  @
      ?:(=(v 0) 1 0)
    ::
    ++  clz
      |=  i=instruction
      ?>  ?=(%clz -.i)
      =/  base=@  ?-(type.i %i32 32, %i64 64)
      |=  v=@
      (sub base (xeb v))
    ::
    ++  convert  ::  complete, check correctness
      |=  i=instruction
      ?>  ?=(%convert -.i)
      ?>  ?=(%f64 type.i)
      ?>  ?=(%i64 source-type.i)
      ?>  ?=(%u mode.i)
      |=  v=@
      ^-  @
      (sun:rd v)
    ::
    ++  reinterpret  ::  complete, check correctness
      |=  i=instruction
      ?>  ?=(%reinterpret -.i)
      ?>  ?=(%f64 type.i)
      ?>  ?=(%i64 source-type.i)
      |=  v=@
      ^-  @
      v
    ::
    --
  ::  +bina: binary gate fetcher.
  ::  Attention, arithmetic gates are shadowed here, but not elsewhere
  ::
  ++  bina
    =-  |=  i=instruction
        ?>  ?=(binary-num:kind -.i)
        ^-  $-([@ @] @)
        ~+
        ((~(got by m) ;;(@tas -.i)) i)
    ^~
    ^=  m
    ^-  (map @tas $-(instruction $-([@ @] @)))
    |^
    %-  my
    :~
      add+add  ne+ne  lt+lt  gt+gt  ge+ge  le+le  shr+shr  shl+shl
      sub+sub  mul+mul  div+div  or+or  xor+xor  rotl+rotl  and+and
      eq+eq
    ==
    ::
    ++  add
      |=  i=instruction
      ?>  ?=(%add -.i)
      =/  modul=@  ?+(type.i 0 %i32 (bex 32), %i64 (bex 64))
      |=  [v=@ w=@]
      ^-  @
      ?+  type.i  !!
        ?(%i32 %i64)  (~(sum fo modul) v w)
        %f32  (add:rs v w)
        %f64  (add:rd v w)
      ==
    ::
    ++  ne
      |=  *
      |=  [v=@ w=@]
      ^-  @
      ?:(!=(v w) 1 0)
    ::
    ++  lt
      |=  i=instruction
      ?>  ?=(%lt -.i)
      =/  mode=?(%s %u)  (fall mode.i %u)
      =/  negat=@  ?+(type.i 0 %i32 (bex 31), %i64 (bex 63))
      |=  [v=@ w=@]
      ^-  @
      ?+    type.i  !!
          %f32  ?:((lth:rs v w) 1 0)
          %f64  ?:((lth:rd v w) 1 0)
      ::
          ?(%i32 %i64)
        ?-    mode
            %u  ?:((lth v w) 1 0)
            %s
          ::  if both are positive or both a negative,
          ::  then comparison is simple
          ::
          ?:  ?|  &((lth v negat) (lth w negat))
                  &((gte v negat) (gte w negat))
              ==
            ?:((lth v w) 1 0)
          ::  otherwise v and w have different signs,
          ::  check which is negative
          ::
          ?:((gth v w) 1 0)
        ==
      ==
    ::
    ++  gt
      |=  i=instruction
      ?>  ?=(%gt -.i)
      |=  [v=@ w=@]
      ^-  @
      %.  [w v]
      (lt ;;(instruction [%lt +.i]))
    ::
    ++  le
      |=  i=instruction
      ?>  ?=(%le -.i)
      |=  [v=@ w=@]
      ^-  @
      ?:  =(v w)  1
      %.  [v w]
      (lt ;;(instruction [%lt +.i]))
    ::
    ++  ge
      |=  i=instruction
      ?>  ?=(%ge -.i)
      |=  [v=@ w=@]
      ^-  @
      %.  [w v]
      (le ;;(instruction [%le +.i]))
    ::
    ++  shr
      |=  i=instruction
      ?>  ?=(%shr -.i)
      =/  negat=@  ?-(type.i %i32 (bex 31), %i64 (bex 63))
      =/  negat-dec=@  (dec negat)
      =/  base=@   ?-(type.i %i32 32, %i64 64)
      |=  [v=@ w=@]
      ^-  @
      ?-    mode.i
          %u  (rsh [0 (mod w base)] v)
          %s
        ?:  (lth v negat)
          (rsh [0 (mod w base)] v)
        ;:  ^add
          negat
          (^sub negat-dec (dec (bex (^sub (dec base) (mod w base)))))
          (rsh [0 (mod w base)] (^sub v negat))
        ==
      ==
    ::
    ++  shl
      |=  i=instruction
      ?>  ?=(%shl -.i)
      =/  base=@   ?-(type.i %i32 32, %i64 64)
      |=  [v=@ w=@]
      ^-  @
      (lsh [0 (mod w base)] v)
    ::
    ++  sub
      |=  i=instruction
      ?>  ?=(%sub -.i)
      =/  base=@  ?+(type.i 0 %i32 (bex 32), %i64 (bex 64))
      |=  [v=@ w=@]
      ^-  @
      ?+  type.i  !!
        ?(%i32 %i64)  (~(dif fo base) v w)
        %f32  (sub:rs v w)
        %f64  (sub:rd v w)
      ==
    ::
    ++  mul
      |=  i=instruction
      ?>  ?=(%mul -.i)
      =/  base=@  ?+(type.i 0 %i32 (bex 32), %i64 (bex 64))
      |=  [v=@ w=@]
      ^-  @
      ?+  type.i  !!  ::  assert numerical values, add v128
        ?(%i32 %i64)  (~(pro fo base) v w)
        %f32  (mul:rs v w)
        %f64  (mul:rd v w)
      ==
    ::
    ++  div
      |=  i=instruction
      ?>  ?=(%div -.i)
      =/  base=@  ?+(type.i 0 %i32 32, %i64 64)
      =/  mode=?(%u %s)  (fall mode.i %u)
      |=  [v=@ w=@]
      ^-  @
      ?+    type.i  !!
          ?(%i32 %i64)
        ?-  mode
          %u  (^div v w)
          %s  %+  si-to-complement  base
              %+  fra:si
                (complement-to-si base v)
              (complement-to-si base w)
        ==
      ::
          %f32  (div:rs v w)
          %f64  (div:rd v w)
      ==
    ::
    ++  or
      |=  *
      |=  [v=@ w=@]
      ^-  @
      (con v w)  ::  ATTENTION! loobean conjunction is boolean disjunction
    ::
    ++  xor
      |=  *
      |=  [v=@ w=@]
      ^-  @
      (mix v w)
    ::
    ++  rotl
      |=  i=instruction
      ?>  ?=(%rotl -.i)
      =/  base=@  ?-(type.i %i32 32, %i64 64)
      =/  n=@  ?-(type.i %i32 5, %i64 6)
      |=  [v=@ w=@]
      ^-  @
      (~(rol fe n) 0 (mod w base) v)
    ::
    ++  and
      |=  *
      |=  [v=@ w=@]
      ^-  @
      (dis v w)  ::  ATTENTION! loobean disjunction is boolean conjunction
    ::
    ++  eq
      |=  *
      |=  [v=@ w=@]
      ^-  @
      ?:(=(v w) 1 0)
    --
  --
--