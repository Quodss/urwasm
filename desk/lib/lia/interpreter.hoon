::
/-  *lia
/+  parser=parser-lib
/+  runner=runner-engine
::
|%
++  check-in
  |=  l=(list value)
  ^-  (list coin-wasm)
  %+  turn  l
  |=  v=value
  ^-  coin-wasm
  ?<(?=(%octs -.v) v)
::
++  check-out
  |=  l=(list coin-wasm)
  ^-  (list value)
  %+  turn  l
  |=  c=coin-wasm
  ^-  value
  ?<(?=(%ref -.c) c)
::
++  lia
  |=  [module=octs actions=(list action)]
  ^-  result
  =+  inst=(prep:runner (main:parser module) ~)
  ?:  ?=(%2 -.inst)  [%2 ~]
  ?>  ?=(%0 -.inst)
  =+  st=st.inst
  =/  lia-state=state  [[~] [~] st]
  ?~  actions  [%0 ~]
  |-  ^-  result
  ?~  t.actions  -:(act lia-state i.actions)
  =+  acted=(act lia-state i.actions)
  ?.  ?=(%0 -<.acted)  -.acted
  =.  lia-state  +.acted
  $(actions t.actions)
::
++  act
  |=  [s=state a=action]
  ^-  [result state]
  ?~  a
    [[%0 (flop stack.s)] s(stack ~)]
  =+  applied=(apply-op i.a s)
  ?.  ?=(%0 -<.applied)  applied
  $(s +.applied, a t.a)
::
++  apply-op
  |=  [=op s=state]
  ^-  [result state]
  ?-    -.op
      %get
    :-  [%0 ~]
    s(stack [(snag idx.op space.s) stack.s])
  ::
      %set
    ?>  ?=(^ stack.s)
    :-  [%0 ~]
    s(stack +.stack.s, space (snap space.s idx.op -.stack.s))
  ::
      %run
    =/  id=@  (find-func-id:runner name.op module.store.s)
    =/  id-local=@
      (sub id (lent funcs.import-section.module.store.s))
    =/  =func-type
      ::  May only invoke local functions (to reconsider?)
      ::
      =,  module.store.s
      (snag type-id:(snag id-local function-section) type-section)
    =/  n=@  (lent params.func-type)
    =+  in=(scag n stack.s)
    ?>  =((lent in) n)
    =/  res-wasm=^result
      (invoke:runner name.op (flop (check-in in)) store.s)
    ?:  ?=(%2 -.res-wasm)  [[%2 ~] s]
    ?:  ?=(%1 -.res-wasm)  !!
    :-  [%0 ~]
    =,  res-wasm
    s(stack (weld (check-out (flop out)) (slag n stack.s)), store st)
  ::
      %add
    :-  [%0 ~]
    ?>  ?=([b=value a=value rest=*] stack.s)
    ?>  ?=(?(%i32 %i64) type.op)
    =,  stack.s
    ?>  ?=(?(%i32 %i64 %octs) -.a)
    ?>  ?=(?(%i32 %i64 %octs) -.b)
    =/  base=@  ?-(type.op %i32 (bex 32), %i64 (bex 64))
    =/  n-a=@
      ?:  ?=(%octs -.a.stack.s)  q.a
      +.a
    =/  n-b=@
      ?:  ?=(%octs -.b.stack.s)  q.b
      +.b
    =/  sum=@  (~(sum fo base) n-a n-b)
    =/  v=value  ?-(type.op %i32 [%i32 sum], %i64 [%i64 sum])
    s(stack [v rest])
  ::
      %sub
    :-  [%0 ~]
    ?>  ?=([b=value a=value rest=*] stack.s)
    ?>  ?=(?(%i32 %i64) type.op)
    =,  stack.s
    ?>  ?=(?(%i32 %i64 %octs) -.a)
    ?>  ?=(?(%i32 %i64 %octs) -.b)
    =/  base=@  ?-(type.op %i32 (bex 32), %i64 (bex 64))
    =/  n-a=@
      ?:  ?=(%octs -.a.stack.s)  q.a
      +.a
    =/  n-b=@
      ?:  ?=(%octs -.b.stack.s)  q.b
      +.b
    =/  dif=@  (~(dif fo base) n-a n-b)
    =/  v=value  ?-(type.op %i32 [%i32 dif], %i64 [%i64 dif])
    s(stack [v rest])
  ::
      %cut
    :-  [%0 ~]
    ?>  ?=([o=[%octs octs] rest=*] stack.s)
    =,  stack.s
    ?>  (lth offset.op p.o)
    ?>  (lth (add offset.op len.op) p.o)
    =,  op
    s(stack [[%octs len (cut 3 [offset len] q.o)] rest])
  ::
      %read
    :-  [%0 ~]
    ?>  ?=([len=[%i32 @] pointer=[%i32 @] rest=*] stack.s)
    =,  stack.s
    ?>  ?=(^ mem.store.s)
    =,  u.mem.store.s
    ?>  (lth +.pointer (mul page-size:runner n-pages))
    ?>  (lth (add +.pointer +.len) (mul page-size:runner n-pages))
    s(stack [[%octs +.len (cut 3 [+.pointer +.len] buffer)] rest])
  ::
      %writ
    :-  [%0 ~]
    ?>  ?=([pointer=[%i32 @] o=[%octs octs] rest=*] stack.s)
    =,  stack.s
    ?>  ?=(^ mem.store.s)
    =,  u.mem.store.s
    ?>  (lth +.pointer (mul page-size:runner n-pages))
    ?>  (lth (add +.pointer p.o) (mul page-size:runner n-pages))
    %=    s
        stack  rest
        mem.store
      :-  ~
      %=  u.mem.store.s
        buffer  (sew 3 [+.pointer +.o] buffer)
      ==
    ==
  ::
      %if
    ?>  ?=([f=[%i32 @] rest=*] stack.s)
    =,  stack.s
    ?.  =(0 +.f)
      (act s true.op)
    (act s false.op)
  ::
      %for
    =,  op
    =,  si
    ?>  ?:  (syn step)
          !=(--1 (cmp from to))
        !=(-1 (cmp from to))
    =.  space.s
      =/  v=value  [%i32 (en-si:runner 32 from)]
      (snap space.s i v)
    |-  ^-  [result state]
    ?:  .=  (cmp from to)
        ?:  (syn step)
          --1
        -1
      [[%0 ~] s]
    =+  yield=(act s body)
    ?.  ?=(%0 -<.yield)  yield
    =.  s  +.yield
    %=    $
        from.op  (sum from step)
        space.s
      =/  v=value  [%i32 (en-si:runner 32 (sum from step))]
      (snap space.s i v)
    ==
  ::
      %const
    :-  [%0 ~]
    s(stack [p.op stack.s])
  ::
  ==
--