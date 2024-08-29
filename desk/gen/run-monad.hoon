/+  *lia-monad
/*  bin  %wasm  /tests/flopper/wasm
:: :-  %say  |=  *  :-  %noun
|=  a=tape
=*  mon  monad
=*  lv  value:line:lia-sur
=;  script=lia-monad:mon
  =/  res  (lia-reduce:mon bin script ~ ~)
  ?>  ?=(%0 -.res)
  =/  val=octs  (get-octs:mon p.res)
  (trip q.val)
=,  mon
;<  l=(list lv)  bind
  (call '__wbindgen_add_to_stack_pointer' i32+(sub (bex 32) 16) ~)
=/  retptr=@  (get-1-num %i32 l)
=/  len0=@  (lent a)
;<  l=(list lv)  bind
  (call '__wbindgen_malloc' i32+len0 i32+1 ~)
=/  ptr0=@  (get-1-num %i32 l)
;<  *  bind  (write ptr0 len0 (crip a))
;<  *  bind  (call 'process' i32+retptr i32+ptr0 i32+len0 ~)
;<  l=(list lv)  bind  (read retptr 4)
=/  r0=octs  (get-octs l)
;<  l=(list lv)  bind  (read (add retptr 4) 4)
=/  r1=octs  (get-octs l)
;<  l-out=(list lv)  bind  (read q.r0 q.r1)
;<  *  bind  (call '__wbindgen_add_to_stack_pointer' i32+16 ~)
;<  *  bind  (call '__wbindgen_free' i32+q.r0 i32+q.r1 i32+1 ~)
(pure l-out)