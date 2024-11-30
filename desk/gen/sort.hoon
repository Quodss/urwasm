/+  wasm=wasm-lia
/*  bin  %wasm  /tests/sort/wasm
::
:-  %say  |=  *  :-  %noun
::
=/  lv  lia-value:lia-sur:wasm
=/  a=(list @)  (flop (gulf 0 (bex 12)))
=/  nock
  ~&  %nock
  ~>  %bout
  (sort a lth)
=;  fast
  =(nock fast)
~&  %fast
~>  %bout
=/  len0=@  (lent a)
=;  out=(list lv)
  ?>  ?=([[%octs *] ~] out)
  =|  out-list=(list @)
  =/  ptr=@  (mul 4 len0)
  |-  ^-  (list @)
  ?:  =(0 ptr)  out-list
  =.  ptr  (sub ptr 4)
  $(out-list [(cut 3 [ptr 4] q.+.i.out) out-list])
|^
%-  yield-need:wasm
%^  run-once:wasm  [bin ~]  %$
=/  m  runnable:wasm
=,  wasm
;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
=/  len0-bytes=@  (mul 4 len0)
;<  ptr0=@    try:m  (call-1 '__wbindgen_malloc' len0-bytes 4 ~)
;<  ~         try:m  (memwrite ptr0 len0-bytes (rep 5 a))
;<  *         try:m  (call 'process' retptr ptr0 len0 ~)
;<  r0=octs   try:m  (memread retptr 4)
;<  r1=octs   try:m  (memread (add retptr 4) 4)
;<  r2=octs   try:m  (memread q.r0 (mul 4 q.r1))
(return:m octs+r2 ~)
::
++  i32neg  ^~((cury sub (bex 32)))
--