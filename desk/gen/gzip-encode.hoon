/+  wasm=wasm-lia
/*  bin  %wasm  /tests/gzip/wasm
::
=/  lv  lia-value:lia-sur:wasm
|=  a=octs
^-  octs
=;  out=(list lv)
  ?>  ?=([[%octs *] ~] out)
  +.i.out
|^
%-  yield-need:wasm
%^  run-once:wasm  [bin ~]  %$
=/  m  runnable:wasm
=,  wasm
;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
;<  ptr0=@    try:m  (call-1 '__wbindgen_malloc' p.a 1 ~)
;<  ~         try:m  (memwrite ptr0 p.a q.a)
;<  *         try:m  (call 'encode' retptr ptr0 p.a ~)
;<  r0=octs   try:m  (memread retptr 4)
;<  r1=octs   try:m  (memread (add retptr 4) 4)
;<  r2=octs   try:m  (memread q.r0 q.r1)
(return:m octs+r2 ~)
::
++  i32neg  ^~((cury sub (bex 32)))
--