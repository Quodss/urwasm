/-  *wasm-lia
/+  *wasm-lia
/*  binary  %wasm  /lib/wasm/tools/wat2wasm/wasm
:-  %say  |=  *  :-  %noun
::
=/  lv  lia-value:lia-sur
|^
=,  lia-sur
?>  (levy wat (curr lte 0x7f))
=;  out=(list lia-value)
  ?>  ?=([[%octs *] ~] out)
  (main:parser +.i.out)
%-  yield-need  =<  -
%^  (run-once (list lv) *)  [binary `~]  %$
=/  m  runnable
=/  arr  (arrows *)
=,  arr
;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
=/  len0=@  (lent wat)
;<  ptr0=@    try:m  (call-1 '__wbindgen_malloc' len0 1 ~)
;<  ~         try:m  (memwrite ptr0 len0 (crip wat))
;<  *         try:m  (call 'process' retptr ptr0 len0 ~)
;<  r0=octs   try:m  (memread retptr 4)
;<  r1=octs   try:m  (memread (add retptr 4) 4)
;<  r2=octs   try:m  (memread q.r0 q.r1)
(return:m octs+r2 ~)
::
++  i32neg  ^~((cury sub (bex 32)))
++  wat  ^-  tape
  """
  (module
  (func (export "addTwo") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add))
  """
--