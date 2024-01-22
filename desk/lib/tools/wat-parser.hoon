::  An example of using urwasm in production: parsing of
::  .wat format
::
/-  *engine
/+  parser=parser-lib
/+  *runner-engine
/+  op-def=runner-op-def
/*  bin  %wasm  /lib/tools/wat2wasm/wasm
::
|=  string-in=tape
^-  ^module
::  check text correctness to avoid having to call realloc
::
?>  (levy string-in (curr lte 0x7f))
!.
~&  'parse wat2wasm module'
=+  m=(main:parser bin)
~&  'instantiate'
=+  st=+:(wasm-need (prep m ~))
=^  out=(list coin-wasm)  st
  %-  wasm-need
  ~&  'invoke add-to-stack-pointer'
  %^  invoke  '__wbindgen_add_to_stack_pointer'
  ~[[%i32 (en-si:op-def 32 -16)]]  st
=/  retptr=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
=^  out=(list coin-wasm)  st
  %-  wasm-need
  ~&  'invoke malloc'
  %^  invoke  '__wbindgen_malloc'
  ~[[%i32 (lent string-in)] [%i32 1]]  st
=/  ptr0=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
=/  len0=@  (lent string-in)
?>  ?=(^ mem.st)
=.  buffer.u.mem.st
  ~&  'memory write'
  (sew bloq=3 [ptr0 size=len0 (crip string-in)] buffer.u.mem.st)
=>  .(st `store`st)
=.  st
  =<  +  %-  wasm-need
  ~&  'invoke process'
  %^  invoke  'process'
  ~[[%i32 retptr] [%i32 ptr0] [%i32 len0]]  st
?>  ?=(^ mem.st)
~&  'memory read'
=/  r0=@  (cut 3 [retptr 4] buffer.u.mem.st)
=/  r1=@  (cut 3 [(add retptr 4) 4] buffer.u.mem.st)
=/  module-out=octs
  ~&  'memory read'
  [r1 (cut 3 [r0 r1] buffer.u.mem.st)]
~&  '.wasm -> noun parsing'
(main:parser module-out)