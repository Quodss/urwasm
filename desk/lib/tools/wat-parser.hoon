/-  *engine
/+  parser=parser-lib
/+  *runner-engine
/+  op-def=runner-op-def
/*  bin-atom  %atom  /lib/tools/wat2wasm/atom
::
|=  string-in=tape
^-  ^module
!.
=+  st=+:(wasm-need (prep ;;(^module (cue bin-atom)) ~))
=^  out=(list coin-wasm)  st
  %-  wasm-need
  %^  invoke  '__wbindgen_add_to_stack_pointer'
  ~[[%i32 (si-to-complement:op-def 32 -16)]]  st
=/  retptr=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
=^  out=(list coin-wasm)  st
  %-  wasm-need
  %^  invoke  '__wbindgen_malloc'
  ~[[%i32 (lent string-in)] [%i32 1]]  st
=/  ptr0=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
=/  len0=@  (lent string-in)
?>  ?=(^ mem.st)
=.  buffer.u.mem.st
  (sew bloq=3 [ptr0 size=len0 (crip string-in)] buffer.u.mem.st)
=>  .(st `store`st)
=.  st
  =<  +  %-  wasm-need
  %^  invoke  'process'
  ~[[%i32 retptr] [%i32 ptr0] [%i32 len0]]  st
?>  ?=(^ mem.st)
=/  r0=@  (cut 3 [retptr 4] buffer.u.mem.st)
=/  r1=@  (cut 3 [(add retptr 4) 4] buffer.u.mem.st)
=/  module-out=octs
  [r1 (cut 3 [r0 r1] buffer.u.mem.st)]
(main:parser module-out)