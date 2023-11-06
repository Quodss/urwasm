/-  *wasm
/+  ast-interpreter
/+  wasm-to-ast
/+  handle=handle-operators
/*  bin-wasm  %wasm  /simple-debts/wasm
|=  [n-people=@ debtor-vec=(list @) creditor-vec=(list @) sums-vec=(list @)]
::  end-to-end Wasm module interpretation pipeline
::
|^
~>  %bout
=/  debtor-ids=cord
  %+  can  5
  (fuse (reap 9 1) debtor-vec)
=/  creditor-ids=cord
  %+  can  5
  (fuse (reap 9 1) creditor-vec)
=/  sums=cord
  %+  can  5
  (fuse (reap 9 1) sums-vec)
=/  length-words  (lent sums-vec)  ::  same for all vectors
=+  hwasm=(instantiate:ast-interpreter (main:wasm-to-ast bin-wasm))
::  get retptr:
::
=^  out  hwasm
  %+  call-id:hwasm
    (find-func-id:hwasm '__wbindgen_add_to_stack_pointer' module.hwasm)
  ~[[%i32 (si-to-complement:handle 32 -16)]]
=/  retptr=@  ?>(?=(^ out) n.i.out)
~&  "retptr={<retptr>}"
::  pass vecs to wasm:
::
=^  out  hwasm
  %+  call-id:hwasm
    (find-func-id:hwasm '__wbindgen_malloc' module.hwasm)
  ~[[%i32 (mul 4 length-words)] [%i32 4]]  ::  length and some other argument from JS 
=/  ptr0=@  ?>(?=(^ out) n.i.out)
~&  "ptr0={<ptr0>}"
=.  buffer.hwasm
  (sew bloq=3 [ptr0 size=(mul 4 length-words) debtor-ids] buffer.hwasm)
::
=^  out  hwasm
  %+  call-id:hwasm
    (find-func-id:hwasm '__wbindgen_malloc' module.hwasm)
  ~[[%i32 (mul 4 length-words)] [%i32 4]]  ::  length and some other argument from JS 
=/  ptr1=@  ?>(?=(^ out) n.i.out)
~&  "ptr1={<ptr1>}"
=.  buffer.hwasm
  (sew bloq=3 [ptr1 size=(mul 4 length-words) creditor-ids] buffer.hwasm)
::
=^  out  hwasm
  %+  call-id:hwasm
    (find-func-id:hwasm '__wbindgen_malloc' module.hwasm)
  ~[[%i32 (mul 4 length-words)] [%i32 4]]  ::  length and some other argument from JS 
=/  ptr2=@  ?>(?=(^ out) n.i.out)
~&  "ptr2={<ptr2>}"
=.  buffer.hwasm
  (sew bloq=3 [ptr2 size=(mul 4 length-words) sums] buffer.hwasm)
::  run `simplify`
::
=.  hwasm
  =<  +
  %+  call-id:hwasm
    (find-func-id:hwasm 'simplify' module.hwasm)
  :~  [%i32 retptr]
      [%i32 n-people]
      [%i32 ptr0]
      [%i32 length-words]
      [%i32 ptr1]
      [%i32 length-words]
      [%i32 ptr2]
      [%i32 length-words]
  ==
::  get r0 and r1 at retptr
::
=/  r0=@  (cut 3 [retptr 4] buffer.hwasm)
=/  r1=@  (cut 3 [(add retptr 4) 4] buffer.hwasm)
~&  "ptr-out={<r0>}"
::  get vec with r0 and r1
::
=/  string-out=@
    (cut 3 [r0 (mul 4 r1)] buffer.hwasm)
::  free the memory? nah, later
::
=/  list-out=(list @)  (rip 5 string-out)
=/  list-len=@  (lent list-out)
=/  debtor-new=(list @)  (scag (div list-len 3) list-out)
=/  creditor-new=(list @)  (swag [(div list-len 3) (div list-len 3)] list-out)
=/  sums-new=(list @)  (slag (mul 2 (div list-len 3)) list-out)
[debtor-new creditor-new sums-new]
::
++  fuse                                                ::  cons contents (from /lib/pal by ~paldev)
  |*  [a=(list) b=(list)]
  :: ^-  (list _?>(?=([^ ^] [a b]) [i.a i.b]))  ::TODO  why does this not work?
  ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
  ?~  a  ~
  ?~  b  ~
  :-  [i.a i.b]
  $(a t.a, b t.b)
::
--