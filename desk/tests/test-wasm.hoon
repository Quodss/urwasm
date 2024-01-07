/-  *engine
/+  *test
/+  parser=parser-lib
/+  *runner-engine
/+  op-def=runner-op-def
/*  div-table  %wasm  /tests/del-table/wasm
/*  fac-loop   %wasm  /tests/fac-br/wasm
/*  fib-rust   %wasm  /tests/fib/wasm
/*  fac-if     %wasm  /tests/fac/wasm
/*  two-func   %wasm  /tests/two-functions/wasm
/*  flopper    %wasm  /tests/flopper/wasm
::
|%
++  test-table
  ::  Test table section and indirect calls
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%f32 n=.5.5]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'testdivtable'  ~[[%f32 .11] [%f32 .2]]
    (prep (main:parser div-table))
::
++  test-loop
  ::  Test correct loop execution
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=362.880]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'factorial'  ~[[%i32 9]]
    (prep (main:parser fac-loop))
::
++  test-rust
  ::  Test a module obtained from wasm-pack utility in Rust
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=102.334.155]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'fib'  ~[[%i32 40]]
    (prep (main:parser fib-rust))
::
++  test-if
  ::  Test if branching
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%f64 n=.~362880]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'fac'  ~[[%f64 .~9]]
    (prep (main:parser fac-if))
::
++  test-two-functions
  ::  Test nested function calls
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=43]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'addTwo'  ~[[%i32 21] [%i32 21]]
    (prep (main:parser two-func))
::
::
++  test-flopper
  ::  Test a function that flops a string
  ::
  %+  expect-eq
    !>  `tape`(flop (gulf 'a' 'z'))
    !>
    =/  string-in=tape  (gulf 'a' 'z')
    =+  st=(prep (main:parser flopper))
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
    =.  buffer.mem.st
      (sew bloq=3 [ptr0 size=len0 (crip string-in)] buffer.mem.st)
    =.  st
      =<  +  %-  wasm-need
      %^  invoke  'process'
      ~[[%i32 retptr] [%i32 ptr0] [%i32 len0]]  st
    =/  r0=@  (cut 3 [retptr 4] buffer.mem.st)
    =/  r1=@  (cut 3 [(add retptr 4) 4] buffer.mem.st)
    =/  string-out=tape
      %-  trip
      (cut 3 [r0 r1] buffer.mem.st)
    string-out
  ::
  --