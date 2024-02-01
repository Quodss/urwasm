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
/*  import     %wasm  /tests/import/wasm
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
    +:(wasm-need (prep (main:parser div-table) ~))
::
++  test-loop
  ::  Test correct loop execution
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=362.880]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'factorial'  ~[[%i32 9]]
    +:(wasm-need (prep (main:parser fac-loop) ~))
::
++  test-rust
  ::  Test a module obtained from wasm-pack utility in Rust
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=102.334.155]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'fib'  ~[[%i32 40]]
    +:(wasm-need (prep (main:parser fib-rust) ~))
::
++  test-if
  ::  Test if branching
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%f64 n=.~362880]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'fac'  ~[[%f64 .~9]]
    +:(wasm-need (prep (main:parser fac-if) ~))
::
++  test-two-functions
  ::  Test nested function calls
  ::
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=43]]
    !>
    =<  -  %-  wasm-need
    %^  invoke  'addTwo'  ~[[%i32 21] [%i32 21]]
    +:(wasm-need (prep (main:parser two-func) ~))
::
::
++  test-flopper
  ::  Test a function that flops a string
  ::
  %+  expect-eq
    !>  `tape`(flop (gulf 'a' 'z'))
    !>
    =/  string-in=tape  (gulf 'a' 'z')
    =+  st=+:(wasm-need (prep (main:parser flopper) ~))
    =^  out=(list coin-wasm)  st
      %-  wasm-need
      %^  invoke  '__wbindgen_add_to_stack_pointer'
      ~[[%i32 (en-si:op-def 32 -16)]]  st
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
    =/  string-out=tape
      %-  trip
      (cut 3 [r0 r1] buffer.u.mem.st)
    string-out
  ::
++  test-import
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=43]]
    !>
    |^
    =+  st=+:(wasm-need (prep (main:parser import) ~))
    =<  -
    |-  ^-  (quip coin-wasm store)
    =+  r=(invoke 'succ' ~ st)
    ?-  -.r
      %0  +.r
      %1  $(st (resolve st +.r))
      %2  !!
    ==
    ::
    ++  resolve
      |=  $:  st=store
              [[mod=cord name=cord] =request]        
              mem=(unit [buffer=@ n-pages=@])        
              tables=(list (list $>(%ref coin-wasm)))
              globals=(list coin-wasm)
          ==
      ^-  store
      ?>  =(['./import_test_bg.js' '__wbg_getn_e182583a43d51902'] [mod name])
      ?>  ?=(%func -.request)
      ?>  ?=(~ args.request)
      =/  =item
        [[%i32 42]~ [mem tables globals]]
      st(shop [item shop.st], mem mem, tables tables, globals globals)
    ::
    --
::
++  test-import-2
  %+  expect-eq
    !>  `(list coin-wasm)`~[[type=%i32 n=84]]
    !>
    |^
    =+  st=+:(wasm-need (prep (main:parser import) ~))
    =<  -
    |-  ^-  (quip coin-wasm store)
    =+  r=(invoke 'add' ~ st)
    ?-  -.r
      %0  +.r
      %1  $(st (resolve st +.r))
      %2  !!
    ==
    ::
    ++  resolve
      |=  $:  st=store
              [[mod=cord name=cord] =request]        
              mem=(unit [buffer=@ n-pages=@])        
              tables=(list (list $>(%ref coin-wasm)))
              globals=(list coin-wasm)
          ==
      ^-  store
      ?>  =(['./import_test_bg.js' '__wbg_getn_e182583a43d51902'] [mod name])
      ?>  ?=(%func -.request)
      ?>  ?=(~ args.request)
      =/  =item
        [[%i32 42]~ [mem tables globals]]
      st(shop [item shop.st], mem mem, tables tables, globals globals)
    ::
    --
--