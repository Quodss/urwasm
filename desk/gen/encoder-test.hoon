/+  e=encoder
/+  p=parser-lib
/*  fac-loop   %wasm  /tests/fac-br/wasm
/*  div-table  %wasm  /tests/del-table/wasm
/*  fib-rust   %wasm  /tests/fib/wasm
/*  fac-if     %wasm  /tests/fac/wasm
/*  two-func   %wasm  /tests/two-functions/wasm
/*  flopper    %wasm  /tests/flopper/wasm
/*  import     %wasm  /tests/import/wasm
/*  bin  %wasm  /lib/tools/wat2wasm/wasm
:: /*  printf     %wasm  /tests/printf/wasm
::
:-  %say  |=  *  :-  %noun
::
=>  |%
    ++  toct
      |=  =octs
      ^-  tape
      =/  out=tape  (trip q.octs)
      %+  weld  out
      ^-  tape
      (reap (sub p.octs (lent out)) '\00')
    --
::
=/  ast  (main:p bin)
=/  ast2  (main:p (main:e ast))
=(ast ast2)