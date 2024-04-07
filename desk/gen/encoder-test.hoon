/+  e=encoder
/+  p=parser-lib
/*  fac-loop   %wasm  /tests/fac-br/wasm
/*  div-table  %wasm  /tests/del-table/wasm
/*  fib-rust   %wasm  /tests/fib/wasm
/*  fac-if     %wasm  /tests/fac/wasm
/*  two-func   %wasm  /tests/two-functions/wasm
/*  flopper    %wasm  /tests/flopper/wasm
/*  import     %wasm  /tests/import/wasm
/*  bin        %wasm  /lib/tools/wat2wasm/wasm
/*  printf     %wasm  /tests/printf/wasm
::
:-  %say  |=  *  :-  %noun
::
=/  l=(list (pair @tas octs))
  :~
    fac-loop+fac-loop
    div-table+div-table
    fib-rust+fib-rust
    fac-if+fac-if
    two-func+two-func
    flopper+flopper
    import+import
    bin+bin
    printf+printf
  ==
|-  ^-  ?
?~  l  &
=*  lab  p.i.l
=*  bin  q.i.l
=/  ast
  ~>  %bout  (main:p bin)
=/  new-bin
  ~>  %bout  (main:e ast)
=/  ast2
  ~>  %bout  (main:p new-bin)
?.  =(ast ast2)
  ~&  lab
  |
$(l t.l)

