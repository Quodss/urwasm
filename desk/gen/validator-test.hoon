/+  *validator
/*  div-table  %wasm  /tests/del-table/wasm
/*  fac-loop   %wasm  /tests/fac-br/wasm
/*  fib-rust   %wasm  /tests/fib/wasm
/*  fac-if     %wasm  /tests/fac/wasm
/*  two-func   %wasm  /tests/two-functions/wasm
/*  flopper    %wasm  /tests/flopper/wasm
/*  import     %wasm  /tests/import/wasm
/*  printf     %wasm  /tests/printf/wasm
/*  wat2wasm   %wasm  /lib/tools/wat2wasm/wasm
/*  gzip       %wasm  /tests/gzip/wasm
/*  ack        %wasm  /tests/ackermann-cached/wasm
/*  bf         %wasm  /tests/bf/wasm
::
:-  %say  |=  *  :-  %noun
::
~>  %bout
=;  modules=(list (pair term octs))
  |-  ^-  (list [term (result-form:validator ~)])
  ?~  modules  ~
  =/  res  (validate-module:validator (main:parser q.i.modules))
  ?:  ?=(%& -.res)
    $(modules t.modules)
  [[p.i.modules res] $(modules t.modules)]
:~
  fac-loop+fac-loop
  div-table+div-table
  fac-loop+fac-loop
  fib-rust+fib-rust
  fac-if+fac-if
  two-func+two-func
  flopper+flopper
  import+import
  printf+printf
  wat2wasm+wat2wasm
  gzip+gzip
  ack+ack
  bf+bf
==