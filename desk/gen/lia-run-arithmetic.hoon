/+  *lia-runtime
/*  two-func  %wasm  /tests/two-functions/wasm
::
:-  %say  |=  *  :-  %noun
::
=*  lia  lia-sur
=*  run  runtime
=/  =script:tree:lia
  :*
    ~[%a %b]
    [~[%i32 %i32] ~[%i32]]
  ::
    ^-  code:tree:lia
    :~
      [%let %c %i32]
      [%op ~[[%c %i32]] %run 'addTwo' ~[[%name %a %i32] [%name %b %i32]]]
    ==
  ::
    ~[%c]
  ==
::
%-  lia-main:run
:*
  two-func
  (reap 1 script ~[[%i32 21] [%i32 21]])
  ~
  ~
  ~
  ~
==
