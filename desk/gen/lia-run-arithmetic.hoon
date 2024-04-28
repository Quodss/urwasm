/-  lia
/+  run=lia-runtime
/+  par=parser-lib
/+  op-def=runner-op-def
/*  two-func  %wasm  /tests/two-functions/wasm
::
:-  %say  |=  *  :-  %noun
::
=/  serf  (main:par two-func)
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
:_  ~[~[[%i32 21] [%i32 21]]]
:*
  serf
  ~[script]
  ~
  ~
  ~
  |+~
==
