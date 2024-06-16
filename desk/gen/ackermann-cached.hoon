/+  *lia-runtime
/*  bin  %wasm  /tests/ackermann-cached/wasm
:: :-  %say  |=  *  :-  %noun
|=  [m=@ n=@]
::
=/  run  runtime
=/  =script:tree:lia-sur
  :*
    ~[%m %n]
    [~[%i64 %i64] ~[%i64]]
  ::
    ^-  code:tree:lia-sur
    :~
      [%let %out %i64]
      [%op ~[[%out %i64]] %run 'ackermann_run' ~[[%name %m %i64] [%name %n %i64]]]
    ==
  ::
    ~[%out]
  ==
~>  %bout
%-  lia-main:run
:*
  bin
  (reap 1 script ~[[%i64 m] [%i64 n]])
  ~
  ~
  ~
  ~
  %$
==