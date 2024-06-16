/+  *lia-runtime
/*  bin  %wasm  /tests/parse-inline/wasm
:-  %say  |=  *  :-  %noun
::
=/  run  runtime
=/  =script:tree:lia-sur
  :*
    ~
    [~ ~]
  ::
    ^-  code:tree:lia-sur
    :~
      [%op ~ %run 'process' ~]
    ==
  ::
    ~
  ==
~>  %bout
%-  lia-main:run
:*
  bin
  (reap 1 script ~)
  ~
  ~
  ~
  ~
  %$
==