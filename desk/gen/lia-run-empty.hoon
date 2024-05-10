/+  *lia-runtime
::
:-  %say  |=  *  :-  %noun
::
=/  =script:tree:lia-sur
  :*
    ~[%a %b]
    [~[%i32 %i32] ~[%i32]]
  ::
    ^-  code:tree:lia-sur
    :~
      [%let %c %i32]
      [%op ~[[%c %i32]] %two [%add %i32] [%name %a %i32] [%name %b %i32]]
    ==
  ::
    ~[%c]
  ==
::
=/  =input:line:lia-sur
  =<  -
  %:  main:linearizer
    [8 '\00asm\01']
    (reap 1 script ~[[%i32 2] [%i32 3]])
    ~
    ~
    ~
    ~
    %$
  ==
(main:encoder (main:compiler [module code ext import]:input))
:: %-  lia-main:runtime
:: :*
::   [8 '\00asm\01']
::   (reap 1 script ~[[%i32 2] [%i32 3]])
::   ~
::   ~
::   ~
::   ~
::   %$
:: ==
