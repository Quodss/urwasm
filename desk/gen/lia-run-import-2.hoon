/+  *lia-runtime
/*  printf-i32     %wasm  /tests/printf-i32/wasm
::
:-  %say  |=  *  :-  %noun
::
|^
=/  lia  lia-sur
=/  run  runtime
=/  =script:tree:lia-sur
  :*
    ~
    [~ ~[%i32]]
  ::
    ^-  code:tree:lia
    :~
      [%let %a %i32]
      [%op ~[[%a %i32]] %run 'writeHi' ~]
    ==
  ::
    ~[%a]
  ==
::
=/  ext=(map (pair cord cord) ext-func:tree:lia-sur)
  %-  my
  :~
    :-  ['console' 'log']
    :*
      ~[%off %len]
      [~[%i32 %i32] ~[%i32]]
      ^-  code:tree:lia
      :~
        [%let %c %i32]
        [%let %b %octs]
        [%read %b [%name %off %i32] [%name %len %i32]]
        [%run-lia %printf ~[[%name %b %octs]] ~[[%c %i32]]]
      ==
      ~[%c]
    ==
  ==
::
=|  shop=(list (list value:line:lia-sur))
=/  import=(map term script-type:tree:lia-sur)
  %-  my
  :~
    [%printf [~[%octs] ~[%i32]]]
  ==
::
|-  ^-  result:line:lia-sur
=/  res=result:line:lia-sur
  (lia-main:run [printf-i32 (reap 1 script ~) shop ext import ~])
?-    -.res
    %0  res
    %2  res
    %1
  ?>  ?=(%king +<.res)
  ?>  ?=(%printf name.res)
  =/  pole-in=(pole value:line:lia)  in.res
  ?>  ?=([[%octs =octs] ~] pole-in)
  ~&  [%print (of-octs octs.pole-in)]
  $(shop (snoc shop ~[[%i32 42]]))
==
::
++  i-32
  |=  n=@
  ^-  op:tree:lia-sur
  [%zero %const %i32 n]
::
++  to-octs
  |=  =tape
  ^-  octs
  :-  (lent tape)
  (rep 3 tape)
::
++  rope
  |=  [b=bloq s=step a=@]
  ^-  (list @)
  ?:  =(s 0)  ~
  :-  (end b a)
  $(a (rsh b a), s (dec s))
::
++  of-octs
  |=  =octs
  ^-  tape
  (rope 3 octs)
::
--