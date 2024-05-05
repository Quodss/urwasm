/+  *lia-runtime
/*  printf     %wasm  /tests/printf/wasm
::
:-  %say  |=  *  :-  %noun
::
|^
=*  lia  lia-sur
=*  run  runtime
=/  =script:tree:lia-sur
  :*
    ~
    [~ ~]
  ::
    ^-  code:tree:lia-sur
    :~
      [%op ~ %run 'writeHi' ~]
    ==
  ::
    ~
  ==
::
=/  ext=(map (pair cord cord) ext-func:tree:lia)
  %-  my
  :~
    :-  ['console' 'log']
    :*
      ~[%off %len]
      [~[%i32 %i32] ~]
      ^-  code:tree:lia-sur
      :~
        [%let %b %octs]
        [%read %b [%name %off %i32] [%name %len %i32]]
        [%run-lia %printf ~[[%name %b %octs]] ~]
      ==
      ~
    ==
  ==
::
=|  shop=(list (list value:line:lia))
=/  import=(map term script-type:tree:lia)
  %-  my
  :~
    [%printf [~[%octs] ~]]
  ==
::
|-  ^-  result:line:lia-sur
=/  res=result:line:lia-sur
  (lia-main:run printf (reap 1 script ~) shop ext import ~)
?-    -.res
    %0  res
    %2  res
    %1
  ~&  %import
  ?>  ?=(%king +<.res)
  ?>  ?=(%printf name.res)
  =/  pole-in=(pole value:line:lia)  in.res
  ?>  ?=([[%octs =octs] ~] pole-in)
  ~&  [%print (of-octs octs.pole-in)]
  $(shop (snoc shop ~))
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