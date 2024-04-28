/-  lia
/+  run=lia-runtime
/+  par=parser-lib
/+  op-def=runner-op-def
/+  encoder
/*  printf-i32     %wasm  /tests/printf-i32/wasm
::
:-  %say  |=  *  :-  %noun
::
|^
=/  serf  (main:par printf-i32)
=/  =script:tree:lia
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
=/  ext=(map (pair cord cord) ext-func:tree:lia)
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
=|  shop=(list (list value:line:lia))
=/  import=(map term script-type:tree:lia)
  %-  my
  :~
    [%printf [~[%octs] ~[%i32]]]
  ==
::
:: (main:encoder (main:comp:run [module code ext import]:input-line))
:: (lia-main:run [serf (reap 1 script) shop ext import |+~] (reap 1 ~))
|-  ^-  result:line:lia
=/  res=result:line:lia
  (lia-main:run [serf (reap 1 script) shop ext import |+~] (reap 1 ~))
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
  ^-  op:tree:lia
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