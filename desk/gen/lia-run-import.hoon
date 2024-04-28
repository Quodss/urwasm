/-  lia
/+  run=lia-runtime
/+  par=parser-lib
/+  op-def=runner-op-def
/+  encoder
/*  printf     %wasm  /tests/printf/wasm
::
:-  %say  |=  *  :-  %noun
::
|^
=/  serf  (main:par printf)
=/  =script:tree:lia
  :*
    ~
    [~ ~]
  ::
    ^-  code:tree:lia
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
      ^-  code:tree:lia
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
:: =/  input-line  (main:line:run [serf (reap 1 script) shop ext import |+~])
:: (main:encoder (main:comp:run [module code ext import]:input-line))
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
  $(shop (snoc shop ~))
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