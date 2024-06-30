/+  *lia-runtime
/*  import-ream  %wasm  /tests/wish-test/wasm
::
:-  %say  |=  *  :-  %noun
::
|^
=*  lia  lia-sur
=*  run  runtime
=/  =script:tree:lia
  :*
    ~
    [~ ~[%i32]]
  ::
    ^-  code:tree:lia-sur
    :~
      [%let %ret %i32]
      [%op ~[[%ret %i32]] %run 'run' ~]
    ==
  ::
    ~[%ret]
  ==
::
=/  ext=(map (pair cord cord) ext-func:tree:lia-sur)
  %-  my
  :~
    :-  ['./import_test_bg.js' '__wbg_wish_725f7272ed56e40f']
    :*
      ~[%retptr %ptr %len]
      [~[%i32 %i32 %i32] ~]
      :~
        [%let %string %octs]
        [%let %out %octs]
        [%read %string [%name %ptr %i32] [%name %len %i32]]
        [%op ~ %run '__wbindgen_free' ~[[%name %ptr %i32] [%name %len %i32] (i-32 1)]]
        [%run-lia %wish ~[[%name %string %octs]] ~[[%out %octs]]]
        [%let %ptr-out %i32]
        [%op ~[[%ptr-out %i32]] %run '__wbindgen_malloc' ~[[%len %out] (i-32 1)]]
        [%writ %out [%name %ptr-out %i32] (i-32 0) [%len %out]]
        [%let %ptr-out-octs %octs]
        [%let %len-octs %octs]
        [%writ-octs-i32 %ptr-out-octs [%name %ptr-out %i32]]
        [%writ-octs-i32 %len-octs [%len %out]]
        [%writ %ptr-out-octs [%name %retptr %i32] (i-32 0) (i-32 4)]
        [%writ %len-octs [%two [%add %i32] [%name %retptr %i32] (i-32 4)] (i-32 0) (i-32 4)]
      ==
      ~
    ==
  ==
::
=|  shop=(list (list value:line:lia-sur))
=/  import=(map term script-type:tree:lia-sur)
  %-  my
  :~
    [%wish [~[%octs] ~[%octs]]]
  ==
::
~>  %bout
|-  ^-  result:line:lia-sur
=/  res=result:line:lia-sur
  (lia-main:run [import-ream (reap 1 script ~) shop ext import ~ %$])
?-    -.res
    %2  !!
    %0  res
::
    %1
  ?>  ?=(%king +<.res)
  ?>  ?=(%wish name.res)
  =/  pole-in=(pole value:line:lia)  in.res
  ?>  ?=([[%octs =octs] ~] pole-in)
  =/  str  (of-octs octs.pole-in)
  $(shop (snoc shop ~[[%octs (to-octs (wish str))]]))
==
::
++  wish
  |=  t=tape
  ^-  tape
  =/  gen  (ream (crip t))
  =.  gen  [%tell ~[gen]]
  !<  tape
  (slap ^~(!>(..zuse)) gen)
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