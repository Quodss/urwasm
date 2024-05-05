/+  *lia-runtime
/*  import-ream  %wasm  /tests/import-ream/wasm
::
:-  %say  |=  *  :-  %noun
::
|^
=*  lia  lia-sur
=*  run  runtime
=/  =script:tree:lia
  :*
    ~
    [~ ~[%octs]]
  ::
    ^-  code:tree:lia-sur
    :~
      [%let %retptr %i32]
      [%op ~[[%retptr %i32]] %run '__wbindgen_add_to_stack_pointer' ~[(i-32 (en-si:op-def 32 -16))]]
      [%op ~ %run 'run' ~[[%name %retptr %i32]]]
      [%let %r0 %octs]
      [%read %r0 [%name %retptr %i32] (i-32 4)]
      [%let %r1 %octs]
      [%read %r1 [%two [%add %i32] [%name %retptr %i32] (i-32 4)] (i-32 4)]
      [%let %r0-i32 %i32]
      [%let %r1-i32 %i32]
      [%op ~[[%r0-i32 %i32]] %read-octs-i %r0 %i32 (i-32 0) (i-32 4)]
      [%op ~[[%r1-i32 %i32]] %read-octs-i %r1 %i32 (i-32 0) (i-32 4)]
      [%let %b %octs]
      [%read %b [%name %r0-i32 %i32] [%name %r1-i32 %i32]]
    ==
  ::
    ~[%b]
  ==
::
=/  ext=(map (pair cord cord) ext-func:tree:lia-sur)
  %-  my
  :~
    :-  ['./import_test_bg.js' '__wbg_workstring_1b3e07327280b69f']
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
|-  ^-  tape
=/  res=result:line:lia-sur
  (lia-main:run [import-ream (reap 1 script ~) shop ext import ~ %$])
?-    -.res
    %2  !!
    %0
  ?>  ?=([[%octs =octs] ~] out.res)
  (of-octs octs.i.out.res)
::
    %1
  ?>  ?=(%king +<.res)
  ?>  ?=(%wish name.res)
  =/  pole-in=(pole value:line:lia)  in.res
  ?>  ?=([[%octs =octs] ~] pole-in)
  =/  str  (of-octs octs.pole-in)
  ~&  [%print str]
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