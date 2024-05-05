/+  *lia-runtime
/*  flopper  %wasm  /tests/flopper/wasm
::
:-  %say  |=  *  :-  %noun
::
|^
=*  lia  lia-sur
=*  run  runtime
=/  =script:tree:lia
  :*
    ~[%a]
    [~[%octs] ~[%octs]]
  ::
    ^-  code:tree:lia
    :~
      [%let %retptr %i32]
      [%op ~[[%retptr %i32]] %run '__wbindgen_add_to_stack_pointer' ~[(i-32 (en-si:op-def 32 -16))]]
      [%let %len0 %i32]
      [%op ~[[%len0 %i32]] %len %a]
      [%let %ptr0 %i32]
      [%op ~[[%ptr0 %i32]] %run '__wbindgen_malloc' ~[[%len %a] (i-32 1)]]
      [%writ %a [%name %ptr0 %i32] (i-32 0) [%name %len0 %i32]]
      [%op ~ %run 'process' ~[[%name %retptr %i32] [%name %ptr0 %i32] [%name %len0 %i32]]]
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
      [%op ~[[%retptr %i32]] %run '__wbindgen_add_to_stack_pointer' ~[(i-32 16)]]
      [%op ~ %run '__wbindgen_free' ~[[%name %r0-i32 %i32] [%name %r1-i32 %i32] (i-32 1)]]
    ==
  ::
    ~[%b]
  ==
::
=;  res
  :: res
  ?>  &(?=(%0 -.res) ?=([[%octs =octs] ~] out.res))
  (of-octs octs.i.out.res)
%-  lia-main:run
:*
  flopper
  %+  fuse:linearizer  `(list script:tree:lia)`(reap 4 script)
  :~
    ~[[%octs (to-octs "")]]
    ~[[%octs (to-octs "abc")]]
    ~[[%octs (to-octs "")]]
    ~[[%octs (to-octs "zxczxc")]]
  ==
  ~
  ~
  ~
  ~
  %$
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