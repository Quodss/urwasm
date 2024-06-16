/+  *lia-runtime
/*  bin  %wasm  /lib/tools/wat2wasm/wasm
::
|=  jetted=@tas
|=  string-in=tape
^-  module:wasm-sur
=/  lia  lia-sur
=/  run  runtime
=;  res=result:line:lia
  ?>  &(?=(%0 -.res) ?=([[%octs =octs] ~] out.res))
  (main:parser octs.i.out.res)
|^
?>  (levy string-in (curr lte 0x7f))
=/  =script:tree:lia-sur
  :*
    ~[%a]
    [~[%octs] ~[%octs]]
  ::
    ^-  code:tree:lia-sur
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
    ==
  ::
    ~[%b]
  ==
::
~>  %bout
%-  lia-main:run
:*
  bin
  (reap 1 script ~[[%octs (to-octs string-in)]])
  ~
  ~
  ~
  ~
  jetted
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