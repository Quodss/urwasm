/-  *lia
::
=>  |%
    +$  vars  (map name:tree idx:line)
    +$  code  action:line
    +$  free  (set idx:line)
    +$  huge  idx:line
    +$  dest  $@(%stack [%idx p=(list idx:line)])  ::  destination: stack, or space index, or dropped
    +$  depf  (unit @)  ::  break depth
    +$  gen   [=vars =code =free =huge =dest =depf]
    --
::
=|  =gen
|=  b=block:tree
^-  (list op:line)
=<  ?>  ?=(~ vars)
    code
|-  ^+  gen
=*  block-gen  $
|^
::  'return' codegen
::
?^  return.b
  block-gen(return.b t.return.b, gen (op-gen i.return.b))
::  body codegen
::
=.  body.b  (flop body.b)
|-  ^+  gen
?~  body.b  gen
=*  phrase  i.body.b
:: =/  slots=(list (unit idx:line))
::   (turn names.phrase ~(get by vars.gen))
=^  slots=(list idx:line)  gen
  %^  spin  names.phrase  gen
  |=  [name=name:tree =^gen]
  =+  idx=(~(get by vars.gen) name)
  ?^  idx
    [u.idx gen]
  ?~  free.gen
    [huge.gen gen(huge +(huge.gen), vars (~(put by vars.gen) name huge.gen))]
  :-  n.free.gen
  %=  gen
    vars  (~(put by vars.gen) name n.free.gen)
    free  (~(del in `free`free.gen) n.free.gen)
  ==
=.  dest.gen  [%idx slots]
:: =.  vars.gen
::   %+  roll  names.phrase
::   |:  [n=*name:tree v=vars.gen]
::   (~(del by v) n)
::
:: =.  free.gen
::   %-  ~(uni in free.gen)
::   (silt `(list idx:line)`(murn slots same))
::
=.  gen  (op-gen op.phrase)
$(body.b t.body.b)
::
++  put-one
  |=  [d=dest c=code]
  ^-  code
  ?@  d  c
  ?>  ?=([* ~] p.d)
  [[%set i.p.d] c]
::
++  put-n
  |=  [d=dest c=code]
  ^-  code
  ?@  d  c
  %+  roll  p.d
  |:  [i=*idx:line c=c]
  [[%set i] c]
::
++  op-gen
  |=  =op:tree
  =*  this-op-gen  $
  ^+  gen
  ?@  op
    ::  resolve name
    ::
    =?  gen  !(~(has by vars.gen) op)
      ?~  free.gen
        ~!  vars.gen
        gen(vars (~(put by vars.gen) op huge.gen), huge +(huge.gen))
      %=  gen
        vars  (~(put by vars.gen) op n.free.gen)
        free  (~(del in `free`free.gen) n.free.gen)
      ==
    =/  slot  (~(got by vars.gen) op)
    =.  code.gen  (put-one dest.gen code.gen)
    gen(code [[%get slot] code.gen])
  ::
  ?-    -.op
      %let
    =/  slots=(list (unit idx:line))
      (turn p.op ~(get by vars.gen))
    =.  vars.gen
      %+  roll  p.op
      |:  [n=*name:tree v=vars.gen]
      (~(del by v) n)
    =.  free.gen
      %-  ~(uni in free.gen)
      (silt `(list idx:line)`(murn slots same))
    ~&  free.gen
    gen
  ::
      %run
    =.  code.gen  (put-n dest.gen code.gen)
    =.  code.gen  [[%run p.op] code.gen]
    %+  reel  q.op
    |:  [o=*op:tree gen=gen]
    this-op-gen(op o, dest.gen %stack, gen gen)
  ::
      %cut
    =.  code.gen  (put-one dest.gen code.gen)
    =.  code.gen  [[%cut ~] code.gen]
    =.  gen  this-op-gen(op len.op, dest.gen %stack)
    =.  gen  this-op-gen(op offset.op, dest.gen %stack)
    this-op-gen(op octs.op, dest.gen %stack)
  ::
      %read
    =.  code.gen  (put-one dest.gen code.gen)
    =.  code.gen  [[%read ~] code.gen]
    =.  gen  this-op-gen(op len.op, dest.gen %stack)
    this-op-gen(op offset.op, dest.gen %stack)
  ::
      %writ
    =.  code.gen  (put-one dest.gen code.gen)
    =.  code.gen  [[%writ ~] code.gen]
    =.  gen  this-op-gen(op offset.op, dest.gen %stack)
    this-op-gen(op octs.op, dest.gen %stack)
  ::
      %lit
    =.  code.gen  (put-one dest.gen code.gen)
    gen(code [op code.gen])
  ::
      %len
    =.  code.gen  (put-one dest.gen code.gen)
    =.  code.gen  [[%len ~] code.gen]
    this-op-gen(op octs.op, dest.gen %stack)
  ::
      %octs
    =.  code.gen  (put-one dest.gen code.gen)
    =.  code.gen  [[%octs ~] code.gen]
    =.  gen  this-op-gen(op len.op, dest.gen %stack)
    this-op-gen(op dat.op, dest.gen %stack)
  ::
      %add
    =.  code.gen  (put-one dest.gen code.gen)
    =.  code.gen  [[%add type.op] code.gen]
    =.  gen  this-op-gen(op q.op, dest.gen %stack)
    this-op-gen(op p.op, dest.gen %stack)
  ::
      %sub
    =.  code.gen  (put-one dest.gen code.gen)
    =.  code.gen  [[%sub type.op] code.gen]
    =.  gen  this-op-gen(op q.op, dest.gen %stack)
    this-op-gen(op p.op, dest.gen %stack)
  ::
      %if
    =.  code.gen  (put-n dest.gen code.gen)
    ?>  =(type.true.op type.false.op)
    =/  true-gen
      block-gen(b true.op, gen gen(dest %stack, code ~))
    =/  false-gen
      %=  block-gen
        b  false.op
        vars.gen  (~(uni by vars.true-gen) vars.gen)
        code.gen  ~
        free.gen  (~(int in free.true-gen) free.gen)
        huge.gen  (max huge.true-gen huge.gen)
        dest.gen  %stack
      ==
    =.  code.gen
      ^-  code
      :_  code.gen
      `op:line`[%if type.true.op code.true-gen code.false-gen]
    %=  this-op-gen
      op  cond.op
      vars.gen  vars.false-gen
      free.gen  free.false-gen
      huge.gen  huge.false-gen
      dest.gen  %stack
    ==
  ::
      %while
    ?>  =(~ type.body.op)
    =/  body-gen
      block-gen(b body.op, dest.gen %stack, code.gen ~[[%br 1]], depf.gen `1)
    =/  cond-gen
      %=  this-op-gen
        op  p.op
        vars.gen  (~(uni by vars.body-gen) vars.gen)
        code.gen  ~
        free.gen  (~(int in free.body-gen) free.gen)
        huge.gen  (max huge.body-gen huge.gen)
        dest.gen  %stack
      ==
    =.  code.gen
      ^-  code
      :_  code.gen
      ^-  op:line
      :+  %loop  [~ ~]
      ^-  (list op:line)
      %+  weld  code.cond-gen
      `(list op:line)`[%if [~ ~] code.body-gen ~]~
    cond-gen(code code.gen)
  ::
      %break
    ?>  ?=(^ depf.gen)
    gen(code [[%br u.depf.gen] code.gen])
  ::
  ==
--