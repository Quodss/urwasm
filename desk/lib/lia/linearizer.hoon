/-  *lia
::
=>  |%
    +$  vars  (map name:tree idx:line)  ::  variable-address binding for current scope
    +$  code  action:line
    +$  free  (set idx:line)
    +$  huge  idx:line
    +$  dest  $@(%stack [%idx p=(list idx:line)])
    +$  depf  (unit @)  ::  break depth
    +$  fore  (map name:tree idx:line)  ::  var-address binding for outside scope
    +$  last  ?
    +$  gen   [=vars =code =free =huge =dest =depf =fore =last]
    --
::
|%
++  try-inc
  |=  a=depf
  ^-  depf
  ?~  a  ~
  `+(u.a)
::
++  main
  =|  gen
  =*  gen  -
  |=  =script:tree
  ^-  (list op:line)
  ?>  =((lent input:script) (lent p.type.code.script))
  =.  gen  %+  roll  input:script
           |:  [name=*name:tree gen=gen]
           gen(huge +(huge.gen), fore (~(put by fore.gen) name huge.gen))
  =/  fore-init  fore
  |^
  =.  gen  (block-gen code.script)
  ?>  =(fore-init fore)
  ?>  =(~ vars)
  code
  ::
  ++  block-gen
    |=  b=block:tree
    ^-  ^gen
    =.  body.b  (flop body.b)
    |-  ^+  gen
    ?~  body.b  gen
    =*  phrase  i.body.b
    =^  slots=(list idx:line)  gen
      %^  spin  names.phrase  gen
      |:  [name=*name:tree g=gen]
      ^-  [idx:line ^gen]
      ::  local scope first
      ::
      =/  idx-vars  (~(get by vars.g) name)
      ?^  idx-vars
        [u.idx-vars g]
      ::  look behind for local definition sites
      ::
      ?:  %+  lien  t.body.b
          |=  p=phrase:tree
          &(?=([%let *] op.p) ?=(^ (find ~[name] p.op.p)))
        ?~  free.g
          :-  huge.g
          g(huge +(huge.g), vars (~(put by vars.g) name huge.g))
        :-  n.free.g
        %=  g
          vars  (~(put by vars.g) name n.free.g)
          free  (~(del in `^free`free.g) n.free.g)
        ==
      =/  idx-fore  (~(get by fore.g) name)
      ?^  idx-fore
        [u.idx-fore g]
      ?~  free.g
        :-  huge.g
        g(huge +(huge.g), fore (~(put by fore.g) name huge.g))
      :-  n.free.g
      %=  g
        fore  (~(put by fore.g) name n.free.g)
        free  (~(del in `^free`free.g) n.free.g)
      ==
    =.  dest  [%idx slots]
    =.  gen  (op-gen op.phrase t.body.b)
    $(body.b t.body.b, last |)
  ::
  ++  set-one
    |=  [d=^dest c=^code]
    ^-  ^code
    ?@  d  c
    ?>  ?=([* ~] p.d)
    [[%set i.p.d] c]
  ::
  ++  set-n
    |=  [d=^dest c=^code]
    ^-  ^code
    ?@  d  c
    %+  roll  p.d
    |:  [i=*idx:line c=c]
    [[%set i] c]
  ::
  ++  op-gen
    |=  [=op:tree behind=(list phrase:tree)]
    =*  this-op-gen  $
    ^-  ^gen
    ?@  op
      =^  addr=idx:line  gen
        =/  slot-vars  (~(get by vars) op)
        ?^  slot-vars  [u.slot-vars gen]
        ?:  %+  lien  behind
            |=  p=phrase:tree
            &(?=([%let *] op.p) ?=(^ (find ~[op] p.op.p)))
          ?~  free
            [huge gen(vars (~(put by vars) op huge), huge +(huge))]
          =/  noob  n.free
          =>  .(free `^free`free)
          :-  noob
          gen(vars (~(put by vars) op noob), free (~(del in free) noob))
        =/  slot-fore  (~(get by fore) op)
        ?^  slot-fore  [u.slot-fore gen]
        ?~  free
          [huge gen(fore (~(put by fore) op huge), huge +(huge))]
        =/  noob  n.free
        =>  .(free `^free`free)
        :-  noob
        gen(fore (~(put by fore) op noob), free (~(del in free) noob))
      =.  code.gen  (set-one dest.gen code.gen)
      gen(code [[%get addr] code.gen])
    ::
    ?-    -.op
        %let
      =/  slots=(list (unit idx:line))
        (turn p.op ~(get by vars))
      =.  vars
        %+  roll  p.op
        |:  [n=*name:tree v=vars]
        (~(del by v) n)
      =.  free
        %-  ~(uni in free)
        (silt `(list idx:line)`(murn slots same))
      gen
    ::
        %run
      =.  code  (set-n dest code)
      =.  code  [[%run p.op] code]
      %+  reel  q.op
      |:  [o=*op:tree g=gen]
      this-op-gen(op o, gen g(dest %stack))
    ::
        %cut
      =.  code  (set-one dest code)
      =.  code  [[%cut ~] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      =.  gen  this-op-gen(op offset.op, dest %stack)
      this-op-gen(op octs.op, dest %stack)
    ::
        %read
      =.  code  (set-one dest code)
      =.  code  [[%read ~] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      this-op-gen(op offset.op, dest %stack)
    ::
        %writ
      =.  code  (set-one dest code)
      =.  code  [[%writ ~] code]
      =.  gen  this-op-gen(op offset.op, dest %stack)
      this-op-gen(op octs.op, dest %stack)
    ::
        %lit
      =.  code  (set-one dest code)
      gen(code [op code])
    ::
        %len
      =.  code  (set-one dest code)
      =.  code  [[%len ~] code]
      this-op-gen(op octs.op, dest %stack)
    ::
        %octs
      =.  code  (set-one dest code)
      =.  code  [[%octs ~] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      this-op-gen(op dat.op, dest %stack)
    ::
        %add
      =.  code  (set-one dest code)
      =.  code  [[%add type.op] code]
      =.  gen  this-op-gen(op q.op, dest %stack)
      this-op-gen(op p.op, dest %stack)
    ::
        %sub
      =.  code  (set-one dest code)
      =.  code  [[%sub type.op] code]
      =.  gen  this-op-gen(op q.op, dest %stack)
      this-op-gen(op p.op, dest %stack)
    ::
        %if
      ?>  =(type.true.op type.false.op)
      =.  code  (set-n dest code)
      =/  true-gen
        =<  (block-gen true.op)
        %=  .
          dest  %stack
          code  ~
          fore  vars
          vars  ~
          depf  (try-inc depf)
          last  &
        ==
      ?>  =(~ vars.true-gen)  ::  should be redundant
      =.  free  free.true-gen
      =.  huge  huge.true-gen
      =.  gen
        =/  vars-dif  (~(dif by fore.true-gen) vars)
        %+  roll  ~(tap by vars-dif)
        |:  [[name=*name:tree idx=*idx:line] g=gen]
        ^-  ^gen
        ?:  %+  lien  behind
            |=  p=phrase:tree
            &(?=([%let *] op.p) ?=(^ (find ~[name] p.op.p)))
          g(vars (~(put by vars.g) name idx))
        g(fore (~(put by fore.g) name idx))
      =/  false-gen
        =<  (block-gen false.op)
        %=  .
          dest  %stack
          code  ~
          fore  vars
          vars  ~
          depf  (try-inc depf)
          last  &
        ==
      ?>  =(~ vars.false-gen)  ::  should be redundant
      =.  free  free.true-gen
      =.  huge  huge.true-gen
      =.  gen
        =/  vars-dif  (~(dif by fore.false-gen) vars)
        %+  roll  ~(tap by vars-dif)
        |:  [[name=*name:tree idx=*idx:line] g=gen]
        ^-  ^gen
        ?:  %+  lien  behind
            |=  p=phrase:tree
            &(?=([%let *] op.p) ?=(^ (find ~[name] p.op.p)))
          g(vars (~(put by vars.g) name idx))
        g(fore (~(put by fore.g) name idx))
      =.  code
        ^-  ^code
        :_  code
        `op:line`[%if type.true.op code.true-gen code.false-gen]
      this-op-gen(op test.op, dest %stack)
    ::
        %while
      ?>  ?=([~ ~] type.body.op)
      =/  body-gen
        =<  (block-gen body.op)
        %=  .
          dest  %stack
          code  ~[[%br 1]]
          fore  vars
          vars  ~
          depf  `0
          last  &
        ==
      ?>  =(~ vars.body-gen)  ::  should be redundant
      =.  free  free.body-gen
      =.  huge  huge.body-gen
      =.  gen
        =/  vars-dif  (~(dif by fore.body-gen) vars)
        %+  roll  ~(tap by vars-dif)
        |:  [[name=*name:tree idx=*idx:line] g=gen]
        ^-  ^gen
        ?:  %+  lien  behind
            |=  p=phrase:tree
            &(?=([%let *] op.p) ?=(^ (find ~[name] p.op.p)))
          g(vars (~(put by vars.g) name idx))
        g(fore (~(put by fore.g) name idx))
      =/  test-gen
        this-op-gen(op test.op, dest %stack, code ~)
      =.  code
        ^-  ^code
        :_  code
        ^-  op:line
        :+  %loop  [~ ~]
        ^-  (list op:line)
        %+  weld  code.test-gen
        `(list op:line)`[%if [~ ~] code.body-gen ~]~
      test-gen(code code)
    ::
        %break
      ?>  ?=(^ depf)
      gen(code [[%br u.depf] code])
    ::
        %yield
      =?  code  !last
        [[%yeet ~] code]
      %+  reel  p.op
      |:  [o=*op:tree g=gen]
      this-op-gen(op o, gen g(dest %stack))
    ==
  --
--