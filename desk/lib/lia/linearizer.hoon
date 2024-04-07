/-  *lia
::
=>  |%
    +$  typed-idx  (pair idx:line value-type:line)
    +$  vars  (map name:tree typed-idx)  ::  variable-address binding for current scope
    +$  code  (list op:line)
    +$  free  (set idx:line)
    +$  huge  idx:line
    +$  dest  $@(%stack [%idx p=(list idx:line)])
    +$  depf  (unit @)  ::  break depth
    +$  fore  (map name:tree typed-idx)  ::  var-address binding for outside scope
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
++  give-idx
  |=  behind=(list phrase:tree)
  |=  [[=name:tree type=value-type:line] =gen]
  ^-  [idx:line ^gen]
  =/  slot-vars  (~(get by vars) name)
  ?^  slot-vars
    ?>  =(q.u.slot-vars type)
    [p.u.slot-vars gen]
  ?:  %+  lien  behind
      |=  p=phrase:tree
      &(?=(%let -.p) =(name p.p))
    ?~  free
      [huge gen(vars (~(put by vars) name huge type), huge +(huge))]
    =/  noob  n.free
    =>  .(free `^free`free)
    :-  noob
    gen(vars (~(put by vars) name noob type), free (~(del in free) noob))
  =/  slot-fore  (~(get by fore) op)
  ?^  slot-fore
    ?>  =(q.u.slot-fore type)
    [u.slot-fore gen]
  [huge gen(fore (~(put by fore) name huge type), huge +(huge))]
::
++  fuse
  |*  [a=(list) b=(list)]
  ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
  ?~  a  ~
  ?~  b  ~
  :-  [i.a i.b]
  $(a t.a, b t.b)
::
++  main
  |=  =script:tree
  ^-  action:line
  ?>  =((lent input:script) (lent p.type.code.script))
  =|  gen
  =*  gen  -
  =.  gen  %+  roll  (fuse input:script p.type.code.script)
           |:  [[name=*name:tree type=*value-type:tree] gen=gen]
           gen(huge +(huge.gen), fore (~(put by fore.gen) name huge.gen type))
  =/  fore-init  fore
  |^
  =.  gen  (block-gen code.script)
  ?>  =(fore-init fore)
  ?>  =(~ vars)
  [type.code.script code]
  ::
  ++  block-gen
    |=  b=block:tree
    ^-  ^gen
    =.  body.b  (flop body.b)
    |-  ^+  gen
    ?~  body.b  gen
    $(gen (phrase-gen i.body.b t.body.b), body.b t.body.b, last |)
  ::
  ++  phrase-gen
    |=  [=phrase:tree behind=(list phrase:tree)]
    =*  this-phrase-gen  $
    ^-  ^gen
    ?:  ?=(%op -.phrase)
      =^  slots=(list idx:line)  gen
        (spin names.phrase gen (give-idx behind))
      =.  dest  [%idx slots]
      (op-gen op.phrase behind)
    ?-    -.phrase
        %let
      =/  slot=(unit typed-idx)  (~(get by vars) p.phrase)
      =.  vars  (~(del by vars) p.phrase)
      =?  free  ?=(^ slot)
        (~(put in free) p.u.slot)
      ?~  slot  gen
      ?>  =(q.u.slot q.phrase)
      gen(code [[%let q.phrase p.u.slot] code])
    ::
        %if
      ?>  &(?=([~ ~] type.true.phrase) ?=([~ ~] type.false.phrase))
      =/  true-gen
        =<  (block-gen true.phrase)
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
        =<  (block-gen false.phrase)
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
        `op:line`[%if type.true.phrase code.true-gen code.false-gen]
      this-op-gen(op test.phrase, dest %stack)  XX  replace with proper op-gen
    ::
        %while
      ?>  ?=([~ ~] type.body.phrase)
      =/  body-gen
        =<  (block-gen body.phrase)
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
        %read
      =^  slot=idx:line  gen
        ((give-idx behind) [to.phrase %octs] gen)
      =.  code  [[%read slot] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      =.  gen  this-op-gen(op offset.op, dest %stack)
      gen
    ::
        %writ
      =^  slot=idx:line  gen
        ((give-idx behind) [from.phrase %octs] gen)
      =.  code  [[%writ slot] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      =.  gen  this-op-gen(op offset.op, dest %stack)
      gen
    ::
        %octs
      =^  slot=idx:line  gen
        ((give-idx behind) [to.phrase %octs] gen)
      =.  code  [[%octs to] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      =.  gen  this-op-gen(op dat.op, dest %stack)
      gen
    ::
        %cut
      =^  slot-from=idx:line  gen
        ((give-idx behind) [from.phrase %octs] gen)
      =^  slot-to=idx:line  gen
        ((give-idx behind) [to.phrase %octs] gen)
      =.  code  [[%cut slot-from slot-to] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      =.  gen  this-op-gen(op offset.op, dest %stack)
      gen
    ::
    ==
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
    ?-    -.op
        %name
      =^  addr=idx:line  gen
        ((give-idx behind) [p.op q.op] gen)
       =.  code  (set-one dest code)
      =.  code  [[%get addr] code]
      gen
    ::
        %run
      =.  code  (set-n dest code)
      =.  code  [[%run p.op] code]
      %+  reel  q.op
      |:  [o=*op:tree g=gen]
      this-op-gen(op o, gen g(dest %stack))
    ::
        %run-ext
      =.  code  (set-n dest code)
      =.  code  [[%run-ext p.op] code]
      %+  reel  q.op
      |:  [o=*op:tree g=gen]
      this-op-gen(op o, gen g(dest %stack))
    ::
        %len
      =.  code  (set-one dest code)
      =^  slot=idx:line  gen
        ((give-idx behind) [from.op %octs] gen)
      =.  code  [[%len slot] code]
      gen
    ::
        %zero
       =.  code  (set-one dest code)
       =.  code  [p.op code]
       gen
    ::
        %one
      =.  code  (set-one dest code)
      =.  code  [p.op code]
      =.  gen  this-op-gen(op q.op, dest %stack)
      gen
    ::
        %two
      =.  code  (set-one dest code)
      =.  code  [p.op code]
      =.  gen  this-op-gen(op r.op, dest %stack)
      =.  gen  this-op-gen(op q.op, dest %stack)
      gen
    ::
        %read-octs-i
      =.  code  (set-one dest code)
      =^  slot=idx:line  gen
        ((give-idx behind) [from.op %octs] gen)
      =.  code  [[%read-octs-i slot type.op] code]
      =.  gen  this-op-gen(op len.op, dest %stack)
      =.  gen  this-op-gen(op off.op, dest %stack)
      gen
    ::
        %read-octs-f
      =.  code  (set-one dest code)
      =^  slot=idx:line  gen
        ((give-idx behind) [from.op %octs] gen)
      =.  code  [[%read-octs-f slot type.op] code]
      =.  gen  this-op-gen(op off.op, dest %stack)
      gen
    ==
  --
--