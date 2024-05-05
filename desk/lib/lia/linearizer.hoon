/+  runner-engine
=>  runner-engine
:: ~%  %lia-line  +  ~
|%
++  linearizer
  =,  lia-sur
  =>  |%
      +$  typed-idx  (pair idx:line value-type:line)
      +$  vars  (map name:tree typed-idx)  ::  variable-address binding for current scope
      +$  code  (list op:line)
      +$  free  (set idx:line)
      +$  huge  idx:line
      +$  dest  $@(%stack [%idx p=(list typed-idx)])
      +$  depf  (unit @)  ::  break depth
      +$  fore  (map name:tree typed-idx)  ::  var-address binding for outside scope
      +$  gen   [=vars =code =free =huge =dest =depf =fore]
      --
  ::
  |%
  ++  flip  |*(^ [+<+ +<-])
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
    =/  slot-vars  (~(get by vars.gen) name)
    ?^  slot-vars
      ?>  =(q.u.slot-vars type)
      [p.u.slot-vars gen]
    =/  is-behind=?
      =>  [name=name behind=behind ..tree]  ~+
      %+  lien  behind
      |=  p=phrase:tree
      &(?=(%let -.p) =(name p.p))
    ?:  is-behind
      ?~  free.gen
        :-  huge.gen
        %=  gen
          huge  +(huge.gen)
          vars  (~(put by vars.gen) name [huge.gen type])
        ==
      =/  noob  n.free.gen
      =>  .(free.gen `free`free.gen)
      :-  noob
      %=  gen
        free  (~(del in free.gen) noob)
        vars  (~(put by vars.gen) name [noob type])
      ==
    =/  slot-fore  (~(get by fore.gen) name)
    ?^  slot-fore
      ?>  =(q.u.slot-fore type)
      [p.u.slot-fore gen]
    :-  huge.gen
    %=  gen
      huge  +(huge.gen)
      fore  (~(put by fore.gen) name [huge.gen type])
    ==
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
    |=  =input:tree
    ^-  [input:line (list (list value:line))]
    =/  diff-line=$@(~ (each action:line (list value:line)))
      ?~  diff.input  ~
      ?:  ?=(%| -.diff.input)
        diff.input
      [%& (script-gen p.diff.input)]
    :_  (turn code-vals.input tail)
    :*
      (main:parser module.input)
      (turn (turn code-vals.input head) script-gen)
      shop.input
      (~(run by ext.input) ext-gen)
      import.input
      diff-line
    ==
  ::
  ++  ext-gen
    |=  =ext-func:tree
    ^-  ext-func:line
    [type.ext-func +:(script-gen ext-func)]
  ::
  ++  script-gen
    |=  =script:tree
    ^-  action:line
    =|  gen
    =*  gen  -
    =.  gen
      %+  roll  (fuse input:script p.type.script)
      |:  [[name=*name:tree type=*value-type:tree] gen=gen]
      gen(huge +(huge.gen), fore (~(put by fore.gen) name [huge.gen type]))
    =/  fore-init  fore
    |^
    =.  gen  (return-gen (fuse return.script q.type.script) code.script)
    =.  gen  (code-gen code.script)
    ?>  =(fore-init fore)
    ?>  =(~ vars)
    [type.script code]
    ::
    ++  return-gen
      |=  $:  return=(list (pair name:tree value-type:tree))
              behind=(list phrase:tree)
          ==
      ^-  ^gen
      %+  reel  return
      |:  [[n=*name:tree t=*value-type:tree] g=gen]
      ^-  ^gen
      =^  =idx:line  g  ((give-idx behind) [n t] g)
      =.  code.g  [[%get t idx] code.g]
      g
    ::
    ++  code-gen
      |=  c=code:tree
      ^-  ^gen
      =.  c  (flop c)
      |-  ^+  gen
      ?~  c  gen
      $(gen (phrase-gen i.c t.c), c t.c)
    ::
    ++  phrase-gen
      |=  [=phrase:tree behind=(list phrase:tree)]
      =*  this-phrase-gen  $
      ^-  ^gen
      ?:  ?=(%op -.phrase)
        =^  slots=(list idx:line)  gen
          (spin names.phrase gen (give-idx behind))
        =.  dest  [%idx (fuse slots (turn names.phrase tail))]
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
        =/  true-gen
          =<  (code-gen true.phrase)
          %=  .
            dest  %stack
            code  ~
            fore  vars
            vars  ~
            depf  (try-inc depf)
          ==
        ?>  =(~ vars.true-gen)  ::  should be redundant
        =.  free  free.true-gen
        =.  huge  huge.true-gen
        =.  gen
          =/  vars-dif  (~(dif by fore.true-gen) vars)
          %+  roll  ~(tap by vars-dif)
          |:  [[name=*name:tree idx=*typed-idx] g=gen]
          ^-  ^gen
          ?:  %+  lien  behind
              |=  p=phrase:tree
              &(?=(%let -.p) =(name p.p))
            g(vars (~(put by vars.g) name idx))
          g(fore (~(put by fore.g) name idx))
        =/  false-gen
          =<  (code-gen false.phrase)
          %=  .
            dest  %stack
            code  ~
            fore  vars
            vars  ~
            depf  (try-inc depf)
          ==
        ?>  =(~ vars.false-gen)  ::  should be redundant
        =.  free  free.true-gen
        =.  huge  huge.true-gen
        =.  gen
          =/  vars-dif  (~(dif by fore.false-gen) vars)
          %+  roll  ~(tap by vars-dif)
          |:  [[name=*name:tree idx=*typed-idx] g=gen]
          ^-  ^gen
          ?:  %+  lien  behind
              |=  p=phrase:tree
              &(?=(%let -.p) =(name p.p))
            g(vars (~(put by vars.g) name idx))
          g(fore (~(put by fore.g) name idx))
        =.  code
          ^-  ^code
          :_  code
          `op:line`[%if [~ ~] code.true-gen code.false-gen]
        =.  dest  %stack
        (op-gen test.phrase behind)
      ::
          %while
        =/  body-gen
          =<  (code-gen body.phrase)
          %=  .
            dest  %stack
            code  ~[[%br 1]]
            fore  vars
            vars  ~
            depf  `0
          ==
        ?>  =(~ vars.body-gen)  ::  should be redundant
        =.  free  free.body-gen
        =.  huge  huge.body-gen
        =.  gen
          =/  vars-dif  (~(dif by fore.body-gen) vars)
          %+  roll  ~(tap by vars-dif)
          |:  [[name=*name:tree idx=*typed-idx] g=gen]
          ^-  ^gen
          ?:  %+  lien  behind
              |=  p=phrase:tree
              &(?=(%let -.p) =(name p.p))
            g(vars (~(put by vars.g) name idx))
          g(fore (~(put by fore.g) name idx))
        =/  test-gen
          =.  dest  %stack
          =.  code  ~
          (op-gen test.phrase behind)
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
        =.  gen  (op-gen len.phrase behind):.(dest %stack)
        =.  gen  (op-gen offset.phrase behind):.(dest %stack)
        gen
      ::
          %writ
        =^  slot=idx:line  gen
          ((give-idx behind) [from.phrase %octs] gen)
        =.  code  [[%writ slot] code]
        =.  gen  (op-gen len.phrase behind):.(dest %stack)
        =.  gen  (op-gen offset.phrase behind):.(dest %stack)
        =.  gen  (op-gen ptr.phrase behind):.(dest %stack)
        gen
      ::
          %run-lia
        =^  slots=(list idx:line)  gen
          (spin r.phrase gen (give-idx behind))
        =.  code  [[%run-lia p.phrase slots] code]
        %+  reel  q.phrase
        |:  [o=*op:tree g=gen]
        (op-gen o behind):.(gen g(dest %stack))
      ::
          %writ-octs-i32
        =^  slot=idx:line  gen
          ((give-idx behind) [to.phrase %octs] gen)
        =.  code  [[%writ-octs-i32 slot] code]
        =.  gen  (op-gen dat.phrase behind):.(dest %stack)
        gen
      ==
    ::
    ++  set-one
      |=  [d=^dest c=^code]
      ^-  ^code
      ?@  d  c
      ?>  ?=([* ~] p.d)
      [[%set (flip i.p.d)] c]
    ::
    ++  set-n
      |=  [d=^dest c=^code]
      ^-  ^code
      ?@  d  c
      %+  roll  p.d
      |:  [i=*typed-idx c=c]
      [[%set (flip i)] c]
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
        =.  code  [[%get q.op addr] code]
        gen
      ::
          %run
        =.  code  (set-n dest code)
        =.  code  [[%run p.op] code]
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
  --  ::  |linearizer
--