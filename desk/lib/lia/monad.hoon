/+  runner-engine
=>  runner-engine
|%
++  monad
  |%
  ++  tag-call       %0
  ++  tag-read       %1
  ++  tag-write      %2
  ++  tag-bind       %3
  ++  tag-pure       %4
  ++  tag-call-lia   %5
  +$  mold-call      %0
  +$  mold-read      %1
  +$  mold-write     %2
  +$  mold-bind      %3
  +$  mold-pure      %4
  +$  mold-call-lia  %5
  ::
  +$  cw  coin-wasm:wasm-sur
  +$  lv  value:line:lia-sur
  +$  lia-monad
    $~  [tag-pure ~]
    $%
      [mold-call name=cord args=(list cw)]
      [mold-read ptr=@ len=@]
      [mold-write ptr=@ len=@ src=@]
      [mold-bind p=lia-monad q=$-((list lv) lia-monad)]
      [mold-pure p=(list lv)]
      [mold-call-lia name=term p=(list lv)]
    ==
  ::
  +$  seed
    $:
      module=octs
      script=lia-monad
      shop=(list (list lv))
      import=(map (pair cord cord) $-((list cw) lia-monad))
    ==
  ::
  ++  lia-state  (pair store:engine-sur (list (list lv)))
  ++  call
    |=  [name=cord args=(list cw)]
    `lia-monad`[tag-call +<]
  ::
  ++  read
    |=  [ptr=@ len=@]
    `lia-monad`[tag-read +<]
  ::
  ++  write
    |=  [ptr=@ len=@ src=@]
    `lia-monad`[tag-write +<]
  ::
  ++  bind
    |=  *
    |=  [p=lia-monad q=$-((list lv) lia-monad)]
    `lia-monad`[tag-bind +<]
  ::
  ++  pure
    |=  [p=(list lv)]
    `lia-monad`[tag-pure +<]
  ::
  ++  call-lia
    |=  [name=term p=(list lv)]
    `lia-monad`[tag-call-lia +<]
  ::
  +$  result
    $%  [%0 p=(list lv)]
        [%1 name=term args=(list lv)]
        [%2 ~]
    ==
  ::
  ++  demote
    |=  l=(list lv)
    ^-  (list cw)
    %+  turn  l
    |=  =lv
    ^-  cw
    ?<  ?=(%octs -.lv)
    lv
  ::
  ++  promote
    |=  l=(list cw)
    ^-  (list lv)
    %+  turn  l
    |=  =cw
    ^-  lv
    ?<  ?=(%ref -.cw)
    cw
  ::
  ++  page-size  ^~((bex 16))
  ++  lia-reduce
    =*  run  engine
    |=  =seed
    =/  ast  (main:parser module.seed)  ::  TODO validate
    =/  =lia-state  [(conv:run ast ~) shop.seed]
    |^  ^-  result
    =^  res=result  lia-state  init
    ?.  ?=(%0 -.res)  res
    -:(monad-reduce script.seed)
    ::
    ++  monad-reduce
      |=  script=lia-monad
      |-  ^-  [result _lia-state]
      ?-    -.script
          mold-call
        |-  ^-  [result _lia-state]
        =/  res-engine  (invoke:run name.script args.script p.lia-state)
        ?:  ?=(%0 -.res-engine)
          [0+(promote out.res-engine) st.res-engine q.lia-state]
        ?:  ?=(%2 -.res-engine)  [2+~ lia-state]
        ?>  ?=(%func -.request.res-engine)
        =/  before-block-store  p.lia-state
        =.  p.lia-state  [~ +>.res-engine]
        =/  import-monad
          (~(got by import.seed) mod.res-engine name.res-engine)
        =^  res-lia  lia-state
          (monad-reduce (import-monad args.request.res-engine))
        ?.  ?=(%0 -.res-lia)  [res-lia lia-state]
        =.  shop.before-block-store
          (snoc shop.before-block-store (demote p.res-lia) +>.res-engine)
        $(p.lia-state before-block-store)  ::  type check?
      ::
          mold-read
        ?~  mem.p.lia-state  !!
        =,  u.mem.p.lia-state
        ?>  (lte (add [ptr len]:script) (mul n-pages page-size))
        [0+[%octs len.script (cut 3 [ptr len]:script buffer)]~ lia-state]
      ::
          mold-write
        ?~  mem.p.lia-state  !!  ::  p.store
        =,  u.mem.p.lia-state
        ?>  (lte (add [ptr len]:script) (mul n-pages page-size))
        :-  0+~
        lia-state(buffer.u.mem.p (sew 3 [ptr len src]:script buffer))
      ::
          mold-bind
        =^  res  lia-state  (monad-reduce p.script)
        ?.  ?=(%0 -.res)  [res lia-state]
        (monad-reduce (q.script p.res))
      ::
          mold-pure
        [0+p.script lia-state]
      ::
          mold-call-lia
        ?~  q.lia-state  [1+[name p]:script lia-state]
        [0+i.q.lia-state lia-state(q t.q.lia-state)]
      ::
      ==
    ::
    ++  init
      =|  init-shop=shop:engine
      |-  ^-  [result _lia-state]
      =/  engine-res=result:engine
        (instantiate:run p.lia-state(shop init-shop))
      ?:  ?=(%0 -.engine-res)  [0+~ st.engine-res q.lia-state]
      ?:  ?=(%2 -.engine-res)  [2+~ lia-state]
      ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
      ::
      ?>  ?=(%func -.request.engine-res)
      =/  store-b4-block  p.lia-state
      =.  p.lia-state  [~ +>.engine-res]
      =/  import-monad
        (~(got by import.seed) mod.engine-res name.engine-res)
      =^  res-lia-import  lia-state
        (monad-reduce (import-monad args.request.engine-res))
      ?.  ?=(%0 -.res-lia-import)  [res-lia-import lia-state]
      =.  shop.store-b4-block
        (snoc shop.store-b4-block (demote p.res-lia-import) +>.engine-res)
      $(p.lia-state store-b4-block)  ::  type check?
    --
  ::
  ++  lia-next
    |=  [next=(each lia-monad (list lv)) =seed]
    ^-  [result _seed]
    =.  seed
      ?-  -.next
        %&  seed(script [tag-bind script.seed _p.next])
        %|  seed(shop (snoc shop.seed p.next))
      ==
    [(lia-reduce seed) seed]
  ::
  ++  seed-init
    |=  [wasm=octs import=(map (pair cord cord) $-((list cw) lia-monad))]
    ^-  seed
    [wasm [tag-pure ~] ~ import]
  ::
  ++  get-1-num
    |=  [type=num-type:wasm-sur l=(list lv)]
    ^-  @
    ?>  ?=([* ~] l)
    =/  val  -.l
    ?>  =(type -.val)
    ?<  ?=(%octs -.val)
    +.val
  ::
  ++  get-octs
    |=  l=(list lv)
    ^-  octs
    ?>  ?=([* ~] l)
    =/  val  -.l
    ?>  ?=(%octs -.val)
    +.val
  ::
  --  ::  |monad
--