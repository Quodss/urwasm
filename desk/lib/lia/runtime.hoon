/-  wasm
/-  lia
/-  engine
/+  run=runner-engine
/+  comp=lia-compiler
/+  line=lia-linearizer
::
|%
++  demote
  |=  l=(list value:line:lia)
  ^-  (list coin-wasm:wasm)
  %+  turn  l
  |=  v=value:line:lia
  ^-  coin-wasm:wasm
  ?<  ?=(%octs -.v)
  v
::
++  chain-name
  |=  [a=cord b=cord]
  ^-  cord
  =/  size=@  (met 3 a)
  ;:  add  
    a
    (lsh [3 size] '/')
    (lsh [3 +(size)] b)
  ==
::
++  get-export-global-con-i32
  |=  [=module:engine name=cord]
  ^-  @
  =/  map  (make-export-map:run export-section.module)
  =/  export-desc
    (~(got by map) name)
  ?>  ?=(%glob -.export-desc)
  =/  i-local=@  (sub i.export-desc (lent globs.import-section.module))
  =/  glob  (snag i-local global-section.module)
  ?>  ?=(%i32 v.glob)
  ?>  ?=([%const %i32 @] i.glob)
  +.p.i.glob
::
++  get-export-global-var-i32
  |=  [=module:engine name=cord globals=(list coin-wasm:wasm)]
  ^-  @
  =/  map  (make-export-map:run export-section.module)
  =/  export-desc
    (~(got by map) name)
  ?>  ?=(%glob -.export-desc)
  =/  i-local=@  (sub i.export-desc (lent globs.import-section.module))
  =/  glob  (snag i-local globals)
  ?>  ?=(%i32 -.glob)
  +.glob
::
+$  them-result
  $%  [%0 out=(list coin-wasm:wasm)]
  ::
      $:  %1
          [[mod=cord name=cord] =request:engine]
          $=  king-paused  %-  unit
          $:
            =module:engine
            mem=(unit [buffer=@ n-pages=@])         
            tables=(list (list $>(%ref coin-wasm:wasm))) 
            globals=(list coin-wasm:wasm)
      ==  ==
  ::
      [%2 ~]
  ==
::
+$  lord-result
  $%
    $<(%0 them-result)
    [%0 out=(list coin-wasm:wasm) king=store:engine]
  ==
::
++  lia-main
  |=  [=input:tree:lia vals=(list (list value:line:lia))]
  ^-  result:line:lia
  =/  input-line  (main:line input)
  =/  king  (main:comp [module code ext import]:input-line)
  =/  =lord-result
    (lord module.input-line king vals shop.input-line import.input-line)
  ?-    -.lord-result
      %2  lord-result
  ::
      %1
    :-  %1
    ?~  king-paused.lord-result
      [%serf +<.lord-result]
    ?>  ?=(%lia mod.lord-result)
    ?>  ?=(%func -.request.lord-result)
    ?>  ((sane %tas) name.lord-result)
    =/  types  p:(~(got by import.input-line) name.lord-result)
    =,  lord-result
    :+  %king  name 
    (extract types args.request u.king-paused)
  ::
      %0
    :-  %0
    =,  lord-result
    (extract q.type:(rear code.input-line) out +.king)
  ::
  ==
::
++  extract
  |=  $:  types=(list value-type:line:lia)
          coins=(list coin-wasm:wasm)
          =module:engine
          mem=(unit [buffer=@ n-pages=@])         
          tables=(list (list $>(%ref coin-wasm:wasm))) 
          globals=(list coin-wasm:wasm)
      ==
  |-  ^-  (list value:line:lia)
  ?:  &(?=(@ types) ?=(@ coins))  ~
  ?>  &(?=(^ types) ?=(^ coins))
  :_  $(types t.types, coins t.coins)
  ?.  ?=(%octs i.types)
    ?>  =(i.types -.i.coins)
    ?<  ?=(%ref -.i.coins)
    i.coins
  ?>  ?=(%i32 -.i.coins)
  =/  out=(pole coin-wasm:wasm)
    =<  -  %-  wasm-need:run
    (invoke:run 'get-space-ptr' ~[i.coins] ~ module mem tables globals)
  ?>  ?=([[%i32 ptr=@] ~] out)
  ?>  (gth ptr.out 0)
  ?:  =(minus-one-32:comp ptr.out)  [%octs 0 0]
  ?>  ?=(^ mem)
  =/  len=@   (cut 3 [ptr.out 4] buffer.u.mem)
  =/  data=@  (cut 3 [(add ptr.out 4) len] buffer.u.mem)
  [%octs len data]
::
++  lord
  |=  $:  serf-mod=module:wasm
          king-mod=module:wasm
          input=(list (list value:line:lia))
          lia-shop=(list (list value:line:lia))
          import=(map term block-type:line:lia)
      ==
  ^-  lord-result
  =/  them=[king=store:engine serf=store:engine]
      :-  +:(wasm-need:run (prep:run king-mod ~))  ::  king store initialized, should not block
          (conv:run serf-mod ~)                    ::  serf store, uninitialized
  |^
  =^  res=them-result  them  serf-init
  ?.  ?=(%0 -.res)  res
  =/  act-func-idx=@
    (get-export-global-con-i32 module.king.them 'act-0-func-idx')
  =/  n-funcs=@
    (get-export-global-con-i32 module.king.them 'n-funcs')
  ?<  =(0 n-funcs)
  ?>  =(n-funcs (lent input))
  =/  idx-last  (dec (add act-func-idx n-funcs))
  |-  ^-  lord-result
  ?:  =(act-func-idx idx-last)
    =^   res=them-result  them  (king-invoke-act act-func-idx -.input)
    ?.  ?=(%0 -.res)  res
    [%0 out.res king.them]
  =^   res=them-result  them  (king-invoke-act act-func-idx -.input)
  ?.  ?=(%0 -.res)  res
  %=  $
    input  +.input
    act-func-idx  +(act-func-idx)
    them  king-invoke-clear
  ==
  ::
  ++  serf-init
    =|  serf-shop=shop:engine
    |-  ^-  [them-result _them]
    =*  serf-loop  $
    =/  serf-engine-res=result:engine
      (instantiate:run serf.them(shop serf-shop))
    ?:  ?=(%0 -.serf-engine-res)  [[%0 ~] king.them st.serf-engine-res]
    ?:  ?=(%2 -.serf-engine-res)  [[%2 ~] them]
    ::  serf-engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?.  ?=(%func -.request.serf-engine-res)
      ?~  lia-shop  [[%1 +<.serf-engine-res ~] them]
      %=  serf-loop
        lia-shop   t.lia-shop
        serf-shop  (snoc serf-shop [(demote i.lia-shop) +>.serf-engine-res])
      ==
    =/  name-chained=cord  (chain-name +<-.serf-engine-res)
    =/  serf-b4-block  serf.them
    =.  serf.them  [~ +>.serf-engine-res]
    =^  king-res=them-result  them
      (king-invoke-name name-chained args.request.serf-engine-res)
    ?.  ?=(%0 -.king-res)  [king-res them]  ::  them will be discarded
    %=  serf-loop
      serf.them  serf-b4-block
      serf-shop  (snoc serf-shop [out.king-res +.serf.them])
    ==
  ::
  ++  king-invoke-idx
    |=  [idx=@ in=(list coin-wasm:wasm)]
    ^-  [them-result _them]
    =/  king-engine-res=result:engine
      (invoke-id:run idx in king.them)
    ?:  ?=(%0 -.king-engine-res)
      [[%0 out.king-engine-res] st.king-engine-res serf.them]
    ?:  ?=(%2 -.king-engine-res)
      [[%2 ~] them]
    ::  king-engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?>  ?=(%func -.request.king-engine-res)
    ?+    mod.king-engine-res  ~|(%weird-mod !!)
        %lia
      ?~  lia-shop  [[%1 +<.king-engine-res `+>.king-engine-res] them]
      =/  types  q:(~(got by import) name.king-engine-res)
      ?>  =(types (turn i.lia-shop head))
      %=    $
          lia-shop  t.lia-shop
          shop.king.them
        %+  snoc  shop.king.them
        :-  ~
        =*  pause  +>.king-engine-res
        =/  off=@
          %^  get-export-global-var-i32  module.pause
            'space-start'
          globals.king-engine-res
        =/  data-idx=@
          %^  get-export-global-var-i32  module.pause
            'space-clue'
          globals.king-engine-res
        =/  target-data  (snag data-idx data-section.module.pause)
        ?>  ?=(%pass -.target-data)
        =/  targets=(list @)
          %-  turn  :_  (curr add off)
          (rope:simd:run 3 p.b.target-data q.b.target-data)
        |-  ^+  pause
        ?:  &(?=(@ targets) ?=(@ i.lia-shop))
          pause
        ?>  &(?=(^ targets) ?=(^ i.lia-shop))
        ?.  ?=(?(%octs %v128) -.i.i.lia-shop)
          =/  ptr=@  (mul i.targets 8)
          =/  [buffer=@ n-pages=@]  (need mem.pause)
          %=    $
              targets  t.targets
              i.lia-shop  t.i.lia-shop
              mem.pause
            :-  ~
            :_  n-pages
            `@ux`(sew 3 [ptr 4 +.i.i.lia-shop] buffer)
          ==
        =/  =octs
          ?-  -.i.i.lia-shop
            %octs  +.i.i.lia-shop
            %v128  [16 +.i.i.lia-shop]
          ==
        =/  [out=(pole coin-wasm:wasm) king-pause-new=store:engine]
          %-  wasm-need:run
          (invoke:run 'set-octs-ext' ~[[%i32 p.octs] [%i32 i.targets]] ~ pause)
        ?>  ?=([[%i32 ptr=@] ~] out)
        ?:  =(minus-one-32:comp ptr.out)
          ?>  =([0 0] octs)
          %=  $
            targets  t.targets
            i.lia-shop  t.i.lia-shop
            pause  +.king-pause-new
          ==
        %=    $
            targets  t.targets
            i.lia-shop  t.i.lia-shop
            pause
          %=    +.king-pause-new
              mem
            =/  [buffer=@ n-pages=@]  (need mem.king-pause-new)
            :-  ~  :_  n-pages
            (sew 3 [ptr.out p.octs q.octs] buffer)
          ==
        ==
      ==
    ::
        %memio
      =/  args=(pole coin-wasm:wasm)  args.request.king-engine-res
      ?>  ?=([from=[%i32 @] to=[%i32 @] len=[%i32 @] ~] args)
      =/  [from=@ to=@ len=@]  [+.from +.to +.len]:args
      ?+    name.king-engine-res  ~|(%weird-memio !!)
          %read
        %=    $
            shop.king.them
          %+  snoc  shop.king.them
          :-  ~
          %=    +>.king-engine-res
              mem
            ?>  ?=(^ mem.king-engine-res)
            =*  king-mem  buffer.u.mem.king-engine-res
            ?>  ?=(^ mem.serf.them)  ::  assert: local memory of serf for memio funcs, revisit?
            =*  serf-mem  buffer.u.mem.serf.them
            :-  ~  :_  n-pages.u.mem.king-engine-res
            (sew 3 [to len (cut 3 [from len] serf-mem)] king-mem)
          ==
        ==
      ::
          %write
        %=    $
            shop.king.them
          (snoc shop.king.them `+>.king-engine-res)
        ::
            mem.serf.them
          ?>  ?=(^ mem.king-engine-res)
          =*  king-mem  buffer.u.mem.king-engine-res
          ?>  ?=(^ mem.serf.them)  ::  assert: local memory of serf for memio funcs, revisit?
          =*  serf-mem  buffer.u.mem.serf.them
          :-  ~  :_  n-pages.u.mem.serf.them
          (sew 3 [to len (cut 3 [from len] king-mem)] serf-mem)
        ==
      ::
      ==
    ::
        %serf
      =/  king-b4-block  king.them
      =.  king.them  [~ +>.king-engine-res]
      =^  serf-res=them-result  them
        =,  king-engine-res
        (serf-invoke-name name args.request)
      ?.  ?=(%0 -.serf-res)  [serf-res them]  ::  them will be discarded
      %=    $
          king.them
        %=  king-b4-block
          shop  (snoc shop.king-b4-block [out.serf-res +.king.them])
        ==
      ==
    ::
    ==
  ::
  ++  king-invoke-name
    |=  [name=cord in=(list coin-wasm:wasm)]
    ^-  [them-result _them]
    =/  idx=@  (find-func-id:run name module.king.them)
    (king-invoke-idx idx in)
::
  ++  king-invoke-act
    |=  [i=@ in=(list value:line:lia)]
    =/  target=@  0
    |-  ^-  [them-result _them]
    ?~  in
      (king-invoke-idx i ~)
    ?.  ?=(?(%octs %v128) -.i.in)
      =/  ptr=@  (mul target 8)
      =/  [buffer=@ n-pages=@]  (need mem.king.them)
      %=  $
        target  +(target)
        in  t.in
        mem.king.them  `[(sew 3 [ptr 4 +.i.in] buffer) n-pages]
      ==
    =/  =octs
      ?-  -.i.in
        %octs  +.i.in
        %v128  [16 +.i.in]
      ==
    =/  [out=(pole coin-wasm:wasm) king-temp-new=store:engine]
      %-  wasm-need:run
      (invoke:run 'set-octs-ext' ~[[%i32 p.octs] [%i32 target]] king.them)
    ?>  ?=([[%i32 ptr=@] ~] out)
    ?:  =(minus-one-32:comp ptr.out)
      ?>  =([0 0] octs)
      %=  $
        target  +(target)
        in  t.in
        king.them  king-temp-new
      ==
    %=    $
        target  +(target)
        in  t.in
        king.them
      %=    king-temp-new
          mem
        =/  [buffer=@ n-pages=@]  (need mem.king-temp-new)
        :-  ~  :_  n-pages
        (sew 3 [ptr.out p.octs q.octs] buffer)
      ==
    ==
  ::
  ++  king-invoke-clear
    ^+  them
    =+  res=(king-invoke-name 'clear-space' ~)
    ?>  ?=([%0 ~] -.res)
    +.res
  ::
  ++  serf-invoke-name
    |=  [name=cord in=(list coin-wasm:wasm)]
    ^-  [them-result _them]
    =/  idx=@  (find-func-id:run name module.serf.them)
    (serf-invoke-idx idx in)
  ::
  ++  serf-invoke-idx
    |=  [idx=@ in=(list coin-wasm:wasm)]
    ^-  [them-result _them]
    =/  serf-engine-res=result:engine
      (invoke-id:run idx in serf.them)
    ?:  ?=(%0 -.serf-engine-res)
      [[%0 out.serf-engine-res] king.them st.serf-engine-res]
    ?:  ?=(%2 -.serf-engine-res)
      [[%2 ~] them]
    ::  serf-engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?.  ?=(%func -.request.serf-engine-res)
      ?~  lia-shop  [[%1 +<.serf-engine-res ~] them]
      %=    $
          lia-shop
        t.lia-shop
      ::
          shop.serf.them
        (snoc shop.serf.them [(demote i.lia-shop) +>.serf-engine-res])
      ::
      ==
    =/  name-chained=cord  (chain-name [mod name]:serf-engine-res)
    =/  serf-b4-block  serf.them
    =.  serf.them  [~ +>.serf-engine-res]
    =^  king-res=them-result  them
      (king-invoke-name name-chained args.request.serf-engine-res)
    ?.  ?=(%0 -.king-res)  [king-res them]  ::  them will be discarded
    %=    $
        serf.them
      %=  serf-b4-block
        shop  (snoc shop.serf-b4-block [out.king-res +.serf.them])
      ==
    ==
  ::  
  --
::
--