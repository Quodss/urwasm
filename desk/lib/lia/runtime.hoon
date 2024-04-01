/-  wasm
/-  lia
/-  engine
/+  run=runner-engine
::
|%
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
++  get-export-global-i32
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
+$  lord-result
  $%  [%0 out=(list coin-wasm:wasm)]
      [%1 [[cord cord] =request:engine]]
      [%2 ~]
  ==
::
++  lord
  |=  $:  serf-mod=module:wasm
          king-mod=module:wasm
          lia-shop=(list (list coin-wasm:wasm))
      ==
  ^-  lord-result
  =/  them=[king=store:engine serf=store:engine]
      :-  +:(wasm-need:run (prep:run king-mod ~))  ::  king store initialized, should not block
          (conv:run serf-mod ~)                ::  serf store, uninitialized
  |^
  =^  res=lord-result  them  serf-init
  ?.  ?=(%0 -.res)  res
  =/  act-func-idx=@
    (get-export-global-i32 module.king.them 'act-0-func-idx')
  =/  n-funcs=@
    (get-export-global-i32 module.king.them 'n-funcs')
  ?<  =(0 n-funcs)
  =/  idx-last  (dec (add act-func-idx n-funcs))
  |-  ^-  lord-result
  ?:  =(act-func-idx idx-last)
    -:(king-invoke-act act-func-idx)
  =^   res=lord-result  them  (king-invoke-act act-func-idx)
  ?.  ?=(%0 -.res)  res
  %=  $
    them          king-invoke-clear
    act-func-idx  +(act-func-idx)
  ==
  ::
  ++  serf-init
    =|  serf-shop=shop:engine
    |-  ^-  [lord-result _them]
    =*  serf-loop  $
    =/  serf-engine-res=result:engine
      (instantiate:run serf.them(shop serf-shop))
    ?:  ?=(%0 -.serf-engine-res)  [[%0 ~] king.them st.serf-engine-res]
    ?:  ?=(%2 -.serf-engine-res)  [[%2 ~] them]
    ::  serf-engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?.  ?=(%func -.request.serf-engine-res)
      ?~  lia-shop  [[%1 +<.serf-engine-res] them]
      %=  serf-loop
        lia-shop   t.lia-shop
        serf-shop  (snoc serf-shop [i.lia-shop +>.serf-engine-res])
      ==
    =/  name-chained=cord  (chain-name +<-.serf-engine-res)
    =/  serf-b4-block  serf.them
    =.  serf.them  [~ +>.serf-engine-res]
    =^  king-res=lord-result  them
      (king-invoke-name name-chained args.request.serf-engine-res)
    ?.  ?=(%0 -.king-res)  [king-res them]  ::  them will be discarded
    %=  serf-loop
      serf.them  serf-b4-block
      serf-shop  (snoc serf-shop [out.king-res +.serf.them])
    ==
  ::
  ++  king-invoke-idx
    |=  [idx=@ in=(list coin-wasm:wasm)]
    ^-  [lord-result _them]
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
      ?~  lia-shop  [[%1 +<.king-engine-res] them]
      %=  $
        lia-shop  t.lia-shop
        shop.king.them  (snoc shop.king.them [i.lia-shop +>.king-engine-res])
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
      =^  serf-res=lord-result  them
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
    ^-  [lord-result _them]
    =/  idx=@  (find-func-id:run name module.king.them)
    (king-invoke-idx idx in)
  ::
  ++  king-invoke-act
    |=  i=@
    ^-  [lord-result _them]
    (king-invoke-idx i ~)
  ::
  ++  king-invoke-clear
    ^+  them
    =+  res=(king-invoke-name 'clear-space' ~)
    ?>  ?=([%0 ~] -.res)
    +.res
  ::
  ++  serf-invoke-name
    |=  [name=cord in=(list coin-wasm:wasm)]
    ^-  [lord-result _them]
    =/  idx=@  (find-func-id:run name module.serf.them)
    (serf-invoke-idx idx in)
  ::
  ++  serf-invoke-idx
    |=  [idx=@ in=(list coin-wasm:wasm)]
    ^-  [lord-result _them]
    =/  serf-engine-res=result:engine
      (invoke-id:run idx in serf.them)
    ?:  ?=(%0 -.serf-engine-res)
      [[%0 out.serf-engine-res] king.them st.serf-engine-res]
    ?:  ?=(%2 -.serf-engine-res)
      [[%2 ~] them]
    ::  serf-engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?.  ?=(%func -.request.serf-engine-res)
      ?~  lia-shop  [[%1 +<.serf-engine-res] them]
      %=  $
        lia-shop   t.lia-shop
        shop.serf.them  (snoc shop.serf.them [i.lia-shop +>.serf-engine-res])
      ==
    =/  name-chained=cord  (chain-name +<-.serf-engine-res)
    =/  serf-b4-block  serf.them
    =.  serf.them  [~ +>.serf-engine-res]
    =^  king-res=lord-result  them
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