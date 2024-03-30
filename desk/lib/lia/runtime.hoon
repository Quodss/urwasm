/-  wasm
/-  lia
/-  engine
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
+$  lord-result
  $%  [%0 out=(list coin-wasm:wasm)]
      [%1 [[cord cord] =request:engine]]
      [%2 ~]
  ==
::
++  serf-bind
  |=  [a=result:engine b=$-((quip coin-wasm store) lord-result)]
  ^-  lord-result
  ?.  ?=(%1 -.a)
    ?-  -.a
      %0  (b +.a)
      %2  [%2 ~]
    ==
  ::  a == [%1 [[mod=cord name=cord] =request] mem tables globals]
  ::
  ?.  ?=(%func -.request.a)
    [%1 +<.a]
  
  ==
::
++  lord
  |=  $:  serf-mod=module:wasm
          king-mod=module:wasm
          lia-shop=(list (list coin-wasm))
      ==
  ^-  lord-result
  =+  king=+:(wasm-need (prep king-mod ~))
  |^
  =+  serf-res=(serf-prep serf-mod lia-shop)
  !!
  ::
  ++  serf-prep
    |=  [serf-mod=module:wasm lia-shop=(list (list coin-wasm))]
    =|  serf-shop=shop:engine
    |-  ^-  (each [store:engine _lia-shop] $<(%0 lord-result))
    =*  serf-loop  $
    =/  serf-res  (prep serf-mod serf-shop)
    ?:  ?=(%0 -.serf-res)  [%& st.serf-res lia-shop]
    ?:  ?=(%2 -.serf-res)  [%| %2 ~]
    ::  serf-res = [%1 [[mod=cord name=cord] =request] mem tables globals]
    ::
    ?.  ?=(%func -.request.serf-res)
      ?~  lia-shop  [%| %1 +<.serf-res]
      %=  serf-loop
        lia-shop   t.lia-shop
        serf-shop  (snoc serf-shop [i.lia-shop +>.serf-res])
      ==
    =/  name-chained=cord  (chain-name +<-.serf-res)
    ::  update king and/or serf-res
    ::
    =^  result=(each (list coin-wasm) $<(%0 result:engine))  .
      =*  subj  .
      |-  ^-  [(each (list coin-wasm) $<(%0 result:engine)) _subj]
      =*  king-loop  $
      =/  king-res
        %^  invoke  name-chained
          args.request.serf-res
        king
      ?:  ?=(%0 -.king-res)
        [[%& out.king-res] subj(king st.king-res)]
      ?:  ?=(%2 -.king-res)
        [[%| %2 ~] subj]
      ::  king-res = [%1 [[mod=cord name=cord] =request] mem tables globals]
      ::
      ?>  ?=(%func -.request.king-res)
      ?+    mod.king-res  ~|(%weird-mod !!)
          %lia
        ?~  lia-shop  [[%| king.res] subj]
        %=  king-loop
          lia-shop   t.lia-shop
          shop.king  (snoc shop.king [i.lia-shop +>.king-res])
        ==
      ::
          %memio
        =/  args=(pole coin-wasm:wasm)  args.request.king-res
        ?>  ?=([from=* to=* len=* ~] args)
        ?+    name.king-res  ~|(%weird-memio !!)
            %read
          %=    king-loop
              shop.king
            %+  snoc  shop.king
            :-  ~  :_  [tables globals]:king-res
            ?>  ?=(^ mem.king-res)
            =*  king-mem  buffer.u.mem.king-res
            ?>  ?=(^ mem.serf-res)
            =*  serf-mem  buffer.u.mem.serf-res
            :-  ~  :_  n-pages.u.mem.king-res
            =,  args
            (sew 3 [to len (cut 3 [from len] serf-mem)] king-mem)
          ==
        ::
            %write
          %=    king-loop
              shop.king
            %+  snoc  shop.king
            `[mem tables globals]:king-res
          ::
              mem.serf-res
            ?>  ?=(^ mem.king-res)
            =*  king-mem  buffer.u.mem.king-res
            ?>  ?=(^ mem.serf-res)
            =*  serf-mem  buffer.u.mem.serf-res
            :-  ~  :_  n-pages.u.mem.serf-res
            =,  args
            (sew 3 [to len (cut 3 [from len] king-mem)] serf-mem)
          ==
        ::
        ==
      ::
          %serf
        !!  ::  construct transient serf store and invoke?
      ::
      ==
    ?.  -.result
      ?:  ?=(%2 -.p.result)
        [%| %2 ~]
      [%| %1 +<.p.result]
    serf-loop(serf-shop (snoc serf-shop p.result))
  ::
  --
--