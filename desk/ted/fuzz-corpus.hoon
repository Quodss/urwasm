/-  spider
/+  strandio
/+  wasm=wasm-lia
=,  strand=strand:spider
|^  ^-  thread:spider
|=  arg=vase
=+  !<([~ l=(list octs)] arg)
=/  idx=@  0
=|  l-out=(list [@ octs])
=/  m  (strand ,vase)
|-  ^-  form:m
=*  this-thread  $
?~  l  (pure:m !>(l-out))
;<  ~  bind:m  (send-thread:strandio %wasm (ted i.l) /compute/(scot %ud idx))
|=  tin=strand-input:strand
?+    in.tin  `[%skip ~]
    ~  `[%wait ~]
::
    [~ %sign [%compute @ ~] %khan %arow *]
  ?.  =(`idx (slaw %ud i.t.wire.u.in.tin))
    ~&  >>  %skipped
    `[%skip ~]
  =/  res=(avow:khan cage)  p.sign-arvo.u.in.tin
  ?:  ?=(%| -.res)
    ~&  bailed+idx
    `[%cont this-thread(idx +(idx), l t.l)]
  ~&  heard-from+idx
  `[%cont this-thread(idx +(idx), l t.l, l-out [idx^i.l l-out])]
==
::
++  ted
  |=  bin=octs
  =/  m  (strand ,vase)
  ^-  form:m
  ;<  ~  bind:m  (sleep:strandio `@dr`2)
  %-  pure:m
  !>
  %^  (run-once:wasm ,~ *)  [bin `~]  %$
  =/  m1  (script:lia-sur:wasm ,~ *)
  (return:m1 ~)
--