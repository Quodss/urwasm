/+  *lia-monad
/*  bin  %wasm  /tests/nanoapp/wasm
:-  %say  |=  *  :-  %noun
|^
=/  =seed:monad  (seed-init:monad bin ~)


=^  res  seed  (lia-next:monad &+get seed)
~&  res
=^  res  seed  (lia-next:monad &+poke-succ seed)
=^  res  seed  (lia-next:monad &+poke-succ seed)
=^  res  seed  (lia-next:monad &+get seed)
~&  res
=^  res  seed  (lia-next:monad &+poke-dec seed)
=^  res  seed  (lia-next:monad &+get seed)
~&  res
=^  res  seed  (lia-next:monad &+(poke-set 42) seed)
=^  res  seed  (lia-next:monad &+poke-succ seed)
=^  res  seed  (lia-next:monad &+get seed)
~&  res
~
::
++  get        `lia-monad:monad`(call:monad 'get' ~)
++  poke-succ  `lia-monad:monad`(call:monad 'succ' ~)
++  poke-dec   `lia-monad:monad`(call:monad 'dec' ~)
++  poke-set
  |=  new=@
  ^-  lia-monad:monad
  (call:monad 'set' i32+new ~)
--