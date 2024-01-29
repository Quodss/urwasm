/-  *engine
/+  *test
/+  op-def=runner-op-def
::
|%
++  test-int
  ;:  weld
    %+  expect-eq  ::  -2^(N-1) / -1 = {}
      !>  ~
      =,  op-def
      !>  %+  (bina:fetch:op-def [%div %i32 ~ %s])
            (en-si 32 (new:si | (bex 31)))
          (en-si 32 -1)
  ::
  ==
::
++  test-float
  ;:  weld
    %+  expect-eq
      !>  `@`1.077.936.128
      !>  (need ((bina:fetch:op-def [%add %f32]) .1 .2))
  ::
    %+  expect-eq
      !>  `@`1.051.372.203
      !>  (need ((bina:fetch:op-def [%div %f32 ~]) .1 .3))
  ::
    %+  expect-eq
      !>  `@`1.150.191.188
      !>  (need ((unar:fetch:op-def [%sqrt %f32]) .1300051))
  ::
    %+  expect-eq
      !>  `@`4.698.334.488.248.188.928
      !>  (need ((unar:fetch:op-def [%promote ~]) .1300051))
  ::
    %+  expect-eq
      !>  `@`4.653.432.500.322.828.288
      !>  (need ((unar:fetch:op-def [%promote ~]) .1300.051))
  ==
::
++  test-vec
  =|  l=local-state
  ;:  weld
    %+  expect-eq
      !>  (can 6 (turn ~[7.241.902.928.051.372.565 241.972.029.883.607] (lead 1)))
      !>
      =;  [v1=@ v2=@]
        =<  -:va.stack
        %-  (plain:simd:op-def [%avgr %i16 %u])
        l(va.stack [v2 v1 va.stack.l])
      :-
         %+  can  4
         %-  turn  :_  (lead 1)
         ~[334 999 30.000 27.999 42 42.424 59.999 0]
      %+  can  4
      %-  turn  :_  (lead 1)
      ~[732 1.234 23.423 23.456 2.435 23.566 52.677 0]
  ::
    %+  expect-eq
      !>  31
      !>
      =/  [v1=@ v2=@]  [1.000 1.000]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  1.068
      !>
      =/  [v1=@ v2=@]  [5.000 7.000]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  8.192
      !>
      =/  [v1=@ v2=@]  [16.383 16.384]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  0
      !>
      =/  [v1=@ v2=@]  [1 (dec (bex 16))]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  (dec (bex 16))
      !>
      =/  [v1=@ v2=@]  [65.535 32.767]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  0
      !>
      =/  [v1=@ v2=@]  [(dec (bex 16)) (dec (bex 16))]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  :: ::
    %+  expect-eq
      !>  32.766
      !>
      =/  [v1=@ v2=@]  [32.767 32.767]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ==
--