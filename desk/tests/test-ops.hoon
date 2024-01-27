/-  *engine
/+  *test
/+  op-def=runner-op-def
::
|%
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
        -:va.stack:((plain:simd:op-def [%avgr %i16 %u]) l(va.stack [v2 v1 va.stack.l]))
      :-  (can 4 (turn `(list @)`~[334 999 30.000 27.999 42 42.424 59.999 0] (lead 1)))
      (can 4 (turn `(list @)`~[732 1.234 23.423 23.456 2.435 23.566 52.677 0] (lead 1)))
  ==
--