/-  ast=wasm
::
|%
++  main
  |=  wasm=@
  ^-  module.ast
  (rash wasm module.r)
::  r: parsing rules
:: 
++  r
  |%
  ++  module
    ;~  pfix
      (jest '\00asm\01\00\00\00')
      %+  ifix  [(star unrecognized-section) (star unrecognized-section)]
      ;~  (glue (star unrecognized-section))
        type-section
        function-section
        export-section
        code-section
      ==
    ==
  ::  Unsigned integer rule
  ::
  ++  u-n
    |=  n-bits=@
    =*  this  $
    %+  knee  *@u
    |.  ~+
    ;~  pose
      ::  multiple byte case
      ::
      ?:  (lte n-bits 7)  fail
      %+  cook
        |=  [n=@u m=@u]
        ^-  @u
        %+  add
          (mul 128 m)
        (sub n 128)
      ;~  plug
        (cook ,@u (shim 128 255))
        this(n-bits (sub n-bits 7))
      ==
      ::  single byte case
      ::
      (cook ,@ (shim 0 (dec (bex (min n-bits 7)))))
    ==
    ::
    ++  s-n
      |=  n-bits=@
      =*  this  $
      %+  knee  *@s
      |.  ~+
      ;~  pose
        ::  single byte: positive
        ::
        (cook |=(n=@ (new:si & n)) (shim 0 (dec (bex (min (dec n-bits) 6)))))
        ::  single byte: negative
        ::
        %+  cook
          |=  n=@
          =,  si
          (dif (new & n) --128)
        ;~  simu
          (shim 64 127)
          (shim (sub 128 (min 128 (bex (dec n-bits)))) 127)
        ==
        ::  multiple bytes
        ::
        ?:  (lte n-bits 7)  fail
        %+  cook
          |=  [n=@s m=@s]
          =,  si
          (sum (dif n --128) (pro --128 m))
        ;~  plug
          (cook |=(n=@ (new:si & n)) (shim 128 255))
          this(n-bits (sub n-bits 7))
        ==
      ==
    ::
    ++  bild
      |*  [vex=edge gat=_=>(rule |*(* *rule))]
      ?~  q.vex
      vex
      %.  [vex (gat p.u.q.vex)]
      (comp |*([a=* b=*] b))
    ::
    ++  vec           ::  vec-i-32 == (vec (i-n 32))
      |*  rul=rule
      ;~  bild
        (u-n 32)
        |=  n=@
        (stun [n n] rul)
      ==
    ++  f64
      %+  cook
        |=  =(list @)
        ;;  @rd
        %+  can  3
        %+  fuse  (reap 8 1)
        list
      (stun [8 8] next)
  ::
    ++  f32
      %+  cook
        |=  =(list @)
        ;;  @rs
        %+  can  3
        %+  fuse  (reap 4 1)
        list
      (stun [4 4] next)
  ::
    ++  fuse                                                ::  from ~paldev
      |*  [a=(list) b=(list)]
      ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
      ?~  a  ~
      ?~  b  ~
      :-  [i.a i.b]
      $(a t.a, b t.b)
    ::
    ++  unrecognized-bytes  ::  we recognize 0x1, 0x3, 0x7, 0xa
      ^~
      ^-  (list char)
      %+  skip  (gulf '\00' '\ff')
      |=  c=char
      (~(has in (silt "\01\03\07\0a")) c)
    ::
    ++  unrecognized-section  ;~(plug (mask uncrecognized-bytes) (vec next))
    ++  numtype
      %+  cook  valtype:handle
      (mask "\7f\7e\7d\7c")
    ::
    ++  type-section  !!  ::  TODO
    ++  function-section
      %+  cook  function-section.ast
      ;~  pfix
        ;~(plug (just '\03') (u-n 32))
        (vec (u-n 32))
      ==
  --
++  handle
  |%
  ++  valtype
    |=  c=char
    ^-  valtype
    ?+  c  !!
      %0x7f  %i32
      %0x7e  %i64
      %0x7d  %f32
      %0x7c  %f64
    ==
  --
--