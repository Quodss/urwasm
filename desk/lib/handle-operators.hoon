::  Core with operator definitions
::
/-  *wasm
::  Overshadows sub, mul, add, div!
::
|%
++  complement-to-si
  |=  [base=@ n=@]
  ^-  @s
  =.  n  (mod n (bex base))
  =/  sign=?  (lth n (bex (dec base)))
  %+  new:si  sign
  ?:  sign  n
  (^sub (bex base) n)
::
++  si-to-complement
  |=  [base=@ s=@s]
  ^-  @
  ?:  (syn:si s)
    +:(old:si s)
  (^sub (bex base) +:(old:si s))
::
++  add
  |=  [type=valtype a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?-    type
      %i32
    :-  %i32
    ?>  &(?=(%i32 type.a) ?=(%i32 type.b))
    (~(sum fo (bex 32)) n.a n.b)
  ::
      %i64
    :-  %i64
    ?>  &(?=(%i64 type.a) ?=(%i64 type.b))
    (~(sum fo (bex 64)) n.a n.b)
  ::
      %f32
    :-  %f32
    ?>  &(?=(%f32 type.a) ?=(%f32 type.b))
    (add:rs n.a n.b)
  ::
      %f64
    :-  %f64
    ?>  &(?=(%f64 type.a) ?=(%f64 type.b))
    (add:rd n.a n.b)
  ==
::
++  sub
  |=  [type=valtype a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?-    type
      %i32
    :-  %i32
    ?>  &(?=(%i32 type.a) ?=(%i32 type.b))
    (~(dif fo (bex 32)) n.a n.b)
  ::
      %i64
    :-  %i64
    ?>  &(?=(%i64 type.a) ?=(%i64 type.b))
    (~(dif fo (bex 64)) n.a n.b)
  ::
      %f32
    :-  %f32
    ?>  &(?=(%f32 type.a) ?=(%f32 type.b))
    (sub:rs n.a n.b)
  ::
      %f64
    :-  %f64
    ?>  &(?=(%f64 type.a) ?=(%f64 type.b))
    (sub:rd n.a n.b)
  ==
::
++  mul
  |=  [type=valtype a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?-    type
      %i32
    :-  %i32
    ?>  &(?=(%i32 type.a) ?=(%i32 type.b))
    (~(pro fo (bex 32)) n.a n.b)
  ::
      %i64
    :-  %i64
    ?>  &(?=(%i64 type.a) ?=(%i64 type.b))
    (~(pro fo (bex 64)) n.a n.b)
  ::
      %f32
    :-  %f32
    ?>  &(?=(%f32 type.a) ?=(%f32 type.b))
    (mul:rs n.a n.b)
  ::
      %f64
    :-  %f64
    ?>  &(?=(%f64 type.a) ?=(%f64 type.b))
    (mul:rd n.a n.b)
  ==
::
++  lt
  |=  [type=valtype mode=(unit ?(%s %u)) a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  :-  %i32
  ?-    type
  ::
      %f64
    ?>  &(?=(%f64 type.a) ?=(%f64 type.b))
    ?:  (lth:rd n.a n.b)
      1
    0
  ::
      %f32
    ?>  &(?=(%f32 type.a) ?=(%f32 type.b))
    ?:  (lth:rs n.a n.b)
      1
    0
  ::
      %i32
    ?>  &(?=(%i32 type.a) ?=(%i32 type.b))
    ?>  ?=(^ mode)
    ?-  u.mode
      %u  ?:((lth n.a n.b) 1 0)
        %s
      ::  if both are positive or both a negative, then comparison is simple
      ::
      ?:  ?|  &((lth n.a ^~((bex 31))) (lth n.b ^~((bex 31))))
              &((gte n.a ^~((bex 31))) (gte n.b ^~((bex 31))))
          ==
        ?:((lth n.a n.b) 1 0)
      ::  otherwise a and b have different signs, check which is negative
      ::
      ?:((gth n.a n.b) 1 0)
    ==
  ::
      %i64
    ?>  &(?=(%i64 type.a) ?=(%i64 type.b))
    ?>  ?=(^ mode)
    ?-  u.mode
      %u  ?:((lth n.a n.b) 1 0)
        %s
      ::  if both are positive or both a negative, then comparison is simple
      ::
      ?:  ?|  &((lth n.a ^~((bex 63))) (lth n.b ^~((bex 63))))
              &((gte n.a ^~((bex 63))) (gte n.b ^~((bex 63))))
          ==
        ?:((lth n.a n.b) 1 0)
      ::  otherwise a and b have different signs, check which is negative
      ::
      ?:((gth n.a n.b) 1 0)
    ==
  ==
::
++  gt
  |=  [type=valtype mode=(unit ?(%s %u)) a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  (lt type mode b a)
::
++  le
  |=  [type=valtype mode=(unit ?(%s %u)) a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?>  =(type type.a)
  ?>  =(type type.b)
  ?:  =(n.a n.b)
    [%i32 1]
  (lt type mode a b)
::
++  ge
  |=  [type=valtype mode=(unit ?(%s %u)) a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  (le type mode b a)
::
++  div
  |=  [type=valtype mode=(unit ?(%s %u)) a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?-    type
  ::
      %f32
    :-  %f32
    ?>  &(?=(%f32 type.a) ?=(%f32 type.b))
    (div:rs n.a n.b)
  ::
      %f64
    :-  %f64
    ?>  &(?=(%f64 type.a) ?=(%f64 type.b))
    (div:rd n.a n.b)
  ::
      %i32
    ?>  ?=(^ mode)
    :-  %i32
    ?-  u.mode
      %u  (^div n.a n.b)
        %s
      =/  base=@  32
      %+  si-to-complement  base
      %+  fra:si
        (complement-to-si base n.a)
      (complement-to-si base n.b)
    ==
  ::
      %i64
    ?>  ?=(^ mode)
    :-  %i64
    ?-  u.mode
      %u  (^div n.a n.b)
        %s
      =/  base=@  64
      %+  si-to-complement  base
      %+  fra:si
        (complement-to-si base n.a)
      (complement-to-si base n.b)
    ==
  ==
::
++  ne
  |=  [type=valtype a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?>  =(type type.a)
  ?>  =(type type.b)
  :-  %i32
  ?:(!=(n.a n.b) 1 0)
::
++  eq
  |=  [type=valtype a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?>  =(type type.a)
  ?>  =(type type.b)
  :-  %i32
  ?:(=(n.a n.b) 1 0)
::
++  shr
  |=  [type=?(%i32 %i64) mode=?(%s %u) a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?>  &(=(type type.a) =(type type.b))
  ?-    mode
      %u
    ?-    type
        %i32
      :-  %i32
      (rsh [0 (mod n.b 32)] n.a)
    ::
        %i64
      :-  %i64
      (rsh [0 (mod n.b 64)] n.a)
    ==
  ::
      %s
    ?-    type
        %i32
      :-  %i32
      ?:  (lth n.a ^~((bex 31)))
        (rsh [0 (mod n.b 32)] n.a)
      ;:  ^add
        ^~((bex 31))
        (^sub ^~((dec (bex 31))) (dec (bex (^sub 31 (mod n.b 32)))))
        (rsh [0 (mod n.b 32)] (^sub n.a ^~((bex 31))))
      ==
    ::
        %i64
      :-  %i64
      ?:  (lth n.a ^~((bex 63)))
        (rsh [0 (mod n.b 64)] n.a)
      ;:  ^add
        ^~((bex 63))
        (^sub ^~((dec (bex 63))) (dec (bex (^sub 63 (mod n.b 64)))))
        (rsh [0 (mod n.b 64)] (^sub n.a ^~((bex 63))))
      ==
    ==
  ==
::
++  shl
  |=  [type=?(%i32 %i64) a=coin-wasm b=coin-wasm]
    ^-  coin-wasm
    ?>  &(=(type type.a) =(type type.b))
    ?-    type
        %i32
      :-  %i32
      (lsh [0 (mod n.b 32)] n.a)
    ::
        %i64
      :-  %i64
      (lsh [0 (mod n.b 64)] n.a)
    ==
::
++  or
  |=  [type=?(%i32 %i64) a=coin-wasm b=coin-wasm]
  ;;  coin-wasm
  ?>  =(type type.a)
  ?>  =(type type.b)
  :-  type
  (con n.a n.b)  ::  ATTENTION! loobean conjunction is boolean disjunction
::
++  xor
  |=  [type=?(%i32 %i64) a=coin-wasm b=coin-wasm]
  ;;  coin-wasm
  ?>  =(type type.a)
  ?>  =(type type.b)
  :-  type
  (mix n.a n.b)
::
++  and
  |=  [type=?(%i32 %i64) a=coin-wasm b=coin-wasm]
  ;;  coin-wasm
  ?>  =(type type.a)
  ?>  =(type type.b)
  :-  type
  (dis n.a n.b)  ::  ATTENTION! loobean disjunction is boolean conjunction
::
++  rotl
  |=  [type=?(%i32 %i64) a=coin-wasm b=coin-wasm]
  ^-  coin-wasm
  ?>  =(type type.a)
  ?>  =(type type.b)
  ?-    type 
      %i32
    :-  %i32
    (~(rol fe 5) 0 (mod n.b 32) n.a)
  ::
      %i64
    :-  %i64
    (~(rol fe 6) 0 (mod n.b 64) n.a)
  ==
::
++  popcnt
  |=  [type=?(%i32 %i64) a=coin-wasm]
  ?>  =(type type.a)
  ;;  coin-wasm  ::  compiler isn't convinced that [?(%i32 i64) @] nests under coin-wasm. why?
  :-  type
  =+  counter=0
  |-  ^-  @
  ?:  =(n.a 0)  counter
  $(n.a (^div n.a 2), counter (^add counter (mod n.a 2)))
::
++  eqz
  |=  [type=?(%i32 %i64) a=coin-wasm]
  ^-  coin-wasm
  ?>  =(type type.a)
  :-  %i32
  ?:(=(n.a 0) 1 0)
::
++  clz
  |=  [type=?(%i32 %i64) a=coin-wasm]
  ^-  coin-wasm
  ?>  =(type type.a)
  ?-    type
      %i32
    :-  %i32
    (^sub 32 (xeb n.a))
  ::
      %i64
    :-  %i64
    (^sub 64 (xeb n.a))
  ==
::
++  extend
  |=  [type=?(%i32 %i64) source=?(%8 %16 %32) mode=?(%s %u) a=coin-wasm]  ::  just one case of extend for now
  ^-  coin-wasm 
  ?-    type
      %i32
    :-  %i32
    ?-    mode
        %s
      (si-to-complement 32 (complement-to-si source n.a))
    ::
      %u  (mod n.a 32)
    ==
  ::
      %i64
    :-  %i64
    ?-    mode
        %s
      (si-to-complement 64 (complement-to-si source n.a))
    ::
      %u  (mod n.a 64)
    ==
  ==
::
++  ctz
  |=  [type=?(%i32 %i64) a=coin-wasm]
  ^-  coin-wasm
  ?>  =(type type.a)
  ?-    type
      %i32
    :-  %i32
    =+  counter=0
    |-  ^-  @
    ?:  =(counter 32)  counter
    ?:  =(1 (mod n.a 2))  counter
    $(n.a (^div n.a 2), counter +(counter))
  ::
      %i64
    :-  %i64
    =+  counter=0
    |-  ^-  @
    ?:  =(counter 64)  counter
    ?:  =(1 (mod n.a 2))  counter
    $(n.a (^div n.a 2), counter +(counter))
  ==
--