::  Mapping of opcodes of non-prefixed instructions with
::  no immediate arguments to $instructions.
::
/-  *wasm
=-  |=  op=char
    ^-  (unit instruction)
    (~(get by m) op)
^~  ^=  m
^-  (map char instruction)
|^
=|  =(map char instruction)
=+  op=;;(char 0)
|-  ^+  map
?:  =(256 op)  map
=+  mayb=(link op)
=?  map  ?=(^ mayb)
  (~(put by map) op u.mayb)
$(op +(op))
::
++  link
  |=  op=char
  ^-  (unit instruction)
  ?+  op  ~
    %0x0   `[%unreachable ~]
    %0x1   `[%nop ~]
    %0xf   `[%return ~]
    %0x1a  `[%drop ~]
    %0x1b  `[%select ~]
    %0xa7  `[%wrap ~]
    %0xb6  `[%demote ~]
    %0xbb  `[%promote ~]
    %0xd1  `[%ref-is-null ~]
  ::
      eqz-opcodes
    :-  ~
    :-  %eqz
    ?-  op
      %0x45  %i32
      %0x50  %i64
    ==
  ::
      eq-opcodes
    :-  ~
    :-  %eq
    ?-  op
      %0x46  %i32
      %0x51  %i64
      %0x5b  %f32
      %0x61  %f64
    ==
  ::
      ne-opcodes
    :-  ~
    :-  %ne
    ?-  op
      %0x47  %i32
      %0x52  %i64
      %0x5c  %f32
      %0x62  %f64
    ==
  ::
      lt-opcodes
    :-  ~
    :-  %lt
    ?-  op
      %0x48  [%i32 `%s]
      %0x49  [%i32 `%u]
      %0x53  [%i64 `%s]
      %0x54  [%i64 `%u]
      %0x5d  [%f32 ~]
      %0x63  [%f64 ~]
    ==
  ::
      gt-opcodes
    :-  ~
    :-  %gt
    ?-  op
      %0x4a  [%i32 `%s]
      %0x4b  [%i32 `%u]
      %0x55  [%i64 `%s]
      %0x56  [%i64 `%u]
      %0x5e  [%f32 ~]
      %0x64  [%f64 ~]
    ==
  ::
      le-opcodes
    :-  ~
    :-  %le
    ?-  op
      %0x4c  [%i32 `%s]
      %0x4d  [%i32 `%u]
      %0x57  [%i64 `%s]
      %0x58  [%i64 `%u]
      %0x5f  [%f32 ~]
      %0x65  [%f64 ~]
    ==
  ::
      ge-opcodes
    :-  ~
    :-  %ge
    ?-  op
      %0x4e  [%i32 `%s]
      %0x4f  [%i32 `%u]
      %0x59  [%i64 `%s]
      %0x5a  [%i64 `%u]
      %0x60  [%f32 ~]
      %0x66  [%f64 ~]
    ==
  ::
      clz-opcodes
    :-  ~
    :-  %clz
    ?-  op
      %0x67  %i32
      %0x79  %i64
    ==
  ::
      ctz-opcodes
    :-  ~
    :-  %ctz
    ?-  op
      %0x68  %i32
      %0x7a  %i64
    ==
  ::
      popcnt-opcodes
    :-  ~
    :-  %popcnt
    ?-  op
      %0x69  %i32
      %0x7b  %i64
    ==
  ::
      add-opcodes
    :-  ~
    :-  %add
    ?-  op
      %0x6a  %i32
      %0x7c  %i64
      %0x92  %f32
      %0xa0  %f64
    ==
  ::
      sub-opcodes
    :-  ~
    :-  %sub
    ?-  op
      %0x6b  %i32
      %0x7d  %i64
      %0x93  %f32
      %0xa1  %f64
    ==
  ::
      mul-opcodes
    :-  ~
    :-  %mul
    ?-  op
      %0x6c  %i32
      %0x7e  %i64
      %0x94  %f32
      %0xa2  %f64
    ==
  ::
      div-opcodes
    :-  ~
    :-  %div
    ?-  op
      %0x6d  [%i32 `%s]
      %0x6e  [%i32 `%u]
      %0x7f  [%i64 `%s]
      %0x80  [%i64 `%u]
      %0x95  [%f32 ~]
      %0xa3  [%f64 ~]
    ==
  ::
      rem-opcodes
    :-  ~
    :-  %rem
    ?-  op
      %0x6f  [%i32 %s]
      %0x70  [%i32 %u]
      %0x81  [%i64 %s]
      %0x82  [%i64 %u]
    ==
  ::
      and-opcodes
    :-  ~
    :-  %and
    ?-  op
      %0x71  %i32
      %0x83  %i64
    ==
  ::
      or-opcodes
    :-  ~
    :-  %or
    ?-  op
      %0x72  %i32
      %0x84  %i64
    ==
  ::
      xor-opcodes
    :-  ~
    :-  %xor
    ?-  op
      %0x73  %i32
      %0x85  %i64
    ==
  ::
      shl-opcodes
    :-  ~
    :-  %shl
    ?-  op
      %0x74  %i32
      %0x86  %i64
    ==
  ::
      shr-opcodes
    :-  ~
    :-  %shr
    ?-  op
      %0x75  [%i32 %s]
      %0x76  [%i32 %u]
      %0x87  [%i64 %s]
      %0x88  [%i64 %u]
    ==
  ::
      rotl-opcodes
    :-  ~
    :-  %rotl
    ?-  op
      %0x77  %i32
      %0x89  %i64
    ==
  ::
      rotr-opcodes
    :-  ~
    :-  %rotr
    ?-  op
      %0x78  %i32
      %0x8a  %i64
    ==
  ::
      abs-opcodes
    :-  ~
    :-  %abs
    ?-  op
      %0x8b  %f32
      %0x99  %f64
    ==
  ::
      neg-opcodes
    :-  ~
    :-  %neg
    ?-  op
      %0x8c  %f32
      %0x9a  %f64
    ==
  ::
      ceil-opcodes
    :-  ~
    :-  %ceil
    ?-  op
      %0x8d  %f32
      %0x9b  %f64
    ==
  ::
      floor-opcodes
    :-  ~
    :-  %floor
    ?-  op
      %0x8e  %f32
      %0x9c  %f64
    ==
  ::
      trunc-opcodes
    :-  ~
    :-  %trunc
    ?-  op
      %0x8f  [%f32 ~ ~]
      %0x9d  [%f64 ~ ~]
      %0xa8  [%i32 `%f32 `%s]
      %0xa9  [%i32 `%f32 `%u]
      %0xaa  [%i32 `%f64 `%s]
      %0xab  [%i32 `%f64 `%u]
      %0xae  [%i64 `%f32 `%s]
      %0xaf  [%i64 `%f32 `%u]
      %0xb0  [%i64 `%f64 `%s]
      %0xb1  [%i64 `%f64 `%u]
    ==
  ::
      nearest-opcodes
    :-  ~
    :-  %nearest
    ?-  op
      %0x90  %f32
      %0x9e  %f64
    ==
  ::
      sqrt-opcodes
    :-  ~
    :-  %sqrt
    ?-  op
      %0x91  %f32
      %0x9f  %f64
    ==
  ::
      min-opcodes
    :-  ~
    :-  %min
    ?-  op
      %0x96  %f32
      %0xa4  %f64
    ==
  ::
      max-opcodes
    :-  ~
    :-  %max
    ?-  op
      %0x97  %f32
      %0xa5  %f64
    ==
  ::
      copysign-opcodes
    :-  ~
    :-  %copysign
    ?-  op
      %0x98  %f32
      %0xa6  %f64
    ==
  ::
      extend-opcodes
    :-  ~
    :-  %extend
    ?-  op
      %0xac  [%i64 %32 %s]
      %0xad  [%i64 %32 %u]
      %0xc0  [%i32 %8 %s]
      %0xc1  [%i32 %16 %s]
      %0xc2  [%i64 %8 %s]
      %0xc3  [%i64 %16 %s]
      %0xc4  [%i64 %32 %s]
    ==
  ::
      convert-opcodes
    :-  ~
    :-  %convert
    ?-  op
      %0xb2  [%f32 %i32 %s]
      %0xb3  [%f32 %i32 %u]
      %0xb4  [%f32 %i64 %s]
      %0xb5  [%f32 %i64 %u]
      %0xb7  [%f64 %i32 %s]
      %0xb8  [%f64 %i32 %u]
      %0xb9  [%f64 %i64 %s]
      %0xba  [%f64 %i64 %u]
    ==
  ::
      reinterpret-opcodes
    :-  ~
    :-  %reinterpret
    ?-  op
      %0xbc  [%i32 %f32]
      %0xbd  [%i64 %f64]
      %0xbe  [%f32 %i32]
      %0xbf  [%f64 %i64]
    ==
  ::
  ==
::
--