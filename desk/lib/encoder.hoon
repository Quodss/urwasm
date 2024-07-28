:: /-  sur=wasm
/+  lia-compiler
=>  lia-compiler
~%  %encoder  +  ~
|%
++  encoder
  =/  sur  wasm-sur
  |%
  ++  main
    |=  =module:sur
    ^-  octs
    ;:  weld-octs
      [4 '\00asm'] 
      [4 '\01\00\00\00']
      (mayb-weld [1 1] (mayb-len-vec (type-section type-section.module)))
      (mayb-weld [1 2] (mayb-len-vec (import-section import-section.module)))
      (mayb-weld [1 3] (mayb-len-vec (function-section function-section.module)))
      (mayb-weld [1 4] (mayb-len-vec (table-section table-section.module)))
      (mayb-weld [1 5] (mayb-len-vec (memory-section memory-section.module)))
      (mayb-weld [1 6] (mayb-len-vec (global-section global-section.module)))
      (mayb-weld [1 7] (mayb-len-vec (export-section export-section.module)))
      (mayb-weld [1 8] (mayb-len (start-section start-section.module)))
      (mayb-weld [1 9] (mayb-len-vec (elem-section elem-section.module)))
      (mayb-weld [1 12] (mayb-len (datacnt-section datacnt-section.module)))
      (mayb-weld [1 10] (mayb-len-vec (code-section code-section.module)))
      (mayb-weld [1 11] (mayb-len-vec (data-section data-section.module)))
    ==
  ::
  ++  weld-octs
    |=  [a=octs b=octs]
    ^-  octs
    :-  (add p.a p.b)
    (add q.a (lsh [3 p.a] q.b))
  ::
  ++  zing-octs
    |=  l=(list octs)
    ^-  octs
    ?~  l  [0 0]
    (weld-octs i.l $(l t.l))
  ::
  ++  vec
    |=  l=(list octs)
    ^-  octs
    (weld-octs (u-n (lent l)) (zing-octs l))
  ::
  ++  u-n
    |=  a=@
    ^-  octs
    ?:  =(0 a)  [1 0]
    =/  bit-size=@  (met 0 a)
    =/  sept-blocks=@  +((div (dec bit-size) 7))
    |-  ^-  octs
    ?:  =(1 sept-blocks)  [1 a]
    %+  weld-octs  [1 (add 128 (mod a 128))]
    $(a (div a 128), sept-blocks (dec sept-blocks))
  ::
  ++  s-n
    |=  a=@s
    ^-  octs
    =/  [syn=? val=@]  (old:si a)
    =/  bit-size=@  (met 0 val)
    =/  sept-blocks=@  +((div bit-size 7))
    =?  val  !syn
      =.  val  (not [0 (mul 7 sept-blocks) val])  ::  negate bits
      =.  val  (~(sum fo (bex (mul 7 sept-blocks))) val 1)  ::  two's complement
      val
    |-  ^-  octs
    ?:  =(1 sept-blocks)  [1 val]
    %+  weld-octs  [1 (add 128 (mod val 128))]
    $(val (div val 128), sept-blocks (dec sept-blocks))
  ::
  ++  mayb-len
    |=  a=octs
    ^-  octs
    ?:  =([0 0] a)  [0 0]
    (weld-octs (u-n p.a) a)
  ::
  ++  mayb-len-vec
    |=  a=octs
    ^-  octs
    ?:  =([0 0] a)  [0 0]
    ?:  =([1 0] a)  [0 0]
    (weld-octs (u-n p.a) a)
  ::
  ++  add-len
    |=  a=octs
    ^-  octs
    (weld-octs (u-n p.a) a)
  ::
  ++  mayb-weld
    |=  [a=octs b=octs]
    ^-  octs
    ?:  =([0 0] b)  [0 0]
    (weld-octs a b)
  ::
  ++  f32
    |=  a=@rs
    ^-  octs
    [4 a]
  ::
  ++  f64
    |=  a=@rd
    ^-  octs
    [8 a]
  ::
  ++  name
    |=  a=cord
    ^-  octs
    (vec (turn `(list @)`(trip a) (lead 1)))
  ::
  ++  vec-u32
    |=  a=(list @)
    ^-  octs
    (vec (turn a u-n))
  ::
  ++  num-type
    |=  a=num-type:sur
    ^-  octs
    ?-  a
      %i32  [1 '\7f']
      %i64  [1 '\7e']
      %f32  [1 '\7d']
      %f64  [1 '\7c']
    ==
  ::
  ++  vec-type  [1 '\7b']
  ++  ref-type
    |=  a=ref-type:sur
    ^-  octs
    ?-  a
      %extn  [1 '\6f']
      %func  [1 '\70']
    ==
  ::
  ++  valtype
    |=  a=valtype:sur
    ^-  octs
    ?-  a
      num-type:sur  (num-type a)
      vec-type:sur  vec-type
      ref-type:sur  (ref-type a)
    ==
  ::
  ++  func-type
    |=  a=func-type:sur
    ^-  octs
    ;:  weld-octs
      [1 '\60']
      (vec (turn params.a valtype))
      (vec (turn results.a valtype))
    ==
  ::
  ++  limits
    |=  a=limits:sur
    ^-  octs
    ?-  -.a
      %flor  (weld-octs [1 0] (u-n p.a))
      %ceil  :(weld-octs [1 1] (u-n p.a) (u-n q.a))
    ==
  ::
  ++  expr
    |=  a=expression:sur
    ^-  octs
    (zing-octs (snoc (turn a instr) end))
  ::
  ++  expr-pair
    |=  [a=expression:sur b=expression:sur]
    ^-  octs
    ?~  b
      (zing-octs (snoc (turn a instr) end))
    %+  weld-octs
      (zing-octs (snoc (turn a instr) else))
    (zing-octs (snoc (turn b instr) end))
  ::
  ++  end        `octs`[1 '\0b']
  ++  else       `octs`[1 '\05']
  ++  const-i32  `octs`[1 '\41']
  ++  const-i64  `octs`[1 '\42']
  ++  const-f32  `octs`[1 '\43']
  ++  const-f64  `octs`[1 '\44']
  ++  block-op   `octs`[1 '\02']
  ++  loop-op    `octs`[1 '\03']
  ++  if-op      `octs`[1 '\04']
  ++  arg
    |%
    +$  zero
      $?
        %unreachable  %nop  %return  %drop  %select
        %wrap  %demote  %promote  %ref-is-null  %eqz
        %eq  %ne  %lt  %gt  %le
        %ge  %clz  %ctz  %popcnt  %add
        %sub  %mul  %div  %rem  %and
        %or  %xor  %shl  %shr  %rotl
        %rotr  %abs  %neg  %ceil  %floor
        %trunc  %nearest  %sqrt  %min  %max
        %copysign  %extend  %convert  %reinterpret
      ==
    ::
    +$  one
      $?
        %br  %br-if  %call  %local-get  %local-set
        %local-tee  %global-get  %global-set  %table-get
        %table-set  %memory-size  %memory-grow
      ==
    ::
    +$  two  ?(%call-indirect %load %store)
    ++  fc
      |%
      +$  one
        $?  %data-drop  %memory-fill  %elem-drop
            %table-grow  %table-size  %table-fill
        ==
      ::
      +$  two
        $?  %memory-init
            %memory-copy
            %table-init 
            %table-copy
        ==
      ::
      --
    ++  vec
      |%
      +$  zero
        $?
          %swizzle  %popcnt   %not  %and  %andnot  %or
          %xor  %bitselect  %any-true  %q15mul-r-sat  %dot
          %demote  %promote  %nearest  %sqrt  %div
          %pmin  %pmax  %avgr  %ceil  %floor
          %all-true  %bitmask  %narrow  %shl  %extadd
          %convert  %mul  %splat  %eq  %ne
          %abs  %neg  %trunc  %lt  %gt
          %le  %ge  %shr  %min  %max
          %add  %sub  %extend  %extmul
        ==
      ::
      +$  lane  ?(%extract %replace)
      +$  mem-lane  ?(%load-lane %store-lane)
      +$  mem-arg  ?(%load %store)
      --
    --
  ::
  ++  instr
    |=  a=instruction:sur
    ^-  octs
    ?-  -.a
      %vec  (vec-instr +.a)
      %select  (select a)
      %br-table  (br-table a)
      %block  (block a)
      %loop  (loop a)
      %if  (if a)
      %ref-null  (ref-null a)
      %ref-func  (ref-func a)
      %const  (const a)
      zero:arg  (zero a)
      one:arg  (one a)
      two:arg  (two a)
      one:fc:arg  (one-fc a)
      two:fc:arg  (two-fc a)
      %dbug  [0 0]
    ==
  ::
  ++  vec-instr
    |=  a=instr-vec:sur
    ^-  octs
    %+  weld-octs  [1 '\fd']
    ?-  -.a
      zero:vec:arg      (vec-instr-zero a)
      lane:vec:arg      (lane a)
      %shuffle          (shuffle a)
      %const            (const-vec a)
      mem-lane:vec:arg  (mem-lane a)
      mem-arg:vec:arg    (memarg a)
    ==
  ::
  ++  shuffle
    |=  a=[%shuffle lane-ids=(list @)]
    ^-  octs
    %+  weld-octs  (u-n 13)
    [16 (rep 3 lane-ids.a)]
  ::
  ++  const-vec
    |=  a=[%const p=$>(%v128 coin-wasm:sur)]
    ^-  octs
    %+  weld-octs  (u-n 12)
    [16 +.p.a]
  ::
  ++  lane
    |=  a=$~([%replace %i32 0] $>(lane:vec:arg instr-vec:sur))
    ^-  octs
    =/  l=@
      ?:  ?=(%extract -.a)
        l.a
      l.a
    =;  op=@
      (weld-octs (u-n op) [1 l])
    ?+  a  !!
      [%extract %i8 @ %s]   21
      [%extract %i8 @ %u]   22
      [%replace %i8 @]      23
      [%extract %i16 @ %s]  24
      [%extract %i16 @ %u]  25
      [%replace %i16 @]     26
      [%extract %i32 @ %u]  27
      [%replace %i32 @]     28
      [%extract %i64 @ %u]  29
      [%replace %i64 @]     30
      [%extract %f32 @ %u]  31
      [%replace %f32 @]     32
      [%extract %f64 @ %u]  33
      [%replace %f64 @]     34
    ==
  ::
  ++  mem-lane
    |=  a=$~([%load-lane [0 0] %32 0] $>(mem-lane:vec:arg instr-vec:sur))
    ^-  octs
    =/  [m=memarg:sur l=@]
      ?:  ?=(%load-lane -.a)
        [m l]:a
      [m l]:a
    =;  op=@
      :(weld-octs (u-n op) (u-n align.m) (u-n offset.m) [1 l])
    ?+  a  !!
      [%load-lane ^ %8 @]    84
      [%load-lane ^ %16 @]   85
      [%load-lane ^ %32 @]   86
      [%load-lane ^ %64 @]   87
      [%store-lane ^ %8 @]   88
      [%store-lane ^ %16 @]  89
      [%store-lane ^ %32 @]  90
      [%store-lane ^ %64 @]  91
    ==
  ::
  ++  memarg
    |=  a=$~([%store [0 0]] $>(mem-arg:vec:arg instr-vec:sur))
    ^-  octs
    =/  m=memarg:sur
      ?:  ?=(%load -.a)
        m.a
      m.a
    =;  op=@
      :(weld-octs (u-n op) (u-n align.m) (u-n offset.m))
    ?+    a  !!
        [%load ^ ~]
      0 
    ::
        [%load ^ ~ %8 %extend %s]
      1 
    ::
        [%load ^ ~ %8 %extend %u]
      2 
    ::
        [%load ^ ~ %16 %extend %s]
      3 
    ::
        [%load ^ ~ %16 %extend %u]
      4 
    ::
        [%load ^ ~ %32 %extend %s]
      5 
    ::
        [%load ^ ~ %32 %extend %u]
      6 
    ::
        [%load ^ ~ %8 %splat]
      7 
    ::
        [%load ^ ~ %16 %splat]
      8 
    ::
        [%load ^ ~ %32 %splat]
      9 
    ::
        [%load ^ ~ %64 %splat]
      10
    ::
        [%load ^ ~ %32 %zero]
      92
    ::
        [%load ^ ~ %64 %zero]
      93
    ::
        [%store ^]
      11
    ::
    ==
  ::
  ++  vec-instr-zero
    |=  i=$~([%swizzle ~] $>(zero:vec:arg instr-vec:sur))
    ~+  ^-  octs
    %-  u-n
    ^-  @
    ?-    -.i
        %swizzle
      14
    ::
        %popcnt
      98
    ::
        %not
      77
    ::
        %and
      78
    ::
        %andnot 
      79
    ::
        %or
      80
    ::
        %xor
      81
    ::
        %bitselect
      82
    ::
        %any-true
      83
    ::
        %q15mul-r-sat
      130
    ::
        %dot
      186
    ::
        %demote 
      94
    ::
        %promote
      95
    ::
        %nearest
      ?-  p.i
        %f32  106
        %f64  148
      ==
    ::
        %sqrt
      ?-  p.i
        %f32  227
        %f64  239
      ==
    ::
        %div
      ?-  p.i
        %f32  231
        %f64  243
      ==
    ::
        %pmin
      ?-  p.i
        %f32  234
        %f64  246
      ==
    ::
        %pmax
      ?-  p.i
        %f32  235
        %f64  247
      ==
    ::
        %avgr
      ?-  p.i
        %i8  123
        %i16  155
      ==
    ::
        %ceil
      ?-  p.i
        %f32  103
        %f64  116
      ==
    ::
        %floor
      ?-  p.i
        %f32  104
        %f64  117
      ==
    ::
        %all-true
      ?-  p.i
        %i8   99
        %i16  131
        %i32  163
        %i64  195
      ==
    ::
        %bitmask
      ?-  p.i
        %i8   100
        %i16  132
        %i32  164
        %i64  196
      ==
    ::
        %narrow
      ?+  +.i  !!  ::  ??
        [%i8 %s]   101
        [%i8 %u]   102
        [%i16 %s]  133
        [%i16 %u]  134
      ==
    ::
        %shl
      ?-  p.i
        %i8   107
        %i16  139
        %i32  171
        %i64  203
      ==
    ::
        %extadd
      ?+  +.i  !!  ::  ??
        [%i16 %s]  124
        [%i16 %u]  125
        [%i32 %s]  126
        [%i32 %u]  127
      ==
    ::
        %convert
      ?+  +.i  !!
        [%f32 %s]  250
        [%f32 %u]  251
        [%f64 %s]  254
        [%f64 %u]  255
      ==
    ::
        %mul
      ?+  p.i  !!
        %i16  149
        %i32  181
        %i64  213
        %f32  230
        %f64  242
      ==
    ::
        %splat
      ?-  p.i
        %i8   15
        %i16  16
        %i32  17
        %i64  18
        %f32  19
        %f64  20
      ==
    ::
        %eq
      ?-  p.i
        %i8   35
        %i16  45
        %i32  55
        %i64  214
        %f32  65
        %f64  71
      ==
    ::
        %ne
      ?-  p.i
        %i8   36
        %i16  46
        %i32  56
        %i64  215
        %f32  66
        %f64  72
      ==
    ::
        %abs
      ?-  p.i
        %i8   96
        %i16  128
        %i32  160
        %i64  192
        %f32  224
        %f64  236
      ==
    ::
        %neg
      ?-  p.i
        %i8   97
        %i16  129
        %i32  161
        %i64  193
        %f32  225
        %f64  237
      ==
    ::
        %trunc
      ?+  +.i  !!
        [%f32 %f32 %s]  105
        [%f64 %f64 %s]  122
        [%i32 %f32 %s]  248
        [%i32 %f32 %u]  249
        [%i32 %f64 %s]  252
        [%i32 %f64 %u]  253
      ==
    ::
        %lt
      ?+  +.i  !!
        [%i8 %s]   37
        [%i8 %u]   38
        [%i16 %s]  47
        [%i16 %u]  48
        [%i32 %s]  57
        [%i32 %u]  58
        [%i64 %s]  216
        [%f32 %s]  67
        [%f64 %s]  73
      ==
    ::
        %gt
      ?+  +.i  !!
        [%i8 %s]   39
        [%i8 %u]   40
        [%i16 %s]  49
        [%i16 %u]  50
        [%i32 %s]  59
        [%i32 %u]  60
        [%i64 %s]  217
        [%f32 %s]  68
        [%f64 %s]  74
      ==
    ::
        %le
      ?+  +.i  !!
        [%i8 %s]   41
        [%i8 %u]   42
        [%i16 %s]  51
        [%i16 %u]  52
        [%i32 %s]  61
        [%i32 %u]  62
        [%i64 %s]  218
        [%f32 %s]  69
        [%f64 %s]  75
      ==
    ::
        %ge
      ?+  +.i  !!
        [%i8 %s]   43
        [%i8 %u]   44
        [%i16 %s]  53
        [%i16 %u]  54
        [%i32 %s]  63
        [%i32 %u]  64
        [%i64 %s]  219
        [%f32 %s]  70
        [%f64 %s]  76
      ==
    ::
        %shr
      ?+  +.i  !!
        [%i8 %s]   108
        [%i8 %u]   109
        [%i16 %s]  140
        [%i16 %u]  141
        [%i32 %s]  172
        [%i32 %u]  173
        [%i64 %s]  204
        [%i64 %u]  205
      ==
    ::
        %min
      ?+  +.i  !!
        [%i8 %s]   118
        [%i8 %u]   119
        [%i16 %s]  150
        [%i16 %u]  151
        [%i32 %s]  182
        [%i32 %u]  183
        [%f32 %s]  232
        [%f64 %s]  244
      ==
    ::
        %max
      ?+  +.i  !!
        [%i8 %s]   120
        [%i8 %u]   121
        [%i16 %s]  152
        [%i16 %u]  153
        [%i32 %s]  184
        [%i32 %u]  185
        [%f32 %s]  233
        [%f64 %s]  245
      ==
    ::
        %add
      ?+  +.i  !!
        [%i8 ~]      110
        [%i8 ~ %s]   111
        [%i8 ~ %u]   112
        [%i16 ~]     142
        [%i16 ~ %s]  143
        [%i16 ~ %u]  144
        [%i32 ~]     174
        [%i64 ~]     206
        [%f32 ~]     228
        [%f64 ~]     240
      ==
    ::
        %sub
      ?+  +.i  !!
        [%i8 ~]      113
        [%i8 ~ %s]   114
        [%i8 ~ %u]   115
        [%i16 ~]     145
        [%i16 ~ %s]  146
        [%i16 ~ %u]  147
        [%i32 ~]     177
        [%i64 ~]     209
        [%f32 ~]     229
        [%f64 ~]     241
      ==
    ::
        %extend
      ?+  +.i  !!
        [%i16 %s %low]   135
        [%i16 %s %high]  136
        [%i16 %u %low]   137
        [%i16 %u %high]  138
        [%i32 %s %low]   167
        [%i32 %s %high]  168
        [%i32 %u %low]   169
        [%i32 %u %high]  170
        [%i64 %s %low]   199
        [%i64 %s %high]  200
        [%i64 %u %low]   201
        [%i64 %u %high]  202
      ==
    ::
        %extmul
      ?+  +.i  !!
        [%i16 %s %low]   156
        [%i16 %s %high]  157
        [%i16 %u %low]   158
        [%i16 %u %high]  159
        [%i32 %s %low]   188
        [%i32 %s %high]  189
        [%i32 %u %low]   190
        [%i32 %u %high]  191
        [%i64 %s %low]   220
        [%i64 %s %high]  221
        [%i64 %u %low]   222
        [%i64 %u %high]  223
      ==
    ::
    ==
  ::
  ++  select
    |=  a=[%select (unit (list valtype:sur))]
    ^-  octs
    ?~  +.a  [1 0x1b]
    %+  weld-octs  [1 0x1c]
    (vec (turn u.+.a valtype))
  ::
  ++  br-table
    |=  a=[%br-table label-vec=(list @) label-default=@]
    ^-  octs
    ;:  weld-octs
      [1 0xe]
      (vec (turn label-vec.a u-n))
      (u-n label-default.a)
    ==
  ::
  ++  block
    |=  a=[%block type=block-type:sur body=expression:sur]
    ^-  octs
    ;:  weld-octs
      block-op
      (block-type type.a)
      (expr body.a)
    ==
  ::
  ++  loop
    |=  a=[%loop type=block-type:sur body=expression:sur]
    ^-  octs
    ;:  weld-octs
      loop-op
      (block-type type.a)
      (expr body.a)
    ==
  ::
  ++  if
    |=  $:  %if
          type=block-type:sur
          branch-true=expression:sur
          branch-false=expression:sur
        ==
    ^-  octs
    ;:  weld-octs
      if-op
      (block-type type)
      (expr-pair branch-true branch-false)
    ==
  ::
  ++  block-type
    |=  t=block-type:sur
    ^-  octs
    ?@  t  (s-n (new:si & t))
    ?:  =([~ ~] t)  [1 '\40']
    ?>  =(~ params.t)
    ?>  ?=([valtype:sur ~] results.t)
    (valtype i.results.t)
  ::
  ++  ref-null
    |=  [%ref-null t=ref-type:sur]
    ^-  octs
    (weld-octs [1 '\d0'] (ref-type t))
  ::
  ++  ref-func
    |=  [%ref-func func-id=@]
    ^-  octs
    (weld-octs [1 '\d2'] (u-n func-id))
  ::
  ++  to-si
    |=  [base=@ n=@]
    ^-  @s
    =.  n  (mod n (bex base))
    =/  sign=?  (lth n (bex (dec base)))
    %+  new:si  sign
    ?:  sign  n
    (sub (bex base) n)
  ::
  ++  const
    |=  [%const p=$<(?(%v128 %ref) coin-wasm:sur)]
    ^-  octs
    =/  op=@
      ?-  -.p
        %i32  '\41'
        %i64  '\42'
        %f32  '\43'
        %f64  '\44'
      ==
    %+  weld-octs  [1 op]
    ?-  -.p
      %i32  (s-n (to-si 32 +.p))
      %i64  (s-n (to-si 64 +.p))
      %f32  (f32 +.p)
      %f64  (f64 +.p)
    ==
  ::
  ++  zero
    |=  a=$~([%unreachable ~] $>(zero:arg instruction:sur))
    ^-  octs
    ?-    -.a
        %unreachable
      [1 0x0] 
    ::
        %nop
      [1 0x1] 
    ::
        %return
      [1 0xf] 
    ::
        %drop
      [1 0x1a]
    ::
        %select
      [1 0x1b]
    ::
        %wrap
      [1 0xa7]
    ::
        %demote
      [1 0xb6]
    ::
        %promote
      [1 0xbb]
    ::
        %ref-is-null
      [1 0xd1]
    ::
        %eqz
      ?-  +.a
        %i32  [1 0x45]
        %i64  [1 0x50]
      ==
    ::
        %eq
      ?+  +.a  !!
        %i32  [1 0x46]
        %i64  [1 0x51]
        %f32  [1 0x5b]
        %f64  [1 0x61]
      ==
    ::
        %ne
      ?+  +.a  !!
        %i32  [1 0x47]
        %i64  [1 0x52]
        %f32  [1 0x5c]
        %f64  [1 0x62]
      ==
    ::
        %lt
      ?+  +.a  !!
        [%i32 ~ %s]  [1 0x48]
        [%i32 ~ %u]  [1 0x49]
        [%i64 ~ %s]  [1 0x53]
        [%i64 ~ %u]  [1 0x54]
        [%f32 ~]  [1 0x5d]
        [%f64 ~]  [1 0x63]
      ==
    ::
        %gt
      ?+  +.a  !!
        [%i32 ~ %s]  [1 0x4a]
        [%i32 ~ %u]  [1 0x4b]
        [%i64 ~ %s]  [1 0x55]
        [%i64 ~ %u]  [1 0x56]
        [%f32 ~]  [1 0x5e]
        [%f64 ~]  [1 0x64]
      ==
    ::
        %le
      ?+  +.a  !!
        [%i32 ~ %s]  [1 0x4c]
        [%i32 ~ %u]  [1 0x4d]
        [%i64 ~ %s]  [1 0x57]
        [%i64 ~ %u]  [1 0x58]
        [%f32 ~]  [1 0x5f]
        [%f64 ~]  [1 0x65]
      ==
    ::
        %ge
      ?+  +.a  !!
        [%i32 ~ %s]  [1 0x4e]
        [%i32 ~ %u]  [1 0x4f]
        [%i64 ~ %s]  [1 0x59]
        [%i64 ~ %u]  [1 0x5a]
        [%f32 ~]  [1 0x60]
        [%f64 ~]  [1 0x66]
      ==
    ::
        %clz
      ?-  +.a
        %i32  [1 0x67]
        %i64  [1 0x79]
      ==
    ::
        %ctz
      ?-  +.a
        %i32  [1 0x68]
        %i64  [1 0x7a]
      ==
    ::
        %popcnt
      ?+  +.a  !!
        %i32  [1 0x69]
        %i64  [1 0x7b]
      ==
    ::
        %add
      ?+  +.a  !!
        %i32  [1 0x6a]
        %i64  [1 0x7c]
        %f32  [1 0x92]
        %f64  [1 0xa0]
      ==
    ::
        %sub
      ?+  +.a  !!
        %i32  [1 0x6b]
        %i64  [1 0x7d]
        %f32  [1 0x93]
        %f64  [1 0xa1]
      ==
    ::
        %mul
      ?+  +.a  !!
        %i32  [1 0x6c]
        %i64  [1 0x7e]
        %f32  [1 0x94]
        %f64  [1 0xa2]
      ==
    ::
        %div
      ?+  +.a  !!
        [%i32 ~ %s]  [1 0x6d]
        [%i32 ~ %u]  [1 0x6e]
        [%i64 ~ %s]  [1 0x7f]
        [%i64 ~ %u]  [1 0x80]
        [%f32 ~]  [1 0x95]
        [%f64 ~]  [1 0xa3]
      ==
    ::
        %rem
      ?+  +.a  !!
        [%i32 %s]  [1 0x6f]
        [%i32 %u]  [1 0x70]
        [%i64 %s]  [1 0x81]
        [%i64 %u]  [1 0x82]
      ==
    ::
        %and
      ?-  +.a
        %i32  [1 0x71]
        %i64  [1 0x83]
      ==
    ::
        %or
      ?-  +.a
        %i32  [1 0x72]
        %i64  [1 0x84]
      ==
    ::
        %xor
      ?-  +.a
        %i32  [1 0x73]
        %i64  [1 0x85]
      ==
    ::
        %shl
      ?+  +.a  !!
        %i32  [1 0x74]
        %i64  [1 0x86]
      ==
    ::
        %shr
      ?+  +.a  !!
        [%i32 %s]  [1 0x75]
        [%i32 %u]  [1 0x76]
        [%i64 %s]  [1 0x87]
        [%i64 %u]  [1 0x88]
      ==
    ::
        %rotl
      ?-  +.a
        %i32  [1 0x77]
        %i64  [1 0x89]
      ==
    ::
        %rotr
      ?-  +.a
        %i32  [1 0x78]
        %i64  [1 0x8a]
      ==
    ::
        %abs
      ?+  +.a  !!
        %f32  [1 0x8b]
        %f64  [1 0x99]
      ==
    ::
        %neg
      ?+  +.a  !!
        %f32  [1 0x8c]
        %f64  [1 0x9a]
      ==
    ::
        %ceil
      ?-  +.a
        %f32  [1 0x8d]
        %f64  [1 0x9b]
      ==
    ::
        %floor
      ?-  +.a
        %f32  [1 0x8e]
        %f64  [1 0x9c]
      ==
    ::
        %trunc
      ?:  =(& sat.a)
        (trunc-fc a)
      ~!  +.a
      ?+  +.a  !!
        [%f32 ~ ~ %|]  [1 0x8f]
        [%f64 ~ ~ %|]  [1 0x9d]
        [%i32 [~ %f32] [~ %s] %|]  [1 0xa8]
        [%i32 [~ %f32] [~ %u] %|]  [1 0xa9]
        [%i32 [~ %f64] [~ %s] %|]  [1 0xaa]
        [%i32 [~ %f64] [~ %u] %|]  [1 0xab]
        [%i64 [~ %f32] [~ %s] %|]  [1 0xae]
        [%i64 [~ %f32] [~ %u] %|]  [1 0xaf]
        [%i64 [~ %f64] [~ %s] %|]  [1 0xb0]
        [%i64 [~ %f64] [~ %u] %|]  [1 0xb1]
      ==
    ::
        %nearest
      ?-  +.a
        %f32  [1 0x90]
        %f64  [1 0x9e]
      ==
    ::
        %sqrt
      ?-  +.a
        %f32  [1 0x91]
        %f64  [1 0x9f]
      ==
    ::
        %min
      ?-  +.a
        %f32  [1 0x96]
        %f64  [1 0xa4]
      ==
    ::
        %max
      ?-  +.a
        %f32  [1 0x97]
        %f64  [1 0xa5]
      ==
    ::
        %copysign
      ?-  +.a
        %f32  [1 0x98]
        %f64  [1 0xa6]
      ==
    ::
        %extend
      ?+  +.a  !!
        [%i64 %i32 %32 %s]  [1 0xac]
        [%i64 %i32 %32 %u]  [1 0xad]
        [%i32 %i32 %8 %s]  [1 0xc0]
        [%i32 %i32 %16 %s]  [1 0xc1]
        [%i64 %i64 %8 %s]  [1 0xc2]
        [%i64 %i64 %16 %s]  [1 0xc3]
        [%i64 %i64 %32 %s]  [1 0xc4]
      ==
    ::
        %convert
      ?+  +.a  !!
        [%f32 %i32 %s]  [1 0xb2]
        [%f32 %i32 %u]  [1 0xb3]
        [%f32 %i64 %s]  [1 0xb4]
        [%f32 %i64 %u]  [1 0xb5]
        [%f64 %i32 %s]  [1 0xb7]
        [%f64 %i32 %u]  [1 0xb8]
        [%f64 %i64 %s]  [1 0xb9]
        [%f64 %i64 %u]  [1 0xba]
      ==
    ::
        %reinterpret
      ?+  +.a  !!
        [%i32 %f32]  [1 0xbc]
        [%i64 %f64]  [1 0xbd]
        [%f32 %i32]  [1 0xbe]
        [%f64 %i64]  [1 0xbf]
      ==
    ::
    ==
  ::
  ++  trunc-fc
    |=  $:  %trunc
            type=num-type:sur
            source-type=(unit ?(%f32 %f64))
            mode=(unit ?(%s %u))
            sat=?
        ==
    ^-  octs
    ?>  ?=(^ mode)
    ?>  ?=(^ source-type)
    ?>  ?=(%& sat)
    ?>  ?=(?(%i32 %i64) type)
    =;  op=@
      (weld-octs [1 0xfc] (u-n op))
    ;:  add
      ?-(u.mode %s 0, %u 1)
      ?-(u.source-type %f32 0, %f64 2)
      ?-(type %i32 0, %i64 4)
    ==
  ::
  ++  one
    |=  a=$~([%br 0] $>(one:arg instruction:sur))
    ^-  octs
    =;  op=@
      (weld-octs [1 op] (u-n +.a))
    ^-  @
    ?-  -.a
      %br  0xc 
      %br-if  0xd 
      %call  0x10
      %local-get  0x20
      %local-set  0x21
      %local-tee  0x22
      %global-get  0x23
      %global-set  0x24
      %table-get  0x25
      %table-set  0x26
      %memory-size  0x3f
      %memory-grow  0x40
    ==
  ::
  ++  two
    |=  a=$~([%call-indirect 0 0] $>(two:arg instruction:sur))
    ^-  octs
    =/  [arg1=@ arg2=@]
      ?:  ?=(%load -.a)
        m.a
      ?:  ?=(%store -.a)
        m.a
      +.a
    =;  op=@
      :(weld-octs [1 op] (u-n arg1) (u-n arg2))
    ^-  @
    ?+  a  !!
      [%call-indirect ^]  0x11
      [%load %i32 ^ ~ ~]  0x28
      [%load %i64 ^ ~ ~]  0x29
      [%load %f32 ^ ~ ~]  0x2a
      [%load %f64 ^ ~ ~]  0x2b
      [%load %i32 ^ [~ %8] ~ %s]  0x2c
      [%load %i32 ^ [~ %8] ~ %u]  0x2d
      [%load %i32 ^ [~ %8] ~ %s]  0x2e
      [%load %i32 ^ [~ %16] ~ %u]  0x2f
      [%load %i64 ^ [~ %8] ~ %s]  0x30
      [%load %i64 ^ [~ %8] ~ %u]  0x31
      [%load %i64 ^ [~ %16] ~ %s]  0x32
      [%load %i64 ^ [~ %16] ~ %u]  0x33
      [%load %i64 ^ [~ %32] ~ %s]  0x34
      [%load %i64 ^ [~ %32] ~ %u]  0x35
      [%store %i32 ^ ~]  0x36
      [%store %i64 ^ ~]  0x37
      [%store %f32 ^ ~]  0x38
      [%store %f64 ^ ~]  0x39
      [%store %i32 ^ ~ %8]  0x3a
      [%store %i32 ^ ~ %16]  0x3b
      [%store %i64 ^ ~ %8]  0x3c
      [%store %i64 ^ ~ %16]  0x3d
      [%store %i64 ^ ~ %32]  0x3e
    ==
  ::
  ++  one-fc
    |=  a=$~([%memory-fill %0] $>(one:fc:arg instruction:sur))
    ^-  octs
    =;  op=@
      :(weld-octs [1 0xfc] (u-n op) (u-n +.a))
    ?-  -.a
      %data-drop  9 
      %memory-fill  11
      %elem-drop  13
      %table-grow  15
      %table-size  16
      %table-fill  17
    ==
  ::
  ++  two-fc
    |=  a=$~([%memory-init 0 %0] $>(two:fc:arg instruction:sur))
    ^-  octs
    =;  op=@
      :(weld-octs [1 0xfc] (u-n op) (u-n +<.a) (u-n +>.a))
    ?-  -.a
      %memory-init  8 
      %memory-copy  10
      %table-init   12
      %table-copy  14
    ==
  ::
  ++  type-section
    |=  s=type-section:sur
    ^-  octs
    (vec (turn s func-type))
  ::
  ++  import-section
    |=  s=import-section:sur
    ^-  octs
    (vec (turn s import))
  ::
  ++  import
    |=  a=import:sur
    ^-  octs
    ;:  weld-octs
      (name mod.a)
      (name name.a)
      ?-    -.desc.a
          %func  (weld-octs [1 0] (u-n type-id.desc.a))
          %tabl  (weld-octs [1 1] (table t.desc.a))
          %memo  (weld-octs [1 2] (limits l.desc.a))
          %glob
        ;:  weld-octs
          [1 3]
          (valtype v.desc.a)
          ?-(m.desc.a %con [1 0], %var [1 1])
        ==
      ==
    ==
  ::
  ++  table
    |=  t=table:sur
    ^-  octs
    (weld-octs (ref-type p.t) (limits q.t))
  ::
  ++  function-section
    |=  s=function-section:sur
    ^-  octs
    (vec (turn s u-n))
  ::
  ++  table-section
    |=  s=table-section:sur
    ^-  octs
    %-  vec
    (turn s table)
  ::
  ++  memory-section
    |=  s=memory-section:sur
    ^-  octs
    (vec (turn s limits))
  ::
  ++  global-section
    |=  s=global-section:sur
    ^-  octs
    %-  vec
    %+  turn  s
    |=  g=global:sur
    ^-  octs
    ;:  weld-octs
      (valtype v.g)
      ?-(m.g %con [1 0], %var [1 1])
      (instr i.g)
      end
    ==
  ::
  ++  export-section
    |=  s=export-section:sur
    ^-  octs
    %-  vec
    %+  turn  s
    |=  e=export:sur
    ^-  octs
    %+  weld-octs  (name name.e)
    ?-  -.export-desc.e
      %func  (weld-octs [1 0] (u-n i.export-desc.e))
      %tabl  (weld-octs [1 1] (u-n i.export-desc.e))
      %memo  (weld-octs [1 2] (u-n i.export-desc.e))
      %glob  (weld-octs [1 3] (u-n i.export-desc.e))
    ==
  ::
  ++  start-section
    |=  s=start-section:sur
    ^-  octs
    ?~  s  [0 0]
    (u-n u.s)
  ::
  ++  elem-section
    |=  s=elem-section:sur
    ^-  octs
    %-  vec
    %+  turn  s
    |=  e=elem:sur
    ^-  octs
    ?-    -.m.e
        %pass
      ;:  weld-octs
        [1 5]
        (ref-type t.e)
      ::
        %-  vec
        %+  turn  i.e
        |=  i=instruction:sur
        (weld-octs (instr i) end)
      ::
      ==
    ::
        %acti
      ;:  weld-octs
        [1 6]
        (u-n tab.m.e)
        (weld-octs (instr off.m.e) end)
        (ref-type t.e)
      ::
        %-  vec
        %+  turn  i.e
        |=  i=instruction:sur
        (weld-octs (instr i) end)
      ::
      ==
        %decl
      ;:  weld-octs
        [1 7]
        (ref-type t.e)
      ::
        %-  vec
        %+  turn  i.e
        |=  i=instruction:sur
        (weld-octs (instr i) end)
      ::
      ==
    ==
  ::
  ++  datacnt-section
    |=  s=datacnt-section:sur
    ^-  octs
    ?~  s  [0 0]
    (u-n u.s)
  ::
  ++  code-section
    |=  s=code-section:sur
    ^-  octs
    %-  vec
    %+  turn  s
    |=  c=code:sur
    ^-  octs
    %-  add-len
    (weld-octs (locals locals.c) (expr expression.c))
  ::
  ++  locals
    |=  l=(list valtype:sur)
    ^-  octs
    =;  l-repeat=(list [@ v=valtype:sur])
      %-  vec
      %+  turn  l-repeat
      |=  [n=@ v=valtype:sur]
      ^-  octs
      (weld-octs (u-n n) (valtype v))
    =|  l-repeat=(list [@ v=valtype:sur])
    |-  ^+  l-repeat
    ?~  l  (flop l-repeat)
    ?~  l-repeat
      $(l t.l, l-repeat ~[[1 i.l]])
    ?.  =(i.l +.i.l-repeat)
      $(l t.l, l-repeat `(list [@ v=valtype:sur])`[[1 i.l] l-repeat])
    %=    $
        l  t.l
        l-repeat
      ^-  (list [@ v=valtype:sur])
      [[+(-.i.l-repeat) +.i.l-repeat] t.l-repeat]
    ==
  ::
  ++  data-section
    |=  s=data-section:sur
    ^-  octs
    %-  vec
    %+  turn  s
    |=  d=data:sur
    ^-  octs
    ?-    -.d
        %acti
      ;:  weld-octs
        [1 0]
        (instr off.d)
        end
        (add-len b.d)
      ==
    ::
        %pass
      ;:  weld-octs
        [1 1]
        (add-len b.d)
      ==
    ==
  ::
  --  ::  |encoder
--