/-  wasm
/-  lia
::
|%
++  minus-one  ^~((dec (bex 32)))
++  null-ptr  ^~((bex 31))
++  addr-size  `@`8                                 ::  space address size in bits
++  space-width  `@`4                               ::  size of space element in bytes
++  space-number  ^~((bex addr-size))               ::  number of elements in space
++  space-size  ^~((mul space-width space-number))  ::  space size in bytes
++  page-size  `@`65.536                            ::  page size in bytes
++  heap-pages  `@`128                              ::  addressable heap limit in pages; need ~2x of this memory for gc
++  heap-lim  ^~((mul page-size heap-pages))        ::  addressable heap limit in bytes
++  len-size  `@`4                                  ::  size of length prefix
++  offset  `@`0  :: v                              ::  space offset
::  #################|+++++++++++++++++++++++++++++++++++++|...
::  ^static data       ^space with numeric values/pointers   ^heap
::
++  main
  |=  =,  line:lia
      $:  serf=module:wasm
          code=(list action)
          ext=(map (pair cord cord) ext-func)
          import=(map term block-type)
      ==
  ^-  module:wasm
  =|  king=module:wasm
  ::  Add funcs exported by serf as imports in king
  ::  Exports of globals, tables and memory are not treated
  ::
  =/  exports-serf=(map cord @)
    =|  out=(map cord @)
    |-  ^-  (map cord @)
    ?~  export-section.serf  out
    =*  exp  i.export-section.serf
    =?  out  ?=(%func -.exp)
      (~(put by out) name.exp i.exp)
    $(export-section.serf t.export-section.serf)
  =^  serf-diary=(map cord @)  king
    =|  d=(map cord @)
    =/  i=@  0
    %-  ~(reb by exports-serf)
    |:  [[name=*cord idx=*@] d=d i=i king=king]
    ^-  module:wasm
    =/  type-idx  (snag idx function-section.serf)
    =/  type  (snag type-idx type-section.serf)
    =^  king-type-idx=@  king
      =/  maybe  (find ~[type] type-section.king)
      ?^  maybe
        [u.maybe king]
      :-  (lent type-section.king)
      king(type-section (snoc type-section.king type))
    %=    +<+
        i  +(i)
        d  (~(put by d) name i)
        import-section.king
      %+  snoc  import-section.king
      :+  'serf'  name
      [%func king-type-idx]
    ==
  =.  memory-section.king  ~[[%flor 1]]  ::  init single page, potentially unbound
  =.  global-section.king
    ?>  (lth (add offset space-size) page-size)
    ~[[%i32 %var %const %i32 (add offset space-size)]]  ::  heap edge
  ::  add memory read and write (Wasm modules support
  ::  up to one memory for now, so we'll have to resort
  ::  to import functions to copy stuff around instead
  ::  of calling memory.copy)
  ::
  =^  type-memio-idx=@  type-section.king
    (get-type-idx [~[%i32 %i32 %i32] ~] type-section.king)  ::  from, to, len
  =.  import-section.king
    %+  weld  import-section.king
    :~
      ['memio' 'read' %func type-memio-idx]
      ['memio' 'write' %func type-memio-idx]
    ==
  =/  mem-read-idx=@   (sub (lent import-section.king) 2)
  =/  mem-write-idx=@  +(mem-read-idx)
  ::  add Lia imports
  ::
  =^  king-diary=(map cord @)  king
    =|  d=(map cord @)
    =/  i=@  (lent import-section.king)
    %-  ~(reb by import)
    |:  [[name=*term type=*block-type:line:lia] d=d i=i king=king]
    ^-  module:wasm
    =^  type-idx=@  type-section.king
      %+  get-type-idx
        [(turn -.type convert) (turn +.type convert)]
      type-section.king
    %=    +<+
        i  +(i)
        d  (~(put by d) name i)
        import-section.king
      %+  snoc  import-section.king
      ['lia' name %func type-idx]
    ==
  ::  local funcs
  ::
  =/  alloc-idx=@      (lent import-section.king)
  =/  gc-idx=@         +(alloc-idx)
  =/  set-i32-idx=@    +(gc-idx)
  =/  set-i64-idx=@    +(set-i32-idx)
  =/  set-f32-idx=@    +(set-i64-idx)
  =/  set-f64-idx=@    +(set-f32-idx)
  =/  set-vec-idx=@    +(set-f64-idx)
  =/  get-i32-idx=@    +(set-vec-idx)
  =/  get-i64-idx=@    +(get-i32-idx)
  =/  get-f32-idx=@    +(get-i64-idx)
  =/  get-f64-idx=@    +(get-f32-idx)
  =/  get-vec-idx=@    +(get-f64-idx)
  ::
  =/  set-octs-idx=@  +(get-vec-idx)
  =/  get-octs-idx=@  +(set-octs-idx)
  =/  len-idx=@       +(get-octs-idx)
  =/  read-octs-i32-idx=@  +(len-idx)
  =/  read-octs-i64-idx=@  +(read-octs-i32-idx)
  =/  read-octs-f32-idx=@  +(read-octs-i64-idx)
  =/  read-octs-f64-idx=@  +(read-octs-f32-idx)
  ::  allocator
  ::
  =^  type-alloc-idx=@  type-section.king
    (get-type-idx [~[%i32] ~[%i32]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-alloc-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32 %i32]  ::  edge after allocation, memsize in bytes
    ::  local var indices
    ::
    =/  len=@  0
    =/  edge-new=@  1
    =/  memsize=@  2
    ^-  expression:wasm
    :~
      [%local-get len]
      [%global-get 0]
      [%add %i32]
      [%local-tee edge-new]
      [%const %i32 page-size]
      [%memory-size %0]
      [%mul %i32]
      [%local-tee memsize]
      [%ge %i32 `%u]
    ::
      [%local-get edge-new]
      [%const %i32 heap-lim]
      [%ge %i32 `%u]
    ::
      [%or %i32]
      :^    %if  ::  if heap_edge+len >= min(heap-lim, page-size*memsize): gc
          [~ ~]
        :~
          [%call gc-idx]
          [%local-get len]
          [%global-get 0]
          [%add %i32]
          [%local-tee edge-new]
          [%const %i32 heap-lim]
          [%ge %i32 `%u]
          :^    %if  ::  if heap_edge+len >= heap-lim after gc: crash
              [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get edge-new]
          [%local-get memsize]
          [%ge %i32 `%u]
          :^    %if  ::  else if heap_edge+len >= page-size*memsize: grow memory
              [~ ~]
            :~
              [%local-get edge-new]
              [%local-get memsize]
              [%sub %i32]
              [%const %i32 page-size]
              [%div %i32 `%u]
              [%const %i32 1]
              [%add %i32]
              [%memory-grow %0]
              [%const %i32 minus-one]
              [%eq %i32]
              :^    %if  ::  if memory-grow yields -1: crash
                  [~ ~]
                ~[[%unreachable ~]]
              ~
            ==
          ~
        ==
      ~
    ::
      [%global-get 0]  ::  old edge is the pointer
      [%local-get edge-new]
      [%global-set 0]  ::  increase the edge
    ==
  ::  gc
  ::
  =^  type-gc-idx=@  type-section.king
    (get-type-idx [~ ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-gc-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  :~  %i32  ::  (0) memsize in bytes
            %i32  ::  (1) edge_copy
            %i32  ::  (2) space ptr address w/o offset
            %i32  ::  (3) space value/ptr
            %i32  ::  (4) datum size
        ==
    =/  memsize=@  0
    =/  edge-copy=@  1
    =/  space-val-ptr=@  2
    =/  space-val=@  3
    =/  datum-size=@  4
    ^-  expression:wasm
    :~
      [%const %i32 space-size]
      [%global-get 0]
      [%add %i32]
      [%local-tee edge-copy]
      [%const %i32 page-size]
      [%memory-size %0]
      [%mul %i32]
      [%local-tee memsize]
      [%ge %i32 `%u]
      :^    %if  ::  if edge_copy >= memsize: attempt to grow
          [~ ~]
        :~
          [%local-get edge-copy]
          [%local-get memsize]
          [%sub %i32]
          [%const %i32 page-size]
          [%div %i32 `%u]
          [%const %i32 1]
          [%add %i32]
          [%memory-grow %0]
          [%const %i32 minus-one]
          [%eq %i32]
          :^    %if  ::  if memory-grow yields -1: crash, else update memsize
              [~ ~]
            ~[[%unreachable ~]]
          :~
            [%const %i32 page-size]
            [%memory-size %0]
            [%mul %i32]
            [%local-set memsize]
          ==
        ==
      ~
    ::
      [%const %i32 0]
      [%local-set space-val-ptr]
      :+  %loop  [~ ~]
      :~
        [%local-get space-val-ptr]
        [%const %i32 space-size]
        [%ge %i32 `%u]
        :^    %if  ::  if space ptr address >= space-size: end loop
            [~ ~]
          ~
        :~  ::  else continue loop
          [%local-get space-val-ptr]
          [%load %i32 [0 offset] ~]
          [%local-tee space-val]
          [%const %i32 null-ptr]
          [%lt %i32 `%u]
          :^    %if  ::  if MSB set to 0 then it's numerical value, copy
              [~ ~]
            :~
              [%global-get 0]
              [%local-get space-val-ptr]
              [%add %i32]
              [%local-get space-val]
              [%store %i32 [0 0] ~]
            ==
          :~  ::  else it's a pointer
            [%local-get space-val]
            [%const %i32 null-ptr]
            [%sub %i32]
            [%local-tee space-val]  ::  remove leading 1
            [%eqz %i32]
            :^    %if  ::  if NULL: write NULL to destination
                [~ ~]
              :~
                [%global-get 0]
                [%local-get space-val-ptr]
                [%add %i32]
                [%const %i32 null-ptr]
                [%store %i32 [0 0] ~]
              ==
            :~  ::  else: set pointer, copy data
              [%global-get 0]
              [%local-get space-val-ptr]
              [%add %i32]
            ::
              [%local-get edge-copy]
              [%global-get 0]
              [%sub %i32]
              [%const %i32 offset]
              [%add %i32]             ::  (copy_edge - edge + offset = edge after shift)
              [%const %i32 null-ptr]  ::  leading 1
              [%add %i32]
            ::
              [%store %i32 [0 0] ~]
            ::
              [%local-get space-val]
              [%load %i32 [0 0] ~]
              [%local-tee datum-size]
              [%local-get edge-copy]
              [%add %i32]
              [%const %i32 len-size]
              [%add %i32]
              [%local-get memsize]
              [%ge %i32 `%u]
              :^  %if  [~ ~]  ::  if copy_edge+datum_size+len_size >= memsize: attempt to grow
                :~
                  [%local-get datum-size]
                  [%local-get edge-copy]
                  [%add %i32]
                  [%local-get 0]
                  [%sub %i32]
                  [%const %i32 page-size]
                  [%div %i32 `%u]
                  [%const %i32 1]
                  [%add %i32]
                  [%memory-grow %0]
                  [%const %i32 minus-one]
                  [%eq %i32]
                  :^    %if  ::  if memory-grow yields -1: crash, else update memsize
                      [~ ~]
                    ~[[%unreachable ~]]
                  :~
                    [%const %i32 page-size]
                    [%memory-size %0]
                    [%mul %i32]
                    [%local-set memsize]
                  ==
                ==
              ~
            ::
              [%local-get edge-copy]  ::  copy to edge_copy
              [%local-get space-val]  ::  from ptr
              [%local-get datum-size]
              [%const %i32 len-size]
              [%add %i32]     ::  n+len-size bytes
              [%memory-copy %0 %0]
              [%local-get datum-size]
              [%const %i32 len-size]
              [%add %i32]
              [%local-get edge-copy]
              [%add %i32]
              [%local-set edge-copy]  ::  update edge_copy
            ==
          ==
        ::
          [%local-get space-val-ptr]
          [%const %i32 space-width]
          [%add %i32]
          [%local-set space-val-ptr]
          [%br 1]  ::  jump to loop
        ==
      ==
    ::
      [%const %i32 offset]  ::  copy to offset
      [%global-get 0]       ::  from edge
      [%local-get edge-copy]
      [%global-get 0]
      [%sub %i32]           ::  edge_copy - edge bytes
      [%memory-copy %0 %0]
    ::
      [%local-get edge-copy]
      [%global-get 0]
      [%sub %i32]
      [%const %i32 offset]
      [%add %i32]
      [%global-set 0]  ::  edge = edge_copy - edge + offset
    ==
  ::  set-i32-idx: space idx and value
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32 %i32] ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  allocated ptr
    =/  val=@  0
    =/  idx=@  1
    =/  ptr=@  2
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%const %i32 null-ptr]
      [%lt %i32 `%u]
      :^    %if
          [~ ~]
        :~
          [%local-get idx]
          [%const %i32 space-width]
          [%mul %i32]
          [%local-get val]
          [%store %i32 [0 offset] ~]
        ==
      :~  
        [%const %i32 ^~((add len-size 4))]
        [%call alloc-idx]
        [%local-tee ptr]
        [%const %i32 4]
        [%store %i32 [0 0] ~]
        [%local-get ptr]
        [%local-get val]
        [%store %i32 [0 len-size] ~]
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%local-get ptr]
        [%const %i32 null-ptr]
        [%add %i32]
        [%store %i32 [0 offset] ~]
      ==
    ==
  ::  set-i64-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i64 %i32] ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  allocated ptr
    =/  val=@  0
    =/  idx=@  1
    =/  ptr=@  2
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%const %i32 ^~((add len-size 8))]
      [%call alloc-idx]
      [%local-tee ptr]
      [%const %i32 8]
      [%store %i32 [0 0] ~]
      [%local-get ptr]
      [%local-get val]
      [%store %i64 [0 len-size] ~]
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%local-get ptr]
      [%const %i32 null-ptr]
      [%add %i32]
      [%store %i32 [0 offset] ~]
    ==
  ::  set-f32-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%f32 %i32] ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  allocated ptr
    =/  val=@  0
    =/  idx=@  1
    =/  ptr=@  2
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%reinterpret %i32 %f32]
      [%const %i32 null-ptr]
      [%lt %i32 `%u]
      :^    %if
          [~ ~]
        :~
          [%local-get idx]
          [%const %i32 space-width]
          [%mul %i32]
          [%local-get val]
          [%store %f32 [0 offset] ~]
        ==
      :~  
        [%const %i32 ^~((add len-size 4))]
        [%call alloc-idx]
        [%local-tee ptr]
        [%const %i32 4]
        [%store %i32 [0 0] ~]
        [%local-get ptr]
        [%local-get val]
        [%store %f32 [0 len-size] ~]
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%local-get ptr]
        [%const %i32 null-ptr]
        [%add %i32]
        [%store %i32 [0 offset] ~]
      ==
    ==
  ::  set-f64-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%f64 %i32] ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  allocated ptr
    =/  val=@  0
    =/  idx=@  1
    =/  ptr=@  2
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%const %i32 ^~((add len-size 8))]
      [%call alloc-idx]
      [%local-tee ptr]
      [%const %i32 8]
      [%store %i32 [0 0] ~]
      [%local-get ptr]
      [%local-get val]
      [%store %f64 [0 len-size] ~]
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%local-get ptr]
      [%const %i32 null-ptr]
      [%add %i32]
      [%store %i32 [0 offset] ~]
    ==
  ::  set-vec-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%v128 %i32] ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  allocated ptr
    =/  val=@  0
    =/  idx=@  1
    =/  ptr=@  2
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%const %i32 ^~((add len-size 16))]
      [%call alloc-idx]
      [%local-tee ptr]
      [%const %i32 16]
      [%store %i32 [0 0] ~]
      [%local-get ptr]
      [%local-get val]
      [%vec %store [0 len-size]]
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%local-get ptr]
      [%const %i32 null-ptr]
      [%add %i32]
      [%store %i32 [0 offset] ~]
    ==
  ::  get-i32-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32] ~[%i32]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  loaded value
    =/  idx=@  0
    =/  val=@  1
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%load %i32 [0 offset] ~ ~]
      [%local-tee val]
      [%const %i32 null-ptr]
      [%lt %i32 `%u]
      :^    %if
          [~ ~[%i32]]
        ~[[%local-get 1]]
      :~
        [%local-get val]
        [%const %i32 null-ptr]
        [%sub %i32]
        [%local-tee val]
        [%eqz %i32]
        :^    %if
            [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get val]
        [%load %i32 [0 0] ~ ~]
        [%const %i32 4]
        [%ne %i32]
        :^    %if
            [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get val]
        [%load %i32 [0 len-size] ~ ~]
      ==
    ==
  ::  get-i64-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32] ~[%i64]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  loaded value
    =/  idx=@  0
    =/  val=@  1
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%load %i32 [0 offset] ~ ~]
      [%local-tee val]
      [%const %i32 null-ptr]
      [%lt %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%const %i32 null-ptr]
      [%sub %i32]
      [%local-tee val]
      [%eqz %i32]
      :^    %if
          [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%load %i32 [0 0] ~ ~]
      [%const %i32 8]
      [%ne %i32]
      :^    %if
          [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%load %i64 [0 len-size] ~ ~]
    ==
  ::  get-f32-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32] ~[%f32]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  loaded value
    =/  idx=@  0
    =/  val=@  1
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%load %i32 [0 offset] ~ ~]
      [%local-tee val]
      [%const %i32 null-ptr]
      [%lt %i32 `%u]
      :^    %if
          [~ ~[%f32]]
        ~[[%local-get val]]
        [%reinterpret %f32 %i32]
      :~
        [%local-get val]
        [%const %i32 null-ptr]
        [%sub %i32]
        [%local-tee val]
        [%eqz %i32]
        :^    %if
            [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get val]
        [%load %i32 [0 0] ~ ~]
        [%const %i32 4]
        [%ne %i32]
        :^    %if
            [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get val]
        [%load %f32 [0 len-size] ~ ~]
      ==
    ==
  ::  get-f64-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32] ~[%f64]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  loaded value
    =/  idx=@  0
    =/  val=@  1
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%load %i32 [0 offset] ~ ~]
      [%local-tee val]
      [%const %i32 null-ptr]
      [%lt %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%const %i32 null-ptr]
      [%sub %i32]
      [%local-tee val]
      [%eqz %i32]
      :^    %if
          [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%load %i32 [0 0] ~ ~]
      [%const %i32 8]
      [%ne %i32]
      :^    %if
          [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%load %f64 [0 len-size] ~ ~]
    ==
  ::  get-vec-idx
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32] ~[%v128]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  loaded value
    =/  idx=@  0
    =/  val=@  1
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%load %i32 [0 offset] ~ ~]
      [%local-tee val]
      [%const %i32 null-ptr]
      [%lt %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%const %i32 null-ptr]
      [%sub %i32]
      [%local-tee val]
      [%eqz %i32]
      :^    %if
          [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%load %i32 [0 0] ~ ~]
      [%const %i32 16]
      [%ne %i32]
      :^    %if
          [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get val]
      [%vec %load [0 len-size] ~]
    ==
  ::  set-octs
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32 %i32 %i32] ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]  ::  ptr-king
    =/  ptr-serf=@  0
    =/  len=@  1
    =/  idx=@  2
    =/  ptr-king=@  3
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%const %i32 len-size]
      [%local-get len]
      [%add %i32]
      [%call alloc-idx]
      [%local-tee ptr-king]
      [%local-get len]
      [%store %i32 [0 0] ~]
      [%local-get ptr-serf]
      [%local-get ptr-king]
      [%const %i32 len-size]
      [%add %i32]
      [%local-get len]
      [%call mem-read-idx]
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%local-get ptr-king]
      [%const %i32 null-ptr]
      [%add %i32]
      [%store %i32 [0 offset] ~]
    ==
  ::  get-octs
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32 %i32 %i32 %i32] ~] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32 %i32]  ::  ptr-king, len
    =/  ptr-serf=@  0
    =/  offset-octs=@  1
    =/  len=@  2
    =/  idx=@  3
    =/  ptr-king=@  4
    =/  len-all=@  5
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%load %i32 [0 offset] ~ ~]
      [%local-tee ptr-king]
      [%const %i32 null-ptr]
      [%le %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get ptr-king]
      [%const %i32 null-ptr]
      [%sub %i32]
      [%local-tee ptr-king]
      [%load %i32 [0 0] ~ ~]
      [%local-set len-all]
    ::
      [%local-get offset-octs]
      [%local-get len]
      [%add %i32]
      [%local-get len-all]
      [%gt %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get ptr-king]
      [%const %i32 len-size]
      [%add %i32]
      [%local-get offset-octs]
      [%add %i32]
      [%local-get ptr-serf]
      [%local-get len]
      [%call mem-write-idx]
    ==
  ::  len
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32] ~[%i32]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32]
    =/  idx=@  0
    =/  ptr-king=@  1
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get idx]
      [%const %i32 space-width]
      [%mul %i32]
      [%load %i32 [0 offset] ~ ~]
      [%local-tee ptr-king]
      [%const %i32 null-ptr]
      [%le %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get ptr-king]
      [%const %i32 null-ptr]
      [%sub %i32]
      [%local-tee ptr-king]
      [%load %i32 [0 0] ~ ~]
    ==
  ::  read-octs-i32
  ::
   =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32 %i32 %i32] ~[%i32]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32 %i32]
    =/  off=@  0
    =/  len=@  1
    =/  idx=@  2
    =/  ptr-king=@  3
    =/  len-all=@  4
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get len]
      [%eqz %i32]
      :^  %if  [~ ~[%i32]]
          ~[[%const %i32 0]]
      :~
        [%local-get len]
        [%const %i32 4]
        [%gt %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%load %i32 [0 offset] ~ ~]
        [%local-tee ptr-king]
        [%const %i32 null-ptr]
        [%le %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr-king]
        [%const %i32 null-ptr]
        [%sub %i32]
        [%local-tee ptr-king]
        [%load %i32 [0 0] ~ ~]
        [%local-set len-all]
      ::
        [%local-get off]
        [%local-get len]
        [%add %i32]
        [%local-get len-all]
        [%gt %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get len]
        [%const %i32 1]
        [%eq %i32]
        :^  %if  [~ ~[%i32]]
          :~
            [%local-get ptr-king]
            [%local-get off]
            [%add %i32]
            [%load %i32 [0 len-size] `%8 `%u]
          ==
        :~
          [%local-get len]
          [%const %i32 2]
          [%eq %i32]
          :^  %if  [~ ~[%i32]]
            :~
              [%local-get ptr-king]
              [%local-get off]
              [%add %i32]
              [%load %i32 [0 len-size] `%16 `%u]
            ==
          :~
            [%local-get len]
            [%const %i32 3]
            [%eq %i32]
            :^  %if  [~ ~[%i32]]
              :~
                [%local-get ptr-king]
                [%local-get off]
                [%add %i32]
                [%load %i32 [0 len-size] `%16 `%u]
                [%local-get ptr-king]
                [%local-get off]
                [%add %i32]
                [%load %i32 [0 (add len-size 16)] `%8 `%u]
                [%const %i32 16]
                [%shl %i32]
                [%add %i32]
              ==
            :~
              [%local-get ptr-king]
              [%local-get off]
              [%add %i32]
              [%load %i32 [0 len-size] ~ ~]
            ==
          ==
        ==
      ==
    ==
  ::  read-octs-i64
  ::
  =^  type-idx=@  type-section.king
    (get-type-idx [~[%i32 %i32 %i32] ~[%i64]] type-section.king)
  =.  function-section.king
    (snoc function-section.king type-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32 %i32 %i64 %i32]
    =/  off=@  0
    =/  len=@  1
    =/  idx=@  2
    =/  ptr-king=@  3
    =/  len-all=@  4
    =/  res=@  5
    =/  pow=@  6
    ^-  expression:wasm
    :~
      [%local-get idx]
      [%const %i32 space-number]
      [%ge %i32 `%u]
      :^  %if  [~ ~]
        ~[[%unreachable ~]]
      ~
    ::
      [%local-get len]
      [%eqz %i32]
      :^  %if  [~ ~[%i32]]
          ~[[%const %i32 0]]
      :~
        [%local-get len]
        [%const %i32 8]
        [%gt %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%load %i32 [0 offset] ~ ~]
        [%local-tee ptr-king]
        [%const %i32 null-ptr]
        [%le %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr-king]
        [%const %i32 null-ptr]
        [%sub %i32]
        [%local-tee ptr-king]
        [%load %i32 [0 0] ~ ~]
        [%local-set len-all]
      ::
        [%local-get off]
        [%local-get len]
        [%add %i32]
        [%local-get len-all]
        [%gt %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        :+  %loop  [~ ~[%i64]]
        :~
          [%local-get len]
          [%local-get pow]
          [%const %i32 3]
          [%shr %i32 %u]
          [%le %i32 `%u]
          :^  %if  [~ ~[%i64]]
              ~[[%local-get res]]
          :~
            [%local-get ptr-king]
            [%local-get off]
            [%add %i32]
            [%load %i64 [0 len-size] `%8 `%u]
            [%local-get pow]
            [%shl %i64]
            [%local-get res]
            [%add %i64]
            [%local-set res]
            [%const %i32 8]
            [%local-get pow]
            [%add %i32]
            [%local-set pow]
            [%br 1]
          ==
        ==
      ==
    ==
  ::  read-octs-f32
  ::  read-octs-f64
  ::
  ::  Function imports of serf are defined in ext field of the sample.
  ::  Imports of other parts of the store are not treated by king, and
  ::  an attempt to access them leads to serf blocking which immediately
  ::  blocks king.  Same applies to call_indirect on an imported table.
  ::  An attempt to dereference a function reference from an imported table
  ::  will cause a trap, since Lia only handles exactly one Wasm module for 
  ::  now, and passing external function references through Arvo between
  ::  two Lia instances is some extremely cursed business wrt jetting
  ::  (i might be wrong)
  ::
  ::  compile and export main, ext
  ::
  =/  func-idx=@
    (add (lent import-section.king) (lent function-section.king))
  =.  king
    |-  ^-  module:wasm
    ?~  code  king
    =*  act  i.code
    =^  type-idx=@  type-section.king
      %+  get-type-idx
        [(turn -.type.act convert) (turn +.type.act convert)]
      type-section.king
    =.  function-section.king
      (snoc function-section.king type-idx)
    %=    $
        code  t.code
        code-section.king
      %+  snoc  code-section.king
      :-  ~
      ^-  expression:wasm
      (zing (turn body.act translate))
    ==
::
++  get-type-idx
  |=  [t=func-type.wasm s=type-section.wasm]
  ^-  [@ type-section.wasm]
  =/  maybe  (find ~[t] s)
  ?^  maybe  [u.maybe s]
  :-  (lent s)
  (snoc s t)
::
++  convert
  |=  v=value-type:line:lia
  ^-  valtype:wasm
  ?:  ?=(%octs v)  %i32  ::  space idx
  v
::
++  translate
  |=  =op:line:lia
  ^-  (list instruction:wasm)
  ?-    -.op
      ?(%add %sub %br %br-if %nop %drop)  ~[op]
      %get
    ?>  (lth idx.op space-number)
    :~
      [%const %i32 idx.op]
      :-  %call
      ?-  type.op
        %i32  get-i32-idx
        %i64  get-i64-idx
        %f32  get-f32-idx
        %f64  get-f64-idx
        %vec  get-vec-idx
      ==
    ==
  ::
      %set
    ?>  (lth idx.op space-number)
    :~
      [%const %i32 idx.op]
      :-  %call
      ?-  type.op
        %i32  set-i32-idx
        %i64  set-i64-idx
        %f32  set-f32-idx
        %f64  set-f64-idx
        %vec  set-vec-idx
      ==
    ==
  ::
      %let  ~
      %run  [%call (~(got by serf-diary) name.op)]~
      %run-ext  [%call (~(got by king-diary) name.op)]~
      %read
    ?>  (lth p.op space-number)
    :~
      [%const %i32 p.op]
      [%call set-octs-idx]
    ==
  ::
      %writ
    ?>  (lth p.op space-number)
    :~
      [%const %i32 p.op]
      [%call get-octs-idx]
    ==
  ::
      %block
    :_  ~
    :+  %block
      [(turn -.type.op convert) (turn +.type.op convert)]
    ^-  expression:wasm
    (zing (turn body.op translate))
  ::
      %if
    :_  ~
    :^    %if
        [(turn -.type.op convert) (turn +.type.op convert)]
      ^-  expression:wasm
      (zing (turn true.op translate))
    ^-  expression:wasm
    (zing (turn false.op translate))
  ::
      %loop
    :_  ~
    :+  %loop
      [(turn -.type.op convert) (turn +.type.op convert)]
    ^-  expression:wasm
    (zing (turn body.op translate))
  ::
      %const
    :_  ~
    ?.  ?=(%v128 -.p.op)
      op
    [%vec %const p.op]
  ::
      %len
    ?>  (lth idx.op space-number)
    :~
      [%const %i32 idx.op]
      [%call len-idx]
    ==
  ::
    
  ==
--