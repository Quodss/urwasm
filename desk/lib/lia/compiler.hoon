/+  lia-linearizer
=>  lia-linearizer
:: ~%  %lia-comp  +  ~
|%
++  compiler
  =/  wasm  wasm-sur
  =/  lia   lia-sur
  ::
  |%
  ++  addr-size  `@`8                                 ::  space address size in bits
  ++  space-width  `@`8                               ::  size of space element in bytes
  ++  minus-one-32  ^~((dec (bex 32)))
  ++  minus-one-64  ^~((dec (bex 64)))
  ++  null-ptr  ^~((bex (dec (mul 8 space-width))))
  ++  empty-octs  ^~(minus-one-32)
  ++  space-number  ^~((bex addr-size))               ::  number of elements in space
  ++  space-size  ^~((mul space-width space-number))  ::  space size in bytes
  ++  page-size  `@`65.536                            ::  page size in bytes
  ++  heap-pages  `@`1                                ::  addressable heap limit in pages; need ~2x of this memory for gc
  ++  heap-lim  ^~((mul page-size heap-pages))        ::  addressable heap limit in bytes
  ++  len-size  `@`4                                  ::  size of length prefix in bytes
  ++  offset  `@`0  :: v                              ::  space offset
  ::  #################|+++++++++++++++++++++++++++++++++++++|...
  ::  ^static data       ^space with numeric values/pointers   ^heap
  ::
  ++  main
    |=  $:  serf=module:wasm
            code=(list action:line:lia)
            ext=(map (pair cord cord) ext-func:line:lia)
            import=(map term block-type:line:lia)
        ==
    ^-  module:wasm
    =/  n-import-funcs=@
      (lent (skim import-section.serf |=(import:wasm ?=(%func -.desc))))
    =|  king=module:wasm
    ::  Add funcs exported by serf as imports in king
    ::  Exports of globals, tables and memory are not treated
    ::
    =/  exports-serf=(list [cord @])
      =|  out=(list [cord @])
      |-  ^-  (list [cord @])
      ?~  export-section.serf  out
      =/  exp=export:wasm  i.export-section.serf
      =?  out  ?=(%func -.export-desc.exp)
        [[name.exp i.export-desc.exp] out]
      $(export-section.serf t.export-section.serf)
    =^  serf-diary=(map cord @)  king
      =|  d=(map cord @)
      =/  i=@  0
      =<  [d k]
      %+  roll  exports-serf
      |:  [[name=*cord idx=*@] acc=[d=d i=i k=king]]
      ^-  [(map cord @) @ module:wasm]
      =/  idx-local  (sub idx n-import-funcs)
      =/  type-idx  (snag idx-local function-section.serf)
      =/  type=func-type:wasm  (snag type-idx type-section.serf)
      =^  king-type-idx=@  type-section.k.acc
        (get-type-idx type type-section.k.acc)
      %=    acc
          i  +(i.acc)
          d  (~(put by d.acc) name i.acc)
      ::
          import-section.k
        %+  snoc  import-section.k.acc
        ^-  import:wasm
        :+  'serf'  name
        [%func king-type-idx]
      ==
    =.  memory-section.king  ~[[%flor 1]]  ::  init single page, potentially unbound
    =.  global-section.king
      ?>  (lth (add offset space-size) page-size)
      :~
        [%i32 %var %const %i32 (add offset space-size)]  ::  heap edge
        [%i32 %var %const %i32 0]                        ::  min space element idx used
        [%i32 %var %const %i32 0]                        ::  max space element idx used
        [%i32 %var %const %i32 0]                        ::  data elem with target
      ==
    =/  heap-edge=@    0
    =/  space-start=@  1
    =/  space-end=@    2
    =/  space-clue=@   3
    ::  add memory read and write (Wasm modules support
    ::  up to one memory for now, so we'll have to resort
    ::  to import functions to copy stuff around instead
    ::  of calling memory.copy)
    ::
    =^  type-memio-idx=@  type-section.king
      (get-type-idx [~[%i32 %i32 %i32] ~] type-section.king)  ::  from, to, len
    =.  import-section.king
      %+  weld  import-section.king
      ^-  import-section:wasm
      :~
        ['memio' 'read' %func type-memio-idx]
        ['memio' 'write' %func type-memio-idx]
      ==
    =/  mem-write-idx=@  (dec (lent import-section.king))
    =/  mem-read-idx=@   (dec mem-write-idx)
    ::  add Lia imports
    ::
    =^  king-diary=(map cord @)  king
      =|  d=(map cord @)
      =/  i=@  (lent import-section.king)
      =<  [d.acc k.acc]
      %-  ~(rep by import)
      |:  [[name=*term type=*block-type:line:lia] acc=[d=d i=i k=king]]
      ^-  [(map cord @) @ module:wasm]
      =^  type-idx=@  type-section.k.acc
        %+  get-type-idx
          [(turn p.type convert) ~]
        type-section.k.acc
      %=    acc
          i  +(i.acc)
          d  (~(put by d.acc) name i.acc)
      ::
          import-section.k
        %+  snoc  import-section.k.acc
        ['lia' name %func type-idx]
      ==
    ::  local funcs
    ::
    =/  alloc-idx=@          (lent import-section.king)
    =/  gc-idx=@             +(alloc-idx)
    =/  set-i32-idx=@        +(gc-idx)
    =/  set-i64-idx=@        +(set-i32-idx)
    =/  set-f32-idx=@        +(set-i64-idx)
    =/  set-f64-idx=@        +(set-f32-idx)
    =/  set-vec-idx=@        +(set-f64-idx)
    =/  get-i32-idx=@        +(set-vec-idx)
    =/  get-i64-idx=@        +(get-i32-idx)
    =/  get-f32-idx=@        +(get-i64-idx)
    =/  get-f64-idx=@        +(get-f32-idx)
    =/  get-vec-idx=@        +(get-f64-idx)
    =/  set-octs-idx=@       +(get-vec-idx)
    =/  give-octs-idx=@      +(set-octs-idx)
    =/  len-idx=@            +(give-octs-idx)
    =/  read-octs-i32-idx=@  +(len-idx)
    =/  read-octs-i64-idx=@  +(read-octs-i32-idx)
    =/  read-octs-f32-idx=@  +(read-octs-i64-idx)
    =/  read-octs-f64-idx=@  +(read-octs-f32-idx)
    =/  writ-octs-i32-idx=@  +(read-octs-f64-idx)
    =/  set-octs-ext-idx=@   +(writ-octs-i32-idx)
    =/  get-space-ptr-idx=@  +(set-octs-ext-idx)
    =/  clear-space-idx=@    +(get-space-ptr-idx)
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
        [%global-get heap-edge]
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
            [%global-get heap-edge]
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
                [%const %i32 minus-one-32]
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
        [%global-get heap-edge]  ::  old edge is the pointer
        [%local-get edge-new]
        [%global-set heap-edge]  ::  increase the edge
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
              %i64  ::  (3) space value/ptr
              %i32  ::  (4) space ptr truncated
              %i32  ::  (4) datum size
          ==
      =/  memsize=@        0
      =/  edge-copy=@      1
      =/  space-val-ptr=@  2
      =/  space-val=@      3
      =/  space-ptr-trunc=@  4
      =/  datum-size=@     5
      ^-  expression:wasm
      :~
        [%const %i32 space-size]
        [%global-get heap-edge]
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
            [%const %i32 minus-one-32]
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
        ^-  instruction:wasm
        :+  %loop  [~ ~]
        :~
          [%local-get space-val-ptr]
          [%const %i32 space-size]
          [%ge %i32 `%u]
          ^-  instruction:wasm
          :^    %if  ::  if space ptr address >= space-size: end loop
              [~ ~]
            ~
          ^-  (list instruction:wasm)
          :~  ::  else continue loop
            [%local-get space-val-ptr]
            [%load %i64 [0 offset] ~ ~]
            [%local-tee space-val]
            [%const %i64 null-ptr]
            [%lt %i64 `%u]
            ^-  instruction:wasm
            :^    %if  ::  if MSB set to 0 then it's numerical value, copy
                [~ ~]
              ^-  (list instruction:wasm)
              :~
                [%global-get heap-edge]
                [%local-get space-val-ptr]
                [%add %i32]
                [%local-get space-val]
                [%store %i64 [0 0] ~]
              ==
            ^-  (list instruction:wasm)
            :~  ::  else it's a pointer
              [%local-get space-val]
              [%const %i64 null-ptr]
              [%sub %i64]
              [%wrap ~]
              [%local-tee space-ptr-trunc]  ::  remove leading 1
              [%eqz %i32]
              [%local-get space-ptr-trunc]
              [%const %i32 empty-octs]
              [%eq %i32]
              [%or %i32]
              ^-  instruction:wasm
              :^    %if  ::  if NULL or empty: write to destination
                  [~ ~]
                :~
                  [%global-get heap-edge]
                  [%local-get space-val-ptr]
                  [%add %i32]
                  [%local-get space-val]
                  [%store %i64 [0 0] ~]
                ==
              ^-  (list instruction:wasm)
              :~  ::  else: set pointer, copy data
                [%global-get heap-edge]
                [%local-get space-val-ptr]
                [%add %i32]
              ::
                [%local-get edge-copy]
                [%global-get heap-edge]
                [%sub %i32]
                [%const %i32 offset]
                [%add %i32]             ::  (copy_edge - edge + offset = edge after shift)
                [%extend %i64 %32 %u]
                [%const %i64 null-ptr]  ::  leading 1
                [%add %i64]
              ::
                [%store %i64 [0 0] ~]
              ::
                [%local-get space-ptr-trunc]
                [%load %i32 [0 0] ~ ~]
                [%local-tee datum-size]
                [%local-get edge-copy]
                [%add %i32]
                [%const %i32 len-size]
                [%add %i32]
                [%local-get memsize]
                [%ge %i32 `%u]
                ^-  instruction:wasm
                :^  %if  [~ ~]  ::  if copy_edge+datum_size+len_size >= memsize: attempt to grow
                  :~
                    [%local-get datum-size]
                    [%local-get edge-copy]
                    [%add %i32]
                    [%const %i32 len-size]
                    [%add %i32]
                    [%local-get memsize]
                    [%sub %i32]
                    [%const %i32 page-size]
                    [%div %i32 `%u]
                    [%const %i32 1]
                    [%add %i32]
                    [%memory-grow %0]
                    [%const %i32 minus-one-32]
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
                [%local-get space-ptr-trunc]  ::  from ptr
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
        [%global-get heap-edge]       ::  from edge
        [%local-get edge-copy]
        [%global-get heap-edge]
        [%sub %i32]           ::  edge_copy - edge bytes
        [%memory-copy %0 %0]
      ::
        [%local-get edge-copy]
        [%global-get heap-edge]
        [%sub %i32]
        [%const %i32 offset]
        [%add %i32]
        [%global-set heap-edge]  ::  edge = edge_copy - edge + offset
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
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%local-tee ptr]
        [%local-get val]
        [%store %i32 [0 offset] ~]
      ::
        [%local-get ptr]
        [%const %i32 0]
        [%store %i32 [0 (add offset 4)] ~]  ::  zero out upper half
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
        [%local-get val]
        [%const %i64 null-ptr]
        [%lt %i64 `%u]
        :^    %if
            [~ ~]
          :~
            [%local-get idx]
            [%const %i32 space-width]
            [%mul %i32]
            [%local-get val]
            [%store %i64 [0 offset] ~]
          ==
        :~
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
          [%extend %i64 %32 %u]
          [%const %i64 null-ptr]
          [%add %i64]
          [%store %i64 [0 offset] ~]
        ==
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
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%local-tee ptr]
        [%local-get val]
        [%store %f32 [0 offset] ~]
      ::
        [%local-get ptr]
        [%const %i32 0]
        [%store %i32 [0 (add offset 4)] ~]  ::  zero out upper half
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
        [%local-get val]
        [%reinterpret %i64 %f64]
        [%const %i64 null-ptr]
        [%lt %i64 `%u]
        :^    %if
            [~ ~]
          :~
            [%local-get idx]
            [%const %i32 space-width]
            [%mul %i32]
            [%local-get val]
            [%store %f64 [0 offset] ~]
          ==
        :~  
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
          [%extend %i64 %32 %u]
          [%const %i64 null-ptr]
          [%add %i64]
          [%store %i64 [0 offset] ~]
        ==
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
        [%extend %i64 %32 %u]
        [%const %i64 null-ptr]
        [%add %i64]
        [%store %i64 [0 offset] ~]
      ==
    ::  get-i32-idx
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32] ~[%i32]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~
      =/  idx=@  0
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
      ==
    ::  get-i64-idx
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32] ~[%i64]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64 %i32]  ::  loaded value, ptr
      =/  idx=@  0
      =/  val=@  1
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
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%load %i64 [0 offset] ~ ~]
        [%local-tee val]
        [%const %i64 null-ptr]
        [%lt %i64 `%u]
        :^  %if  [~ ~[%i64]]
          ~[[%local-get val]]
        :~
          [%local-get val]
          [%const %i64 null-ptr]
          [%sub %i64]
          [%wrap ~]
          [%local-tee ptr]
          [%eqz %i32]
          :^    %if
              [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get ptr]
          [%load %i32 [0 0] ~ ~]
          [%const %i32 8]
          [%ne %i32]
          :^    %if
              [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get ptr]
          [%load %i64 [0 len-size] ~ ~]
        ==
      ==
    ::  get-f32-idx
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32] ~[%f32]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~
      =/  idx=@  0
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
        [%load %f32 [0 offset] ~ ~]
      ==
    ::  get-f64-idx
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32] ~[%f64]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64 %i32]  ::  loaded value, ptr
      =/  idx=@  0
      =/  val=@  1
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
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%load %i64 [0 offset] ~ ~]
        [%local-tee val]
        [%const %i64 null-ptr]
        [%lt %i64 `%u]
        :^  %if  [~ ~[%f64]]
          :~
            [%local-get val]
            [%reinterpret %f64 %i64]
          ==
        :~
          [%local-get val]
          [%const %i64 null-ptr]
          [%sub %i64]
          [%wrap ~]
          [%local-tee ptr]
          [%eqz %i32]
          :^    %if
              [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get ptr]
          [%load %i32 [0 0] ~ ~]
          [%const %i32 8]
          [%ne %i32]
          :^    %if
              [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get ptr]
          [%load %f64 [0 len-size] ~ ~]
        ==
      ==
    ::  get-vec-idx
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32] ~[%v128]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64 %i32]  ::  loaded value, ptr
      =/  idx=@  0
      =/  val=@  1
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
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%load %i64 [0 offset] ~ ~]
        [%local-tee val]
        [%const %i64 null-ptr]
        [%lt %i64 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get val]
        [%const %i64 null-ptr]
        [%sub %i64]
        [%wrap ~]
        [%local-tee ptr]
        [%eqz %i32]
        :^    %if
            [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr]
        [%load %i32 [0 0] ~ ~]
        [%const %i32 16]
        [%ne %i32]
        :^    %if
            [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr]
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
        [%local-get len]
        [%eqz %i32]
        :^  %if  [~ ~]
          :~
            [%local-get idx]
            [%const %i32 space-width]
            [%mul %i32]
            [%const %i64 minus-one-64]
            [%store %i64 [0 offset] ~]
          ==
        :~
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
          [%extend %i64 %32 %u]
          [%const %i64 null-ptr]
          [%add %i64]
          [%store %i64 [0 offset] ~]
        ==
      ==
    ::  give-octs
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32 %i32 %i32 %i32] ~] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i32 %i64 %i32]  ::  ptr-king, king-val, len
      =/  ptr-serf=@     0
      =/  offset-octs=@  1
      =/  len=@          2
      =/  idx=@          3
      =/  ptr-king=@     4
      =/  king-val=@     5
      =/  len-all=@      6
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
        [%load %i64 [0 offset] ~ ~]
        [%local-tee king-val]
        [%const %i64 null-ptr]
        [%le %i64 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get king-val]
        [%const %i64 null-ptr]
        [%sub %i64]
        [%wrap ~]
        [%local-tee ptr-king]
        [%const %i32 empty-octs]
        [%eq %i32]
        :^  %if  [~ ~]
          ~
        :~
          [%local-get ptr-king]
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
      ==
    ::  len
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32] ~[%i32]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64 %i32]
      =/  idx=@  0
      =/  king-val=@  1
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
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%load %i64 [0 offset] ~ ~]
        [%local-tee king-val]
        [%const %i64 null-ptr]
        [%le %i64 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get king-val]
        [%const %i64 null-ptr]
        [%sub %i64]
        [%wrap ~]
        [%local-tee ptr]
        [%const %i32 empty-octs]
        [%eq %i32]
        :^  %if  [~ ~[%i32]]
          ~[[%const %i32 0]]
        :~
          [%local-get ptr]
          [%load %i32 [0 0] ~ ~]
        ==
      ==
    ::  read-octs-i32
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32 %i32 %i32] ~[%i32]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64 %i32 %i32]
      =/  off=@  0
      =/  len=@  1
      =/  idx=@  2
      =/  king-val=@  3
      =/  ptr-king=@  4
      =/  len-all=@   5
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
          [%load %i64 [0 offset] ~ ~]
          [%local-tee king-val]
          [%const %i64 null-ptr]
          [%le %i64 `%u]
          :^  %if  [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get king-val]
          [%const %i64 null-ptr]
          [%sub %i64]
          [%wrap ~]
          [%local-tee ptr-king]
          [%const %i32 empty-octs]
          [%eq %i32]
          :^  %if  [~ ~]
            ~[[%unreachable ~]]
          ~
          [%local-get ptr-king]
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
      :-  ~[%i64 %i32 %i32 %i64 %i32]
      =/  off=@  0
      =/  len=@  1
      =/  idx=@  2
      =/  king-val=@  3
      =/  ptr-king=@  4
      =/  len-all=@   5
      =/  res=@       6
      =/  pow=@       7
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
        :^  %if  [~ ~[%i64]]
            ~[[%const %i64 0]]
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
          [%load %i64 [0 offset] ~ ~]
          [%local-tee king-val]
          [%const %i64 null-ptr]
          [%le %i64 `%u]
          :^  %if  [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get king-val]
          [%const %i64 null-ptr]
          [%sub %i64]
          [%wrap ~]
          [%local-tee ptr-king]
          [%const %i32 empty-octs]
          [%eq %i32]
          :^  %if  [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get ptr-king]
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
              [%extend %i64 %32 %u]
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
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32 %i32] ~[%f32]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64 %i32 %i32]
      =/  off=@  0
      =/  idx=@  1
      =/  king-val=@  2
      =/  ptr-king=@  3
      =/  len-all=@   4
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
        [%load %i64 [0 offset] ~ ~]
        [%local-tee king-val]
        [%const %i64 null-ptr]
        [%le %i64 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get king-val]
        [%const %i64 null-ptr]
        [%sub %i64]
        [%wrap ~]
        [%local-tee ptr-king]
        [%const %i32 empty-octs]
        [%eq %i32]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr-king]
        [%load %i32 [0 0] ~ ~]
        [%local-set len-all]
      ::
        [%local-get off]
        [%const %i32 4]
        [%add %i32]
        [%local-get len-all]
        [%gt %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr-king]
        [%local-get off]
        [%add %i32]
        [%load %f32 [0 len-size] ~ ~]
      ==
    ::  read-octs-f64
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32 %i32] ~[%f64]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64 %i32 %i32]
      =/  off=@  0
      =/  idx=@  1
      =/  king-val=@  2
      =/  ptr-king=@  3
      =/  len-all=@   4
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
        [%load %i64 [0 offset] ~ ~]
        [%local-tee king-val]
        [%const %i64 null-ptr]
        [%le %i64 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get king-val]
        [%const %i64 null-ptr]
        [%sub %i64]
        [%wrap ~]
        [%local-tee ptr-king]
        [%const %i32 empty-octs]
        [%eq %i32]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr-king]
        [%load %i32 [0 0] ~ ~]
        [%local-set len-all]
      ::
        [%local-get off]
        [%const %i32 8]
        [%add %i32]
        [%local-get len-all]
        [%gt %i32 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get ptr-king]
        [%local-get off]
        [%add %i32]
        [%load %f64 [0 len-size] ~ ~]
      ==
    ::  writ-octs-i32-idx
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32 %i32] ~] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i32]
      =/  dat=@  0
      =/  idx=@  1
      =/  king-ptr=@  2
      ^-  expression:wasm
      :~
        [%const %i32 len-size]
        [%const %i32 4]
        [%add %i32]
        [%call alloc-idx]
        [%local-tee king-ptr]
        [%const %i32 4]
        [%store %i32 [0 0] ~]
        [%local-get king-ptr]
        [%local-get dat]
        [%store %i32 [0 len-size] ~]
        [%local-get idx]
        [%const %i32 space-width]
        [%mul %i32]
        [%local-get king-ptr]
        [%extend %i64 %32 %u]
        [%const %i64 null-ptr]
        [%add %i64]
        [%store %i64 [0 offset] ~]
      ==
    ::  set-octs-ext
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32 %i32] ~[%i32]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i32]
      =/  len=@  0
      =/  idx=@  1
      =/  king-ptr=@  2
      ^-  expression:wasm
      :~
        [%local-get len]
        :^  %if  [~ ~[%i32]]
          :~
            [%const %i32 len-size]
            [%local-get len]
            [%add %i32]
            [%call alloc-idx]
            [%local-tee king-ptr]
            [%local-get len]
            [%store %i32 [0 0] ~]
          ::
            [%local-get idx]
            [%const %i32 space-width]
            [%mul %i32]
            [%local-get king-ptr]
            [%extend %i64 %32 %u]
            [%const %i64 null-ptr]
            [%add %i64]
            [%store %i64 [0 offset] ~]
          ::
            [%local-get king-ptr]
            [%const %i32 len-size]
            [%add %i32]
          ==
        :~
          [%local-get idx]
          [%const %i32 space-width]
          [%mul %i32]
          [%const %i64 minus-one-64]
          [%store %i64 [0 offset] ~]
          [%const %i32 minus-one-32]
        ==
      ==
    ::  get-space-ptr
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~[%i32] ~[%i32]] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~[%i64]
      =/  idx=@  0
      =/  king-val=@  1
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
        [%load %i64 [0 offset] ~ ~]
        [%local-tee king-val]
        [%const %i64 null-ptr]
        [%le %i64 `%u]
        :^  %if  [~ ~]
          ~[[%unreachable ~]]
        ~
      ::
        [%local-get king-val]
        [%const %i64 null-ptr]
        [%sub %i64]
        [%wrap ~]
      ==
    ::  clear-space
    ::
    =^  type-idx=@  type-section.king
      (get-type-idx [~ ~] type-section.king)
    =.  function-section.king
      (snoc function-section.king type-idx)
    =.  code-section.king
      %+  snoc  code-section.king
      :-  ~
      ^-  expression:wasm
      :~
        [%const %i32 offset]
        [%const %i32 0]
        [%const %i32 space-size]
        [%memory-init 0 %0]
        [%const %i32 ^~((add offset space-size))]
        [%global-set heap-edge]
        [%const %i32 0]
        [%global-set space-start]
        [%const %i32 0]
        [%global-set space-end]
      ==
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
    ::  initialize data
    ::
    =.  data-section.king
      :_  ~
      :+  %acti  [%const %i32 offset]
      [space-size (fil 6 space-number null-ptr)]
    ::  compile and export actions, ext
    ::
    |^
    =/  act-func-first-idx=@
      (add (lent import-section.king) (lent function-section.king))
    ::
    =.  king
      |-  ^-  module:wasm
      ?~  code  king
      =*  act  i.code
      =/  type=func-type:wasm  [~ (turn q.type.act convert)]
      =^  type-idx=@  type-section.king
        (get-type-idx type type-section.king)
      =^  result=(list (list instruction:wasm))  data-section.king
        (spin body.act data-section.king translate)
      %=    $
          code  t.code
      ::
          function-section.king
        (snoc function-section.king type-idx)
      ::
          code-section.king
        %+  snoc  code-section.king
        [~ `expression:wasm`(zing result)]
      ==
    =/  n-funcs=@  (lent code)
    ?<  =(0 n-funcs)
    =.  global-section.king
      %+  weld  global-section.king
      ^-  global-section:wasm
      :~  [%i32 %con %const %i32 act-func-first-idx]
          [%i32 %con %const %i32 n-funcs]
      ==
    =.  export-section.king
      =/  len-glob=@  (lent global-section.king)
      :~  ['act-0-func-idx' %glob (sub len-glob 2)]
          ['n-funcs' %glob (sub len-glob 1)]
          ['space-start' %glob space-start]
          ['space-clue' %glob space-clue]
          ['clear-space' %func clear-space-idx]
          ['set-octs-ext' %func set-octs-ext-idx]
          ['get-space-ptr' %func get-space-ptr-idx]
          ['set-i32' %func set-i32-idx]
          ['set-i64' %func set-i64-idx]
          ['set-f32' %func set-f32-idx]
          ['set-f64' %func set-f64-idx]
          ['set-vec' %func set-vec-idx]
          ['get-i32' %func get-i32-idx]
          ['get-i64' %func get-i64-idx]
          ['get-f32' %func get-f32-idx]
          ['get-f64' %func get-f64-idx]
          ['get-vec' %func get-vec-idx]
      ==
    ::
    =.  king
      =<  +  %-  ~(rep by ext)
      |:  :*  [name=['' ''] ext-func=*ext-func:line:lia]
              f-idx=(add act-func-first-idx n-funcs)
              king=king
          ==
      ^-  [@ module:wasm]
      =/  type=func-type:wasm  type.ext-func
      =/  space-start-save=@  (lent params.type)
      =/  space-end-cache=@  +(space-start-save)
      =/  ext-translate-app  (ext-translate space-end-cache)
      =^  result=(list (list instruction:wasm))  data-section.king
        (spin body.ext-func data-section.king ext-translate-app)
      =^  type-idx=@  type-section.king
        (get-type-idx type type-section.king)
      :-  +(f-idx)
      %=    king
          function-section
        (snoc function-section.king type-idx)
      ::
          code-section
        %+  snoc  code-section.king
        :-  ~[%i32 %i32]  ::  space-start prev frame, new space-end cache
        ^-  expression:wasm
        ;:  weld
          ^-  (list instruction:wasm)
          :~
            [%global-get space-start]
            [%local-set space-start-save]
            [%global-get space-end]
            [%global-set space-start]
          ==
        ::
          =/  n-params=@  (lent p.type.ext-func)
          ^-  (list instruction:wasm)
          %-  zing
          |-  ^-  (list (list instruction:wasm))
          ?~  p.type.ext-func  ~
          =/  idx-param=@  (dec n-params)
          :_  $(p.type.ext-func t.p.type.ext-func, n-params (dec n-params))
          ^-  (list instruction:wasm)
          :*
            [%local-get idx-param]
            -:(ext-translate-app [%set i.p.type.ext-func idx-param] *data-section:wasm)
          ==
        ::
          `(list instruction:wasm)`(zing result)
        ::
          ^-  (list instruction:wasm)
          :~
            [%local-get space-start-save]
            [%global-set space-start]
          ==
        ==
      ::
          export-section
        %+  snoc  export-section.king
        :_  [%func f-idx]
        =/  size=@  (met 3 -.name)
        ;:  add  
          -.name
          (lsh [3 size] '/')
          (lsh [3 +(size)] +.name)
        ==
      ::
      ==
    =.  datacnt-section.king  `(lent data-section.king)
    ::  Done!
    ::
    king
    ::
    ++  ext-translate
      |=  space-end-cache=@
      |=  [=op:line:lia d=data-section:wasm]
      =*  this  .
      ^-  [(list instruction:wasm) data-section:wasm]
      ?.  ?=(?(%let %loop %if) -.op)  (translate op d)
      ?-    -.op
          %if
        =^  result-true=(list (list instruction:wasm))  d
          (spin true.op d this)
        =^  result-false=(list (list instruction:wasm))  d
          (spin false.op d this)
        :_  d
        :_  ~
        :^    %if
            [(turn p.type.op convert) (turn q.type.op convert)]
          ^-  expression:wasm
          (zing result-true)
        ^-  expression:wasm
        (zing result-false)
      ::
          %loop
        =^  result=(list (list instruction:wasm))  d
          (spin body.op d this)
        :_  d
        :_  ~
        :+  %loop
          [(turn p.type.op convert) (turn q.type.op convert)]
        ^-  expression:wasm
        (zing result)
      ::
          %let
        :_  d
        ?>  (lth idx.op space-number)
        ?-    type.op
            %i32
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
            [%local-tee space-end-cache]
            [%global-get space-end]
            [%ge %i32 `%u]
            :^  %if  [~ ~[%i32]]
              ~[[%local-get space-end-cache] [%const %i32 1] [%add %i32]]
            ~[[%global-get space-end]]
          ::
            [%global-set space-end]
            [%const %i32 0]
            [%local-get space-end-cache]
            [%call set-i32-idx]
          ==
        ::
            %i64
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
            [%local-tee space-end-cache]
            [%global-get space-end]
            [%ge %i32 `%u]
            :^  %if  [~ ~[%i32]]
              ~[[%local-get space-end-cache] [%const %i32 1] [%add %i32]]
            ~[[%global-get space-end]]
          ::
            [%global-set space-end]
            [%const %i64 0]
            [%local-get space-end-cache]
            [%call set-i64-idx]
          ==
        ::
            %f32
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
            [%local-tee space-end-cache]
            [%global-get space-end]
            [%ge %i32 `%u]
            :^  %if  [~ ~[%i32]]
              ~[[%local-get space-end-cache] [%const %i32 1] [%add %i32]]
            ~[[%global-get space-end]]
          ::
            [%global-set space-end]
            [%const %f32 `@rs`0]
            [%local-get space-end-cache]
            [%call set-f32-idx]
          ==
        ::
            %f64
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
            [%local-tee space-end-cache]
            [%global-get space-end]
            [%ge %i32 `%u]
            :^  %if  [~ ~[%i32]]
              ~[[%local-get space-end-cache] [%const %i32 1] [%add %i32]]
            ~[[%global-get space-end]]
          ::
            [%global-set space-end]
            [%const %f64 `@rd`0]
            [%local-get space-end-cache]
            [%call set-f64-idx]
          ==
        ::
            %v128
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
            [%local-tee space-end-cache]
            [%global-get space-end]
            [%ge %i32 `%u]
            :^  %if  [~ ~[%i32]]
              ~[[%local-get space-end-cache] [%const %i32 1] [%add %i32]]
            ~[[%global-get space-end]]
          ::
            [%global-set space-end]
            [%vec %const %v128 0]
            [%local-get space-end-cache]
            [%call set-vec-idx]
          ==
        ::
            %octs
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
            [%local-tee space-end-cache]
            [%global-get space-end]
            [%ge %i32 `%u]
            :^  %if  [~ ~[%i32]]
              ~[[%local-get space-end-cache] [%const %i32 1] [%add %i32]]
            ~[[%global-get space-end]]
          ::
            [%global-set space-end]
          ::
            [%local-get space-end-cache]
            [%const %i32 space-number]
            [%ge %i32 `%u]
            :^  %if  [~ ~]
              ~[[%unreachable ~]]
            ~
          ::
            [%local-get space-end-cache]
            [%const %i32 space-width]
            [%mul %i32]
            [%const %i64 minus-one-64]
            [%store %i64 [0 offset] ~]
          ==
        ==
      ==
    ::
    ++  translate-lia
      |=  [[%run-lia name=term target=(list idx:line:lia)] d=data-section:wasm]
      ^-  [(list instruction:wasm) data-section:wasm]
      =/  data-idx=@  (lent d)
      ?>  (lth data-idx ^~((bex 32)))
      =/  data=octs
        :-  (lent target)
        (rep 3 target)
      :_  (snoc d [%pass data])
      :~
        [%const %i32 data-idx]
        [%global-set space-clue]
        [%call (~(got by king-diary) name)]
      ==
    ::
    ++  translate
      |=  [=op:line:lia d=data-section:wasm]
      ^-  [(list instruction:wasm) data-section:wasm]
      ?:  ?=(%run-lia -.op)  (translate-lia op d)
      ?:  ?=(?(%if %loop) -.op)
        ?-    -.op
            %if
          =^  result-true=(list (list instruction:wasm))  d
            (spin true.op d translate)
          =^  result-false=(list (list instruction:wasm))  d
            (spin false.op d translate)
          :_  d
          :_  ~
          :^    %if
              [(turn p.type.op convert) (turn q.type.op convert)]
            ^-  expression:wasm
            (zing result-true)
          ^-  expression:wasm
          (zing result-false)
        ::
            %loop
          =^  result=(list (list instruction:wasm))  d
            (spin body.op d translate)
          :_  d
          :_  ~
          :+  %loop
            [(turn p.type.op convert) (turn q.type.op convert)]
          ^-  expression:wasm
          (zing result)
        ::
        ==
      :_  d
      ?:  ?=(instr-num:wasm op)  ~[op]
      ?:  ?=(%br -.op)  ~[op]
      ?-    -.op
          %let
        ?>  (lth idx.op space-number)
        ?-    type.op
            %i32
          :~
            :+  %block  [~ ~]
            :~
              [%const %i32 idx.op]
              [%global-get space-end]
              [%ge %i32 `%u]
              :^  %if  [~ ~[%i32]]
                ~[[%const %i32 idx.op] [%const %i32 1] [%add %i32]]
              ~[[%global-get space-end]]
            ::
              [%global-set space-end]
              [%const %i32 0]
              [%const %i32 idx.op]
              [%call set-i32-idx]
            ==
          ==
        ::
            %i64
          :~
            :+  %block  [~ ~]
            :~
              [%const %i32 idx.op]
              [%global-get space-end]
              [%ge %i32 `%u]
              :^  %if  [~ ~[%i32]]
                ~[[%const %i32 idx.op] [%const %i32 1] [%add %i32]]
              ~[[%global-get space-end]]
            ::
              [%global-set space-end]
              [%const %i64 0]
              [%const %i32 idx.op]
              [%call set-i64-idx]
            ==
          ==
        ::
            %f32
          :~
            :+  %block  [~ ~]
            :~
              [%const %i32 idx.op]
              [%global-get space-end]
              [%ge %i32 `%u]
              :^  %if  [~ ~[%i32]]
                ~[[%const %i32 idx.op] [%const %i32 1] [%add %i32]]
              ~[[%global-get space-end]]
            ::
              [%global-set space-end]
              [%const %f32 `@rs`0]
              [%const %i32 idx.op]
              [%call set-f32-idx]
            ==
          ==
        ::
            %f64
          :~
            :+  %block  [~ ~]
            :~
              [%const %i32 idx.op]
              [%global-get space-end]
              [%ge %i32 `%u]
              :^  %if  [~ ~[%i32]]
                ~[[%const %i32 idx.op] [%const %i32 1] [%add %i32]]
              ~[[%global-get space-end]]
            ::
              [%global-set space-end]
              [%const %f64 `@rd`0]
              [%const %i32 idx.op]
              [%call set-f64-idx]
            ==
          ==
        ::
            %v128
          :~
            :+  %block  [~ ~]
            :~
              [%const %i32 idx.op]
              [%global-get space-end]
              [%ge %i32 `%u]
              :^  %if  [~ ~[%i32]]
                ~[[%const %i32 idx.op] [%const %i32 1] [%add %i32]]
              ~[[%global-get space-end]]
            ::
              [%global-set space-end]
              [%vec %const %v128 0]
              [%const %i32 idx.op]
              [%call set-vec-idx]
            ==
          ==
        ::
            %octs
          :~
            :+  %block  [~ ~]
            :~
              [%const %i32 idx.op]
              [%global-get space-end]
              [%ge %i32 `%u]
              :^  %if  [~ ~[%i32]]
                ~[[%const %i32 idx.op] [%const %i32 1] [%add %i32]]
              ~[[%global-get space-end]]
            ::
              [%global-set space-end]
            ::
              [%const %i32 idx.op]
              [%const %i32 space-number]
              [%ge %i32 `%u]
              :^  %if  [~ ~]
                ~[[%unreachable ~]]
              ~
            ::
              [%const %i32 idx.op]
              [%const %i32 space-width]
              [%mul %i32]
              [%const %i64 minus-one-64]
              [%store %i64 [0 offset] ~]
            ==
          ==
        ==
      ::
          %run  [%call (~(got by serf-diary) name.op)]~
          %nop  ~
      ::
          %get
        ?>  (lth idx.op space-number)
        ?:  ?=(%octs type.op)
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
          ==
        =;  call=instruction:wasm
          :~
            [%const %i32 idx.op]
            [%global-get space-start]
            [%add %i32]
            call
          ==
        :-  %call
        ?-  type.op
          %i32   get-i32-idx
          %i64   get-i64-idx
          %f32   get-f32-idx
          %f64   get-f64-idx
          %v128  get-vec-idx
        ==
      ::
          %set
        ?>  (lth idx.op space-number)
        :~
          [%const %i32 idx.op]
          [%global-get space-start]
          [%add %i32]
          :-  %call
          ?-  type.op  
            %i32   set-i32-idx
            %i64   set-i64-idx
            %f32   set-f32-idx
            %f64   set-f64-idx
            %v128  set-vec-idx
            %octs  ~|(%set-octs !!)
          ==
        ==
      ::
          %read
        ?>  (lth p.op space-number)
        :~
          [%const %i32 p.op]
          [%global-get space-start]
          [%add %i32]
          [%call set-octs-idx]
        ==
      ::
          %writ
        ?>  (lth p.op space-number)
        :~
          [%const %i32 p.op]
          [%global-get space-start]
          [%add %i32]
          [%call give-octs-idx]
        ==
      ::
          %len
        ?>  (lth idx.op space-number)
        :~
          [%const %i32 idx.op]
          [%global-get space-start]
          [%add %i32]
          [%call len-idx]
        ==
      ::
          %read-octs-i
        ?>  (lth p.op space-number)
        :~
          [%const %i32 p.op]
          [%global-get space-start]
          [%add %i32]
          :-  %call
          ?-  type.op
            %i32  read-octs-i32-idx
            %i64  read-octs-i64-idx
          ==
        ==
      ::
          %read-octs-f
        ?>  (lth p.op space-number)
        :~
          [%const %i32 p.op]
          [%global-get space-start]
          [%add %i32]
          :-  %call
          ?-  type.op
            %f32  read-octs-f32-idx
            %f64  read-octs-f64-idx
          ==
        ==
      ::
          %writ-octs-i32
        ?>  (lth p.op space-number)
        :~
          [%const %i32 p.op]
          [%global-get space-start]
          [%add %i32]
          [%call writ-octs-i32-idx]
        ==
      ==
    ::
    --  ::  ++main
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
  --  ::  |compiler
--