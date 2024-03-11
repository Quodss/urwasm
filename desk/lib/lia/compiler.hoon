/-  wasm
/-  lia
::
|%
++  addr-size  `@`8                                 ::  space address size in bits
++  space-width  `@`4                               ::  size of space element in bytes
++  space-number  ^~((bex addr-size))               ::  number of elements in space
++  space-size  ^~((mul space-width space-number))  ::  space size in bytes
++  page-size  `@`65.536                            ::  page size in bytes
++  heap-pages  `@`128                              ::  addressable heap limit in pages; need ~2x of this memory for gc
++  heap-lim  ^~((mul page-size heap-pages))        ::  addressable heap limit in bytes
++  offset  `@`0  :: v                              ::  space offset
::  #################|+++++++++++++++++++++++++++++++++++++|...
::  ^static data       ^space with numeric values/pointers   ^heap
::
++  main
  |=  [serf=module:wasm =action:line:lia n-args=@]
  ^-  module:wasm
  =|  king=module:wasm
  ::  add funcs exported by module as imports
  ::
  =/  exports-serf=(map cord @)
    =|  out=(map cord @)
    |-  ^-  (map cord @)
    ?~  export-section.module  out
    =*  exp  i.export-section.module
    =?  out  ?=(%func -.exp)
      (~(put by out) name.exp i.exp)
    $(export-section.module t.export-section.module)
  =.  king
    %-  ~(reb by exports-serf)
    |:  [[name=*cord idx=*@] out=out]
    ^-  module:wasm
    =/  type-idx  (snag idx function-section.serf)
    =/  type  (snag type-idx type-section.serf)
    =^  king-type-idx=@  king
      =/  maybe  (find ~[type] type-section.king)
      ?^  maybe
        [u.maybe king]
      :-  (lent type-section.king)
      king(type-section (snoc type-section.king type))
    %=    king
        import-section
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
    =/  type=func-type:wasm  [~[%i32 %i32 %i32] ~]
    =/  maybe  (find ~[type] type-section.king)
    ?^  maybe  [u.maybe type-section.king]
    :-  (lent type-section.king)
    (snoc type-section.king type)
  =.  import-section.king
    %+  weld  import-section.king
    :~
      ['memio' 'read' %func type-memio-idx]
      ['memio' 'write' %func type-memio-idx]
    ==
  =/  mem-read-idx=@   (sub (lent import-section.king) 2)
  =/  mem-write-idx=@  +(mem-read-idx)
  =/  alloc-idx=@      +(mem-write-idx)
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
  ::  allocator
  ::
  =^  type-alloc-idx=@  type-section.king
    =/  type=func-type:wasm  [~[%i32] ~[%i32]]
    =/  maybe  (find ~[type] type-section.king)
    ?^  maybe  [u.maybe type-section.king]
    :-  (lent type-section.king)
    (snoc type-section.king type)
  =.  function-section.king
    (snoc function-section.king type-alloc-idx)
  =.  code-section.king
    %+  snoc  code-section.king
    :-  ~[%i32 %i32]  ::  edge after allocation, memsize in bytes
    ^-  expression:wasm
    :~
      [%local-get 0]
      [%global-get 0]
      [%add %i32]
      [%local-tee 1]
      [%const %i32 page-size]
      [%memory-size %0]
      [%mul %i32]
      [%local-tee 2]
      [%ge %i32 `%u]
    ::
      [%local-get 1]
      [%const %i32 heap-lim]
      [%ge %i32 `%u]
    ::
      [%or %i32]
      :^    %if  ::  if heap_edge+len >= min(heap-lim, page-size*memsize): gc
          [~ ~]
        :~
          [%call gc-idx]
          [%local-get 0]
          [%global-get 0]
          [%add %i32]
          [%local-tee 1]
          [%const %i32 heap-lim]
          [%ge %i32 `%u]
          :^    %if  ::  if heap_edge+len >= heap-lim after gc: crash
              [~ ~]
            ~[[%unreachable ~]]
          ~
        ::
          [%local-get 1]
          [%local-get 2]
          [%ge %i32 `%u]
          :^    %if  ::  else if heap_edge+len >= page-size*memsize: grow memory
              [~ ~]
            :~
              [%local-get 1]
              [%local-get 2]
              [%sub %i32]
              [%const %i32 page-size]
              [%div %i32 `%u]
              [%const %i32 1]
              [%add %i32]
              [%memory-grow %0]
              [%const %i32 ^~((dec (bex 32)))]
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
      [%local-get 1]
      [%global-set 0]  ::  increase the edge
    ==
  ::  gc
  ::
  =^  type-gc-idx=@  type-section.king
    =/  type=func-type:wasm  ~
    =/  maybe  (find ~[type] type-section.king)
    ?^  maybe  [u.maybe type-section.king]
    :-  (lent type-section.king)
    (snoc type-section.king type)
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
    ^-  expression:wasm
    :~
      [%const %i32 space-size]
      [%global-get 0]
      [%add %i32]
      [%local-tee 1]
      [%const %i32 page-size]
      [%memory-size %0]
      [%mul %i32]
      [%local-tee 0]
      [%ge %i32 `%u]
      :^    %if  ::  if edge_copy >= memsize: attempt to grow
          [~ ~]
        :~
          [%local-get 1]
          [%local-get 0]
          [%sub %i32]
          [%const %i32 page-size]
          [%div %i32 `%u]
          [%const %i32 1]
          [%add %i32]
          [%memory-grow %0]
          [%const %i32 ^~((dec (bex 32)))]
          [%eq %i32]
          :^    %if  ::  if memory-grow yields -1: crash, else update memsize
              [~ ~]
            ~[[%unreachable ~]]
          :~
            [%const %i32 page-size]
            [%memory-size %0]
            [%mul %i32]
            [%local-set 0]
          ==
        ==
      ~
    ::
      [%const %i32 0]
      [%local-set 2]
      :+  %loop  [~ ~]
      :~
        [%local-get 2]
        [%const %i32 ^~((mul 8 space-number))]
        [%ge %i32 `%u]
        :^    %if  ::  if space ptr address >= 8*space-number: end loop
            [~ ~]
          ~
        :~  ::  else continue loop
          [%local-get 2]
          [%load %i32 [0 offset] ~]
          [%local-tee 3]
          [%const %i32 ^~((bex 31))]
          [%lt %i32 `%u]
          :^    %if  ::  if MSB set to 0 then it's numerical value, copy
              [~ ~]
            :~
              [%global-get 0]
              [%local-get 2]
              [%add %i32]
              [%local-get 3]
              [%store %i32 [0 0] ~]
            ==
          :~  ::  else it's a pointer
            [%local-get 3]
            [%const %i32 ^~((bex 31))]
            [%sub %i32]
            [%local-tee 3]  ::  remove leading 1
            [%eqz %i32]
            :^    %if  ::  if NULL: write NULL to destination
                [~ ~]
              :~
                [%global-get 0]
                [%local-get 2]
                [%add %i32]
                [%const %i32 ^~((bex 31))]
                [%store %i32 [0 0] ~]
              ==
            :~  ::  else: set pointer, copy data
              [%global-get 0]
              [%local-get 2]
              [%add %i32]
              [%local-get 1]
              [%global-get 0]
              [%sub %i32]  ::  (copy_edge - edge = edge after copy back)
              [%const %i32 ^~((bex 31))]
              [%add %i32]
              [%store %i32 [0 0] ~]
              [%local-get 3]
              [%load %i32 [0 0] ~]
              [%local-tee 4]
              [%local-get 1]
              [%add %i32]
              [%const %i32 4]
              [%add %i32]
              [%local-get 0]
              [%ge %i32 `%u]
              :^  %if  [~ ~]  ::  if copy_edge+datum_size+4 >= memsize: attempt to grow
                :~
                  [%local-get 4]
                  [%local-get 1]
                  [%add %i32]
                  [%local-get 0]
                  [%sub %i32]
                  [%const %i32 page-size]
                  [%div %i32 `%u]
                  [%const %i32 1]
                  [%add %i32]
                  [%memory-grow %0]
                  [%const %i32 ^~((dec (bex 32)))]
                  [%eq %i32]
                  :^    %if  ::  if memory-grow yields -1: crash, else update memsize
                      [~ ~]
                    ~[[%unreachable ~]]
                  :~
                    [%const %i32 page-size]
                    [%memory-size %0]
                    [%mul %i32]
                    [%local-set 0]
                  ==
                ==
              ~
            ::
              [%local-get 1]  ::  copy to edge_copy
              [%local-get 3]  ::  from ptr
              [%local-get 4]
              [%const %i32 4]
              [%add %i32]     ::  n+4 bytes
              [%memory-copy %0 %0]
              [%local-get 4]
              [%const %i32 4]
              [%add %i32]
              [%local-get 1]
              [%add %i32]
              [%local-set 1]  ::  update edge_copy
            ==
          ==
        ::
          [%local-get 2]
          [%const %i32 8]
          [%add %i32]
          [%local-set 2]
          [%br 1]  ::  jump to loop
        ==
      ==
    ::
    ==
  ::  space read/write
  ::
  ::  compile and add main
  ::
  
--