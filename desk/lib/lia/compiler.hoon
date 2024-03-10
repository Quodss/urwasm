/-  wasm
/-  lia
::
|%
++  addr-size  `@`8                                    ::  space address size in bits
++  space-width  `@`16                                 ::  size of space element in bytes
++  space-size  ^~((mul space-width (bex addr-size)))  ::  space size in bytes
++  limit  `@`65.536                                   ::  page size in bytes
++  offset  `@`0  :: v                                 ::  space offset
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
  =.  memory-section.king  ~[[%ceil 1 1]]  ::  single page
  =.  global-section.king
    ?>  (lth (add offset space-size) limit)
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
    :-  ~
    ^-  expression:wasm
    :~
      [%local-get 0]
      [%global-get 0]
      [%add %i32]
      [%const %i32 limit]
      [%ge %i32 `%u]
      :^    %if  ::  if heap_edge+len >= limit: gc
          [~ ~]
        :~
          [%call gc-idx]
          [%local-get 0]
          [%global-get 0]
          [%add %i32]
          [%const %i32 limit]
          [%ge %i32 `%u]
          :^    %if  ::  if heap_edge+len >= limit after gc: crash
              [~ ~]
            ~[[%unreachable ~]]
          ~
        ==
      ~
    ::
      [%global-get 0]  ::  old edge is the pointer
      [%global-get 0]
      [%local-get 0]
      [%add %i32]
      [%global-set 0]  ::  increase the edge
    ==
  ::  gc
  ::
  
  ::  space read/write
  ::
  ::  compile and add main
  ::
  
--