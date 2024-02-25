/+  *lia-interpreter
/*  bin  %wasm  /tests/flopper/wasm
:-  %say  |=  *  :-  %noun
::
|^
=-
  ?>  ?=(%0 -.res)
  =/  item  -.out.res
  ?>  ?=(%octs -.item)
  (of-octs +.item)
^=  res
%+  lia  bin
=-  ~[(action "Hello world") (action "world hello")]
^=  action
|=  =tape
:~
  [%lit %octs (to-octs tape)]
  [%set 0]                                  ::  string
  [%lit %i32 (en-si:runner 32 -16)]
  [%run '__wbindgen_add_to_stack_pointer']
  [%set 1]                                  ::  retptr
  [%get 0]
  [%len ~]
  [%lit %i32 1]
  [%run '__wbindgen_malloc']
  [%set 2]                                  ::  ptr0
  [%get 0]
  [%get 2]
  [%writ ~]
  [%get 1]
  [%get 2]
  [%get 0]
  [%len ~]
  [%run 'process']
  [%get 1]
  [%lit %i32 4]
  [%read ~]
  [%lit %i32 0]
  [%add %i32]                               ::  convert r0 to i32
  [%set 3]                                  ::  r0
  [%get 1]
  [%lit %i32 4]
  [%add %i32]
  [%lit %i32 4]
  [%read ~]
  [%lit %i32 0]
  [%add %i32]                               ::  convert r1 to i32
  [%set 4]                                  ::  r1
  [%get 3]
  [%get 4]
  [%read ~]
  [%lit %i32 16]
  [%run '__wbindgen_add_to_stack_pointer']
  [%get 3]
  [%get 4]
  [%lit %i32 1]
  [%run '__wbindgen_free']
==
::
++  to-octs
  |=  =tape
  ^-  octs
  :-  (lent tape)
  (can 3 (turn tape (lead 1)))
::
++  of-octs
  |=  =octs
  ^-  tape
  (rope:simd:runner 3 octs)
--