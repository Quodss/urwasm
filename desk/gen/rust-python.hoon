/+  wasm=wasm-lia
/*  bin  %wasm  /tests/rustpython/wasm
::
:-  %say  |=  *  :-  %noun
::
=<
  =/  arr  (arrows:wasm acc-mold)
  %-  yield-need:wasm  =<  -
  %^  (run-once:wasm yil-mold acc-mold)  [bin imports]  %$
  =/  m  (script:lia-sur:wasm yil-mold acc-mold)
  ^-  form:m
  =,  arr
  ;<  *  try:m  (call '_start' ~)
  (return:m ~)
::
=>
  |%
  +$  acc-mold  *
  +$  yil-mold  ~
  ++  arr  (arrows:wasm acc-mold)
  --
::
|%
++  imports
  ^~  ^-  (import:lia-sur:wasm acc-mold)
  :-  ~
  =*  cw  coin-wasm:wasm-sur:wasm
  =/  m  (script:lia-sur:wasm (list cw) acc-mold)
  %-  malt
  ^-  (list [[cord cord] $-((list cw) form:m)])
  :~
    :-  'wasi_snapshot_preview1'^'random_get'  ::  bad lazy random
    =/  eny  0v10c.6m74m.rvdl2.q2u4u.l9ap9.
             ai9c7.73adf.5tisi.k7tp6.8rr79.
             h4roj.nk32m.ia920.fl9dm.5asnm.
             5g28j.vn3rv.ci30q.6c1c4.c2deq.
             3qubi
    ::
    =/  len-eny  64
    |=  args=(pole cw)
    ^-  form:m
    ~&  %give-random
    ?>  ?=([[%i32 ptr-y=@] [%i32 size-w=@] ~] args)
    =,  arr  =,  args
    ?:  (gth size-w len-eny)
      (return:m i32+6 ~)  ::    $again error
    ;<  ~  try:m  (memwrite ptr-y size-w eny)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'environ_sizes_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 num-u=@] [%i32 len-u=@] ~] args)
    =,  arr  =,  args
    ;<  ~  try:m  (memwrite num-u 4 0)  ::  no environment vars
    ;<  ~  try:m  (memwrite len-u 4 0)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'args_sizes_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 num-u=@] [%i32 len-u=@] ~] args)
    =,  arr  =,  args
    ;<  ~  try:m  (memwrite num-u 4 2)  ::  number of arguments
    ;<  ~  try:m
      %^  memwrite  len-u  4
      ^~  %-  roll
      :_  |=([i=cord acc=@] (add acc +((met 3 i))))  ::  \0 terminator
      ^-  (list cord)
      :~
        'rustpython'  ::  name of the program
        'eval.py'     ::  path to a fake .py file
      ==
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'args_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 arg-u=@] [%i32 buf-u=@] ~] args)
    =,  arr  =,  args
    =/  name  'rustpython'
    =/  len-name  +((met 3 name))
    =/  path  'eval.py'
    =/  len-path  +((met 3 path))
    ;<  ~  try:m  (memwrite buf-u len-name name)
    ;<  ~  try:m  (memwrite (add buf-u len-name) len-path path)
    ;<  ~  try:m  (memwrite arg-u 4 buf-u)
    ;<  ~  try:m  (memwrite (add 4 arg-u) 4 (add buf-u len-name))
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_prestat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 prestat-u=@] ~] args)
    =,  arr  =,  args
    ?.  =(3 fd)
      (return:m i32+8 ~)  ::  $badf error, we only "have" one preopened dir
    ;<  ~  try:m  (memwrite prestat-u 4 0)                             ::  tag $prestat_dir
    ;<  ~  try:m  (memwrite (add prestat-u 4) 4 1)                     ::  length of a fake directory to open ('.')
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_prestat_dir_name'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 path-u=@] [%i32 len-w=@] ~] args)
    =,  arr  =,  args
    ?>  =(3 fd)
    ?>  =(1 len-w)
    ;<  ~  try:m  (memwrite path-u 1 '.')
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'path_filestat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 fd=@]
                [%i32 map-w=@]
                [%i32 str-u=@]
                [%i32 len-w=@]
                [%i32 stat-u=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ;<  str=octs  try:m  (memread str-u len-w)
    =/  result=(unit [serial-num=@ size=@])
      ?+  q.str  ~&(missing-file+`@t`q.str ~)
        %'rustpython'  `[1 777]    :: fake size
      ==
    ::
    ?~  result  (return:m i32+44 ~)  ::  $noent
    =,  u.result
    ;<  ~  try:m  (memwrite stat-u 8 1)                   ::  device 1
    ;<  ~  try:m  (memwrite (add stat-u 8) 8 serial-num)  ::  inode
    ;<  ~  try:m  (memwrite (add stat-u 16) 1 4)          ::  regular file
    ;<  ~  try:m  (memwrite (add stat-u 24) 8 1)          ::  link count
    ;<  ~  try:m  (memwrite (add stat-u 32) 8 size)
    ;<  ~  try:m  (memwrite (add stat-u 40) 8 0)          ::  null timestamps
    ;<  ~  try:m  (memwrite (add stat-u 48) 8 0)
    ;<  ~  try:m  (memwrite (add stat-u 56) 8 0)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_filestat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 stat-u=@] ~] args)
    =,  arr  =,  args
    =/  [device=@ serial-num=@ file-type=@ size=@]
      ?+  fd.args  ~|(fd !!)
        %0  [0 1 2 0]
        %1  [0 2 2 0]
        %2  [0 3 2 0]
      ==
    ::
    ;<  ~  try:m  (memwrite stat-u 8 device)
    ;<  ~  try:m  (memwrite (add stat-u 8) 8 serial-num)
    ;<  ~  try:m  (memwrite (add stat-u 16) 1 file-type)
    ;<  ~  try:m  (memwrite (add stat-u 24) 8 1)          ::  link count
    ;<  ~  try:m  (memwrite (add stat-u 32) 8 size)
    ;<  ~  try:m  (memwrite (add stat-u 40) 8 0)          ::  null timestamps
    ;<  ~  try:m  (memwrite (add stat-u 48) 8 0)
    ;<  ~  try:m  (memwrite (add stat-u 56) 8 0)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_fdstat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 fdstat-u=@] ~] args)
    =,  arr  =,  args
    =/  [device=@ flags=@ rights=@ rights-inherit=@]
      ?+  fd.args  ~|(fd !!)
        %0        [2 0 0b10 0]
        ?(%1 %2)  [2 0 0b110.0100 0]
      ==
    ::
    ;<  ~  try:m  (memwrite fdstat-u 1 device)
    ;<  ~  try:m  (memwrite (add fdstat-u 2) 2 flags)
    ;<  ~  try:m  (memwrite (add fdstat-u 8) 8 rights)
    ;<  ~  try:m  (memwrite (add fdstat-u 16) 8 rights-inherit)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_seek'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 fd=@]
                [%i64 offset-ds=@]
                [%i32 whence-w=@]
                [%i32 size-u=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ?>  |(=(0 fd) =(1 fd) =(2 fd))
    ?>  =(0 offset-ds)
    ;<  ~  try:m  (memwrite size-u 8 0)
    (return:m i32+0 ~)
  ==
--
