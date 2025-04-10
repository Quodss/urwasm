/+  wasm=wasm-lia
/*  bin  %wasm  /tests/rustpython/wasm
::
|=  code=@t
^-  [std-out=(list @t) std-err=(list @t)]
::
=<
  =/  arr  (arrows:wasm acc-mold)
  %-  yield-need:wasm  =<  -
  ~>  %bout
  %^  (run-once:wasm yil-mold acc-mold)  [bin imports]  %$
  =/  m  (script:lia-sur:wasm yil-mold acc-mold)
  ^-  form:m
  =,  arr
  ;<  *  try:m  (call '_start' ~)
  ;<  acc=acc-mold  try:m  get-acc
  (return:m (flop std-out.acc) (flop std-err.acc))
::
=>
  |%
  +$  prestat-dir  @  ::  length of a preopened directory
  +$  filestat  $:  dev=@
                    ino=@
                    typ=@
                    lin=@
                    siz=@
                    ati=@
                    mti=@
                    cti=@
                ==
  ::
  +$  acc-mold  $:  seed-rnd=@
                    $=  fds
                    %+  map  @
                    $:  pre=(unit prestat-dir)
                        stat=filestat
                        off=@  ::  offset
                    ==
                ::
                    std-out=(list @t)
                    std-err=(list @t)
                ==
  ::
  +$  yil-mold  [std-out=(list @t) std-err=(list @t)]
  ++  arr  (arrows:wasm acc-mold)
  --
::
|%
++  imports
  ^~  ^-  (import:lia-sur:wasm acc-mold)
  =/  preopen-dir-name=@t  '/home/user'
  :-  :*  0v186.737jc.mc20j.vhl92.2l9kl.
          5bfah.nli68.jt0mu.0lds1.4d0md.
          8tf7r.qfphc.me2t1.ttd0c.40jbe.
          9oirc.ps427.1ovdt.5s05o.q3u4p.
          i39kv
        ::
          [[3 `(met 3 preopen-dir-name) [0 3 3 1 0 0 0 0] 0] ~ ~]
          ~
          ~
      ==
  =*  cw  coin-wasm:wasm-sur:wasm
  =/  m  (script:lia-sur:wasm (list cw) acc-mold)
  =/  cmd-args=(list @t)
    :~
      'rustpython.wasm'
      '/home/user/eval.py'
    ==
  =.  code        (rap 3 code '\0a' ~)
  =/  code-len=@  (met 3 code)
  %-  malt
  ^-  (list [[cord cord] $-((list cw) form:m)])
  :~
    :-  'wasi_snapshot_preview1'^'random_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 ptr-y=@] [%i32 size-w=@] ~] args)
    =,  arr  =,  args
    ;<  acc=acc-mold  try:m  get-acc
    =/  bits=@  (~(raw og seed-rnd.acc) (max 512 (mul size-w 8)))
    ;<  ~  try:m  (set-acc acc(seed-rnd (cut 0 [0 512] bits)))
    ;<  ~  try:m  (memwrite ptr-y size-w bits)
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
    ::
    ::  write 4 bytes of (lent cmd-args)=2 to num-u pointer
    ;<  ~  try:m  (memwrite num-u 4 (lent cmd-args))
    ::
    :: add together lengths in bytes of command line arguments +1 for \0 terminator
    =/  len=@  (roll cmd-args |=([i=cord acc=@] (add acc +((met 3 i)))))
    ;<  ~  try:m  (memwrite len-u 4 len)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'args_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 arg-u=@] [%i32 buf-u=@] ~] args)
    =,  arr  =,  args
    ?~  cmd-args  (return:m i32+0 ~)
    =/  len=@  +((met 3 i.cmd-args))
    ;<  ~  try:m  (memwrite buf-u len i.cmd-args)
    ;<  ~  try:m  (memwrite arg-u 4 buf-u)
    %=  $
      cmd-args  t.cmd-args
      buf-u.args  (add buf-u len)
      arg-u.args  (add arg-u 4)
    ==
  ::
    :-  'wasi_snapshot_preview1'^'fd_prestat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 prestat-u=@] ~] args)
    =,  arr  =,  args
    ;<  acc=acc-mold  try:m  get-acc
    ?~  fd-data=(~(get by fds.acc) fd)
      (return:m i32+8 ~)  ::  $badf
    ?~  len=pre.u.fd-data  (return:m i32+8 ~)
    ;<  ~  try:m  (memwrite prestat-u 4 0)                        ::  tag $prestat_dir
    ;<  ~  try:m  (memwrite (add prestat-u 4) 4 u.len)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_prestat_dir_name'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 path-u=@] [%i32 len-w=@] ~] args)
    =,  arr  =,  args
    ;<  acc=acc-mold  try:m  get-acc
    ?>  =(3 fd)
    =/  len=@  (met 3 preopen-dir-name)
    ?>  =(len len-w)
    ;<  ~  try:m  (memwrite path-u len preopen-dir-name)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_filestat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 stat-u=@] ~] args)
    =,  arr  =,  args
    ;<  acc=acc-mold  try:m  get-acc
    =/  stat=(unit filestat)
      ?:  |(=(0 fd) =(1 fd) =(2 fd))
        `[0 fd 2 1 0 0 0 0]
      ?~  fd-data=(~(get by fds.acc) fd)  ~
      `stat.u.fd-data
    ?~  stat  (return:m i32+8 ~)
    =,  u.stat
    ;<  ~  try:m  (memwrite stat-u 8 dev)
    ;<  ~  try:m  (memwrite (add stat-u 8) 8 ino)
    ;<  ~  try:m  (memwrite (add stat-u 16) 1 typ)
    ;<  ~  try:m  (memwrite (add stat-u 24) 8 lin)
    ;<  ~  try:m  (memwrite (add stat-u 32) 8 siz)
    ;<  ~  try:m  (memwrite (add stat-u 40) 8 ati)
    ;<  ~  try:m  (memwrite (add stat-u 48) 8 mti)
    ;<  ~  try:m  (memwrite (add stat-u 56) 8 cti)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_fdstat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] [%i32 fdstat-u=@] ~] args)
    =,  arr  =,  args
    ;<  acc=acc-mold  try:m  get-acc
    =/  type=(unit @)
      ?:  |(=(0 fd) =(1 fd) =(2 fd))
        `2
      ?~  fd-data=(~(get by fds.acc) fd)  ~
      `typ.stat.u.fd-data
    ?~  type  (return:m i32+8 ~)
    ;<  ~  try:m  (memwrite fdstat-u 1 u.type)
    ;<  ~  try:m  (memwrite (add fdstat-u 2) 2 0)
    ;<  ~  try:m  (memwrite (add fdstat-u 8) 8 (dec (bex 30)))
    ;<  ~  try:m  (memwrite (add fdstat-u 16) 8 (dec (bex 30)))
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_seek'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 fd=@]
                [%i64 offset-ds=@]
                [%i32 whence-w=@]
                [%i32 ooff-u=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ?:  |(=(0 fd) =(1 fd) =(2 fd))
      (return:m i32+70 ~)  :: $spipe
    ;<  acc=acc-mold  try:m  get-acc
    ?~  fd-data=(~(get by fds.acc) fd)  (return:m i32+8 ~)
    =/  off-new=@
      %+  ~(sum fo (bex 64))  offset-ds
      ?+  whence-w.args  ~|(%weird-whence !!)
        %0  0
        %1  off.u.fd-data
        %2  siz.stat.u.fd-data
      ==
    ?:  (gte off-new (bex 63))
      (return:m i32+28 ~)  ::  $inval
    =.  off.u.fd-data  off-new
    ;<  ~  try:m  (set-acc acc(fds (~(put by fds.acc) fd u.fd-data)))
    ;<  ~  try:m  (memwrite ooff-u 8 off-new)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'path_filestat_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 fd=@]
                [%i32 sym-o=@]
                [%i32 str-u=@]
                [%i32 len-w=@]
                [%i32 tat-u=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ?>  =(3 fd)
    ;<  str=octs  try:m  (memread str-u len-w)
    =/  result=(unit [serial-num=@ size=@])
      ?+  q.str  ~&(missing-file+`@t`q.str ~)
        %'rustpython.wasm'  `[1 p.bin]
        %'eval.py'          `[2 code-len]
        %'.'                !!
      ==
    ?~  result  (return:m i32+44 ~)  ::  $noent
    =,  u.result
    ;<  ~  try:m  (memwrite tat-u 8 1)                   ::  device 1
    ;<  ~  try:m  (memwrite (add tat-u 8) 8 serial-num)  ::  inode
    ;<  ~  try:m  (memwrite (add tat-u 16) 1 4)          ::  regular file
    ;<  ~  try:m  (memwrite (add tat-u 24) 8 1)          ::  link count
    ;<  ~  try:m  (memwrite (add tat-u 32) 8 size)
    ;<  ~  try:m  (memwrite (add tat-u 40) 8 0)          ::  null timestamps
    ;<  ~  try:m  (memwrite (add tat-u 48) 8 0)
    ;<  ~  try:m  (memwrite (add tat-u 56) 8 0)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'path_open'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 fd=@]
                [%i32 lookup-flags=@]
                [%i32 str-u=@]
                [%i32 len-w=@]
                [%i32 o-flags=@]
                [%i64 rights=@]
                [%i64 rights-inherit=@]
                [%i32 fd-flags=@]
                [%i32 fd-u=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ;<  o=octs  try:m  (memread str-u len-w)
    ?>  =(3 fd)
    ?:  =('rustpython.wasm' q.o)  !!
    ?.  =('eval.py' q.o)
      (return:m i32+44 ~)
    ;<  acc=acc-mold  try:m  get-acc
    =/  new-fd=@  +((~(rep in ~(key by fds.acc)) max))
    =/  stat=filestat
      :*  0         ::  device
          new-fd    ::  inode
          4         ::  regular file
          1         ::  links
          code-len  ::  length
          0  0  0   ::  timestamps
      ==
    =.  fds.acc  (~(put by fds.acc) new-fd ~ stat 0)
    ;<  ~  try:m  (set-acc acc)
    ;<  ~  try:m  (memwrite fd-u 4 new-fd)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_close'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 fd=@] ~] args)
    =,  arr  =,  args
    ?>  =(4 fd)
    ;<  acc=acc-mold  try:m  get-acc
    =.  fds.acc  (~(del by fds.acc) fd)
    ;<  ~  try:m  (set-acc acc)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_read'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 fd=@]
                [%i32 iovec-u=@]
                [%i32 iovec-len-w=@]
                [%i32 size-u=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ?>  =(4 fd)
    ?>  =(1 iovec-len-w)
    ;<  acc=acc-mold  try:m  get-acc
    ?~  fd-data=(~(get by fds.acc) fd)  (return:m i32+8 ~)  ::  $badf
    ?:  (gte off.u.fd-data siz.stat.u.fd-data)
      ;<  ~  try:m  (memwrite size-u 4 0)
      (return:m i32+0 ~)
    ;<  o=octs  try:m  (memread iovec-u 8)
    =/  ptr-u=@  (cut 3 [0 4] q.o)
    =/  len-w=@  (cut 3 [4 4] q.o)
    =/  len=@  (min (sub [siz.stat off]:u.fd-data) len-w)
    ;<  ~  try:m  (memwrite ptr-u len (rsh [3 off.u.fd-data] code))
    ;<  ~  try:m  (memwrite size-u 4 len)
    =.  off.u.fd-data  (add off.u.fd-data len)
    =.  fds.acc  (~(put by fds.acc) fd u.fd-data)
    ;<  ~  try:m  (set-acc acc)
    (return:m i32+0 ~)
  ::
    :-  'wasi_snapshot_preview1'^'fd_write'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 fd=@]
                [%i32 ciovec-u=@]
                [%i32 ciovec-len-w=@]
                [%i32 size-u=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ?>  |(=(2 fd) =(1 fd))
    =/  size-out=@  0
    ;<  acc=acc-mold  try:m  get-acc
    =/  [std-out=(list @t) std-err=(list @t)]  [std-out std-err]:acc
    |-  ^-  form:m
    ?:  =(0 ciovec-len-w)
      ;<  ~  try:m  (memwrite size-u 4 size-out)
      ;<  ~  try:m  (set-acc acc(std-out std-out, std-err std-err))
      (return:m i32+0 ~)
    ;<  o=octs  try:m  (memread ciovec-u 8)
    =/  buf-u=@  (cut 3 [0 4] q.o)
    =/  len-w=@  (cut 3 [4 4] q.o)
    ;<  o=octs  try:m  (memread buf-u len-w)
    =?  std-out  =(1 fd)  [q.o std-out]
    =?  std-err  =(2 fd)  [q.o std-err]
    %=  $
      size-out           (add size-out len-w)
      ciovec-u.args      (add ciovec-u 8)
      ciovec-len-w.args  (dec ciovec-len-w)
    ==
  ::
    :-  'wasi_snapshot_preview1'^'proc_exit'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 exit=@] ~] args)
    =,  arr  =,  args
    ?:  =(0 exit)
      ~&  >  'process exited normally'
      (return:m ~)
    ;<  acc=acc-mold  try:m  get-acc
    ~&  [std-out=std-out std-err=std-err]:acc
    ~&  >>  `@t`(rap 3 'process exited with an error ' (scot %ud exit) ~)
    !!
  ==
--
