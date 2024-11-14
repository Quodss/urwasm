/+  *test
/+  *lia
/*  import-vec  %wasm  /tests/import-vec/wasm
/*  fac-loop    %wasm  /tests/fac-br/wasm
/*  flopper     %wasm  /tests/flopper/wasm
|%
++  test-import-vec
  %+  expect-eq
    !>  `(list lia-value:lia-sur)`~[octs+[5 'olleh']]
    !>
    |^
    %-  yield-need:script-lib
    %^  run-once:script-lib  [import-vec import]  %$
    =/  m  runnable:script-lib
    =,  script-lib
    ;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
    ;<  *         try:m  (call 'process' retptr ~)
    ;<  r0=octs   try:m  (memread retptr 4)
    ;<  r1=octs   try:m  (memread (add retptr 4) 4)
    ;<  r2=octs   try:m  (memread q.r0 q.r1)
    (return:m octs+r2 ~)
    ::
    ++  i32neg  ^~((cury sub (bex 32)))
    ++  import
      ^-  import:lia-sur
      =/  m  (script:lia-sur (list coin-wasm:wasm-sur))
      %-  malt
      ^-  %-  list
          %+  pair  (pair cord cord)
          $-((list coin-wasm:wasm-sur) form:m)
      :~
        :-  ['./len_bg.js' '__wbg_getvec_ab3ebae2a99ce16c']
        |=  args=(pole coin-wasm:wasm-sur)
        ?>  ?=([[%i32 arg0=@] ~] args)
        =/  arg0=@  arg0.args
        =,  script-lib
        ;<  ptr1=@  try:m  (call-1 '__wbindgen_malloc' 5 1 ~)
        ;<  *       try:m  (memwrite ptr1 5 'hello')
        ;<  *       try:m  (memwrite arg0 4 ptr1)
        ;<  *       try:m  (memwrite (add arg0 4) 4 5)
        (return:m ~)
      ::
      ==
    --
++  test-simple
  %+  expect-eq
    !>  `(list lia-value:lia-sur)`~[i32+362.880]
    !>
    %-  yield-need:script-lib
    %^  run-once:script-lib  [fac-loop ~]  %$
    =/  m  runnable:script-lib
    =,  script-lib
    ;<  out=@  try:m  (call-1 'factorial' 9 ~)
    (return:m i32+out ~)
++  test-flop
  %+  expect-eq
    !>  `(list lia-value:lia-sur)`~[octs+[5 'olleh']]
    !>
    |^
    %-  yield-need:script-lib
    %^  run-once:script-lib  [flopper ~]  %$
    =/  m  runnable:script-lib
    =,  script-lib
    ;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
    ;<  ptr0=@    try:m  (call-1 '__wbindgen_malloc' 5 1 ~)
    ;<  *         try:m  (memwrite ptr0 5 'hello')
    ;<  *         try:m  (call 'process' retptr ptr0 5 ~)
    ;<  r0=octs   try:m  (memread retptr 4)
    ;<  r1=octs   try:m  (memread (add 4 retptr) 4)
    ;<  r2=octs   try:m  (memread q.r0 q.r1)
    (return:m octs+r2 ~)
    ::
    ++  i32neg  ^~((cury sub (bex 32)))
    --
++  test-1
  %+  expect-eq
    !>  `(list lia-value:lia-sur)`~[i32+42]
    !>
    %-  yield-need:script-lib
    %^  run-once:script-lib  [flopper ~]  %$
    =/  m  runnable:script-lib
    =,  script-lib
    ;<  ptr0=@  try:m  (call-1 '__wbindgen_malloc' 5 1 ~)
    (return:m i32+42 ~)

--