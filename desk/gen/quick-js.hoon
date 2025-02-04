/+  wasm=wasm-lia
/*  bin  %wasm  /tests/quick-js-emcc/wasm
::
=/  cw  coin-wasm:wasm-sur:wasm
=/  yil-mold  (each cord cord)
=/  acc-mold  *
|=  code=cord
^-  (each cord cord)  ::  result or error
:: ~>  %bout
|^
%-  yield-need:wasm  =<  -
%^  (run-once:wasm yil-mold acc-mold)  [bin imports]  %$
=/  arr  (arrows:wasm acc-mold)
=/  m  (script:lia-sur:wasm yil-mold acc-mold)
^-  form:m
=,  arr
;<  run-u=@  try:m  (call-1 'QTS_NewRuntime' ~)
;<  ctx-u=@  try:m  (call-1 'QTS_NewContext' run-u 0 ~)
::
=/  code-len  (met 3 code)
=/  filename=cord  'eval.js'
=/  filename-len  (met 3 filename)
::
;<  code-u=@      try:m  (call-1 'malloc' +(code-len) ~)  ::  includes \00 terminator
;<  ~             try:m  (memwrite code-u +(code-len) code)
;<  filename-u=@  try:m  (call-1 'malloc' +(filename-len) ~)
;<  ~             try:m  (memwrite filename-u +(filename-len) filename)
;<  val-u=@       try:m
  (call-1 'QTS_Eval' ctx-u code-u code-len filename-u 0 0 ~)
;<  err-u=@       try:m  (call-1 'QTS_ResolveException' ctx-u val-u ~)
?:  =(0 err-u)
  ;<  str-u=@     try:m  (call-1 'QTS_GetString' ctx-u val-u ~)
  ;<  str=cord    try:m  (get-c-string str-u)
  (return:m &+str)
;<  str-u=@       try:m  (call-1 'QTS_GetString' ctx-u err-u ~)
;<  str=cord      try:m  (get-c-string str-u)
(return:m |+str)
::
++  get-c-string
  |=  ptr=@
  =/  m  (script:lia-sur:wasm cord acc-mold)
  ^-  form:m
  =/  arr  (arrows:wasm acc-mold)
  =,  arr
  =/  len=@  0
  =/  cursor=@  ptr
  |-  ^-  form:m
  ;<  char=octs  try:m  (memread cursor 1)
  ?.  =(0 q.char)
    $(len +(len), cursor +(cursor))
  ;<  =octs  try:m  (memread ptr len)
  (return:m q.octs)
::
++  imports
  ^-  (import:lia-sur:wasm acc-mold)
  :-  ~
  =/  m  (script:lia-sur:wasm (list cw) acc-mold)
  =/  arr  (arrows:wasm acc-mold)
  %-  malt
  :~
    :-  ['wasi_snapshot_preview1' 'clock_time_get']
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 clk-id=@] [%i64 ignored-precision=@] [%i32 time-u=@] ~] args)
    =,  arr  =,  args
    ;<  ~  try:m  (memwrite time-u 8 0)
    (return:m i32+0 ~)
  ::
  ==
--