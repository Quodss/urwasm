/+  wasm=wasm-lia
/*  bin  %wasm  /tests/quick-js-emcc/wasm
::
::  foo-u: pointer to foo
::  foo-w: 32 bit integer
::  foo-d: 64 bit integer or double float
::  (same nomenclature as in u3: -u for pointers, -w/d for 32/64 bit values)
::
=/  cw  coin-wasm:wasm-sur:wasm
=/  script-form  script-raw-form:lia-sur:wasm
=/  yil-mold  (each cord cord)  ::  result or error
=/  acc-mold         ::  global parameters
  |-
  $:  run-u=@                                  ::  runtime 
      ctx-u=@                                  ::  context
      fil-u=@                                  ::  file name
      $=  js-imports                           ::  JS imports
      (map @ $-([@ @ @ @] (script-form @ $)))
  ==
=/  arr  (arrows:wasm acc-mold)
|^
|=  plugin=cord
^-  yil-mold
%-  yield-need:wasm  =<  -
~>  %bout
%^  (run-once:wasm yil-mold acc-mold)  [bin imports]  %$
=/  m  (script:lia-sur:wasm yil-mold acc-mold)
^-  form:m
=,  arr
::
::  1. Initialize JS runtime, set global parameters
::
;<  run-u=@    try:m  (call-1 'QTS_NewRuntime' ~)
;<  ctx-u=@    try:m  (call-1 'QTS_NewContext' run-u 0 ~)
=/  filename=cord  'eval.js'
=/  filename-len  (met 3 filename)
;<  fil-u=@    try:m  (malloc-write +(filename-len) filename)
;<  ~          try:m  (set-acc run-u ctx-u fil-u ~)
::
::  2. Prepare context for plugin evaluation
::
;<  err=(unit cord)  try:m  (make-function 'require' require)
?^  err  (return:m |+u.err)
::
;<  *  try:m  (js-eval 'var module = {};')  ::  TODO create `module` in a less deranged way; figure out require/module api
::
::  3. Eval plugin to get the extended class definition
::
;<  res-u=@          try:m  (js-eval plugin)
;<  err=(unit cord)  try:m  (mayb-error res-u)
?^  err  (return:m |+u.err)
::
::  4. Eval our code using the plugin
::
;<  res-u=@  try:m
  %-  js-eval
  '''
  let _p = new module.exports();
  let _s = _p.render_our_now_eny("~sampel-palnet", "2025", "42");
  _s
  '''
::
::  5. Return the result
::
;<  err=(unit cord)  try:m  (mayb-error res-u)
?^  err  (return:m |+u.err)
;<  out=cord         try:m  (get-js-string res-u)
(return:m &+out)
::
++  js-eval
  |=  code=cord
  =/  m  (script:lia-sur:wasm @ acc-mold)
  ^-  form:m
  =,  arr
  ;<  acc=acc-mold  try:m  get-acc
  =,  acc
  ::
  =/  code-len  (met 3 code)
  ;<  code-u=@  try:m  (malloc-write +(code-len) code)
  ;<  res-u=@   try:m  (call-1 'QTS_Eval' ctx-u code-u code-len fil-u 0 0 ~)
  ;<  *         try:m  (call 'free' code-u ~)
  (return:m res-u)
::
++  imports
  ^~  ^-  (import:lia-sur:wasm acc-mold)
  :-  *acc-mold
  =/  m  (script:lia-sur:wasm (list cw) acc-mold)
  %-  malt
  :~
    :-  'wasi_snapshot_preview1'^'clock_time_get'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=([[%i32 clk-id=@] [%i64 ignored-precision=@] [%i32 time-u=@] ~] args)
    =,  arr  =,  args
    ;<  ~  try:m  (memwrite time-u 8 0)
    (return:m i32+0 ~)
  ::
    :-  'env'^'qts_host_call_function'
    |=  args=(pole cw)
    ^-  form:m
    ?>  ?=  $:  [%i32 ctx-u=@]
                [%i32 this-u=@]
                [%i32 argc-w=@]
                [%i32 argv-u=@]
                [%i32 magic-w=@]
                ~
            ==
        args
    ::
    =,  arr  =,  args
    ;<  acc=acc-mold  try:m  get-acc
    ;<  val-u=@       try:m
      ((~(got by js-imports.acc) magic-w) ctx-u this-u argc-w argv-u)
    (return:m i32+val-u ~)
  ::
  ==
::
++  js-val-cord-compare
  |=  [val-u=@ =cord]
  =/  m  (script:lia-sur:wasm ? acc-mold)
  ^-  form:m
  =,  arr
  ;<  acc=acc-mold  try:m  get-acc
  =,  acc
  ::
  ;<  crd-u=@  try:m  (malloc-write +((met 3 cord)) cord)
  ;<  str-u=@  try:m  (call-1 'QTS_NewString' ctx-u crd-u ~)
  ;<  is-eq=@  try:m  (call-1 'QTS_IsEqual' ctx-u val-u str-u 0 ~)  :: QTS_EqualOp_SameValue
  ;<  *        try:m  (call 'QTS_FreeValuePointer' ctx-u str-u ~)
  ;<  *        try:m  (call 'free' crd-u ~)
  (return:m !=(is-eq 0))
::
++  require
  |=  [ctx-u=@ this-u=@ argc-w=@ argv-u=@]
  =/  m  (script:lia-sur:wasm @ acc-mold)
  ^-  form:m
  =,  arr
  ?>  =(1 argc-w)
  ;<  is-toy=?  try:m  (js-val-cord-compare argv-u 'toy')
  ?:  is-toy  (js-eval toy-code)  ::  TODO proper addition in agreement with `require` spec?
  ::  ;<  is-foo=?  try:m  (js-val-cord-compare argv-u 'foo')
  ::  ?:  is-foo  (js-eval foo-code)
  ::  ...
  ::
  (ding 'QTS_Throw' ctx-u (make-error 'Name not recognized by `require`') ~)
::
++  make-error
  |=  txt=cord
  =/  m  (script:lia-sur:wasm @ acc-mold)
  ^-  form:m
  =,  arr
  ;<  acc=acc-mold  try:m  get-acc
  =,  acc
  ::
  ;<  err-u=@  try:m  (call-1 'QTS_NewError' ctx-u ~)
  =/  field=cord  'message'
  ;<  *        try:m
    %:  ring  'QTS_SetProp'  ::  ++ring is like ++call but takes a list of (@ or script that yields @)
      ctx-u
      err-u
      (ding 'QTS_NewString' ctx-u (malloc-write +((met 3 field)) field) ~)
      (ding 'QTS_NewString' ctx-u (malloc-write +((met 3 txt)) txt) ~)
      ~
    ==
  (return:m err-u)
::
++  malloc-write
  |=  data=octs
  =/  m  (script:lia-sur:wasm @ acc-mold)
  ^-  form:m
  =,  arr
  ;<  ptr-u=@  try:m  (call-1 'malloc' p.data ~)
  ;<  ~        try:m  (memwrite ptr-u data)
  (return:m ptr-u)
::
++  toy-code
  ^-  cord
  ::  (code to define the API, e.g. the classes to be extended)
  ::
  '''
  var _o = {
    Plugin: class Plugin
    {
      render_our_now_eny(our, now, eny)
      {
          return now;
      }
    }
  }
  _o
  '''
::
++  get-c-string
  |=  ptr=@
  =/  m  (script:lia-sur:wasm cord acc-mold)
  ^-  form:m
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
++  get-js-string
  |=  val-u=@
  =/  m  (script:lia-sur:wasm cord acc-mold)
  ^-  form:m
  =,  arr
  ;<  acc=acc-mold  try:m  get-acc
  =,  acc
  ::
  ;<  str-u=@  try:m  (call-1 'QTS_GetString' ctx-u val-u ~)
  (get-c-string str-u)
::
++  mayb-error
  |=  res-u=@
  =/  m  (script:lia-sur:wasm (unit cord) acc-mold)
  ^-  form:m
  =,  arr
  ;<  acc=acc-mold  try:m  get-acc
  =,  acc
  ::
  ;<  err-u=@   try:m  (call-1 'QTS_ResolveException' ctx-u res-u ~)
  ?:  =(0 err-u)  (return:m ~)
  ;<  str-u=@   try:m  (call-1 'QTS_GetString' ctx-u err-u ~)
  ;<  str=cord  try:m  (get-c-string str-u)
  (return:m `str)
::
++  make-function
  |=  [name=cord gat=$-([@ @ @ @] (script-raw-form:lia-sur:wasm @ acc-mold))]
  =/  m  (script:lia-sur:wasm (unit cord) acc-mold)  ::  (unit error=cord)
  ^-  form:m
  =,  arr
  ;<  acc=acc-mold  try:m  get-acc
  =,  acc
  ::
  =/  mag-w=@
    ?:  =(~ js-imports)  0
    +((~(rep in ~(key by js-imports)) max))
  ;<  nam-u=@  try:m  (malloc-write +((met 3 name)) name)
  ;<  res-u=@  try:m  (call-1 'QTS_NewFunction' ctx-u mag-w nam-u ~)
  ::
  ;<  err=(unit cord)  try:m  (mayb-error res-u)
  ?^  err  (return:m err)
  ::
  ;<  global-this-u=@  try:m  (call-1 'QTS_GetGlobalObject' ctx-u ~)
  ;<  nam-val-u=@      try:m  (call-1 'QTS_NewString' ctx-u nam-u ~)  ::  free string value?
  ;<  undef-u=@        try:m  (call-1 'QTS_GetUndefined' ~)
  ;<  *                try:m
    %:  call  'QTS_DefineProp'
      ctx-u
      global-this-u
      nam-val-u
      res-u
      undef-u  ::  get
      undef-u  ::  set
      0        ::  configurable
      0        ::  enumerable
      1        ::  has_value
      ~
    ==
  ::
  ;<  ~  try:m  (set-acc acc(js-imports (~(put by js-imports.acc) mag-w gat)))
  (return:m ~)
::  ++ring: complex call. Takes a list of either atoms
::    or scripts that yield atoms. Scripts are evaluated
::    from left to right
::
++  ring
  |=  [func=cord args=(list $@(@ (script-form @ acc-mold)))]
  =/  m  (script:lia-sur:wasm (list @) acc-mold)
  ^-  form:m
  =,  arr
  =|  args-atoms=(list @)
  |-  ^-  form:m
  ?~  args  (call func (flop args-atoms))
  ?@  i.args  $(args t.args, args-atoms [i.args args-atoms])
  ;<  atom=@  try:m  i.args
  $(args t.args, args-atoms [atom args-atoms])
::
++  ding  ::  complex call-1
  |=  [func=cord args=(list $@(@ (script-form @ acc-mold)))]
  =/  m  (script:lia-sur:wasm @ acc-mold)
  ;<  out=(list @)  try:m  (ring func args)
  ?>  =(1 (lent out))
  (return:m -.out)
::
--