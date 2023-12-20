/-  *wasm
|%
+$  store
  $:  =module
      mem=[buffer=@ n-pages=@]         ::  single membuffer
      table=(list $>(%ref coin-wasm))  ::  single table; add multiple
      globals=(list coin-wasm)
  ==
::
+$  local-state  [=stack locals=(list val) =store]
+$  stack  [br=branch va=(pole val)]
+$  val
  $~  `@`0
  $@  @               ::  numerical value
  $>(%ref coin-wasm)  ::  reference
::
+$  branch
  $~  ~
  $@  ~
  $%  [%retr ~]    ::  return to the caller
      [%targ i=@]  ::  targeted block
      [%trap ~]    ::  deterministic crash
  ::  [%impo *]  Execution blocked with an unresolved import.
  ::             Noun should contain import func name and args
  ==
::
+$  wasm-res
  $%
    [%0 out=(list coin-wasm) st=store]  ::  success
  ::  [%1 *]                            ::  import block, to define
    [%2 ~]                              ::  trap, crash
  ==
--