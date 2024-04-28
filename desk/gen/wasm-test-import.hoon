/-  *engine
/+  *runner-engine
/+  parser=parser-lib
/*  test  %wasm  /tests/test-import-global1/wasm
::
:-  %say  |=  *  :-  %noun
::
=/  res
  %^  invoke  'main'  ~
  +:(wasm-need (prep (main:parser test) ~))
?>  ?=(%1 -.res)
globals.res