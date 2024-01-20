/-  *wasm
|%
::  $module: module as seen by the engine.
::  Some sections are removed and some are changed
::
+$  module
  $:
    =type-section
  ::
    =import-section      ::  Changed
    =function-section    ::  Changed to include code
  ::
    =table-section    
    =memory-section   
    =global-section   
    =export-section
    :: =start-section    ::  Removed
    =elem-section
    :: =datacnt-section  ::  Removed
    :: =code-section     ::  Code moved to function section
    =data-section
  ==
::
+$  function
  $:  type-id=@
      locals=(list valtype)
      =expression
  ==
::
+$  function-section
  (list function)
::
+$  import-section
  $:
    funcs=(list [[mod=cord name=cord] type-id=@])
    tables=(list [[mod=cord name=cord] t=table])
    memos=(list [[mod=cord name=cord] l=limits])
    globs=(list [[mod=cord name=cord] v=valtype m=?(%con %var)])
  ==
::
+$  store
  $:  =shop                                    ::  resolved imports
      =module                                  ::  engine representation of module
      mem=(unit [buffer=@ n-pages=@])          ::  single membuffer
      tables=(list (list $>(%ref coin-wasm)))  ::  tables
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
  ::
      $:  %bloq    ::  blocked on import request
          [[mod=cord name=cord] =request]
          mem=(unit [buffer=@ n-pages=@])          ::  single membuffer
          tables=(list (list $>(%ref coin-wasm)))  ::  tables
          globals=(list coin-wasm)
          
  ==  ==
::  $result: what we get after instantiation of
::  or invocation from a module
::
+$  result
  $%
    [%0 out=(list coin-wasm) st=store]             ::  success
  ::
    $:  %1                                         ::  block
        [[mod=cord name=cord] =request]          ::  /module/name, request
        mem=(unit [buffer=@ n-pages=@])         
        tables=(list (list $>(%ref coin-wasm))) 
        globals=(list coin-wasm)
    ==
  ::
    [%2 ~]                                         ::  trap, crash
  ==
::
+$  request
  $%
    [%func args=(list coin-wasm)]  ::  typed values for the call
  ::
    $:  %tabl
        args=(list coin-wasm)
        $=  instr
        $>
          $?  %call-indirect  %table-get
              %table-set      %table-init
              %table-copy     %table-grow
              %table-size     %table-fill
          ==
        instruction
    ==
  ::
    $:  %memo
        args=(list coin-wasm)
        $=  instr
        $?  $>  $?  %load         %store
                    %memory-size  %memory-grow
                    %memory-init  %memory-copy
                    %memory-fill
                ==
            instr-short
        ::
            $:  %vec
                $>  $?  %load   %load-lane
                        %store  %store-lane
                    ==
                instr-vec
    ==  ==  ==
  ::
    $:  %glob
        args=(list coin-wasm)
        instr=$>(?(%global-get %global-set) instruction)
    ==
  ==
::  $shop: list of resolved requests: values to be pushed on
::  the stack and values from the module store
::
+$  shop
  %-  list
  %+  pair  (list coin-wasm)
  $:  mem=(unit [buffer=@ n-pages=@])          ::  single membuffer
      tables=(list (list $>(%ref coin-wasm)))  ::  tables
      globals=(list coin-wasm)
  ==
--