::  Web Assembly AST interpreter
::
::::  /hoon/ast-interpreter/lib
  ::
/-  *engine
/+  *runner-op-def
/+  parser-lib
|%
::  +change: convert stack values to coin-wasm list
::
++  change
  |=  [a=(list valtype) b=(list val)]
  ^-  (list coin-wasm)
  ?.  &(?=(^ a) ?=(^ b))
    ?>  &(?=(~ a) ?=(~ b))
    ~
  :_  $(a t.a, b t.b)
  ;;  coin-wasm  ::  the compiler isn't convinced that the 
  ?-    i.a      ::  expression below is a coin-wasm, why? :(
      ?(num-type vec-type)
    [i.a ?>(?=(@ i.b) i.b)]
  ::
      %extn
    ?>(?=([%ref %extn ~] i.b) i.b)  ::  return non-null external references?
  ::
      %func
    ?>(?=([%ref %func *] i.b) i.b)
  ==
::  +get-types: turn a (list coin-wasm) into a list of types of coins
::
++  get-types
  |=  a=(list coin-wasm)
  ^-  (list valtype)
  %+  turn  a
  |=  c=coin-wasm
  ^-  valtype
  ?.  ?=([%ref r=*] c)
    -.c
  -.r.c
::  +mint:  get stack values from valtypes, used for locals
::
++  mint
  |=  a=(list valtype)
  ^-  (list val)
  ?~  a  ~
  :_  $(a t.a)
  ^-  val
  ?-  i.a
    ?(num-type vec-type)  *@
    ref-type  :-  %ref
              ?-  i.a
                %extn  [%extn ~]
                %func  [%func ~]
  ==          ==
::  +place: places list `b` into list `a`, overwriting contents of `a`
::
++  place
  |*  [a=(list) off=@ b=(list)]
  |-  ^+  b
  ?~  b  a
  ?>  (lth off (lent a))
  $(a (snap a off i.b), b t.b, off +(off))
::  +lim-min: get minimum from limits
::
++  lim-min
  |=  l=limits
  ^-  @
  ?:  ?=(%flor -.l)
    p.l
  p.l
::  +lim-max: get maximum from limits
::
++  lim-max
  |=  l=limits
  ^-  (unit @)
  ?:  ?=(%flor -.l)
    ~
  `q.l
::  +make-export-map: turns export-section into a map [name=cord =export-desc]
::
++  make-export-map
  |=  =export-section
  =|  out=(map cord export-desc)
  |-  ^+  out
  ?~  export-section  out
  =,  i.export-section
  %=  $
    out  (~(put by out) name export-desc)
    export-section  t.export-section
  ==
::  +find-func-id: find func-id from a name of an exported function
::
++  find-func-id
  |=  [name=cord =module]
  ^-  @
  =,  module
  =/  =export-desc
    (~(got by (make-export-map export-section)) name)
  ?>  ?=(%func -.export-desc)
  i.export-desc
::  +prep: instantiate Wasm module. Returns global state
::  after the validation and import handling (TODO both),
::  instantiation of table, globals and the membuffer,
::  and running the start function (TODO)
::
::  Attention: it takes a parsed module, but the store
::  will contain engine version, with locals added and
::  some superfluous sections removed
::
++  prep
  |=  [m=^module =shop]
  ^-  result
  |^
  =|  st=store
  =.  shop.st  shop
  :: =.  s  s(module m)  fill $module while treating imports
  =.  module.st
    =,  m
    :*
      type-section
      (import-upd import-section)
      (fuse:parser-lib function-section code-section)
      table-section
      memory-section
      global-section
      export-section
      elem-section
      data-section
    ==
  ;<  [* st=store]  _wasm-bind  (init-globals st m)
  ;<  [* st=store]  _wasm-bind  (init-table st m)
  ;<  [* st=store]  _wasm-bind  (init-elems st m)
  ;<  [* st=store]  _wasm-bind  (init-mem st m)
  ;<  [* st=store]  _wasm-bind  (init-data st m)
  [%0 ~ st]
  ::
  ++  init-globals
    |=  [st=store m=^module]
    ^-  result
    ?~  global-section.m
      [%0 ~ st(globals (flop globals.st))]
    =*  glob  i.global-section.m
    =;  try-glob=(each (pair coin-wasm shop) $>(%1 result))  ::  coin & shop or block
      ?.  ?=(%& -.try-glob)  p.try-glob
      %=  $
        global-section.m  t.global-section.m
        globals.st  [p.p.try-glob globals.st]
        shop.st  q.p.try-glob
      ==
    ::  if not global-get: it is %const
    ::
    ?.  ?=([%global-get index=@] i.glob)
      [& p.i.glob shop.st]
    ::  else, get from imports
    ::
    ?:  ?=(^ shop.st)
      [& -.i.shop.st t.shop.st]  ::  sucessfull read from shop
    :-  |                        ::  block on empty shop
    :+  %1
      -:(snag index.i.glob globs.import-section.module.st)
    [%glob ~ i.glob]
  ::
  ++  init-table
    |=  [st=store m=^module]
    ^-  result
    :+  %0  ~
    |-  ^-  store
    ?~  table-section.m
      st(tables (flop tables.st))
    =*  tab  i.table-section.m
    %=    $
        table-section.m  t.table-section.m
    ::
        tables.st
      :_  tables.st
      (reap (lim-min q.tab) [%ref %func ~])
    ==
  ::
  ++  init-elems
    |=  [st=store m=^module]
    ^-  result
    :+  %0  ~
    |-  ^-  store
    ?~  elem-section.m  st
    =*  elem  i.elem-section.m
    ?.  ?=(%acti -.m.elem)
      $(elem-section.m t.elem-section.m)
    ::  Assert: only %func ref tables can be
    ::  initialized with an element segment
    ::
    ?>  ?=(%func t.elem)
    ::  Assert: i32 value in the offset
    ::  (it theoretically can be a %global-get of import, revisit later?)
    ::
    ?>  ?=([%const %i32 n=@] off.m.elem)
    =/  tab-loc-id=@
      %+  sub  tab.m.elem                     ::  Assert: elems are instantiated locally
      (lent tables.import-section.module.st)  ::  (to revisit?)
    %=    $
        elem-section.m  t.elem-section.m
    ::
        table.st
      %^  snap  table.st  tab-loc-id
      %^  place  (snag tab-loc-id table.st)  ::  table to change
        n.p.off.m.elem                       ::  offset
      %+  turn  i.elem
      |=  in=instruction
      ^-  $>(%ref coin-wasm)
      ?>  ?=([%ref-func @] in)  ::  Assert: %func refs only
      [%ref %func `func-id.in]
    ::
    ==
  ::
  ++  init-mem
    |=  [st=store m=^module]
    ^-  result
    :+  %0  ~
    ?~  memory-section.m  st
    ?>  ?=(@ t.memory-section.m)  ::  Assert: single memory
    =*  mem  i.memory-section.m
    st(mem `[0 (lim-min mem)])
  ::
  ++  init-data
    |=  [st=store m=^module]
    ^-  result
    ?~  data-section.m  [%0 ~ st]
    =*  data  i.data-section.m
    ?.  ?=(%acti -.data)
      $(data-section.m t.data-section.m)
    ::  Assert: const i32 value as offset
    ::  (it theoretically can be a %global-get of import, revisit later?)
    ::
    ?>  ?=([%const %i32 n=@] off.data)
    %=    $
        data-section.m  t.data-section.m
        mem.st
      :-  ~
      :_  n-pages:(need mem.st)  ::  assert: data is instantiated only locally, revisit later?
      (sew bloq=3 [n.p.off.data b.data] buffer:(need mem.st))
    ==
  ::
  ++  import-upd
    |=  i=^import-section
    =|  out=import-section
    |-  ^-  import-section
    ?~  i
      =,  out
      %_  out
        funcs   (flop funcs)
        tables  (flop tables)
        memos   (flop memos)
        globs   (flop globs)
      ==
    =.  out
      =,  i.i
      =,  out
      ?-  -.desc
        %func  out(funcs [[[mod name] type-id.desc] funcs])
        %tabl  out(tables [[[mod name] t.desc] tables])
        %memo  out(memos [[[mod name] l.desc] memos])
        %glob  out(globs [[[mod name] +.desc] globs])
      ==
    $(i t.i)
  ::
  --
::
++  wasm-need
  |=  a=result
  ^-  (quip coin-wasm store)
  ?>  ?=(%0 -.a)
  +.a
::
++  wasm-bind
  |=  [a=result b=$-((quip coin-wasm store) result)]
  ^-  result
  ?.  ?=(%0 -.a)  a
  (b +.a)
::
::  +invoke: call function by name, to call from the outside
::
++  invoke
  |=  [name=cord in=(list coin-wasm) st=store]
  ^-  result
  =/  id=@  (find-func-id name module.st)
  ::  Type check for the input values
  ::
  =,  module.st
  =/  =func-type
    (snag type-id:(snag id function-section) type-section)
  ?>  =(params.func-type (get-types in))
  =/  [stack-out=stack * st-out=store]
    %+  call  id
    ^-  local-state
    :+  stack=[~ (turn (flop in) coin-to-val)]
      locals=~
    store=st
  ?+  br.stack-out  !!
    ~  :+  %0
         (change results.func-type (flop va.stack-out))
       st-out
    [%bloq p=*]  [%1 p.br.stack-out] 
    [%trap ~]    [%2 ~]
  ==
::  +call: call function by id, inside caller. TODO add imports
::
++  call
  |=  [id=@ l=local-state]
  ^-  local-state
  =,  module.store.l
  =+  f=(func:grab id store.l)  ::  (each function [[mod=cord name=cord] type-id=@])
  =/  type-id=@  =>(f ?:(?=(%& -) type-id.p type-id.p))
  =/  =func-type  (snag type-id type-section)
  ?:  ?=(%| -.f)  ::  import case
    ?~  shop.store.l
      =.  br.stack.l  ::  build a request
        :^  %bloq  -.p.f  %func
        %+  change  params.functype
        %-  flop
        (scag (lent params.func-type) va.stack.l)
      l
    =.  va.stack.l
      %+  weld
        =-  ?>(=((lent -) (lent results.func-type)) -)  ::  external typecheck
        (flop (turn i.shop.store.l coin-to-val))
      (slag (lent params.func-type) va.stack.l)
    l(shop.store t.shop.store.l)
  ::  take input values
  ::
  =/  input-values=(pole val)
    %-  flop
    (scag (lent params.func-type) va.stack.l)
  ::  save the rest of the stack and our locals
  ::
  =/  rest-vals=(pole val)
    (slag (lent params.func-type) va.stack.l)
  =/  our-locs=(list val)  locals.l
  ::  update local state
  ::
  =.  l
    %+  eval  expression.p.f
    ^-  local-state
    :+  stack=[~ ~]
      locals=(weld input-values (mint locals.p.f))
    store=store.l
  ::  If trap or bloq: forward
  ::
  ?:  ?=([?(%trap %bloq) ~] br.stack.l)  l
  ::  Assert: no branch or branch with label 0 or return
  ::
  ?>  ?|  ?=(~ br.stack.l)
          ?=([%retr ~] br.stack.l)
          ?=([%targ %0] br.stack.l)
      ==
  ::  If return or targeted, take appropriate amount of vals from the stack
  ::
  =?  va.stack.l  ?|  ?=([%retr ~] br.stack.l)
                      ?=([%targ %0] br.stack.l)
                  ==
    (scag (lent results.func-type) va.stack.l)
  ::  Push returned values on stack, bring back locals, empty out br
  ::
  %=  l
    va.stack  (weld va.stack.l rest-vals)
    locals    our-locs
    br.stack  ~
  ==
::  +eval: evaluate an expression, mutating local state
::
++  eval
  |=  [e=expression l=local-state]
  ^-  local-state
  ?:  |(=(~ e) !=(~ br.stack.l))  ::  if navigating branch
    l                             ::  jump to the end of expression 
  $(l (apply -.e l), e +.e)
::  +dec-br: "decrement" branch. Preserve absolute branching
::  coordinates (trap, return, block), and safely decrement
::  relative target index. Used for code flow navigation
::
++  dec-br
  |=  br=branch
  ^-  branch
  ?+  br  br
    [%targ %0]   ~
    [%targ i=@]  [%targ (dec i.br)]
  ==
::  +apply: apply an instruction, mutating local state
::  Only recursive control instructions are listed here. The rest
::  are returned by fetch-gate:op-def as gates $-(local-state local-state)
::  and applied to `l`
::
++  apply
  |=  [i=instruction l=local-state]
  ^-  local-state
  :: !.
  ?+    i  ((fetch-gate i) l)
      [%call func-id=@]
    (call func-id.i l)
  ::
      [%block *]  ::  [%block result-type=(list valtype) body=expression]
    ::  save the current frame
    ::
    =/  rest-vals=(pole val)  va.stack.l  ::  nothing is consumed yet, to fix
    ::  execute the block
    ::
    =.  l  (eval body.i l(va.stack *(pole val)))
    ::  If the block was targeted, pop appropriate amount of vals
    ::
    =?  va.stack.l  ?=([%targ %0] br.stack.l)
      =;  n-results=@
        (scag n-results va.stack.l)
      %-  lent
      ?^  type.i  results.type.i
      results:(snag type.i type-section.module.store.l)
    ::  Exit the block, navigate branch
    ::
    l(va.stack (weld va.stack.l rest-vals), br.stack (dec-br br.stack.l))
  ::
      [%loop *]  ::  [%loop result-type=(list valtype) body=(list instruction)], revisit type
    |-  ^-  local-state  ::  not strictly necessary, but prevents from matching `i` again
    ::  save the current frame
    ::
    =/  rest-vals=(pole val)  va.stack.l  ::  nothing is consumed yet, to fix
    ::  execute the block
    ::
    =.  l  (eval body.i l(va.stack *(pole val)))
    ::  If the loop was targeted, pop appropriate amount of vals,
    ::  push them on the stack, clear branching signal and jump to start
    ::  (nothing is popped atm, fix)
    ::
    ?:  ?=([%targ %0] br.stack.l)
      $(l l(va.stack rest-vals, br.stack ~))
    ::  Exit the block, navigate branch
    ::
    l(va.stack (weld va.stack.l rest-vals), br.stack (dec-br br.stack.l))
  ::
      [%call-indirect type-id=@ table-id=%0x0]
    ?>  ?=([tab-id=@ rest=*] va.stack.l)
    ::  Type check of reference
    ::
    =,  module.store.l
    =,  va.stack.l
    =/  type-in-instr=func-type  (snag type-id.i type-section)
    =/  type-of-ref=func-type
      =+  ref=(snag tab-id table.store.l)
      ?>  ?=([%ref %func p=[~ @]] ref)  ::  funcref only, add extern
      (snag (snag u.p.ref function-section) type-section)
    ?>  =(type-in-instr type-of-ref)
    ::  Call referenced function, funcref only, add extern
    ::
    =;  id-func=@
      (call id-func l(va.stack rest))
    =+  ref=(snag tab-id table.store.l)
    ?>  ?=([%ref %func p=[~ @]] ref)
    u.p.ref
  ::
      [%if *]  ::  [%if result-type=(list valtype) branch-true=(list instruction) branch-false=(list instruction)]
    ?>  ?=([f=@ rest=*] va.stack.l)
    =,  va.stack.l
    ?.  =(0 f.va.stack.l)
      $(i [%block type branch-true]:i, va.stack.l rest)
    $(i [%block type branch-false]:i, va.stack.l rest)
  ::
  ==
--
