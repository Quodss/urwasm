::  Web Assembly AST interpreter
::
::::  /hoon/ast-interpreter/lib
  ::
/-  *interpreter
/+  *op-def
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
  ?-    i.a
      ?(num-type vec-type)
    [i.a ?>(?=(@ i.b) i.b)]
  ::
      %extn
    ?>(?=([%ref %null %extn] i.b) i.b)  ::  return non-null external references?
  ::
      %func
    ?>  ?|  ?=([%ref %null %func] i.b)
            ?=([%ref %func @] i.b)
        ==
    i.b
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
  ?:  ?=(%func -.r.c)
    %func
  +.r.c
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
    ref-type  [%ref %null i.a]
  ==
::  +place: places list `b` into list `a`, overwriting contents of `a`
::
++  place
  |*  [[a=(list) off=@] b=(list)]
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
::  +make-export-map: turns export-section into a map [name=tape =export-desc]
::
++  make-export-map
  |=  =export-section
  =|  out=(map tape export-desc)
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
  |=  [name=tape =module]
  ^-  @
  =,  module
  =/  =export-desc  (~(got by (make-export-map export-section)) name)
  ?>  ?=(%func -.export-desc)
  i.export-desc
::  +prep: instantiate Wasm module. Returns global state
::  after the validation and import handling (TODO both),
::  instantiation of table, globals and the membuffer,
::  and running the start function (TODO)
::
++  prep
  |=  module
  =*  module  +<
  ^-  store
  =+  *store
  =*  store  -
  =.  store  store(module module)
  |^
  ::  Globals
  ::
  =.  globals
    |-  ^+  globals
    ?~  global-section  (flop globals)
    =*  glob  i.global-section
    ::  Assert: no global imports (TODO remove limitation)
    ::
    ?>  ?=([%const coin=*] i.glob)
    %=  $
      global-section  t.global-section
      globals  [coin.p.i.glob globals]
    ==
  ::  Table
  ::
  ::  Initialize table
  ::
  =?  table  ?=(^ table-section)
    ?>  ?=(@ t.table-section)        ::  single table REMOVE
    ?>  ?=(%func p.i.table-section)  ::  assert funcref (remove?)
    init-table
  ::  Memory
  ::
  =?  mem  ?=(^ memory-section)
    ?>  ?=(@ t.memory-section)  ::  single memory
    init-mem
  store
  ::
  ++  init-table
    ^+  table
    ::  initialize table with null refs
    ::
    ?>  ?=(^ table-section)
    =.  table
      (reap (lim-min q.i.table-section) [%ref %null %func])
    |-  ^+  table
    ?~  elem-section  table
    =*  elem  i.elem-section
    ?.  ?=(%acti -.m.elem)
      $(elem-section t.elem-section)
    ::  Assert: only one table (remove)
    ::
    ?>  =(0 tab.m.elem)
    ::  Assert: only %func ref tables can be
    ::  initialized with an element segment
    ::
    ?>  ?=(%func t.elem)
    ::  Assert: i32 value in the offset
    ::
    ?>  ?=([%const %i32 n=@] off.m.elem)
    %=    $
        elem-section  t.elem-section
    ::
        table
      %+  place  [table n.p.off.m.elem]
      %+  turn  i.elem
      |=  in=instruction
      ^-  $>(%ref coin-wasm)
      ?>  ?=([%const coin=[%ref %func @]] in)  ::  Assert: %func refs only
      coin.p.in
    ::
    ==
  ::
  ++  init-mem
    ^+  mem
    ?>  ?=(^ memory-section)
    =.  mem  mem(n-pages (lim-min i.memory-section))
    |-  ^+  mem
    ?~  data-section  mem
    =*  data  i.data-section
    ?.  ?=(%acti -.data)
      $(data-section t.data-section)
    ::  Assert: const i32 value as offset (revisit: it might be a global-get)
    ::
    ?>  ?=([%const %i32 n=@] off.data)
    %=    $
        data-section  t.data-section
        buffer.mem
      (sew bloq=3 [n.p.off.data b.data] buffer.mem)  ::  where did p come from???
    ==
  ::
  --
::  +invoke: call function by id, to call from the outside
::
++  invoke
  |=  [id=@ in=(list coin-wasm) st=store]
  ^-  $%  [%0 (list coin-wasm) store]  ::  success
      ::  [%1 *]                       ::  import block, to define
          [%2 ~]                       ::  trap, crash
      ==
  ::  Type check for the input values
  ::
  =,  module.st
  =/  =func-type  (snag (snag id function-section) type-section)
  ?>  =(params.func-type (get-types in))
  =/  [stack-out=stack * st-out=store]
    (call id [`(flop in) ~ st])
  ?+  br.stack-out  !!
    ~  :+  %0
         (change results.func-type (flop va.stack-out))
       st-out
    [%trap ~]  [%2 ~]
  ==
::  +call: call function by id, inside caller. TODO add imports
::
++  call
  |=  [id=@ l=local-state]
  ^-  local-state
  =,  module.store.l
  =/  =func-type
    (snag (snag id function-section) type-section)
  =/  =code  (snag id code-section)
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
    %+  eval  expression.code
    ^-  local-state
    [~ (weld input-values (mint locals.code)) store.l]
  ::  If trap: forward trap
  ::
  ?:  ?=([%trap ~] br.stack.l)  l
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
  %_  l
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
::  +dec-br: "decrement" branch
::
++  dec-br
  |=  br=branch
  ^-  branch
  ?-  br
    ~            ~
    [%trap ~]    [%trap ~]
    [%retr ~]    [%retr ~]
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
  !.
  ?+    i  ((fetch-gate i) l)
      [%call id=@]
    (call id.i l)
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
      (scag (lent result-type.i) va.stack.l)
    ::  Exit the block, navigate branch
    ::
    %_(l va.stack (weld va.stack.l rest-vals), br.stack (dec-br br.stack.l))
  ::
      [%loop ~ body=*]  ::  [%loop result-type=(list valtype) body=(list instruction)], revisit type
    ::  save the current frame
    ::
    =/  rest-vals=(pole val)  va.stack.l  ::  nothing is consumed yet, to fix
    ::  execute the block
    ::
    =.  l  (eval body.i l(va.stack *(pole val)))
    ::  If the loop was targeted, pop appropriate amount of vals,
    ::  push them on the stack and continue (nothing is popped atm, fix)
    ::
    ?:  ?=([%targ %0] br.stack.l)
      $(l l(va.stack rest-vals))
    ::  Exit the block, navigate branch
    ::
    %_(l va.stack (weld va.stack.l rest-vals), br.stack (dec-br br.stack.l))
  ::
      [%call-indirect type-id=@ table-id=%0x0]
    ?>  ?=([tab-id=@ rest=*] va.stack.l)
    ::  Type check of reference
    ::
    =,  module.store.l
    =,  va.stack.l
    =/  type-in-instr=(list valtype)  (snag type-id type-section)
    =/  type-of-ref=(list valtype)
      =+  ref=(snag tab-id table.store.l)
      ?>  ?=([%ref %func n=@] ref)  ::  funcref only, add extern
      (snag (snag n.ref function-section) type-section)
    ?>  =(type-in-instr type-of-ref)
    ::  Call referenced function, funcref only, add extern
    ::
    =;  id-func=@
      (call id-func l(va.stack rest))
    =+  ref=(snag tab-id table.store.l)
    ?>  ?=([%ref %func n=@] ref)
    n.ref
  ::
      [%if *]  ::  [%if result-type=(list valtype) branch-true=(list instruction) branch-false=(list instruction)]
    ?>  ?=([f=@ rest=*] va.stack.l)
    =,  va.stack.l
    ?.  =(0 f.va.stack.l)
      $(i [%block result-type branch-true]:i, va.stack.l rest)
    $(i [%block result-type branch-false]:i, va.stack.l rest)
  ::
  ==
--
