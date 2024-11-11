/+  runner-engine
=>  runner-engine
~%  %monad  +  ~
|%
++  script-lib
  ~%  %core  +  ~
  =,  lia-sur
  =*  cw  coin-wasm:wasm-sur
  |%
  :: 
  +$  run-input
    (each (script-raw-form (list lia-value)) (list lia-value))
  ::
  ++  run  ::  extend & extract
    =,  engine-sur
    =/  m  (script (list lia-value))
    |=  [in=run-input =seed]
    ^-  [yield:m _seed]
    =.  seed
      ?-    -.in
          %&
        seed(past ;<(* try:m past.seed p.in))  ::  past.seed >> p.in
      ::
          %|
        seed(shop (snoc shop.seed p.in))
      ==
    =/  ast  (main:parser module.seed)
    =/  valid  (validate-module:validator ast)
    ?>  ?=(%& -.valid)
    =/  sat=lia-state  [(conv:engine ast ~) shop.seed import.seed]
    |^  ^-  [yield:m _seed]
    :_  seed
    =<  -
    %.  sat
    ;<(* try:m init past.seed)  ::  init >> past.seed
    ::
    ++  init
      =/  m  (script ,~)
      ^-  form:m
      |=  sat=lia-state
      ^-  output:m
      =/  engine-res=result:engine
        (instantiate:engine p.sat)
      ?:  ?=(%0 -.engine-res)  [0+~ sat(p st.engine-res)]
      ?:  ?=(%2 -.engine-res)  [2+~ sat(p st.engine-res)]
      ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
      ::
      ?>  ?=(%func -.request.engine-res)
      =/  sat-blocked=lia-state  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
      =/  import-arrow
        (~(got by import.seed) mod.engine-res name.engine-res)
      =^  import-yil=(script-yield (list cw))  sat-blocked
        ((import-arrow args.request.engine-res) sat-blocked)
      ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
      $(shop.p.sat (snoc shop.p.sat p.import-yil +.p.sat-blocked))
    --
  ::
  ::  Basic Lia ops (Kleisli arrows)
  ::
  ++  call
    |=  [name=cord args=(list @)]
    =/  m  (script (list @))
    ^-  form:m
    |=  sat=lia-state
    =,  module.p.sat
    =/  id=@  (find-func-id:engine name module.p.sat)
    =/  id-local=@
      (sub id (lent funcs.import-section))
    =/  =func-type
      (snag type-id:(snag id-local function-section) type-section)
    ?>  =((lent params.func-type) (lent args))
    =/  engine-res=result:engine
      (invoke:engine name (types-atoms-to-coins params.func-type args) p.sat)
    ?:  ?=(%0 -.engine-res)
      [0+(turn out.engine-res cw-to-atom) sat(p st.engine-res)]
    ?:  ?=(%2 -.engine-res)
      [2+~ sat(p st.engine-res)]
    ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?>  ?=(%func -.request.engine-res)
    =/  sat-blocked=lia-state  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
    =/  import-arrow
      (~(got by r.sat) mod.engine-res name.engine-res)
    =^  import-yil=(script-yield (list cw))  sat-blocked
      ((import-arrow args.request.engine-res) sat-blocked)
    ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
    $(shop.p.sat (snoc shop.p.sat p.import-yil +.p.sat-blocked))
  ::
  ++  call-1
    |=  [name=cord args=(list @)]
    =/  m  (script @)
    ^-  form:m
    ;<  out=(list @)  try:m  (call name args)
    ?>  =(1 (lent out))
    (return:m -.out)
  ::
  ++  memread
    |=  [ptr=@ len=@]
    =/  m  (script octs)
    ^-  form:m
    |=  sat=lia-state
    ?~  mem.p.sat  [2+~ sat]
    =,  u.mem.p.sat
    ?:  (gth (add ptr len) (mul n-pages page-size))
      [2+~ sat]
    [0+[len (cut 3 [ptr len] buffer)] sat]
  ::
  ++  memwrite
    |=  [ptr=@ len=@ src=@]
    =/  m  (script ,~)
    ^-  form:m
    |=  sat=lia-state
    ?~  mem.p.sat  [2+~ sat]
    =,  u.mem.p.sat
    ?:  (gth (add ptr len) (mul n-pages page-size))
      [2+~ sat]
    :-  0+~
    sat(buffer.u.mem.p (sew 3 [ptr len src] buffer))
  ::
  ++  call-ext
    |=  [name=term args=(list lia-value)]
    =/  m  (script (list lia-value))
    ^-  form:m
    |=  sat=lia-state
    ?~  q.sat
      [1+[name args] sat]
    [0+i.q.sat sat(q t.q.sat)]
  ::
  ++  cw-to-atom
    |=  cw=coin-wasm:wasm-sur
    ^-  @
    ?<  ?=(%ref -.cw)
    +.cw
  ::
  ++  types-atoms-to-coins
    |=  [a=(list valtype:wasm-sur) b=(list @)]
    ^-  (list coin-wasm:wasm-sur)
    ?:  &(?=(~ a) ?=(~ b))  ~
    ?>  &(?=(^ a) ?=(^ b))
    :_  $(a t.a, b t.b)
    ?<  ?=(ref-type:wasm-sur i.a)
    ^-  cw
    ?-  i.a
      %i32   [i.a i.b]
      %i64   [i.a i.b]
      %f32   [i.a i.b]
      %f64   [i.a i.b]
      %v128  [i.a i.b]
    ==
  ::
  ++  page-size  ^~((bex 16))
  --
--