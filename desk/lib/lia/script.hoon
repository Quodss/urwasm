/+  runner-engine
=>  runner-engine
~%  %monad  +  ~
|%
++  script-lib
  ~%  %core  +  ~
  =,  lia-sur
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
    =/  =lia-state  [(conv:engine ast ~) shop.seed]
    |^  ^-  [yield:m _seed]
    :_  seed
    =<  -
    %.  lia-state
    ;<(~ try:m init past.seed)  ::  init >> past.seed
    ::
    ++  init
      ^-  (script-raw-form ~)
      !!
    --
  ::
  ::  Basic Lia ops (Kleisli arrows)
  ::
  ++  call      !!
  ++  memread   !!
  ++  memwrite  !!
  ++  call-ext  !!
  --
--