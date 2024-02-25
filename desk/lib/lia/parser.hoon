/-  sur=lia
/+  parser=parser-lib
::
|%
++  calm
  |*  a=(unit (list))
  ^-  _(need a)
  ?~  a  ~
  u.a
::
++  types
  |%
  +$  block  (list phrase)
  +$  phrase  [names=(list name) =op]
  +$  name  @tas
  +$  op
    $@  name
    $%
      [%run p=cord q=(list op)]
      [%cut octs=op offset=op len=op]
      [%read offset=op len=op]
      [%writ octs=op offset=op]
      [%lit p=value:sur]
      [%len octs=op]
      [%octs dat=op len=op]
      [%add p=op q=op]
      [%sub p=op q=op]
      :: [%nop ~]
    ==
  --
::
++  main
  |=  txt=@t
  ^-  block:types
  (rash txt (ifix [(star gah) (star gah)] block))
::
++  nl  ;~(plug (star ace) (just '\0a') (star gah))
++  block  (cook ,block:types (more nl phrase))
++  solid
  |*  sef=rule
  (ifix [(star ace) (star ace)] sef)
::
++  lose
  |*  [bus=rule fel=rule]
  ;~(pfix bus fel)
::
++  gain
  |*  [bus=rule fel=rule]
  ;~(sfix bus fel)
::
++  phrase
  %+  cook  ,phrase:types
  ;~  plug
    %+  cook  calm
    %-  punt
    ;~  sfix
      (more (solid com) sym)
      (solid tis)
    ==
  ::
    op
  ==
::
++  func-name
  %+  cook  crip
  ;~  pose
    (plus ;~(pose low hig nud cab))  ::  alphanumeric and _
  ::
    %+  ifix  [buc buc]              ::  any string inside $
    %-  plus
    ;~  pose
      ;~(less buc prn)
      (cold '$' (jest '\\$'))  ::  escaped $
    ==
  ==
::
++  op
  %+  knee  *op:types
  |.  ~+
  ;~  pose
    %+  stag  %add
    ;~((glue (solid lus)) factor op)
  ::
    %+  stag  %sub
    ;~((glue (solid hep)) factor op)
  ::
    factor
  ==
::
++  factor
  %+  knee  *op:types
  |.  ~+
  %+  cook  handle-method
  ;~(plug piece (punt cont))
::
++  piece
  ;~  pose
    %+  stag  %run
    ;~  plug
      func-name
    ::
      %+  ifix  [pal par]
      (more (solid com) op)
    ==
  ::
    %+  stag  %read
    %+  lose  (jest 'memory.read(')
    %+  gain  ;~((glue (solid com)) op op)
    par
  ::
    %+  stag  %writ
    %+  lose  (jest 'memory.write(')
    %+  gain  ;~((glue (solid com)) op op)
    par
  ::
    %+  stag  %lit
    ;~  pose
      (stag %i32 (cook negate-32 (lose hep dem)))
      (stag %i32 dem)
    ::
      %+  stag  %octs
      %+  ifix  [sel ser]
      ;~((glue ace) dem dem)
    ==
  ::
    %+  stag  %octs
    %+  ifix  [sel ser]
    %+  gain  ;~(plug op op)
    (jest '.to_octs')
  ::
    sym  ::  dereference name
  ==
::
++  cont
  ;~  pose
    %+  stag  %cut
    ;~  plug
      %+  lose  (jest '.cut(')
      %+  gain  ;~(plug op op)
      par
    ==
  ::
    %+  stag  %len
    (cold ~ (jest '.len'))
  ==
++  negate-32
  |=  n=@
  ^-  @
  (sub ^~((bex 32)) n)
::
++  handle-method
  |=  $:  a=op:types
          $=  b
          %-  unit
          $%
            [%len ~]
            [%cut p=op:types q=op:types]
      ==  ==
  ^-  op:types
  ?~  b  a
  ?-  -.u.b
    %len  [%len a]
    %cut  [%cut a p.u.b q.u.b]
  ==
::
--