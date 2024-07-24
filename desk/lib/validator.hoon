/+  parser
=>  parser
|%
++  validator
  =,  wasm-sur
  |%
  +$  glob-type  [v=valtype m=?(%con %var)]
  +$  glob-types  (list glob-type)
  ++  output
    |%
    +$  import
      $:
        funcs=(list func-type)  ::  flopped
        tables=(list table)  ::  flopped
        memo=(unit limits)
        globs=glob-types  ::  flopped
      ==
    --
  ::
  +$  store  import:output  ::  right order
  ::
  ++  result
    |$  [mold]
    (each mold cord)
  ::
  ++  build-try
    |*  m2=mold
    |*  m1=mold
    |=  [a=(result m1) b=$-(m1 (result m2))]
    ^-  (result m2)
    ?:  ?=(%| -.a)  a
    (b p.a)
  ::
  ++  snug
    |=  where=cord
    |*  [a=@ b=(list)]
    |-  ^-  (result _?>(?=(^ b) i.b))
    ?~  b  |+(crip (weld "index error in " (trip where)))
    ?:  =(a 0)  &+i.b
    $(a (dec a), b t.b)
  ::
  ++  validate-module
    |=  m=module
    =/  try  (build-try ,~)
    ^-  (result ~)
    ;<  import-out=import:output  try  (v-import-section m)
    =/  n-funcs-import=@  (lent funcs.import-out)
    ;<  functypes=(list func-type)  try
      (v-function-section m funcs.import-out)
    ;<  tables=(list table)  try  (v-table-section m tables.import-out)
    ;<  memo=(unit limits)  try  (v-memory-section m memo.import-out)
    =/  n-funcs=@  (lent functypes)
    =/  n-tables=@  (lent tables)
    ;<  =glob-types  try
      (v-global-section m globs.import-out n-funcs)
    ;<  ~  try
      %:  v-export-section
        m
        n-funcs
        n-tables
        ?^(memo 1 0)
        (lent glob-types)
      ==
    ;<  ~  try  (v-start-section m functypes)
    ;<  ~  try  (v-elem-section m n-tables)
    ;<  datacnt=@  try  (v-datacnt-section m)
    ;<  ~  try
      %:  v-code-section
        m
        n-funcs-import
        functypes
        tables
        memo
        glob-types
      ==
    (v-data-section m)
  ::
  ++  v-import-section
    |=  m=module
    =|  out=import:output
    =/  num-types=@  (lent type-section.m)
    =/  try  (build-try import:output)
    |-  ^-  (result import:output)
    ?~  import-section.m
      &+out
    ?-    -.desc.i.import-section.m
        %func
      =/  idx=@  type-id.desc.i.import-section.m
      ;<  type=func-type  try  ((snug 'import functype') idx type-section.m)
      %=  $
        import-section.m  t.import-section.m
        funcs.out  [type funcs.out]
      ==
    ::
        %tabl
      ?.  (validate-limits q.t.desc.i.import-section.m)  |+'invalid limits import table'
      =/  =table  t.desc.i.import-section.m
      $(import-section.m t.import-section.m, tables.out [table tables.out])
    ::
        %memo
      ?.  (validate-limits l.desc.i.import-section.m)  |+'invalid limits import memo'
      ?^  memo.out  |+'multiple memos'
      $(import-section.m t.import-section.m, memo.out `l.desc.i.import-section.m)
    ::
        %glob
      %=  $
        import-section.m  t.import-section.m
        globs.out  [+.desc.i.import-section.m globs.out]
      ==
    ==
  ::
  ++  validate-limits
    |=  l=limits
    ^-  ?
    ?-  -.l
      %flor  &
      %ceil  (gte q.l p.l)
    ==
  ::
  ++  v-function-section
    |=  [m=module functypes-import=(list func-type)]
    =/  functypes=(list func-type)  functypes-import
    =/  try  (build-try (list func-type))
    |-  ^-  (result (list func-type))
    ?~  function-section.m  &+(flop functypes)
    =/  idx=@  i.function-section.m
    ;<  type=func-type  try  ((snug 'local functype') idx type-section.m)
    %=  $
      function-section.m  t.function-section.m
      functypes  [type functypes]
    ==
  ::
  ++  v-table-section
    |=  [m=module tables=(list table)]
    ^-  (result (list table))
    ?~  table-section.m  &+(flop tables)
    ?.  (validate-limits q.i.table-section.m)  |+'invalid limits local table'
    $(table-section.m t.table-section.m, tables [i.table-section.m tables])
  ::
  ++  v-memory-section
    |=  [m=module memo=(unit limits)]
    ^-  (result (unit limits))
    =/  len-memos=@  (lent memory-section.m)
    ?:  (gth len-memos 1)  |+'multiple memos'
    ?:  &(?=(^ memo) (gth len-memos 0))  |+'multiple memos'
    ?^  memo  &+memo
    &+`-.memory-section.m
  ::
  ++  v-global-section
    |=  [m=module gt=glob-types n-funcs=@]
    =/  n-glob-import=@  (lent gt)
    |-  ^-  (result glob-types)
    ?~  global-section.m  &+(flop gt)
    =/  glob  i.global-section.m
    ?-    -.i.glob
        %const
      ?.  =(v.glob -.p.i.glob)  |+'global type mismatch'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %vec
      ?.  ?=(%v128 v.glob)  |+'global type mismatch'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %ref-null
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %ref-func
      ?:  (gte func-id.i.glob n-funcs)  |+'invalid funcref'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %global-get
      ?:  (gte index.i.glob n-glob-import)  |+'non-import or nonexisting const global'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ==
  ::
  ++  v-export-section
    |=  [m=module n-funcs=@ n-tables=@ n-memos=@ n-globs=@]
    =|  names=(set cord)
    |-  ^-  (result ~)
    ?~  export-section.m  &+~
    =/  exp  i.export-section.m
    ?:  (~(has in names) name.exp)  |+'name duplicate'
    =;  [i=@ num=@]
      ?:  (gte i num)  |+'invalid export index'
      $(export-section.m t.export-section.m, names (~(put in names) name.exp))
    ?-  -.export-desc.exp
      %func  [i.export-desc.exp n-funcs]
      %tabl  [i.export-desc.exp n-tables]
      %memo  [i.export-desc.exp n-memos]
      %glob  [i.export-desc.exp n-globs]
    ==
  ::
  ++  v-start-section
    |=  [m=module functypes=(list func-type)]
    =/  try  (build-try ,~)
    ^-  (result ~)
    ?~  start-section.m  &+~
    =/  func-idx=@  u.start-section.m
    ;<  type=func-type  try  ((snug 'start section') func-idx functypes)
    ?.  ?=([~ ~] type)
      |+'non-void start function'
    &+~
  ::
  ++  v-elem-section
    ::  elems are additionaly restricted by the parser: offset
    ::  expression is limited to a single %const instruction,
    ::  and init expression are limited to a single %ref* instruction
    ::
    |=  [m=module n-tables=@]
    ^-  (result ~)
    ?~  elem-section.m  &+~
    =/  elem  i.elem-section.m
    ?.  ?=(%acti -.m.elem)  $(elem-section.m t.elem-section.m)
    ?:  (gte tab.m.elem n-tables)  |+'element index error'
    ?.  ?=(%i32 -.p.off.m.elem)  |+'type error in element offset'
    $(elem-section.m t.elem-section.m)
  ::
  ++  v-datacnt-section
    |=  m=module
    ^-  (result @)
    ?~  datacnt-section.m  &+0
    &+u.datacnt-section.m
  ::
  ++  v-code-section
    |=  $:  m=module
            n-funcs-import=@
            =store
        ==  
    =/  idx=@  n-funcs-import
    =/  try  (build-try ,~)
    |-  ^-  (result ~)
    ?~  code-section.m  &+~
    ;<  type=func-type  try  ((snug 'code section') idx funcs.store)
    ;<  ~  try  (validate-code idx i.code-section.m type m store)
    $(idx +(idx), code-section.m t.code-section.m)
  ::
  ++  v-data-section
    ::  data section is additionaly restrained by the parser:
    ::  offset expression may only be %const instruction
    ::
    |=  m=module
    ^-  (result ~)
    ?~  data-section.m  &+~
    =/  data  i.data-section.m
    ?:  ?=(%pass -.data)
      $(data-section.m t.data-section.m)
    ?.  ?=(%i32 -.p.off.data)  |+'type error in data offset'
    $(data-section.m t.data-section.m)
  ::
  ++  validate-code
    |=  $:  idx=@
            =code
            type=func-type
            =module
            =store
        ==
    =/  try  (build-try ,~)
    ^-  (result ~)
    =/  locals  locals.code
    =/  stack=(list valtype)  (flop params.type)
    =/  frames=(list (list valtype))  ~[results.type]
    =/  res
      %:  validate-expr
        expression.code
        module
        store
        locals
        stack
        frames
      ==
    ?-  -.res
      %&  res
      %|  |+(crip (weld "func {<idx>}: " (trip p.res)))
    ==
  ::
  ++  validate-expr
    |=  $:  expr=expression
            =module
            =store
            $=  args
            $:  locals=(list valtype)
                stack=(list valtype)
                frames=(list (list valtype))
        ==  ==
    =/  try  (build-try ,~)
    ^-  (result ~)
    ?~  expr
      ?.  =(-.frames.args (flop stack.args))  |+'type error in result'
      &+~
    =/  instr  i.expr
    ::  stack-polymorphic instructions (unconditional control transfer)
    ::
    ?:  ?=(%unreachable -.instr)  &+~
    ?:  ?=(%br -.instr)
      ;<  results=(list valtype)  try  ((snug 'br frames') label.instr frames.args)
      ?.  =(results (flop (scag (lent results) stack.args)))  |+'br type error'
      &+~
    ?:  ?=(%br-table -.instr)
      =/  labels=(list @)  [label-default label-vec]:instr
      |-  ^-  (result ~)
      ?~  labels  &+~
      ;<  results=(list valtype)  try  ((snug 'br-table frames') i.labels frames.args)
      ?.  =(results (flop (scag (lent results) stack.args)))  |+'br-table type error'
      $(labels t.labels)
    ?:  ?=(%return -.instr)
      ?:  =(~ frames.args)  |+'no frames'
      =/  results=(list valtype)  (rear frames.args)
      ?.  =(results (flop (scag (lent results) stack.args)))  |+'return type error'
      &+~
    ;<  [stack1=_stack.args frames1=_frames.args]  try
      (validate-instr instr module store args)
    $(expr t.expr, stack.args stack1, frames.args frames1)
  ::
  ++  validate-instr
    |=  $:  $=  instr
            $~  [%nop ~]
            $<(?(%unreachable %br %br-table %return) instruction)
        ::
            =module
            =store
            locals=(list valtype)
            stack=(pole valtype)
            frames=(list (list valtype))      
        ==
    =/  try  (build-try _[stack frames])
    ^-  (result _[stack frames])
    ::  value-polymorphic instructions
    ::
    ?:  ?=(%drop -.instr)
      ?~  stack  |+'drop empty'
      &+[+.stack frames]
    ?:  ?=(%select -.instr)
      ?~  +.instr
        ?.  &(?=([%i32 t1=* t2=* *] stack) =(t1.stack t2.stack))
          |+'select type error'
        &+[+>.stack frames]
      ?.  ?&  ?=([%i32 t1=* t2=* *] stack)
              =(t1.stack t2.stack)
              =(t1.stack u.instr)
          ==
        |+'select type error'
      &+[+>.stack frames]
    ::  block instructions
    ::
    ?:  ?=(%block -.instr)
      ;<  type=func-type  try
        ?@  type.instr  ((snug 'type idx in block') type.instr funcs.store)
        &+type.instr
      =/  n-params=@  (lent params.type)
      ?.  =(params.type (flop (scag n-params stack)))
        |+'block params mismatch'
      ;<  ~  try
        %:  validate-expr
          body.instr
          module
          store
          locals
          (flop params.type)
          [results.type frames]
        ==
      &+[(weld (flop results.type) (slag n-params stack)) frames]
    ?:  ?=(%loop -.instr)
      ;<  type=func-type  try
        ?@  type.instr  ((snug 'type idx in loop') type.instr funcs.store)
        &+type.instr
      =/  n-params=@  (lent params.type)
      ?.  =(params.type (flop (scag (lent params.type) stack)))
        |+'block params mismatch'
      ;<  ~  try
        %:  validate-expr
          body.instr
          module
          store
          locals
          (flop params.type)
          [params.type frames]
        ==
      &+[(weld (flop results.type) (slag n-params stack)) frames]
    ?:  ?=(%if -.instr)
      ;<  type=func-type  try
        ?@  type.instr  ((snug 'type idx in loop') type.instr funcs.store)
        &+type.instr
      =/  n-params=@  (lent params.type)
      ?.  =(%i32 -.stack)  |+'if no flag'
      =.  stack  +.stack
      ?.  =(params.type (flop (scag n-params stack)))
        |+'if params mismatch'
      ;<  ~  try
        %:  validate-expr
          branch-true.instr
          module
          store
          locals
          (flop params.type)
          [params.type frames]
        ==
      ;<  ~  try
        %:  validate-expr
          branch-false.instr
          module
          store
          locals
          (flop params.type)
          [params.type frames]
        ==
      &+[(weld (flop results.type) (slag n-params stack)) frames]
    ::  some instructions that are handled separately from the rest
    ::  for no good reason (except for %ref-is-null, it's kinda
    ::  polymorphic)
    ::
    ?:  ?=(%br-if -.instr)
      ;<  results=(list valtype)  try
        ((snug 'br-if frames') label.instr frames)
      ?:  =(~ stack)  |+'br-if no cond'
      ?.  =(%i32 -.stack)  |+'br-if cond type mismatch'
      =.  stack  +.stack
      ?.  =(results (flop stack))  |+'br-if type error'
      &+[stack frames]
    ?:  ?=(%call -.instr)
      ;<  type=func-type  try
        ((snug 'call idx') func-id.instr funcs.store)
      =/  n-params=@  (lent params.type)
      ?.  =(params.type (flop (scag n-params stack)))
        |+'call params mismatch'
      &+[(weld (flop results.type) (slag n-params stack)) frames]
    ?:  ?=(%call-indirect -.instr)
      ;<  type=func-type  try
        ((snug 'call-indirect idx') type-id.instr type-section.module)
      =/  n-params=@  (lent params.type)
      ?.  =(params.type (flop (scag n-params stack)))
        |+'call-indirect params mismatch'
      &+[(weld (flop results.type) (slag n-params stack)) frames]
    ?:  ?=(%ref-is-null -.instr)
      ?:  =(~ stack)  |+'ref-is-null empty stack'
      ?.  ?=(ref-type -.stack)  |+'ref-is-null type mismatch'
      &+[[%i32 +.stack] frames]
    ::  the rest
    ::
    ;<  type=func-type  try  (get-type instr module store locals)
    =/  n-params=@  (lent params.type)
    ?.   =(params.type (flop (scag n-params stack)))
      |+(crip "type mismatch {<instr>}")
    &+[(weld (flop results.type) (slag n-params stack)) frames]
  ::
  ++  get-type
    |=  $:  $=  instr
            $~  [%nop ~]
            $<  $?  %unreachable
                    %br
                    %br-table
                    %return
                    %drop
                    %select
                    %block
                    %loop
                    %if
                    %br-if
                    %call
                    %call-indirect
                    %ref-is-null
                ==
            instruction
        ::
            =module
            =store
            locals=(list valtype)
        ==
    =/  try  (build-try func-type)
    ^-  (result func-type)
    ?-    -.instr
        %vec     (get-type-vec +.instr module store)
        %dbug    ~|(%dbug !!)
        %const   &+[~ ~[(from-coin p.instr)]]
        %eqz     &+[~[type.instr] ~[%i32]]
        %clz     &+[~[type] ~[type]]:instr
        %ctz     &+[~[type] ~[type]]:instr
        %popcnt  &+?>(?=(valtype type.instr) [~[type] ~[type]]:instr)
        %abs     &+?>(?=(valtype type.instr) [~[type] ~[type]]:instr)
        %neg     &+?>(?=(valtype type.instr) [~[type] ~[type]]:instr)
        %ceil    &+[~[type] ~[type]]:instr
        %floor   &+[~[type] ~[type]]:instr
        %trunc
      ?~  source-type.instr
        &+[~[type] ~[type]]:instr
      &+[~[u.source-type] ~[type]]:instr
    ::
        %nearest      &+[~[type] ~[type]]:instr
        %sqrt         &+[~[type] ~[type]]:instr
        %wrap         &+[~[%i64] ~[%i32]]
        %extend       &+[~[%i32] ~[%i64]]
        %convert      &+[~[source-type] ~[type]]:instr
        %demote       &+[~[%f64] ~[%f32]]
        %promote      &+[~[%f32] ~[%f64]]
        %reinterpret  &+[~[source-type] ~[type]]:instr
    ::
        %eq    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %ne    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %lt    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %gt    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %le    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %ge    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %add   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %sub   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %mul   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %div   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %rem   &+[~[type type] ~[type]]:instr
        %and   &+[~[type type] ~[type]]:instr
        %or    &+[~[type type] ~[type]]:instr
        %xor   &+[~[type type] ~[type]]:instr
        %shl   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %shr   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %rotl  &+[~[type type] ~[type]]:instr
        %rotr  &+[~[type type] ~[type]]:instr
        %min   &+[~[type type] ~[type]]:instr
        %max   &+[~[type type] ~[type]]:instr
        %copysign  &+[~[type type] ~[type]]:instr
        %nop  &+[~ ~]
    ::
        %ref-null  &+[~ ~[t.instr]]
        %ref-func  &+[~ ~[%func]]
    ::
        %local-get
      ;<  type=valtype  try  ((snug 'local get') index.instr locals)
      &+[~ ~[type]]
    ::
        %local-set
      ;<  type=valtype  try  ((snug 'local set') index.instr locals)
      &+[~[type] ~]
    ::
        %local-tee
      ;<  type=valtype  try  ((snug 'local tee') index.instr locals)
      &+[~[type] ~[type]]
    ::
        %global-get
      ;<  type=glob-type  try  ((snug 'global get') index.instr globs.store)
      &+[~ ~[v.type]]
    ::
        %global-set
      ;<  type=glob-type  try  ((snug 'global set') index.instr globs.store)
      ?:  ?=(%con m.type)  |+'constant global set'
      &+[~[v.type] ~]
    ::
        %table-get
      ;<  =table  try  ((snug 'table get') tab-id.instr tables.store)
      &+[~[%i32] ~[p.table]]
    ::
        %table-set 
      ;<  =table  try  ((snug 'table get') tab-id.instr tables.store)
      &+[~[%i32 p.table] ~]
    ::
        %table-init
      ;<  =table  try  ((snug 'table init table') tab-id.instr tables.store)
      ;<  =elem  try
        ((snug 'table init elem') elem-id.instr elem-section.module)
      ?.  =(t.elem p.table)  |+'table init type mismatch'
      &+[~[%i32 %i32 %i32] ~]
    ::
        %elem-drop 
      ;<  *  try
        ((snug 'elem drop') elem-id.instr elem-section.module)
      &+[~ ~]
    ::
        %table-copy
      ;<  tab-x=table  try
        ((snug 'table copy') tab-id-x.instr tables.store)
      ;<  tab-y=table  try
        ((snug 'table copy') tab-id-y.instr tables.store)
      ?.  =(p.tab-x p.tab-y)  |+'table copy type mismatch'
      &+[~[%i32 %i32 %i32] ~]
    ::
        %table-grow
      ;<  =table  try  ((snug 'table grow') tab-id.instr tables.store)
      &+[~[p.table %i32] ~[%i32]]
    ::
        %table-size
      ;<  =table  try  ((snug 'table grow') tab-id.instr tables.store)
      &+[~ ~[%i32]]
    ::
        %table-fill
      ;<  =table  try  ((snug 'table grow') tab-id.instr tables.store)
      &+[~[%i32 p.table %i32] ~]
    ::
        %load
      !!
    ::
        %store
      !!
    ::
        %memory-size
      !!
    ::
        %memory-grow
      !!
    ::
        %memory-init
      !!
    ::
        %data-drop
      !!
    ::
        %memory-copy
      !!
    ::
        %memory-fill
      !!
    ::
    ==
  ::
  ++  get-type-vec
    |=  [instr=instr-vec =module =store]
    ^-  (result func-type)
    !!
  ::
  ++  from-coin
    |=  coin=coin-wasm
    ^-  valtype
    !!
  --  ::  |validator
--