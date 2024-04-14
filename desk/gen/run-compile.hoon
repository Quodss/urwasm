/+  *lia-compiler
/+  p=parser-lib
/+  e=encoder
/*  fac-loop   %wasm  /tests/fac-br/wasm
:-  %say  |=  *  :-  %noun
=/  parsed
  ~>  %bout
  (main:p fac-loop)
=/  compiled
  ~>  %bout
  %:  main
    parsed
  ::
    :_  ~
    :-  [~ ~[%i32]]
    :~
      [%const %i32 5]
      [%run 'factorial']
    ==
  ::
    ~
    ~
  ==
=/  enc
  ~>  %bout
  (main:e compiled)
enc
