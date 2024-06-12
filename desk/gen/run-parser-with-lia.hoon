/+  wat=tools-wat-parser-lia
:-  %say  |=  *  :-  %noun
::
=;  script
  .=
    ~&  %jetted
    ~>  %bout
    ((wat %$) script)
  ~&  %unjetted
  ~>  %bout
  ((wat %none) script)
"""
(module
(func $fac (export "fac") (param f64) (result f64)
  local.get 0
  f64.const 1
  f64.lt
  if (result f64)
    f64.const 1
  else
    local.get 0
    local.get 0
    f64.const 1
    f64.sub
    call $fac
    f64.mul
  end))
"""