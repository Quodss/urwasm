/+  gen=lia-linearizer
:-  %say  |=  *  :-  %noun
::
%-  gen
:+  [~ ~]
  :~
    [~ [%let %a ~]]
    [~[%a] [%lit %i32 1]]
    [~ [%let %b ~]]
    [~[%b] [%lit %i32 2]]
    [~ [%let %c ~]]
    [~[%c] [%sub %i32 %b %a]]
    [~ [%let %d ~]]
    [~[%d] [%lit %i32 1]]
    [~ [%let %e ~]]
    [~[%e] [%lit %i32 2]]
    [~ [%let %f ~]]
    [~[%f] [%sub %i32 %e %d]]
  ==
~[%f]