/+  gen=lia-linearizer
:-  %say  |=  *  :-  %noun
::
%-  script-gen:gen
=;  code
  :*
    ~[%a %b %c]
    [~[%i32 %i32 %i32] ~[%i32]]
    code
    ~[%f]
  ==
:~
  [%let %d %i32]
  [%op ~[[%d %i32]] [%two [%add %i32] [%name %a %i32] [%two [%add %i32] [%name %b %i32] [%name %c %i32]]]]
  [%let %f %i32]
  :+  %while  `op:tree:gen`[%two [%gt %i32 `%u] [%name %d %i32] [%zero %const %i32 0]]
  :~
    `phrase:tree:gen`[%op ~[[%f %i32]] [%two [%add %i32] [%name %f %i32] [%zero %const %i32 1]]]
    `phrase:tree:gen`[%op ~[[%d %i32]] [%two [%sub %i32] [%name %d %i32] [%zero %const %i32 1]]]
  ==
==
