/+  gen=lia-linearizer
:-  %say  |=  *  :-  %noun
::
%-  main:gen
:+  ~  [~ ~]
:~
  `[%let %flag %a ~]
  [~[%a] [%lit %i32 33]]
  [~[%flag] [%lit %i32 5]]
  :-  ~
  :+  %while  %flag
  :-  [~ ~]
  :~
    [~[%flag] [%sub %i32 %flag [%lit %i32 1]]]
    `[%let %flag ~]
    [~[%flag] [%lit %i32 42]]
    :-  ~
    :^  %if  %a
      :-  [~ ~]
      ~[`[%break ~]]
    [[~ ~] ~]
  ==
::
  `[%yield %a ~]
==
