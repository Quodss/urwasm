/+  gen=lia-linearizer
:-  %say  |=  *  :-  %noun
::
%-  script-gen:gen
=;  code
  :*
    ~[%a %b %c]
    [~[%i32 %i32 %i32] ~[%i32]]
    code
    ~[%d]
  ==
:~
  `phrase:tree:gen`[%let %d %i32]
  `phrase:tree:gen`[%run-ext %test ~[[%two [%add %i32] [%name %a %i32] [%name %b %i32]] [[%name %c %i32]]] ~[[%d %i32]]]
==
