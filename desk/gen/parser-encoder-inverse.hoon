::  Test to check whether parser . encoder == id for u-n, s-n
::
/+  e=encoder
/+  p=parser-lib
::
:-  %say  |=  *  :-  %noun
::
=>  |%
    ++  toct
      |=  =octs
      ^-  tape
      =/  out=tape  (trip q.octs)
      %+  weld  out
      ^-  tape
      (reap (sub p.octs (lent out)) '\00')
    --
=/  w
  =/  a=@s  -2.147.483.648
  =/  c=@  0
  |-  ^-  (unit @s)
  ?:  =(2.000 c)  ~
  ?.  =(a (scan (toct (s-n:e a)) (s-n:r:p 32)))
    `a
  $(a (dec a), c +(c))
::
~&  %w-done
=/  x
  =/  a=@s  `@`0
  |-  ^-  (unit @s)
  ?:  =(2.000 a)  ~
  ?.  =(a (scan (toct (s-n:e a)) (s-n:r:p 32)))
    `a
  $(a +(a))
::
~&  %x-done
=/  y
  =/  a=@  0
  |-  ^-  (unit @)
  ?:  =(2.000 a)  ~
  ?.  =(a (scan (toct (u-n:e a)) (u-n:r:p 32)))
    ~&  [%y-fail a]
    `a
  $(a +(a))
::
~&  %y-done
=/  z
  =/  a=@  (dec (bex 32))
  =/  c=@  0
  |-  ^-  (unit @)
  ?:  =(2.000 c)  ~
  ?.  =(a (scan (toct (u-n:e a)) (u-n:r:p 32)))
    ~&  [%z-fail a]
    `a
  $(a (dec a), c +(c))
::
~&  %z-done
[w x y z]  ::  [~ ~ ~ ~]
