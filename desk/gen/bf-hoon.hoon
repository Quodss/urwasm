:-  %say  |=  *  :-  %noun
::
=<  ~>  %bout
    example
|%
+$  brain
  $~  [~ ~]
  $@  ?(%'+' %'-' %'<' %'>' %',' %'.')
  [~ p=(list brain)]
::
++  other
  ;~(less (mask "+-<>,.[]") next)
::
++  prep
  %+  knee  *(list brain)
  |.  ~+
  %+  ifix  [(star other) (star other)]
  %+  more  (star other)
  ;~  pose
    (cold %'+' lus)
    (cold %'-' hep)
    (cold %'<' gal)
    (cold %'>' gar)
    (cold %',' com)
    (cold %'.' dot)
    (stag ~ (ifix [sel ser] prep))
  ==
::
+$  state
  $:
    in=tape
    out=tape
    buff=[l=tape curr=@ r=tape]
  ==
::
++  eval
  |=  [prog=(list brain) in=tape]
  ^-  tape
  =-  (flop out)
  =|  =state
  =.  in.state  in
  =>  [prog=prog state=state ..$]
  |-  ^-  ^state
  =*  outer  $
  ?~  prog  state
  ?^  i.prog
    ?:  =(0 curr.buff.state)
      outer(prog t.prog)
    |-  ^-  ^state
    =*  inner  $
    =.  state  outer(prog p.i.prog)
    ?:  =(0 curr.buff.state)
      outer(prog t.prog)
    inner
  =.  state
    ?-    i.prog
        %'+'  state(curr.buff (~(sum fo 256) curr.buff.state 1))
        %'-'  state(curr.buff (~(dif fo 256) curr.buff.state 1))
    ::
        %'<'
      %_    state
          buff
        ?~  l.buff.state  !!
        [t.l i.l [curr r]]:buff.state
      ==
    ::
        %'>'
      %_    state
          buff
        ?~  r.buff.state
          [[curr l] 0 ~]:buff.state
        [[curr l] i.r t.r]:buff.state
      ==
    ::
        %','
      ?~  in.state  !!
      state(in t.in.state, curr.buff i.in.state)
    ::
        %'.'
      state(out [curr.buff.state out.state])
    ==
  outer(prog t.prog)
::
++  example
  %-  eval  :_  "245675313245679742571+245675313245679742571="
  %-  scan  :_  prep
  """
  >> +
    [- >,>+< 
    ----- ----- ----- -----    ; checking with ascii 43 ie plus symbol
    ----- ----- ----- -----
    ---
    [
    +++++ +++++ +++++ +++++
    +++++ +++++ +++++ +++++
    +++
    < ] >>
    ]
    ; first input is over and terminated by a 'plus' symbol
    <->>>>>+
    [- >,>+<
    ----- ----- ----- -----   ; checking with ascii 61 ie = symbol
    ----- ----- ----- -----
    ----- ----- ----- ------
    [
    +++++ +++++ +++++ +++++
    +++++ +++++ +++++ +++++
    +++++ +++++ +++++ ++++++
    < ] >>
    ]
        ; second input is over and terminated by an = symbol
        ; now the array looks like 0 0 0 49 0 50 0 0 0 0 0 0 0 0 49 0 53 0 0 1 0
        ; for an input 12'plus'15=
    <<<<
    [<+<]
                ; filled with 1's in between
    + [<+>-<<[>-]>] ; This is a special loop to traverse LEFT through indefinite no of 0s
                ; Lets call it left traverse
    <<
    [<+<]
    >[>]<
               ; now the array looks like
               ; 0 0 1 49 1 50 0 0 0 0 0 0 0 1 49 1 53 0 0 1 for eg:12plus15
    [
    [->+>   + [>+<->>[<-]<]  ; Right traverse
        >>[>]<+ [<]
        + [<+>-<<[>-]>]  ; Left traverse
        <<-<
    ] 
    + [>+<->>[<-]<] 
    >> [>] <<-<[<]
    + [<+>-<<[>-]>]
    <<-<
    ]
             ; now actual addition took place
             ; ie array is 00000000000000 98 0 103 0 0 1
    + [>+<->>[<-]<]
    >>
    [ 
    ----- ----- ----- -----
    ----- ----- ----- -----
    ----- ---
    >>]
                ; minus 48 to get the addition correct as we add 2 ascii numbers
    >-<         ; well an undesired 1 was there 2 place after 103 right ? just to kill it
            ; now the array is 00000 00000 0000 50 0 55
            ; now comes the biggest task Carry shifting
    <<
    [<<]
    +++++ +++++ +++++ +++++
    +++++ +++++ +++++ +++++
    +++++ +++
    [>>]
        ; we added a 48 before all the digits in case there is an overall carry
        ; to make the size n plus 1
        ; array : 00000 00000 00 48 0 50 0 55
    <<
    <<
    [
    [>>->[>]>+>>>> >>>+<<<< <<<<<[<]><<]
    >+[>]>-
    [-<<[<]>+[>]>]
    >>>>>+>>>
    +++++ +++++ +++++ +++++ +++++
    +++++ +++++ +++++ +++++ +++++
    +++++ +++
    <
                ; comparison loop:  0   1   0   a      b  0
                ;                  (q) (p)    (num)  (58)
    [->-[>]<<]  ; comparison loop to check each digit with 58: greater means 
                ; we need to minus 10 and add 1 to next significant digit
    <[-
            ; n greater than or equal to 58 (at p)
            <<<< <<<
            [<]+
            >
            ----- ----- ; minus 10 to that digit
            <<+         ; plus 1 to next digit
            >
            [>]
            >>>>>>
    ]
    < [-<
            ; n less than 58 (at q)
            <<<<<<
            [<]+
            [>]
            >>>>>
      ]
        ; at (q)
        >>>[-]>[-]
        <<<<< <<<<<
        [<]>
        <<
    ]
        ; Its all over now : something like 0 48 0 52 0 66 ( ie 0 4 18 )
        ; will turn into 0 48 0 53 0 56 (ie 0 5 8)
    >>
    ----- ----- ----- -----
    ----- ----- ----- -----
    ----- ---
            ; here we are just checking first digit is 48 or not
            ; its weird to print 0 ahead but it is defenitely needed
            ; if it is 49 ie 1
    [
    +++++ +++++ +++++ +++++
    +++++ +++++ +++++ +++++
    +++++ +++
    .
    [-]
    ]
    >>
    [.>>]
    +++++ +++++
    .           ; to print nextline : ascii 10
  """
::
--