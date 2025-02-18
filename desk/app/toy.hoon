/-  plugin
/+  default-agent, server, schooner
::
=>
  |%
  +$  versioned-state
    $%  state-0
    ==
  ::
  +$  state-now  state-0
  +$  state-0
    $:  %0
        dir-texts=path    ::  path to texts without beam
        dir-plugins=path  ::  path to plugins without beam
        plugins=(map knot plugin)
    ==
  ::
  +$  card  card:agent:gall
  +$  dojo-action
    $%  [%load-plugins ~]
    ==
  --
::
=<
  =+  *state-now
  =*  state  -
  |_  =bowl:gall
  +*  this  .
      def  ~(. (default-agent this %.n) bowl)
      hc   ~(. helping-core bowl)
  ::
  ++  on-init
    =/  our-k  (scot %p our.bowl)
    =/  now-k  (scot %da now.bowl)
    =.  dir-plugins  /toy-files/plugins
    =.  dir-texts  /toy-files/texts
    :_  this
    :_  ~
    ^-  card
    [%pass /eyre/connect %arvo %e %connect `/apps/toy %toy]
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    =/  sat  !<(versioned-state old)
    ?-  -.sat
      %0  `this(state sat)
    ==
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+    mark  (on-poke:def mark vase)
        %noun
      ?>  =(src.bowl our.bowl)
      =+  !<(=dojo-action vase)
      ?-    -.dojo-action
          %load-plugins
        =^  cards=(list card)  state  (load-plugins:hc state)
        [cards this]
      ==
    ::
        %handle-http-request
      ?>  =(src.bowl our.bowl)
      =^  cards=(list card)  state
        (handle-http:hc !<([@ta =inbound-request:eyre] vase) state)
      [cards this]
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+    path  (on-watch:def path)
        [%http-response *]
      `this
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  ++  on-agent  on-agent:def
  --
::
|%
++  helping-core
  |_  =bowl:gall
  ++  handle-http
    |=  [[eyre-id=@ta =inbound-request:eyre] state=state-now]
    ^-  (quip card state-now)
    =/  ,request-line:server
      (parse-request-line:server url.request.inbound-request)
    =+  send=(cury response:schooner eyre-id)
    ?+      method.request.inbound-request
          [(send [405 ~ [%stock ~]]) state]
    ::
        ?(%'GET' %'POST')
      =>  .(site `(pole @t)`site)
      ?+      site
            :_  state
            (send [404 ~ [%plain "404 - Not Found"]])
      ::
          ?([%apps %toy ~] [%apps %toy %$ ~])
        ?.  authenticated.inbound-request
          :_  state
          %-  send
          [302 ~ [%login-redirect './apps/sign']]
        [(send 200 ~ manx+root-page) state]
      ::
          [%apps %toy rest=*]  ::  rest is guaranteed to be non-empty
        =/  mayb-plug-name=knot  -.rest.site
        ?^  plug=(~(get by plugins.state) mayb-plug-name)  ::  look for a plugin by the first knot
          ?~  current=(read-at-path state +.rest.site)
            [(send [404 ~ [%plain "404 - Not Found"]]) state]
          =^  xml=tape  u.plug
            %~  render  u.plug
            :*  u.current
                [now our eny]:bowl
                dir-texts.state
                inbound-request
            ==
          :-  (send 200 ~ plain+xml)
          state(plugins (~(put by plugins.state) mayb-plug-name u.plug))
        ::  no plugin found with that name
        ::
        ?~  current=(read-at-path state rest.site)
          [(send [404 ~ [%plain "404 - Not Found"]]) state]
        =/  xml=tape
          (render-default u.current [now our eny]:bowl dir-texts.state)
        [(send 200 ~ plain+xml) state]
      ==
    ==
  ::
  ++  load-plugins
    |=  state=state-now
    ^-  (quip card state-now)
    =/  our-k  (scot %p our.bowl)
    =/  now-k  (scot %da now.bowl)
    =+  .^  list-plugins=(list path)
          %ct
          our-k
          %wasm
          now-k
          dir-plugins.state
        ==
    ::
    |-  ^-  (quip card state-now)
    ?~  list-plugins  `state
    ?.  =(%hoon (rear i.list-plugins))
      $(list-plugins t.list-plugins)
    =/  plugin-name=knot
      (snag (sub (lent i.list-plugins) 2) i.list-plugins)
    =+  !<  =plugin
        .^  vase
          %ca
          our-k
          %wasm
          now-k
          i.list-plugins
        ==
    =.  plugins.state
      (~(put by plugins.state) plugin-name plugin)
    $(list-plugins t.list-plugins)
  ::
  ++  render-default
    |=  [file=@t [now=@da our=@p eny=@uvJ] dir=path]
    ^-  tape
    """
    <html><body>
    {(trip file)}
    </body></html>
    """
  ::
  ++  read-at-path
    |=  [state=state-now pax=path]
    ^-  (unit cord)
    ?.  =(%txt (rear pax))  ~
    =/  our-k  (scot %p our.bowl)
    =/  now-k  (scot %da now.bowl)
    =/  pax-full=path
      :*
        our-k
        %wasm
        now-k
        `path`(weld dir-texts.state pax)
      ==
    =+  .^(=arch %cy pax-full)
    ?~  fil.arch  ~
    =+  .^(=wain %cx pax-full)
    `(of-wain:format wain)
  ::
  --
  ::
  ++  root-page
    ^-  manx
    ;html
      ;head
        ;meta(charset "UTF-8");
        ;title: Toy agent page
      ==
      ;body
        ;p: Yes hello.
        ;p: This is root page of the app.
        ;p: Add a path to a file to the end of the url to display it.
        ;p: Prepend the path with a plugin name to render it with a plugin.
      ==
    ==
  ::
--