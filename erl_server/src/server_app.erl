-module(server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([doInitOrRestart/0]).

-include("gameRecord.hrl").
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_IP, "::").
-define(DEFAULT_MAX_CONN, 1000).
-define(BETRAYAL_SERVER_NAME, betrayal_character_server).

getConfig() ->
  IP=case application:get_env(ip) of
    undefined -> ?DEFAULT_IP;
    {ok, I} -> I
  end,
  Port=case application:get_env(port) of
    undefined -> ?DEFAULT_PORT;
    {ok, P} -> P
  end,
  MaxConns=case application:get_env(maxconns) of
    undefined -> ?DEFAULT_MAX_CONN;
    {ok, M} when is_integer(M) -> M
  end,
  {IP, Port, MaxConns}.
  

initMnesia(Nodes) ->
  Tables=[games, players],
  ok=mnesia:start(),
  case mnesia:wait_for_tables(Tables, 100) of
    {timeout, _} ->
      %Our tables are probably not created, so....
      {atomic,ok}=mnesia:create_table(games, [{attributes, record_info(fields, gamerec)},
                                              {ram_copies, Nodes}
                                             ]
                                     ),
      {atomic,ok}=mnesia:create_table(players, [{attributes, record_info(fields, playerrec)},
                                                {ram_copies, Nodes}
                                               ]
                                     ),
      ok=mnesia:wait_for_tables(Tables, 100);
    ok ->
      ok
  end.

doInitOrRestart() ->
  {IP, Port, MaxConns}=getConfig(),
  %Inject the color and variant atoms into the process, so we can use
  %binary_to_existing_atom later.
  VALID_COLORS=char_handler:getValidColors(),
  VALID_VARIANTS=char_handler:getValidVariants(),
  lists:foreach(fun(F) -> binary_to_atom(F, latin1) end, lists:append(VALID_COLORS, VALID_VARIANTS)),
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/api/kickplayer", char_handler, []},
                                           {"/api/getplayers", char_handler, []},
                                           {"/api/getplayer", char_handler, []},
                                           {"/api/creategame", char_handler, []},
                                           {"/api/addplayer", char_handler, []},
                                           {"/api/setcolor", char_handler, []},
                                           {"/api/setstats", char_handler, []},
                                           {"/", cowboy_static, {file, "static/index.html"}},
                                           {"/components/[...]", cowboy_static, {dir, "static/bower_components/"}},
                                           {"/[...]", cowboy_static, {dir, "static/"}}
                                          ]}
                                   ]),
  {ok, BindIP}=inet:parse_address(IP),
  initMnesia([node()]),
  %Just in case we are restarting, stop any existing listener.
  cowboy:stop_listener(?BETRAYAL_SERVER_NAME),
  {ok, _} = cowboy:start_http(?BETRAYAL_SERVER_NAME, MaxConns, [{port, Port}, {ip, BindIP}],
                    [{env, [
                            {dispatch, Dispatch}]}]
                   ).

start(_Type, _Args) ->
  doInitOrRestart(),
  server_sup:start_link().

stop(_State) ->
  cowboy:stop_listener(?BETRAYAL_SERVER_NAME),
  ok.
