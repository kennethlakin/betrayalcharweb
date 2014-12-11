-module(server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("gameRecord.hrl").
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_IP, "::").
-define(DEFAULT_MAX_CONN, 1000).

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
  ok=mnesia:start(),
  {atomic,ok}=mnesia:create_table(games, [{attributes, record_info(fields, gamerec)},
                              {ram_copies, Nodes}
                             ]
                    ),
  {atomic,ok}=mnesia:create_table(players, [{attributes, record_info(fields, playerrec)},
                              {ram_copies, Nodes}
                             ]
                    ),
  ok=mnesia:wait_for_tables([games, players], 500).

start(_Type, _Args) ->
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
                                           {"/api/setstats", char_handler, []}
                                          ]}
                                   ]),
  initMnesia([node()]),
  {ok, BindIP}=inet:parse_address(IP),
  {ok, _} = cowboy:start_http(betrayal_character_server, MaxConns, [{port, Port}, {ip, BindIP}],
                    [{env, [
                            {dispatch, Dispatch}]}]
                   ),
	server_sup:start_link().

stop(_State) ->
	ok.
