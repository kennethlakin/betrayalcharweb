-module(server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("gameRecord.hrl").

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
  ok=mnesia:wait_for_tables([games], 500).

start(_Type, _Args) ->
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
  cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                    [{env, [
                            {dispatch, Dispatch}]}]
                   ),
	server_sup:start_link().

stop(_State) ->
	ok.
