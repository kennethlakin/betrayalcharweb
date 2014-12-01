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
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/character", char_handler, []}
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
