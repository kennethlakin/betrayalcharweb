-module(char_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-define(DEPS, [crypto, cowlib, ranch, cowboy, mnesia, server, inets]).
-define(HOST, element(2, application:get_env(server, ip))).
-define(PORT, integer_to_list(element(2, application:get_env(server, port)))).
-define(URL, "http://[" ++ ?HOST ++ "]:" ++ ?PORT).

test_runner_test_() ->
  {setup,
   fun setup/0,
   fun cleanup/1,
   {inparallel,
   [
    fun canCreateGame/0,
    fun newGameIsEmpty/0,
    fun canAddPlayer/0,
    fun canAddPlayers/0,
    fun addedPlayerIsInGame/0,
    fun canKickPlayer/0,
    fun playerIsKicked/0,
    fun canSetColor/0,
    fun colorIsSet/0,
    fun canSetStats/0,
    fun statsAreSet/0
    ]}
  }.

canCreateGame() ->
  true=maps:is_key(<<"gameid">>, callCreateGame()).

newGameIsEmpty() ->
  GameID=createGame(),
  PlayerList=getPlayers(GameID),
  ?assertEqual(0, length(PlayerList)).

canAddPlayer() ->
  GameID=createGame(),
  Resp=callAddPlayer(GameID, <<"Player%20Name">>),
  ?assertEqual(GameID, maps:get(<<"gameid">>, Resp)).

canAddPlayers() ->
  GameID=createGame(),
  lists:foreach(fun (Name) ->
                    Resp=callAddPlayer(GameID, Name),
                    ?assertEqual(GameID, maps:get(<<"gameid">>, Resp))
                end, [<<"one">>, <<"name%20two">>]).

addedPlayerIsInGame() ->
  {GameID, PlayerID} = createPlayer(<<"Player%20Name">>),
  Player=getPlayer(GameID, PlayerID),
  ?assertEqual(PlayerID, maps:get(<<"playerid">>, Player)).

canKickPlayer() ->
  {GameID, PlayerID} = createPlayer(<<"Player%20Name">>),
  Ret=callKickPlayer(GameID, PlayerID),
  ?assertEqual(<<"ok">>, Ret).

playerIsKicked() ->
  {GameID, P1} = createPlayer(<<"One">>),
  P2=addPlayer(GameID, <<"Two">>),
  BeforeKickPlayers=getPlayers(GameID),
  callKickPlayer(GameID, P1),
  AfterKickPlayers=getPlayers(GameID),
  MissingPlayer=callGetPlayer(GameID, P1),
  ?assert(length(BeforeKickPlayers)-1 == length(AfterKickPlayers)),
  ?assertEqual(P2, maps:get(<<"playerid">>, getPlayer(GameID, P2))),
  ?assert(maps:is_key(<<"error">>, MissingPlayer)),
  ?assertEqual(<<"player_not_found">>, maps:get(<<"error">>, MissingPlayer)).

canSetColor() ->
  {GameID, PlayerID} = createPlayer(<<"Player%20Name">>),
  lists:foreach(fun(Color) -> 
                    lists:foreach(fun(Variant) ->
                                      Resp=callSetColor(GameID, PlayerID, Color, Variant),
                                      ?assertEqual(<<"ok">>, Resp)
                                   end,
                                   char_handler:getValidVariants())
                end,
                char_handler:getValidColors()).

colorIsSet() ->
  {GameID, PlayerID} = createPlayer(<<"Player%20Name">>),
  lists:foreach(fun(Color) -> 
                    lists:foreach(fun(Variant) ->
                                      callSetColor(GameID, PlayerID, Color, Variant),
                                      Player=getPlayer(GameID, PlayerID),
                                      ?assertEqual(Color, maps:get(<<"color">>, Player)),
                                      ?assertEqual(Variant, maps:get(<<"variant">>, Player))
                                   end,
                                   char_handler:getValidVariants())
                end,
                char_handler:getValidColors()).

canSetStats() ->
  {GameID, PlayerID} = createPlayer(<<"Player">>),
  Speed= <<"1">>,
  Might= <<"2">>,
  Sanity= <<"3">>,
  Knowledge= <<"4">>,
  Resp=callSetStats(GameID, PlayerID, Speed, Might, Sanity, Knowledge),
  ?assertEqual(<<"ok">>, Resp).

statsAreSet() ->
  {GameID, PlayerID} = createPlayer(<<"Player">>),
  Speed=random:uniform(100),
  Might=random:uniform(100),
  Sanity=random:uniform(100),
  Knowledge=random:uniform(100),
  setStats(GameID, PlayerID, Speed, Might, Sanity, Knowledge),
  Player=getPlayer(GameID, PlayerID),
  ?assertEqual(Speed, maps:get(<<"speed">>, Player)),
  ?assertEqual(Might, maps:get(<<"might">>, Player)),
  ?assertEqual(Sanity, maps:get(<<"sanity">>, Player)),
  ?assertEqual(Knowledge, maps:get(<<"knowledge">>, Player)).

getPlayers(GameID) ->
  maps:get(<<"players">>, callGetPlayers(GameID)).

getPlayer(GameID, PlayerID) ->
  maps:get(<<"player">>, callGetPlayer(GameID, PlayerID)).

createPlayer(PlayerName) ->
  GameID=createGame(),
  PlayerID=maps:get(<<"playerid">>, callAddPlayer(GameID, PlayerName)),
  {GameID, PlayerID}.

addPlayer(GameID, PlayerName) ->
  maps:get(<<"playerid">>, callAddPlayer(GameID, PlayerName)).

createGame() ->
  maps:get(<<"gameid">>, callCreateGame()).

setStats(GameID, PlayerID, Speed, Might, Sanity, Knowledge) ->
  callSetStats(GameID, PlayerID, integer_to_binary(Speed),
               integer_to_binary(Might), integer_to_binary(Sanity),
               integer_to_binary(Knowledge)).

callCreateGame() ->
  callCreateGame(post).

callCreateGame(M) ->
  getMaps(M, 200, ?URL ++ "/api/creategame").

callGetPlayers(GameID) ->
  callGetPlayers(get, GameID).

callGetPlayers(M, GameID) ->
  getMaps(M, 200, ?URL ++ "/api/getplayers?gameid=" ++ binary_to_list(GameID)).

callGetPlayer(GameID, PlayerID) ->
  callGetPlayer(get, GameID, PlayerID).

callGetPlayer(M, GameID, PlayerID) ->
  getMaps(M, 200, ?URL ++ "/api/getplayer?gameid=" ++ binary_to_list(GameID)
         ++ "&playerid=" ++ binary_to_list(PlayerID)).

callAddPlayer(GameID, PlayerName) ->
  callAddPlayer(post, GameID, PlayerName).

callAddPlayer(M, GameID, PlayerName) ->
  getMaps(M, 200, ?URL ++ "/api/addplayer?gameid=" ++ binary_to_list(GameID)
         ++ "&playername=" ++ binary_to_list(PlayerName)).

callKickPlayer(GameID, PlayerID) ->
  callKickPlayer(delete, GameID, PlayerID).

callKickPlayer(M, GameID, PlayerID) ->
  getMaps(M, 200, ?URL ++ "/api/kickplayer?gameid=" ++ binary_to_list(GameID)
         ++ "&playerid=" ++ binary_to_list(PlayerID)).

callSetColor(GameID, PlayerID, Color, Variant) ->
  callSetColor(post, GameID, PlayerID, Color, Variant).

callSetColor(M, GameID, PlayerID, Color, Variant) ->
  getMaps(M, 200, ?URL ++ "/api/setcolor?gameid=" ++ binary_to_list(GameID)
         ++ "&playerid=" ++ binary_to_list(PlayerID) ++ "&color="
         ++ binary_to_list(Color) ++ "&variant=" ++ binary_to_list(Variant)).

callSetStats(GameID, PlayerID, Speed, Might, Sanity, Knowledge) ->
  callSetStats(post, GameID, PlayerID, Speed, Might, Sanity, Knowledge).

callSetStats(M, GameID, PlayerID, Speed, Might, Sanity, Knowledge) ->
  getMaps(M, 200, ?URL ++ "/api/setstats?gameid=" ++ binary_to_list(GameID)
         ++ "&playerid=" ++ binary_to_list(PlayerID) ++ "&speed="
         ++ binary_to_list(Speed) ++ "&might=" ++ binary_to_list(Might)
         ++ "&sanity=" ++ binary_to_list(Sanity) ++ "&knowledge=" ++ binary_to_list(Knowledge)).

getMaps(M, Code, URL) ->
  Body=verifyRequest(M, Code, URL),
  jiffy:decode(Body, [return_maps]).

verifyRequest(M, Code, URL) ->
  {ok, {{_Version, Code, _Reason}, _Headers, Body}} = makeRequest(M, URL),
  Body.

makeRequest(M, URL) when M == post ->
    httpc:request(M, {URL, [], [], []}, [], []);

makeRequest(M, URL) when M == get orelse M == delete ->
    httpc:request(M, {URL, []}, [], []).


getRandomPort() ->
  Port=random:uniform(65535),
  case Port > 1024 of
    true ->
      Port;
    false ->
      getRandomPort()
  end.

setup() ->
  {A, B, C} = now(),
  random:seed(A, B, C),
  Port=getRandomPort(),
  application:set_env(server, port, Port),
  application:set_env(server, ip, "127.0.0.1"),
  lists:foreach(fun(F) ->
                    ok=application:ensure_started(F)
                end, ?DEPS).

cleanup(_) ->
  %Hide support application stop notices.
  error_logger:tty(false),
  lists:foreach(fun(F) ->
                    ok=application:stop(F)
                end, ?DEPS),
  error_logger:tty(true).
