-module(char_handler).
-behaviour(cowboy_http_handler).

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

-include("gameRecord.hrl").

-define(ACTIONS,
        #{
          <<"kickplayer">> => #{method => <<"DELETE">>, args => [gameid, playerid]},
          <<"getplayers">> => #{method => <<"GET">>, args => [gameid]},
          <<"getplayer">> => #{method => <<"GET">>, args => [gameid, playerid]},
          <<"creategame">> => #{method => <<"POST">>, args => []},
          <<"addplayer">> => #{method => <<"POST">>, args => [gameid, playername]},
          <<"setcolor">> => #{method => <<"POST">>, args => [gameid, playerid, color, variant]},
          <<"setstats">> => #{method => <<"POST">>, args => [gameid, playerid, speed, might, sanity, knowledge]}
         }).

newGameRec(GameID) ->
  #{ gameid => GameID, players => []}.

newPlayerRec(GameID, PlayerID, PlayerName) ->
  #{ gameid => GameID, playerid => PlayerID, name => PlayerName,
     color => null, variant => null,
     speed => null, might => null, sanity => null, knowledge => null }.

newQSRec() ->
  #{ action => undefined, gameid => undefined,
     playerid => undefined, playername => undefined,
     color => undefined, variant => undefined,
     speed => undefined, might => undefined,
     sanity => undefined, knowledge => undefined }.

getValidActions() ->
  maps:keys(?ACTIONS).

isValidAction(Action) ->
  maps:is_key(Action, ?ACTIONS).

getActionArgs(Action) ->
  {ok, Val} = maps:find(Action, ?ACTIONS),
  maps:get(args, Val).

getQueryArgs(R) ->
  BaseQueryRec=newQSRec(),
  {QueryRec, NewReq}=lists:foldl(fun(Key, {QueryRecs, Req}) ->
                    {Val, NextReq} = cowboy_req:qs_val(atom_to_binary(Key, latin1), Req),
                    NextQueryRecs=maps:update(Key, Val, QueryRecs),
                    {NextQueryRecs, NextReq}
              end,
              {BaseQueryRec, R},
              maps:keys(BaseQueryRec)),
  {QueryRec, NewReq}.

verifyQueryArgs(Action, QueryArgs) ->
  Args=getActionArgs(Action),
  MissingArgList=
    lists:foldl(fun (Arg, ArgAcc) ->
                    Rec=maps:get(Arg, QueryArgs),
                    case Rec==undefined of
                      true ->
                        lists:append(ArgAcc, [Arg]);
                      false ->
                        ArgAcc
                    end
                end,
                [],
     Args),
    case length(MissingArgList) of
      0 ->
        ok;
      _ ->
        {missing_args, MissingArgList}
    end.

seedRandom() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3).

%Generate upper-case alphanumeric Game IDs.
generateGameID() ->
  seedRandom(),
  list_to_binary(
    [
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64
    ]
   ).

%Generate upper-case alphanumeric Player IDs.
generatePlayerID() ->
  seedRandom(),
  list_to_binary(
    [
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64,
     random:uniform(26)+64
    ]
   ).

%Used when looking up a particular player.
concatIDs(GameID, PlayerID) ->
  <<GameID/binary, PlayerID/binary>>.

createGame() ->
  GameID=generateGameID(),
  case findGame(GameID) of
    {ok, _} ->
      createGame();
    not_found ->
      GameRec=newGameRec(GameID),
      {atomic, _} = mnesia:transaction(
                        fun() -> mnesia:write({games, GameID, GameRec}) end
                       ),
      GameID
  end.

findGame(GameID) ->
  {atomic, Val} = mnesia:transaction( fun() -> mnesia:read(games, GameID) end),
  case length(Val) >= 1 of
    true ->
      {ok, lists:nth(1, Val)};
    false ->
      not_found
  end.

findPlayer(GameID, PlayerID) ->
  {atomic, Val} = mnesia:transaction( fun() -> mnesia:read(players, concatIDs(GameID, PlayerID)) end),
  case length(Val) >= 1 of
    true ->
      {ok, lists:nth(1, Val)};
    false ->
      not_found
  end.

%Creates some eJSON that will be passed to jiffy to be encoded into JSON.
createPlayerObject(Player) ->
  Ret=lists:foldl(fun(Key, Acc) ->
                   lists:append([{Key, maps:get(Key, Player)}], Acc)
              end,
              [],
              maps:keys(Player)),
  {Ret}.

addPlayer(GameID, Args) ->
  PlayerID=generatePlayerID(),
  case findPlayer(GameID, PlayerID) of
    not_found ->
      {atomic, {ok, PlayerID}} = mnesia:transaction(
                        fun() ->
                            mnesia:lock({table, games}, write),
                            mnesia:lock({table, players}, write),
                            PlayerKey=concatIDs(GameID, PlayerID),
                            PlayerName=maps:get(playername, Args),
                            Player=newPlayerRec(GameID, PlayerID, PlayerName),

                            GameList=mnesia:read({games, GameID}),
                            Game=element(3, lists:nth(1, GameList)),
                            GamePlayers=maps:get(players, Game),
                            UpdatedGame=Game#{players := lists:append(GamePlayers, [Player])},
                            mnesia:write({games, GameID, UpdatedGame}),
                            mnesia:write({players, PlayerKey, Player}),
                            {ok, PlayerID}
                        end
                       ),
      {ok, PlayerID};
    %If that playerID is in this game, create a new ID and retry.
    {ok, _} ->
      addPlayer(GameID, Args)
  end.

handleRequest(Action, Args) when Action == <<"kickplayer">> ->
  PlayerID=maps:get(playerid, Args),
  GameID=maps:get(gameid, Args),
  Key=concatIDs(GameID, PlayerID),
  {atomic, Ret} = mnesia:transaction(
                    fun() ->
                        mnesia:lock({table, games}, write),
                        mnesia:lock({table, players}, write),
                        case findGame(GameID) of
                          {ok, GameRec} ->
                            case findPlayer(GameID, PlayerID) of
                              {ok, _} ->
                                %Remove player from game player array, and from player database.
                                Game=element(3, GameRec),
                                GamePlayerList=maps:get(players, Game),
                                UpdatedPlayerList=lists:filter(
                                                    fun(Player) ->
                                                        maps:get(playerid, Player) /= PlayerID end,
                                                    GamePlayerList),
                                UpdatedGame=Game#{players := UpdatedPlayerList},
                                mnesia:write({games, GameID, UpdatedGame}),
                                mnesia:delete({players, Key}),
                                jiffy:encode(<<"ok">>);
                              not_found ->
                                jiffy:encode({[{error, <<"player_not_found">>}]})
                            end;
                          not_found ->
                            jiffy:encode({[{error, <<"game_not_found">>}]})
                        end
                    end),
  Ret;

handleRequest(Action, Args) when Action == <<"getplayers">> ->
  GameID=maps:get(gameid, Args),
  case findGame(GameID) of
    {ok, GameDBRec} ->
      %Iterate over players in game.
      GameRec=element(3, GameDBRec),
      GamePlayers=maps:get(players, GameRec),
      PlayerArr=lists:foldl(fun (GamePlayer, A) ->
                                PlayerID=maps:get(playerid, GamePlayer),
                                {ok, PlayerDBRec} = findPlayer(GameID, PlayerID),
                                Player=element(3, PlayerDBRec),
                                lists:append([createPlayerObject(Player)], A)
                            end,
                            [], GamePlayers),
      Ret={[{gameid, maps:get(gameid, GameRec)},
            {<<"players">>, PlayerArr}
          ]},
      jiffy:encode(Ret);
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end;

handleRequest(Action, Args) when Action == <<"getplayer">> ->
  GameID=maps:get(gameid, Args),
  PlayerID=maps:get(playerid, Args),
  case findGame(GameID) of
    {ok, _} ->
      case findPlayer(GameID, PlayerID) of
        {ok, PlayerDBRec} ->
          Player=element(3, PlayerDBRec),
          jiffy:encode({[{gameid, maps:get(gameid, Player)},
             {<<"player">>, createPlayerObject(Player)}]});
        not_found ->
          jiffy:encode({[{error, <<"player_not_found">>}]})
      end;
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end;


handleRequest(Action, _Args) when Action == <<"creategame">> ->
  GameID=createGame(),
  jiffy:encode({[{gameid, GameID}]});

handleRequest(Action, Args) when Action == <<"addplayer">> ->
  GameID=maps:get(gameid, Args),
  case findGame(GameID) of
    {ok, _} ->
      {ok, PlayerID} = addPlayer(GameID, Args),
      jiffy:encode({[{gameid, GameID}, {playerid, PlayerID}]});
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end;

handleRequest(Action, Args) when Action == <<"setcolor">> ->
  GameID=maps:get(gameid, Args),
  PlayerID=maps:get(playerid, Args),
  case findPlayer(GameID, PlayerID) of
    {ok, PlayerDBRec} ->
      {atomic, _} = mnesia:transaction(
                      fun() ->
                          mnesia:lock({table, players}, write),
                          Color=binary_to_existing_atom(maps:get(color, Args), latin1),
                          Variant=binary_to_existing_atom(maps:get(variant, Args), latin1),

                          Player=element(3, PlayerDBRec),
                          UpdatedPlayer=Player#{variant := Variant, color := Color},

                          PlayerKey=element(2, PlayerDBRec),
                          mnesia:write({players, PlayerKey, UpdatedPlayer})
                      end),
      jiffy:encode(<<"ok">>);
    not_found ->
      jiffy:encode({[{error, <<"player_not_found">>}]})
  end;

handleRequest(Action, Args) when Action == <<"setstats">> ->
  GameID=maps:get(gameid, Args),
  PlayerID=maps:get(playerid, Args),
  {atomic, Ret} = mnesia:transaction(
                    fun() ->
                        mnesia:lock({table, players}, write),
                        case findPlayer(GameID, PlayerID) of
                          {ok, PlayerDBRec} ->
                            Speed=binary_to_integer(maps:get(speed, Args)),
                            Might=binary_to_integer(maps:get(might, Args)),
                            Sanity=binary_to_integer(maps:get(sanity, Args)),
                            Knowledge=binary_to_integer(maps:get(knowledge, Args)),

                            Player=element(3, PlayerDBRec),
                            UpdatedPlayer=Player#{speed:=Speed, might:=Might,
                                                  sanity:=Sanity, knowledge:=Knowledge},

                            PlayerKey=element(2, PlayerDBRec),
                            mnesia:write({players, PlayerKey, UpdatedPlayer}),
                            jiffy:encode(<<"ok">>);
                          not_found ->
                            jiffy:encode({[{error, <<"player_not_found">>}]})
                        end
                    end),
  Ret.

verifyMethod(Method, Action) ->
  CorrectMethod = maps:get(method, maps:get(Action, ?ACTIONS)),
  {CorrectMethod == Method, CorrectMethod}.

verifyRequest(Method, Args) ->
  Action=maps:get(action, Args),
  case isValidAction(Action) of
    true ->
      case verifyMethod(Method, Action) of
        {true, _} ->
          verifyQueryArgs(Action, Args);
        {false, CorrectMethod} ->
          {invalid_method, jiffy:encode({[{<<"correct_method">>, CorrectMethod}]})}
      end;
    false ->
      {invalid_action, getValidActions()}
  end.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path, Req2} = cowboy_req:path(Req1),
  {StockArgs, Req3} = getQueryArgs(Req2),
  %Methods are in the form /api/action
  %"/api/" has a length of five.
  <<_:5/binary, Action/binary>> = Path,
  Args=StockArgs#{action := Action},
  RequestVerification=verifyRequest(Method, Args),
  {Code, Body} = case RequestVerification of
    ok ->
      case Method of
        Method when Method == <<"POST">> orelse Method == <<"GET">> orelse Method == <<"DELETE">> ->
          {200, handleRequest(Action, Args)};
        UnsupportedMethod ->
          {501, <<UnsupportedMethod/binary, " is not supported.">>}
      end;
    {invalid_method, CorrectMethod} ->
      {405, jiffy:encode(#{correct_method => CorrectMethod})};
    {invalid_action, ValidActions} ->
      {400, jiffy:encode(#{valid_actions => ValidActions})};
    {missing_args, MissingArgs} ->
      {400, jiffy:encode(#{missing_args => MissingArgs})}
  end,
  {ok, Req4} = cowboy_req:reply(Code, [], Body, Req3),
  {ok, Req4, State}.

init(_, Req, Opts) ->
  Req2 = handle(Req, Opts),
  {ok, Req2, Opts}.

terminate(_Reason, _Req, _State) ->
	ok.
