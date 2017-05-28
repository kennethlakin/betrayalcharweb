-module(char_handler).
-behaviour(cowboy_http_handler).

-export([
         init/3,
         handle/2,
         terminate/3,
         getValidColors/0,
         getValidVariants/0
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

-define(VALID_COLORS, [<<"purple">>, <<"green">>, <<"white">>, <<"blue">>, <<"red">>, <<"orange">>]).
-define(VALID_VARIANTS, [<<"front">>, <<"back">>]).

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

getValidColors() ->
  ?VALID_COLORS.

getValidVariants() ->
  ?VALID_VARIANTS.

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
  case fastFindGame(GameID) of
    {ok, _} ->
      createGame();
    not_found ->
      GameRec=newGameRec(GameID),
      {atomic, _} = mnesia:sync_transaction(
                        fun() -> mnesia:write({games, GameID, GameRec}) end
                       ),
      GameID
  end.

fastFindGame(GameID) ->
  Val = mnesia:dirty_read(games, GameID),
  Len = length(Val),
  case Len >= 1 of
    true when Len == 1 ->
      {ok, lists:nth(1, Val)};
    false ->
      not_found
  end.

fastFindPlayer(GameID, PlayerID) ->
  Val = mnesia:dirty_read(players, concatIDs(GameID, PlayerID)),
  Len = length(Val),
  case Len >= 1 of
    true when Len == 1 ->
      {ok, lists:nth(1, Val)};
    false ->
      not_found
  end.

%Used when we intend to update the game record in the same transaction.
findGame(GameID) ->
  {atomic, Val}= mnesia:transaction(fun() -> mnesia:read(games, GameID, write) end),
  Len = length(Val),
  case Len >= 1 of
    true when Len == 1 ->
      {ok, lists:nth(1, Val)};
    false ->
      not_found
  end.

%Used when we intend to update the player record in the same transaction.
findPlayer(GameID, PlayerID) ->
  {atomic, Val}= mnesia:transaction(fun() -> mnesia:read(players, concatIDs(GameID, PlayerID), write) end),
  Len = length(Val),
  case Len >= 1 of
    true when Len == 1 ->
      {ok, lists:nth(1, Val)};
    false ->
      not_found
  end.

extractPlayers(GameID, GamePlayers) ->
  lists:foldl(fun (GamePlayer, A) ->
                  PlayerID=maps:get(playerid, GamePlayer),
                  {ok, PlayerDBRec} = fastFindPlayer(GameID, PlayerID),
                  Player=element(3, PlayerDBRec),
                  lists:append([createPlayerObject(Player)], A)
              end,
              [], GamePlayers).

%Creates some eJSON that will be passed to jiffy to be encoded into JSON.
createPlayerObject(Player) ->
  Ret=lists:foldl(fun(Key, Acc) ->
                   lists:append([{Key, maps:get(Key, Player)}], Acc)
              end,
              [],
              maps:keys(Player)),
  {Ret}.

findConflicting(Things, ValueToCheck, GameID, Players) when length(Things) == length(ValueToCheck) ->
  findConflicting(Things, ValueToCheck, GameID, Players, #{}).
findConflicting([], [], _GameID, _Players, Acc) ->
  Acc;
findConflicting([Thing|TRest], [ValueToCheck|VRest], GameID, Players, Acc) when length(TRest) == length(VRest) ->
  List=lists:filter(fun(P) ->
                   GamePlayerID=maps:get(playerid, P),
                   {ok, PlayerRec}=fastFindPlayer(GameID, GamePlayerID),
                   GamePlayer=element(3, PlayerRec),
                   maps:get(Thing, GamePlayer) == ValueToCheck
               end, Players),
  false=maps:is_key(Thing, Acc),
  NewAcc=maps:put(Thing, List, Acc),
  findConflicting(TRest, VRest, GameID, Players, NewAcc).


addPlayer(GameRec, Args) ->
  Game=element(3, GameRec),
  GameID=maps:get(gameid, Game),
  PlayerID=generatePlayerID(),
  case fastFindPlayer(GameID, PlayerID) of
    not_found ->
      {atomic, {Reason, Response}} = mnesia:sync_transaction(
                        fun() ->
                            %get a write lock on the game.
                            {ok, _}=findGame(GameID),
                            PlayerKey=concatIDs(GameID, PlayerID),
                            PlayerName=maps:get(playername, Args),
                            Players=maps:get(players, Game),
                            ConflictingNames=maps:get(name,
                                                      findConflicting([name], [PlayerName], GameID, Players)
                                                     ),
                            case ConflictingNames == [] of
                              false ->
                                {error, <<"name_already_taken">>};
                              true ->
                                Player=newPlayerRec(GameID, PlayerID, PlayerName),
                                GamePlayers=maps:get(players, Game),
                                UpdatedGame=Game#{players := lists:append(GamePlayers, [Player])},
                                mnesia:write({games, GameID, UpdatedGame}),
                                mnesia:write({players, PlayerKey, Player}),
                                {ok, PlayerID}
                            end
                        end
                       ),
      {Reason, Response};
    %If that playerID is in this game, create a new ID and retry.
    {ok, _} ->
      addPlayer(GameRec, Args)
  end.

handleRequest(Action, Args) when Action == <<"kickplayer">> ->
  PlayerID=maps:get(playerid, Args),
  GameID=maps:get(gameid, Args),
  Key=concatIDs(GameID, PlayerID),
  {atomic, Ret} = mnesia:sync_transaction(
                    fun() ->
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
  case fastFindGame(GameID) of
    {ok, GameDBRec} ->
      %Iterate over players in game.
      GameRec=element(3, GameDBRec),
      GamePlayers=maps:get(players, GameRec),
      PlayerArr=extractPlayers(GameID, GamePlayers),
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
  case fastFindGame(GameID) of
    {ok, _} ->
      case fastFindPlayer(GameID, PlayerID) of
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
  case fastFindGame(GameID) of
    {ok, GameRec} ->
      case addPlayer(GameRec, Args) of
      {ok, PlayerID} ->
          jiffy:encode({[{gameid, GameID}, {playerid, PlayerID}]});
      {error, Resp} ->
          jiffy:encode({[{error, Resp}]})
      end;
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end;

handleRequest(Action, Args) when Action == <<"setcolor">> ->
  GameID=maps:get(gameid, Args),
  PlayerID=maps:get(playerid, Args),
  {atomic, Ret} = mnesia:sync_transaction(
                    fun() ->
                        case findPlayer(GameID, PlayerID) of
                          {ok, PlayerDBRec} ->
                            Color=binary_to_existing_atom(maps:get(color, Args), latin1),
                            Variant=binary_to_existing_atom(maps:get(variant, Args), latin1),

                            {ok, GameRec}=fastFindGame(GameID),
                            Game=element(3, GameRec),
                            Players=maps:get(players, Game),
                            %Find Players with the same color, but ignore
                            %players with a color that's set to null.
                            Conflicts=maps:get(color, findConflicting([color], [Color], GameID, Players)),
                            FilteredConflicts=lists:filter(fun(C) -> maps:get(color, C) /= null end, Conflicts),
                            case FilteredConflicts == [] of
                              false ->
                                jiffy:encode({[{error, <<"color_already_taken">>}]});
                              true ->
                              Player=element(3, PlayerDBRec),
                              UpdatedPlayer=Player#{variant := Variant, color := Color},

                              PlayerKey=element(2, PlayerDBRec),
                              mnesia:write({players, PlayerKey, UpdatedPlayer}),
                              jiffy:encode(<<"ok">>)
                            end;
                          not_found ->
                            jiffy:encode({[{error, <<"player_not_found">>}]})
                        end
                    end),
  Ret;

handleRequest(Action, Args) when Action == <<"setstats">> ->
  GameID=maps:get(gameid, Args),
  PlayerID=maps:get(playerid, Args),
  {atomic, Ret} = mnesia:sync_transaction(
                    fun() ->
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

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
	ok.
