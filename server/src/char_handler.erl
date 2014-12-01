-module(char_handler).
-behaviour(cowboy_http_handler).

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

-include("gameRecord.hrl").
-include("querystringRecord.hrl").

setRec(Rec, Cat, Val) when Cat==action-> Rec#qsRec{action = Val};
setRec(Rec, Cat, Val) when Cat==gameid -> Rec#qsRec{gameid = Val};
setRec(Rec, Cat, Val) when Cat==playerid -> Rec#qsRec{playerid = Val};
setRec(Rec, Cat, Val) when Cat==playername -> Rec#qsRec{playername = Val};
setRec(Rec, Cat, Val) when Cat==color -> Rec#qsRec{color = Val};
setRec(Rec, Cat, Val) when Cat==variant -> Rec#qsRec{variant = Val};
setRec(Rec, Cat, Val) when Cat==speed -> Rec#qsRec{speed = Val};
setRec(Rec, Cat, Val) when Cat==might -> Rec#qsRec{might = Val};
setRec(Rec, Cat, Val) when Cat==sanity -> Rec#qsRec{sanity = Val};
setRec(Rec, Cat, Val) when Cat==knowledge -> Rec#qsRec{knowledge = Val}.

getQueryArgs(R) ->
  QueryArgs=[
             action, gameid, playerid, playername, color,
             variant, speed, might,
             sanity, knowledge
            ],
  {QSStuff, NewReq}=lists:foldl(fun(F, {Stuff, Req}) when is_atom(F) ->
                    {Val, NextReq} = cowboy_req:qs_val(atom_to_binary(F, latin1), Req),
                    NextStuff=setRec(Stuff, F, Val),
                    {NextStuff, NextReq}
              end, 
              {#qsRec{}, R},
              QueryArgs),
  {QSStuff, NewReq}.

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
      GameRec=#gamerec{gameid=GameID, players=[]},
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

addPlayer(GameID, Args) ->
  PlayerID=generatePlayerID(),
  case findPlayer(GameID, PlayerID) of
    not_found ->
      Key=concatIDs(GameID, PlayerID),
      PlayerName=Args#qsRec.playername,
      PlayerRec=#playerrec{id=PlayerID, 
                           player=#player{name=PlayerName, gameid=GameID,
                                         playerid=PlayerID, character=#character{color=purple}}},
      {atomic, _} = mnesia:transaction(
                        fun() -> 
                            GameList=mnesia:read({games, GameID}),
                            GameRec=element(3, lists:nth(1, GameList)),
                            GameRecPlayers=GameRec#gamerec.players,
                            UpdatedGameRec=
                              GameRec#gamerec{players=
                                              lists:append(GameRecPlayers, [PlayerRec])},
                            mnesia:write({games, GameID, UpdatedGameRec}),
                            mnesia:write({players, Key, PlayerRec})
                        end
                       ),
      {ok, PlayerID};
    %If that playerID is in this game, create a new ID and retry.
    {ok, _} ->
      addPlayer(GameID, Args)
  end.

processDelete(Args) when Args#qsRec.action == <<"kickplayer">> ->
  PlayerID=Args#qsRec.playerid,
  GameID=Args#qsRec.gameid,
  Key=concatIDs(GameID, PlayerID),
  case findGame(GameID) of
    {ok, GameRec} ->
      case findPlayer(GameID, PlayerID) of
        {ok, _} ->
          {atomic, _} = mnesia:transaction(
                          fun() -> 
                              %Remove player from game player array, and from player database.
                              Game=element(3, GameRec),
                              GamePlayerList=Game#gamerec.players,
                              UpdatedPlayerList=lists:filter(
                                                  fun(Player) -> 
                                                      Player#playerrec.player#player.playerid /= PlayerID end, 
                                                  GamePlayerList),
                              UpdatedGame=
                              Game#gamerec{
                                players=UpdatedPlayerList},
                              mnesia:write({games, GameID, UpdatedGame}),
                              mnesia:delete({players, Key})
                          end
                         ),
          jiffy:encode(<<"ok">>);
        not_found ->
          jiffy:encode({[{error, <<"player_not_found">>}]})
      end;
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end.

processGet(Args) when Args#qsRec.action == <<"getplayers">> ->
  GameID=Args#qsRec.gameid,
  case findGame(GameID) of
    {ok, GameDBRec} ->
      %Iterate over players in game.
      GameRec=element(3, GameDBRec),
      Players=GameRec#gamerec.players,
      PlayerArr=[lists:foldl(fun (Player, A) -> 
                                lists:append([
                                              {[{name, Player#playerrec.player#player.name}]},
                                              {[{id, Player#playerrec.id}]}
                                             ], A)
                            end,
                            [], Players)],
      Ret=lists:append([{[{gameid, GameRec#gamerec.gameid}]}], PlayerArr),
      jiffy:encode(Ret);
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end;

processGet(Args) when Args#qsRec.action == <<"getgame">> ->
  GameID=Args#qsRec.gameid,
  case findGame(GameID) of
    {ok, Game} ->
      GameBinary=term_to_binary(Game),
      <<GameBinary/binary>>;
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end.

processPost(Args) when Args#qsRec.action == <<"creategame">> ->
  GameID=createGame(),
  jiffy:encode({[{room_code, GameID}]});

processPost(Args) when Args#qsRec.action == <<"addplayer">> ->
  GameID=Args#qsRec.gameid,
  case findGame(GameID) of
    {ok, _} ->
      {ok, PlayerID} = addPlayer(GameID, Args),
      jiffy:encode({[{gameid, GameID}, {playerid, PlayerID}]});
    not_found ->
      jiffy:encode({[{error, <<"game_not_found">>}]})
  end;

processPost(Args) when Args#qsRec.action == <<"setcolor">> ->
  GameID=Args#qsRec.gameid,
  PlayerID=Args#qsRec.playerid,
  case findPlayer(GameID, PlayerID) of
    {ok, PlayerDBRec} ->
      {atomic, _} = mnesia:transaction(
                      fun() -> 
                          Color=binary_to_atom(Args#qsRec.color, latin1),
                          Variant=binary_to_atom(Args#qsRec.variant, latin1),

                          PlayerRec=element(3, PlayerDBRec),
                          Player=PlayerRec#playerrec.player,
                          Character=Player#player.character,
                          UpdatedCharacter=Character#character{
                                             variant=Variant,
                                             color=Color},
                          UpdatedPlayer=Player#player{character=UpdatedCharacter},
                          UpdatedPlayerRec=PlayerRec#playerrec{player=UpdatedPlayer},

                          PlayerKey=element(2, PlayerDBRec),
                          mnesia:write({players, PlayerKey, UpdatedPlayerRec})
                      end),
      jiffy:encode(<<"ok">>);
    not_found ->
      jiffy:encode({[{error, <<"player_not_found">>}]})
  end;

processPost(Args) when Args#qsRec.action == <<"setstats">> ->
  GameID=Args#qsRec.gameid,
  PlayerID=Args#qsRec.playerid,
  case findPlayer(GameID, PlayerID) of
    {ok, PlayerDBRec} ->
      {atomic, _} = mnesia:transaction(
                      fun() -> 
                          %There doesn't really seem to be a way around
                          %breaking out each component of the record
                          %and putting them back together.
                          Speed=binary_to_integer(Args#qsRec.speed),
                          Might=binary_to_integer(Args#qsRec.might),
                          Sanity=binary_to_integer(Args#qsRec.sanity),
                          Knowledge=binary_to_integer(Args#qsRec.knowledge),

                          PlayerRec=element(3, PlayerDBRec),
                          Player=PlayerRec#playerrec.player,
                          Character=Player#player.character,
                          UpdatedStats=#stats{speed=Speed,
                                              might=Might,
                                              sanity=Sanity,
                                              knowledge=Knowledge},
                          UpdatedCharacter=Character#character{stats=UpdatedStats},
                          UpdatedPlayer=Player#player{character=UpdatedCharacter},
                          UpdatedPlayerRec=PlayerRec#playerrec{player=UpdatedPlayer},

                          PlayerKey=element(2, PlayerDBRec),
                          mnesia:write({players, PlayerKey, UpdatedPlayerRec})
                      end),
      jiffy:encode(<<"ok">>);
    not_found ->
      jiffy:encode({[{error, <<"player_not_found">>}]})
  end.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {Args, Req3}  = getQueryArgs(Req2),
  case Method of
    <<"POST">> ->
      Body = processPost(Args),
      {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
      {ok, Req4, State};
    <<"GET">> ->
      Body = processGet(Args),
      {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
      {ok, Req4, State};
    <<"DELETE">> ->
      Body = processDelete(Args),
      {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
      {ok, Req4, State};
    _UnsupportedMethod ->
      Body = <<_UnsupportedMethod/binary, " is not supported.">>,
      {ok, Req4} = cowboy_req:reply(501, [], Body, Req3),
      {ok, Req4, State}
  end.

init(_, Req, Opts) ->
  Req2 = handle(Req, Opts),
  {ok, Req2, Opts}.

terminate(_Reason, _Req, _State) ->
	ok.
