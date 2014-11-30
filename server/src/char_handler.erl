-module(char_handler).
-behaviour(cowboy_http_handler).

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

-include("gameRecord.hrl").
-record(qsRec, {
          action::binary()|atom(),
          gameid::binary()|atom(),
          playerid::binary()|atom(),
          playername::binary()|atom(),
          color::binary()|atom(),
          variant::binary()|atom(),
          speed::binary()|atom(),
          might::binary()|atom(),
          sanity::binary()|atom(),
          knowledge::binary()|atom()
         }).

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

%Generate upper-case alphanumeric Game IDs.
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


init(_, Req, Opts) ->
  Req2 = handle(Req, Opts),
  {ok, Req2, Opts}.

processDelete(Acc, Args) when Args#qsRec.action == <<"kickplayer">> ->
  PlayerID=Args#qsRec.playerid,
  GameID=Args#qsRec.gameid,
  Key=concatIDs(GameID, PlayerID),
  Ret=case findGame(GameID) of
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
                              error_logger:info_msg("Game ~p~n", [Game]),
                              error_logger:info_msg("UpdatedGame ~p~n", [UpdatedGame]),
                              mnesia:write({games, GameID, UpdatedGame}),
                              mnesia:delete({players, Key})
                          end
                         ),
          <<"Player Deleted">>;
        not_found ->
          <<"Did not find player">>
      end;
    not_found ->
      <<"Did not find game">>
  end,
  <<Acc/binary, " ", Ret/binary>>.

processGet(Acc, Args) when Args#qsRec.action == <<"getplayers">> ->
  GameID=Args#qsRec.gameid,
  case findGame(GameID) of
    {ok, GameRec} ->
      GameRecBinary=term_to_binary(GameRec),
      <<Acc/binary, " ", GameRecBinary/binary>>;
    not_found ->
      <<Acc/binary, " Game Not Found">>
  end;

processGet(Acc, Args) when Args#qsRec.action == <<"getgame">> ->
  GameID=Args#qsRec.gameid,
  case findGame(GameID) of
    {ok, Game} ->
      GameBinary=term_to_binary(Game),
      <<Acc/binary, " ", GameBinary/binary>>;
    not_found ->
      <<Acc/binary, " Game Not Found">>
  end.

processPost(Acc, Args) when Args#qsRec.action == <<"creategame">> ->
  GameID=createGame(),
  <<Acc/binary, GameID/binary>>;

processPost(Acc, Args) when Args#qsRec.action == <<"addplayer">> ->
  GameID=Args#qsRec.gameid,
  Ret=case findGame(GameID) of
    {ok, _} ->
      {ok, PlayerID} = addPlayer(GameID, Args),
      term_to_binary(PlayerID);
    not_found ->
      term_to_binary("Game not Found")
  end,
  <<Acc/binary, " ", GameID/binary, " ", Ret/binary>>;

processPost(Acc, Args) when Args#qsRec.action == <<"setcolor">> ->
  GameID=Args#qsRec.gameid,
  PlayerID=Args#qsRec.playerid,
  Ret=case findPlayer(GameID, PlayerID) of
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

                          error_logger:info_msg("PlayerRec ~p", [PlayerRec]),
                          error_logger:info_msg("UpdatedPlayerRec ~p", [UpdatedPlayerRec]),
                          PlayerKey=element(2, PlayerDBRec),
                          mnesia:write({players, PlayerKey, UpdatedPlayerRec})
                      end),
      term_to_binary("Player updated");
    not_found ->
      term_to_binary("Player not found")
  end,
  <<Acc/binary, " ", Ret/binary>>;

processPost(Acc, Args) when Args#qsRec.action == <<"setstats">> ->
  GameID=Args#qsRec.gameid,
  PlayerID=Args#qsRec.playerid,
  Ret=case findPlayer(GameID, PlayerID) of
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
      term_to_binary("Stats updated");
    not_found ->
      term_to_binary("Player not found")
  end,
  <<Acc/binary, " ", Ret/binary>>.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {Args, Req3}  = getQueryArgs(Req2),
  case Method of
    <<"POST">> ->
      Body = processPost(<<"<h1>This is a response for POST</h1>">>, Args),
      {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
      {ok, Req4, State};
    <<"GET">> ->
      Body = processGet(<<"<h1>This is a response for GET</h1>">>, Args),
      {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
      {ok, Req4, State};
    <<"DELETE">> ->
      Body = processDelete(<<"<h1>This is a response for DELETE</h1>">>, Args),
      {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
      {ok, Req4, State};
    _ ->
      Body = <<"<h1>This is a response for other methods</h1>">>,
      {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
      {ok, Req4, State}
  end.

terminate(_Reason, _Req, _State) ->
	ok.
