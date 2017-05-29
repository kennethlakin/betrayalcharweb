class Server {
  async makeRequest(
      path: string, method = 'GET',
      params: null|{[key: string]: string | number} = null) {
    const url = new URL(path, document.baseURI!);
    url.host = 'campstove.lan:8080';
    if (params) {
      for (const key of Object.keys(params)) {
        url.searchParams.set(key, '' + params[key]);
      }
    }
    const partialResponse = await fetch(url.toString(), {method});
    const result = await partialResponse.json();
    if (result && result.error) {
      throw new Error(result.error);
    }
    return result;
  }

  /** @return {Promise<string>} The id of the new game */
  async createGame() {
    const result: CreateGameResult =
        await this.makeRequest('/api/creategame', 'POST');
    return result.gameid;
  }

  async addPlayer(gameId: GameId, playerName: string) {
    const result: AddPlayerResult = await this.makeRequest(
        '/api/addplayer', 'POST', {gameid: gameId, playername: playerName});

    return result.playerid;
  }

  async getPlayer(gameId: GameId, playerId: PlayerId) {
    const player: GetPlayerResult = await this.makeRequest(
        '/api/getplayer', 'GET', {gameid: gameId, playerid: playerId});

    return player.player;
  }

  async getPlayers(gameId: GameId) {
    const players: GetPlayersResult =
        await this.makeRequest('/api/getplayers', 'GET', {gameid: gameId});
    return players.players;
  }

  async kickPlayer(gameId: GameId, playerId: PlayerId) {
    await this.makeRequest(
        '/api/kickplayer', 'DELETE', {gameid: gameId, playerid: playerId});
  }

  async setColor(
      gameId: GameId, playerId: PlayerId, color: Color, variant: Variant) {
    await this.makeRequest(
        '/api/setcolor', 'POST',
        {gameid: gameId, playerid: playerId, color, variant});
  }

  async setStats(
      gameId: GameId, playerId: PlayerId,
      stats:
          {might: number, knowledge: number, sanity: number, speed: number}) {
    await this.makeRequest(
        '/api/setstats', 'POST',
        {gameid: gameId, playerid: playerId, ...stats});
  }

  async listMethods() {
    return this.makeRequest('/api/');
  }
}

class Game {
  private id: GameId;
  private server: Server;
  constructor(id: GameId, server: Server) {
    this.id = id;
    this.server = server;
  }

  async addPlayer(name: string) {
    const playerId = await this.server.addPlayer(this.id, name);
    return new Player(playerId, this.id, this.server);
  }

  async getPlayers() {
    return this.server.getPlayers(this.id);
  }
}

class Player {
  private readonly id: PlayerId;
  private readonly gameId: GameId;
  private readonly server: Server;
  constructor(id: PlayerId, gameId: GameId, server: Server) {
    this.id = id;
    this.gameId = gameId;
    this.server = server;
  }

  async refresh() {
    return this.server.getPlayer(this.gameId, this.id);
  }

  async setColor(color: Color, variant: Variant) {
    return this.server.setColor(this.gameId, this.id, color, variant);
  }

  async setStats(
      stats:
          {might: number, knowledge: number, speed: number, sanity: number}) {
    return this.server.setStats(this.gameId, this.id, stats);
  }

  async kick() {
    return this.server.kickPlayer(this.gameId, this.id);
  }
}

const getStream = (() => {
  let stream: WebSocket;
  function makeStream(gameId: GameId) {
    const url = new URL('/events', document.baseURI!);
    url.protocol = 'ws';
    url.host = 'campstove.lan:8080';
    url.searchParams.set('gameid', gameId);
    stream = new WebSocket(url.toString());
    const handle = setInterval(() => stream.send('ping'), 5000);
    stream.addEventListener('open', () => console.log('ws opened'));
    stream.addEventListener('close', () => {
      console.log('ws closed');
      clearInterval(handle);
      stream = makeStream(gameId);
    });
    stream.addEventListener('error', (err) => console.log(`ws error: ${err}`));
    stream.addEventListener('message', (msg) => {
      if (msg.data === 'pong') {
        console.log('ws pong');
      } else {
        console.log('ws pong');
      }
    });
    return stream;
  }
  function getStream(gameId: GameId) {
    if (!stream) {
      stream = makeStream(gameId);
    }
    return stream;
  }
  return getStream;
})();

type GameId = string&{
  readonly __stringBrand: GameId;
};
type PlayerId = string&{
  readonly __stringBrand: PlayerId;
};

type Color = 'purple'|'green'|'white'|'red'|'orange'|'blue';
type Variant = 'front'|'back';

interface CreateGameResult {
  readonly gameid: GameId;
}

interface AddPlayerResult {
  readonly playerid: PlayerId;
  readonly gameid: GameId;
}

interface GetPlayerResult {
  readonly gameid: GameId;
  readonly player: PlayerSnapshot;
}

interface GetPlayersResult {
  readonly gameid: GameId;
  readonly players: ReadonlyArray<PlayerSnapshot>;
}

interface PlayerSnapshot {
  readonly gameid: GameId;
  readonly playerid: PlayerId;
  readonly color: null|Color;
  readonly variant: null|Variant;
  readonly name: string;

  readonly knowledge: null|number;
  readonly might: null|number;
  readonly sanity: null|number;
  readonly speed: null|number;
}
