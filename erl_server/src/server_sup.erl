-module(server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([reload/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

%Ultra-janky "code-reloading" function:
reload() ->
  application:stop(server),
  lists:foreach(fun (Mod) ->
                    {ok, _}=compile:file("src/"++atom_to_list(Mod),
                                         [verbose,report_errors,report_warnings,
                                          {outdir, "ebin"}]),
                    false=code:purge(Mod),
                    {module, _}=code:load_file(Mod)
                end,
                [char_handler, server_app, server]),
  application:start(server).
