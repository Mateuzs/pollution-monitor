-module(server_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1, start/0]).

start_link(InitValue) ->
supervisor:start_link({local, varSupervisor},
                        ?MODULE, InitValue).



init(InitValue) ->
 {ok, {
{one_for_one, 2, 3},
[
{state_container,
{state_container, start_link, [InitValue]},
permanent, brutal_kill, worker, [state_container]},

{pollution_gen_server,
{pollution_gen_server, start_link, [InitValue]},
permanent, brutal_kill, worker, [pollution_gen_server]}

]}}.


start() ->
    start_link('trashValue').