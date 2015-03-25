-module(transee_sup).
-behaviour(supervisor).
-include("transee.hrl").
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CityWorkers = lists:map(fun({Module, _Name}) ->
        ?CHILD(Module, transee_worker, worker, [Module])
    end, transee:config(cities)),
    PingWorker = ?CHILD(ping, transee_ping, worker, []),
    {ok, {{one_for_one, 5, 10}, [PingWorker | CityWorkers]}}.
