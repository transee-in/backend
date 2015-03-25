-module(transee_worker_test).
-include("transee.hrl").
-include("test.hrl").
-compile([export_all]).

transee_worker_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun test_async_transports/0
    , fun test_async_positions/0
    , fun test_async_routes/0
    , fun test_async_stations/0
    ]}.

start() -> transee_worker:start_link(transee_worker_test), timer:sleep(500).
stop(_) -> ok.

%%
%% Tests
%%

test_async_transports() ->
    ?assertEqual(updated_transports, transee_worker:transports(?MODULE)).

test_async_positions() ->
    ?assertEqual(updated_positions, transee_worker:positions(?MODULE)).

test_async_routes() ->
    ?assertEqual(updated_routes, transee_worker:routes(?MODULE)).

test_async_stations() ->
    ?assertEqual(updated_stations, transee_worker:stations(?MODULE)).

%%
%% Simple city worker
%%

transports(Pid, _) ->
    do_hard_work(),
    Pid ! {update, transports, updated_transports}.

positions(Pid, _) ->
    do_hard_work(),
    Pid ! {update, positions, updated_positions}.

routes(Pid, _) ->
    do_hard_work(),
    Pid ! {update, routes, updated_routes}.

stations(Pid, _) ->
    do_hard_work(),
    Pid ! {update, stations, updated_stations}.

%%
%% Helpers
%%

% sleep 100 ms
do_hard_work() ->
    timer:sleep(100).
