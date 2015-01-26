-module(transee_worker).
-behaviour(gen_server).
-include("transee.hrl").
-export([ start_link/1
        , transports/1
        , positions/1, positions/2, positions/3
        , routes/1
        , stations/1
        % gen_server callbacks
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

start_link(City) ->
    gen_server:start_link({local, City}, ?MODULE, [City], []).

transports(City) ->
    gen_server:call(City, transports).

positions(City) ->
    gen_server:call(City, positions).

positions(City, Type) ->
    [{Type, gen_server:call(City, {positions, Type})}].

positions(City, Type, IDs) ->
    Types = gen_server:call(City, {positions, Type}),
    [{Type, lists:foldl(fun(ID, Acc) ->
        case proplists:get_value(ID, Types) of
            undefined -> Acc;
            Value     -> [{ID, Value} | Acc]
        end
    end, [], IDs)}].

routes(City) ->
    gen_server:call(City, routes).

stations(City) ->
    gen_server:call(City, stations).

%%
%% GenServer callbacks
%%

init([City]) ->
    init_data_timer(), reload_data_timer(),
    {ok, #worker_state{city = City}}.

handle_call(transports, _From, #worker_state{transports = Transports} = State) ->
    {reply, Transports, State};
handle_call(positions, _From, #worker_state{positions = Positions} = State) ->
    {reply, Positions, State};
handle_call({positions, Type}, _From, #worker_state{positions = Positions} = State) ->
    {reply, proplists:get_value(Type, Positions, []), State};
handle_call(routes, _From, #worker_state{routes = Routes} = State) ->
    {reply, Routes, State};
handle_call(stations, _From, #worker_state{stations = Stations} = State) ->
    {reply, Stations, State};
handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(init, #worker_state{city = City} = State) ->
    Source     = open_source(City),
    Transports = City:transports(Source),
    Positions  = City:positions(Source),
    Stations   = City:stations(Source),
    Routes     = City:routes(Source),
    {noreply, State#worker_state{
        positions = Positions, stations = Stations,
        routes = Routes, source = Source,
        transports = Transports}};
handle_info(reload, #worker_state{city = City, source = S} = State) ->
    reload_data_timer(),
    Positions = City:positions(S),
    {noreply, State#worker_state{positions = Positions}};
handle_info(Info, State) ->
    {reply, Info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%

init_data_timer() ->
    erlang:send_after(?INIT_CITY_INTERVAL, self(), init).

reload_data_timer() ->
    erlang:send_after(?RELOAD_CITY_INTERVAL, self(), reload).

open_source(City) ->
    File = atom_to_list(City) ++ ".config",
    Path = filename:join([std:priv_dir(transee), "cities", File]),
    case file:consult(Path) of
        {ok, [V]} -> V;
        _         -> []
    end.
