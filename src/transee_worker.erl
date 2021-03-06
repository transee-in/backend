-module(transee_worker).
-behaviour(gen_server).
-include("transee.hrl").
-export([ start_link/1
        , coordinates/1
        , transports/1
        , positions/1, positions/3
        , routes/1
        , stations/1
        , transport_info/3
        , station_info/2
        % gen_server callbacks
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        % helpers
        , open_source/1
        ]).

start_link(City) ->
    gen_server:start_link({local, City}, ?MODULE, [City], []).

coordinates(City) ->
    gen_server:call(City, coordinates).

transports(City) ->
    gen_server:call(City, transports).

positions(City) ->
    gen_server:call(City, positions).

positions(City, Type, IDs) ->
    FilteredByType = gen_server:call(City, {positions, Type}),
    MaybeTransportItems = lists:map(fun(Transports) ->
        Items = proplists:get_value(<<"items">>, Transports),
        lists:filter(fun(Item) ->
            V = proplists:get_value(<<"id">>, Item),
            lists:member(V, IDs)
        end, Items)
    end, FilteredByType),
    case MaybeTransportItems of
        [] ->
            [ {<<"type">>, Type}
            , {<<"items">>, []}
            ];
        [TransportItems|_] ->
            [ {<<"type">>, Type}
            , {<<"items">>, TransportItems}
            ]
    end.

routes(City) ->
    gen_server:call(City, routes).

stations(City) ->
    gen_server:call(City, stations).

station_info(City, ID) ->
    gen_server:call(City, {station_info, ID}).

transport_info(City, ID, GosID) ->
    gen_server:call(City, {transport_info, ID, GosID}).

%%
%% GenServer callbacks
%%

init([City]) ->
    init_data_timer(),
    {ok, #worker_state{city = City}}.

handle_call(coordinates, _From, #worker_state{city = City} = State) ->
    {reply, City:coordinates(), State};
handle_call(transports, _From, #worker_state{transports = Transports} = State) ->
    {reply, Transports, State};
handle_call(positions, _From, #worker_state{positions = Positions} = State) ->
    {reply, Positions, State};
handle_call({positions, Type}, _From, #worker_state{positions = Positions} = State) ->
    FilteredPositions = lists:filter(fun(E) ->
        proplists:get_value(<<"type">>, E) == Type
    end, Positions),
    {reply, FilteredPositions, State};
handle_call(routes, _From, #worker_state{routes = Routes} = State) ->
    {reply, Routes, State};
handle_call(stations, _From, #worker_state{stations = Stations} = State) ->
    {reply, Stations, State};
handle_call({station_info, ID}, _From, #worker_state{city = City, source = Source, stations = Stations} = State) ->
    {reply, City:station_info(ID, Stations, Source), State};
handle_call({transport_info, ID, GosID}, _From, #worker_state{city = City} = State) ->
    {reply, City:transport_info(ID, GosID), State};
handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(init, #worker_state{city = City} = State) ->
    lager:info("Init city worker: ~p", [City]),
    Source = open_source(City),
    spawn(City, transports, [self(), Source]),
    spawn(City, positions,  [self(), Source]),
    spawn(City, stations,   [self(), Source]),
    spawn(City, routes,     [self(), Source]),
    Ref = start_data_timer(),
    {noreply, State#worker_state{source = Source, timer = Ref}};
handle_info({update, transports, Data}, #worker_state{} = State) ->
    {noreply, State#worker_state{transports = Data}};
handle_info({update, positions, Data}, #worker_state{} = State) ->
    {noreply, State#worker_state{positions = Data}};
handle_info({update, stations, Data}, #worker_state{} = State) ->
    {noreply, State#worker_state{stations = Data}};
handle_info({update, routes, Data}, #worker_state{} = State) ->
    {noreply, State#worker_state{routes = Data}};
handle_info(reload, #worker_state{city = City, source = Source, timer = OldRef} = State) ->
    close_data_timer(OldRef),
    lager:info("Reload data for: ~p", [City]),
    spawn(City, positions, [self(), Source]),
    Ref = start_data_timer(),
    {noreply, State#worker_state{timer = Ref}};
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

start_data_timer() ->
    erlang:send_after(?RELOAD_CITY_INTERVAL, self(), reload).

close_data_timer(Ref) ->
    erlang:cancel_timer(Ref).

open_source(City) ->
    File = atom_to_list(City) ++ ".config",
    Path = filename:join([std:priv_dir(transee), "cities", File]),
    case file:consult(Path) of
        {ok, [V]} -> V;
        _         -> []
    end.
