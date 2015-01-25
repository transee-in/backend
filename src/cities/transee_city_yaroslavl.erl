-module(transee_city_yaroslavl).
-export([positions/1, routes/1, stations/1]).

%%
%% Behavior
%%

positions(Transports) ->
    Positions = lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Response = request_positions(RouteID,
            transee_util:extract_transport_ids(Numbers)),
        {RouteName, case Response of
            {error, _} ->
                [];
            {ok, Positions} ->
                lists:map(fun([_, Lon, Lat, Angle, ID, Title | _]) ->
                    {ID, [Lat, Lon, Angle]}
                end, Positions)
        end}
    end, Transports),
    lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Types = proplists:get_value(RouteName, Positions),
        {RouteName, lists:map(fun({ID, Number}) ->
            BinaryID = list_to_binary(ID),
            ClearPositions = case proplists:lookup_all(BinaryID, Types) of
                [] -> [];
                Values ->
                    lists:map(fun({_, [Lat, Lon, Angle | _]}) ->
                        [Lat, Lon, Angle]
                    end, Values)
            end,
            {BinaryID, ClearPositions}
        end, Numbers)}
    end, Transports).

routes(Transports) ->
    lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        {RouteName, lists:map(fun({ID, Number}) ->
            {ID, case request_route(RouteID, ID) of
                {error, _} ->
                    [];
                {ok, Routes} ->
                    lists:map(fun([Lat, Lon | _]) ->
                        {Lat, Lon}
                    end, Routes)
            end}
        end, Numbers)}
    end, Transports).

stations(Transports) ->
    lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Response = request_stations(RouteID,
            transee_util:extract_transport_ids(Numbers)),
        {RouteName, case Response of
            {error, _} ->
                [];
            {ok, Stations} ->
                lists:map(fun([_, Lat, Lon | _]) ->
                    {Lat, Lon}
                end, Stations)
        end}
    end, Transports).

%%
%% Helpers
%%

request_positions(ID, Numbers) ->
    URL = create_url("http://www.ot76.ru/getpe.php?vt=~s&r=[~s]",
        [ID, numbers_for_url(Numbers)]),
    io:format("~s~n", [URL]),
    {ok, Body} = submit_request(URL),
    parse_json(Body).

request_route(ID, Number) ->
    URL = create_url("http://www.ot76.ru/getroute.php?vt=~s&r=~s",
        [ID, Number]),
    io:format("~s~n", [URL]),
    {ok, Body} = submit_request(URL),
    parse_json(Body).

request_stations(ID, Numbers) ->
    URL = create_url("http://www.ot76.ru/getstations.php?vt=~s&r=[~s]",
        [ID, numbers_for_url(Numbers)]),
    io:format("~s~n", [URL]),
    {ok, Body} = submit_request(URL),
    parse_json(Body).

%%
%% Internal
%%

numbers_for_url(Numbers) ->
    transee_util:qs_numbers(Numbers).

create_url(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

submit_request(URL) ->
    {ok, {_Status, _Headers, Body}} = httpc:request(get,
        {URL, [{"Referer", "http://www.ot76.ru/"}]}, [],
        [{body_format, binary}]),
    {ok, Body}.

parse_json(Body) ->
    try jsx:decode(Body) of
        JSON -> {ok, JSON}
    catch
        _Any:_Error -> {error, Body}
    end.

