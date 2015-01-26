-module(transee_city_yaroslavl).
-include_lib("std/include/std.hrl").
-export([transports/1, positions/1, routes/1, stations/1]).

%%
%% Behavior
%%

transports(Source) ->
    lists:map(fun({{RouteName, _RouteID}, Numbers}) ->
        {RouteName, lists:map(fun({ID, Number}) ->
            {?l2b(ID), Number}
        end, Numbers)}
    end, Source).

positions(Source) ->
    Positions = lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Response = request_positions(RouteID,
            transee_util:extract_transport_ids(Numbers)),
        {RouteName, case Response of
            {error, _} ->
                [];
            {ok, Positions} ->
                lists:map(fun([_, Lon, Lat, Angle, ID, _Title | _]) ->
                    {ID, [Lat, Lon, Angle]}
                end, Positions)
        end}
    end, Source),
    lists:map(fun({{RouteName, _RouteID}, Numbers}) ->
        Types = proplists:get_value(RouteName, Positions),
        {RouteName, lists:map(fun({ID, _Number}) ->
            BinaryID = ?l2b(ID),
            ClearPositions = case proplists:lookup_all(BinaryID, Types) of
                [] -> [];
                Values ->
                    lists:map(fun({_, [Lat, Lon, Angle | _]}) ->
                        [ std_cast:to_number(Lat)
                        , std_cast:to_number(Lon)
                        , std_cast:to_number(Angle)
                        ]
                    end, Values)
            end,
            {BinaryID, ClearPositions}
        end, Numbers)}
    end, Source).

routes(Source) ->
    lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        {RouteName, lists:map(fun({ID, _Number}) ->
            BinaryID = ?l2b(ID),
            {BinaryID, case request_route(RouteID, ID) of
                {error, _} ->
                    [];
                {ok, Routes} ->
                    lists:map(fun([Lat, Lon | _]) ->
                        [ std_cast:to_number(Lat)
                        , std_cast:to_number(Lon)
                        ]
                    end, Routes)
            end}
        end, Numbers)}
    end, Source).

stations(Source) ->
    lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Response = request_stations(RouteID,
            transee_util:extract_transport_ids(Numbers)),
        {RouteName, case Response of
            {error, _} ->
                [];
            {ok, Stations} ->
                lists:map(fun([_, Lat, Lon | _]) ->
                    [ std_cast:to_number(Lat)
                    , std_cast:to_number(Lon)
                    ]
                end, Stations)
        end}
    end, Source).

%%
%% Helpers
%%

request_positions(ID, Numbers) ->
    URL = create_url("http://www.ot76.ru/getpe.php?vt=~s&r=[~s]",
        [ID, numbers_for_url(Numbers)]),
    {ok, Body} = submit_request(URL),
    parse_json(Body).

request_route(ID, Number) ->
    URL = create_url("http://www.ot76.ru/getroute.php?vt=~s&r=~s",
        [ID, Number]),
    {ok, Body} = submit_request(URL),
    parse_json(Body).

request_stations(ID, Numbers) ->
    URL = create_url("http://www.ot76.ru/getstations.php?vt=~s&r=[~s]",
        [ID, numbers_for_url(Numbers)]),
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

