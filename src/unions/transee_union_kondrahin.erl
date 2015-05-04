%% Contains generic functions to prepare data for many cities that
%% works on system developed by individual entrepreneur Kondrahin A.V.
-module(transee_union_kondrahin).
-include("transee.hrl").
-export([transports/1, positions/3, routes/3, stations/3, transport_info/4, station_info/6]).

%% @doc prepare list of transports from city source file
%% to this json format, but in Erlang:
%% [
%%   {
%%     "type": "autobus",
%%     "items": [
%%       {
%%         "id": "2",
%%         "name": "2"
%%       },
%%       {
%%         "id": "2k",
%%         "name": "2к"
%%       },
%%       ...
%%     ]
%%   },
%%   ...
%% ]
transports(Source) ->
    Types = proplists:get_value(types, Source),
    Transports = proplists:get_value(transports, Source),
    lists:map(fun(Type) ->
        format_transport_type(Type, Transports)
    end, Types).

format_transport_type({Type, TypeName}, Transports) ->
    [ {<<"type">>,  TypeName}
    , {<<"items">>, collect_transport_items_for(Type, Transports)}
    ].

collect_transport_items_for(Type, Transports) ->
    lists:foldr(fun(Transport, Acc) ->
        append_if_match_type(Type, Transport, Acc)
    end, [], Transports).

append_if_match_type(MatchType, {ID, Type, Name}, Acc) when MatchType == Type ->
    [format_transport_item(ID, Name) | Acc];
append_if_match_type(_, _, Acc) ->
    Acc.

format_transport_item(ID, Name) ->
    [ {<<"id">>,   ?to_bin(format_ids(ID))}
    , {<<"name">>, ?to_bin(Name)}
    ].

%% @doc fetch, parse and organize transport positions
%% in JSON format like:
%% [
%%   {
%%     "type": "autobus",
%%     "items": [
%%       {
%%         "id": "3",
%%         "name": "3",
%%         "items": [
%%           {
%%             "gos_id": "532",
%%             "angle": 303,
%%             "position": [
%%               57.610682,
%%               39.830793
%%             ]
%%           },
%%           {
%%             "gos_id": "510",
%%             "angle": 359,
%%             "position": [
%%               57.628638,
%%               39.848412
%%             ]
%%           },
%%           ...
%%         ]
%%       },
%%       ...
%%     ]
%%   },
%%   ...
%% ]
positions(City, URL, Source) ->
    Types = create_map_with_types(Source),
    Transports = proplists:get_value(transports, Source),
    RIDs = collect_and_format_transport_ids(Transports),
    Data = request(URL,
        [ {rids, RIDs}, {lat0, 0}, {lng0, 0}
        , {lat1, 90}, {lng1, 180}, {curk, 0}
        , {city, City}, {info, "0123"}]),
    RawItems = process_positions_data(Data, Types),
    format_position_items(RawItems, Types, Transports).

create_map_with_types(Source) ->
    maps:from_list(lists:map(fun({ID, _Name}) ->
        {ID, #{}}
    end, proplists:get_value(types, Source))).

collect_and_format_transport_ids(Transports) ->
    string:join(lists:map(fun({IDs, _Type, _Name}) ->
        format_ids(IDs)
    end, Transports), ",").

process_positions_data(#{<<"anims">> := Positions}, Types) ->
    maps:to_list(lists:foldl(fun process_positions_data_types/2, Types, Positions));
process_positions_data(_, _) ->
    [].

process_positions_data_types(#{<<"rid">> := ID, <<"rtype">> := Type} = Raw, JSON) ->
    Path = [Type, ?to_bin(ID)],
    Item = preformat_positions_data_item(Raw),
    json:add(Path, [Item | transee_json:maybe(Path, JSON)], JSON).

preformat_positions_data_item(#{<<"id">> := GosID, <<"dir">> := Angle, <<"lat">> := Lat, <<"lon">> := Lon}) ->
    [ {<<"gos_id">>,   GosID}
    , {<<"angle">>,    Angle}
    , {<<"position">>, format_lat_lon(Lat, Lon)}
    ].

format_position_items(RawItems, Types, Transports) ->
    lists:map(fun({InternalName, Numbers}) ->
        [ {<<"type">>,  maps:get(InternalName, Types)}
        , {<<"items">>, collect_type_items(Numbers, Transports)}
        ]
    end, RawItems).

collect_type_items(Numbers, Transports) ->
    lists:foldr(fun({NumberID, Items}, Acc) ->
        append_if_found_type(find_by_id(NumberID, Transports), Items, Acc)
    end, [], maps:to_list(Numbers)).

append_if_found_type({TIDs, _Type, Name}, Items, Acc) ->
    [format_position_type_item(TIDs, Name, Items) | Acc];
append_if_found_type(_, _, Acc) ->
    Acc.

format_position_type_item(TIDs, Name, Items) ->
    [ {<<"id">>,    ?to_bin(format_ids(TIDs))}
    , {<<"name">>,  Name}
    , {<<"items">>, Items}
    ].

%% @doc fetch, parse and organize list of routes
%%
%% [
%%   {
%%     "type": "autobus",
%%     "items": [
%%       {
%%         "id": "2",
%%         "route": [
%%           [57.642743, 39.873551]
%%           [57.642502, 39.873647]
%%         ]
%%       },
%%       ...
%%     ]
%%   },
%%   ...
%% ]
routes(City, URL, Source) ->
    Types = proplists:get_value(types, Source),
    Transports = proplists:get_value(transports, Source),
    lists:map(fun({Type, TypeName}) ->
        [ {<<"type">>, TypeName}
        , {<<"items">>, format_route_items(Transports, Type, City, URL)}
        ]
    end, Types).

format_route_items(Transports, Type, City, URL) ->
    lists:foldl(fun({IDs, TType, _Name}, Acc) ->
        if
            Type == TType ->
                [[ {<<"id">>, ?to_bin(format_ids(IDs))}
                 , {<<"route">>, format_route_transports(IDs, City, URL)}
                 ] | Acc];
            true ->
                Acc
        end
    end, [], Transports).

format_route_transports(IDs, City, URL) ->
    lists:foldl(fun(ID, Acc2) ->
        Acc2 ++ format_route_lat_lon(request(URL, [{city, City}, {type, 0}, {rid, ID}]))
    end, [], IDs).

format_route_lat_lon(Resp) ->
    lists:map(fun(#{<<"lat">> := Lat, <<"lng">> := Lng}) ->
        format_lat_lon(Lat, Lng)
    end, Resp).

%% @doc fetch, parse and organize list of stations
%%
%% [
%%   {
%%     "id": "999",
%%     "position": [
%%       57.589139, 39.846764
%%     ]
%%   },
%%   ...
%% ]
stations(City, URL, _Source) ->
    Resp = request(URL, [{city, City}]),
    lists:map(fun(#{<<"id">> := ID, <<"lat">> := Lat, <<"lng">> := Lng, <<"name">> := Name}) ->
        BinID = ?to_bin(ID),
        store_station_name(City, BinID, Name),
        format_stations(BinID, Lat, Lng)
    end, Resp).

format_stations(ID, Lat, Lng) ->
    [ {<<"id">>, ID}
    , {<<"position">>, format_lat_lon(Lat, Lng)}
    ].

store_station_name(City, BinID, Name) ->
    std_cache:set(<<"st_id_", City/binary, BinID/binary>>, fun() ->
        Name
    end).


%% @doc fetch station info for every request and format it
%%
%% {
%%   "name": "Остановка Улица Красноперевальская",
%%   "transports": [
%%     {
%%       "type": "autobus",
%%       "id": "6",
%%       "name": "6",
%%       "from": "ЯШЗ",
%%       "to": "НЗКИ"
%%     },
%%     ...
%%   ],
%%   "forecasts": [
%%     {
%%       "type": "autobus",
%%       "arrived_after": "14:37",
%%       "id": "111-0,112-0",
%%       "name": "55"
%%     },
%%     ...
%%   ]
%% }
station_info(City, URL, ID, _Stations, Source, TZ) ->
    Types = proplists:get_value(types, Source),
    Transports = proplists:get_value(transports, Source),
    Resp = request(URL, [{city, City}, {sid, ID}, {type, 0}]),
    BinID = ?to_bin(ID),
    StationName = get_station_name(City, BinID),
    [ {<<"name">>, StationName}
    , {<<"transports">>, []}
    , {<<"forecasts">>, format_station_forecasts(Resp, Transports, Types, TZ)}
    ].

format_station_forecasts(Resp, Transports, Types, TZ) ->
    lists:map(fun(#{<<"arrt">> := Arrival, <<"rid">> := RID, <<"rtype">> := Type}) ->
        {TIDs, _Type, Name} = find_by_id(RID, Transports),
        TypeName = proplists:get_value(Type, Types),
        [ {<<"type">>, TypeName}
        , {<<"arrived_after">>, format_arrival(Arrival, TZ)}
        , {<<"id">>, ?to_bin(format_ids(TIDs))}
        , {<<"name">>, Name}
        ]
    end, Resp).

get_station_name(City, BinID) ->
    std_cache:get(<<"st_id_", City/binary, BinID/binary>>).

%% @doc fetch transport info for every request and format it
%%
%% [
%%   {
%%     "station": "Комсомольская площадь",
%%     "time": "11:42"
%%   },
%%   ...
%% ]
transport_info(City, URL, GosID, TZ) ->
    Resp = request(URL, [{city, City}, {vid, GosID}, {type, 0}]),
    lists:map(fun(#{<<"arrt">> := Arrival, <<"stname">> := Station}) ->
        [ {<<"station">>, Station}
        , {<<"time">>, format_arrival(Arrival, TZ)}
        ]
    end, Resp).

%%
%% Helpers
%%

format_arrival(Arrived, TZ) ->
    Date = qdate:add_seconds(Arrived + TZ, transee_time:now()),
    qdate:format(<<"H:i">>, Date).

format_lat_lon(Lat, Lon) ->
    [Lat / 1000000, Lon / 1000000].

format_ids(IDs) ->
    string:join(lists:map(fun(I) ->
        std_cast:to_list(I) ++ "-0"
    end, IDs), ",").

find_by_id(ID, Transports) ->
    Finded = lists:filter(fun({IDs, _Type, _Name}) ->
        lists:member(std_cast:to_integer(ID), IDs)
    end, Transports),
    case Finded of
        [Transport|_] -> Transport;
        _             -> []
    end.

request(URL, Params) ->
    QS = cow_qs:qs(lists:map(fun({K, V}) ->
        {?to_bin(K), ?to_bin(V)}
    end, Params)),
    FullURL = lists:flatten(io_lib:format("~s?~s", [URL, QS])),
    parse_json(transee_http:request(get, FullURL, [])).

parse_json({ok, <<>>}) ->
    undefined;
parse_json({ok, Body}) ->
    try
        json:from_binary(Body)
    catch E:R ->
        lager:error("transee_union_kondrahin:parse_json ~p ~p ~s", [E, R, Body])
    end;
parse_json(_) ->
    undefined.
