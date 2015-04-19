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

%% @doc ...
%%
positions(City, URL, Source) ->
    Types = proplists:get_value(types, Source),
    TypesMapped = create_map_with_types(Types),
    Transports = proplists:get_value(transports, Source),
    IDs = collect_transport_ids(Transports),
    RIDs = format_ids_for_url(IDs),
    Data = request(URL,
        [ {rids, RIDs}, {lat0, 0}, {lng0, 0}
        , {lat1, 90}, {lng1, 180}, {curk, 0}
        , {city, City}, {info, "0123"}]),
    RawItems = case Data of
        #{<<"anims">> := Positions} ->
            Fn = fun(#{<<"dir">> := Angle, <<"lat">> := Lat, <<"lon">> := Lon
                      , <<"rid">> := ID, <<"rtype">> := Type, <<"id">> := GosID}, Acc) ->
                Item = [ {<<"gos_id">>, GosID}, {<<"angle">>, Angle}
                       , {<<"position">>, format_lat_lon(Lat, Lon)}],
                RID = ?to_bin(ID),
                OldItems = try
                    json:get([Type, RID], Acc)
                catch _:_ ->
                    []
                end,
                try
                    json:add([Type, RID], [Item | OldItems], Acc)
                catch
                    _:_ -> Acc
                after
                    Acc
                end
            end,
            maps:to_list(lists:foldl(Fn, TypesMapped, Positions));
        _ -> []
    end,
    lists:map(fun({InternalName, Numbers}) ->
        TypeName = proplists:get_value(InternalName, Types),
        TypeItems = lists:foldr(fun({NumberID, Items}, Acc) ->
            case find_by_id(NumberID, Transports) of
                {TIDs, _Type, Name} ->
                    [[ {<<"id">>, ?to_bin(format_ids(TIDs))}
                     , {<<"name">>, Name}
                     , {<<"items">>, Items}
                     ] | Acc];
                _ ->
                    Acc
            end
        end, [], maps:to_list(Numbers)),
        [ {<<"type">>, TypeName}
        , {<<"items">>, TypeItems}
        ]
    end, RawItems).

routes(City, URL, Source) ->
    Types = proplists:get_value(types, Source),
    Transports = proplists:get_value(transports, Source),
    lists:map(fun({Type, TypeName}) ->
        TypeItems = lists:foldl(fun({IDs, TType, _Name}, Acc) ->
            if
                Type == TType ->
                    TransportRoutes = lists:foldl(fun(ID, Acc2) ->
                        Resp = request(URL, [{city, City}, {type, 0}, {rid, ID}]),
                        Acc2 ++ lists:map(fun(#{<<"lat">> := Lat, <<"lng">> := Lng}) ->
                            format_lat_lon(Lat, Lng)
                        end, Resp)
                    end, [], IDs),
                    [[ {<<"id">>, ?to_bin(format_ids(IDs))}
                     , {<<"route">>, TransportRoutes}
                     ] | Acc];
                true ->
                    Acc
            end
        end, [], Transports),
        [ {<<"type">>, TypeName}
        , {<<"items">>, TypeItems}
        ]
    end, Types).

stations(City, URL, _Source) ->
    Resp = request(URL, [{city, City}]),
    lists:map(fun(#{<<"id">> := ID, <<"lat">> := Lat, <<"lng">> := Lng, <<"name">> := Name}) ->
        BinID = ?to_bin(ID),
        std_cache:set(<<"st_id_", City/binary, BinID/binary>>, fun() ->
            Name
        end),
        [ {<<"id">>, BinID}
        , {<<"position">>, format_lat_lon(Lat, Lng)}
        ]
    end, Resp).

station_info(City, URL, ID, _Stations, Source, TZ) ->
    Types = proplists:get_value(types, Source),
    Transports = proplists:get_value(transports, Source),
    Resp = request(URL, [{city, City}, {sid, ID}, {type, 0}]),
    BinID = ?to_bin(ID),
    StationName = std_cache:get(<<"st_id_", City/binary, BinID/binary>>),
    Forecasts = lists:map(fun(#{<<"arrt">> := Arrival, <<"rid">> := RID, <<"rtype">> := Type}) ->
        {TIDs, _Type, Name} = find_by_id(RID, Transports),
        TypeName = proplists:get_value(Type, Types),
        [ {<<"type">>, TypeName}
        , {<<"arrived_after">>, format_arrival(Arrival, TZ)}
        , {<<"id">>, ?to_bin(format_ids(TIDs))}
        , {<<"name">>, Name}
        ]
    end, Resp),
    [ {<<"name">>, StationName}
    , {<<"transports">>, []}
    , {<<"forecasts">>, Forecasts}
    ].

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

create_map_with_types(Types) ->
    maps:from_list(lists:map(fun({Type, _Name}) ->
        {Type, #{}}
    end, Types)).

collect_transport_ids(Transports) ->
    lists:map(fun({IDs, _Type, _Name}) ->
        IDs
    end, Transports).

format_ids_for_url(IDs) ->
    string:join(lists:map(fun format_ids/1, IDs), ",").

request(URL, Params) ->
    QS = cow_qs:qs(lists:map(fun({K, V}) ->
        {?to_bin(K), ?to_bin(V)}
    end, Params)),
    FullURL = lists:flatten(io_lib:format("~s?~s", [URL, QS])),
    parse_json(transee_http:request(get, FullURL, [])).

parse_json({ok, Body}) ->
    try
        json:from_binary(Body)
    catch E:R ->
        lager:error("transee_union_kondrahin:parse_json ~p ~p ~s", [E, R, Body])
    end;
parse_json(_) ->
    undefined.
