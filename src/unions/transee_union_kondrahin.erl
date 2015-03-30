%% Contains generic functions to prepare data for many cities that
%% works on system developed by individual entrepreneur Kondrahin A.V.
-module(transee_union_kondrahin).
-export([transports/1, positions/3, routes/3, stations/3, transport_info/4, station_info/6]).
-define(to_bin(V), std_cast:to_binary(V)).

transports(Source) ->
    Types = proplists:get_value(types, Source),
    Transports = proplists:get_value(transports, Source),
    lists:map(fun({Type, TypeName}) ->
        Items = lists:foldr(fun({ID, TType, Name}, Acc) ->
            if
                Type == TType ->
                    [[ {<<"id">>, ?to_bin(format_ids(ID))}
                     , {<<"name">>, ?to_bin(Name)}
                     ] | Acc];
                true ->
                    Acc
            end
        end, [], Transports),
        [ {<<"type">>, TypeName}
        , {<<"items">>, Items}
        ]
    end, Types).

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
                json:add([Type, RID], [Item | OldItems], Acc)
            end,
            maps:to_list(lists:foldl(Fn, TypesMapped, Positions));
        _ -> []
    end,
    lists:map(fun({InternalName, Numbers}) ->
        TypeName = proplists:get_value(InternalName, Types),
        TypeItems = lists:map(fun({NumberID, Items}) ->
            {TIDs, _Type, Name} = find_by_id(NumberID, Transports),
            [ {<<"id">>, ?to_bin(format_ids(TIDs))}
            , {<<"name">>, Name}
            , {<<"items">>, Items}
            ]
        end, maps:to_list(Numbers)),
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
    lists:map(fun(#{<<"id">> := ID, <<"lat">> := Lat, <<"lng">> := Lng}) ->
        [ {<<"id">>, ?to_bin(ID)}
        , {<<"position">>, format_lat_lon(Lat, Lng)}
        ]
    end, Resp).

% TODO add station name to the response
station_info(City, URL, ID, _Stations, Source, TZ) ->
    Types = proplists:get_value(types, Source),
    Transports = proplists:get_value(transports, Source),
    Resp = request(URL, [{city, City}, {sid, ID}, {type, 0}]),
    Forecasts = lists:map(fun(#{<<"arrt">> := Arrival, <<"rid">> := RID, <<"rtype">> := Type}) ->
        {TIDs, _Type, Name} = find_by_id(RID, Transports),
        TypeName = proplists:get_value(Type, Types),
        [ {<<"type">>, TypeName}
        , {<<"arrived_after">>, format_arrival(Arrival, TZ)}
        , {<<"id">>, ?to_bin(format_ids(TIDs))}
        , {<<"name">>, Name}
        ]
    end, Resp),
    [ {<<"name">>, <<>>}
    , {<<"transports">>, []}
    , {<<"forecasts">>, Forecasts}
    ].

% http://bus125.ru/php/getVehicleForecasts.php?vid=919&type=0&city=vladivostok
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
    json:from_binary(Body);
parse_json(_) ->
    undefined.



