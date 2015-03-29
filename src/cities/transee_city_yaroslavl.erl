-module(transee_city_yaroslavl).
-include("transee.hrl").
-export([ coordinates/0
        , transports/1, transports/2
        , positions/1, positions/2
        , routes/1, routes/2
        , stations/1, stations/2
        , transport_info/2
        , station_info/3
        ]).

%%
%% Behavior
%%

coordinates() ->
    [57.62987, 39.87368].

transports(WorkerPid, Source) ->
    WorkerPid ! {update, transports, transports(Source)}.
transports(Source) ->
    lists:map(fun({{RouteName, _RouteID}, Numbers}) ->
        Items = lists:map(fun({ID, Number}) ->
            [ {<<"id">>, list_to_binary(ID)}
            , {<<"name">>, Number}
            ]
        end, Numbers),
        [ {<<"type">>, RouteName}
        , {<<"items">>, Items}
        ]
    end, Source).

positions(WorkerPid, Source) ->
    WorkerPid ! {update, positions, positions(Source)}.
positions(Source) ->
    Positions = lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Response = request_positions(RouteID,
            transee_util:extract_transport_ids(Numbers)),
        {RouteName, case Response of
            {error, _} ->
                [];
            {ok, Positions} ->
                lists:map(fun([GosID, Lon, Lat, Angle, ID, _Title, _Image | _]) ->
                    {ID, [ {<<"gos_id">>, GosID}
                         , {<<"angle">>, ?to_num(Angle)}
                         , {<<"position">>, [?to_num(Lat), ?to_num(Lon)]}
                         ]}
                end, Positions)
        end}
    end, Source),
    lists:map(fun({{RouteName, _RouteID}, Numbers}) ->
        Types = proplists:get_value(RouteName, Positions),
        Items = lists:foldr(fun({ID, Number}, Acc) ->
            BinaryID = ?l2b(ID),
            case proplists:lookup_all(BinaryID, Types) of
                [] -> Acc;
                Values ->
                    ClearPositions = lists:map(fun({_, V}) -> V end, Values),
                    [[ {<<"id">>, BinaryID}
                     , {<<"name">>, Number}
                     , {<<"items">>, ClearPositions}
                     ] | Acc]
            end
        end, [], Numbers),
        [ {<<"type">>, RouteName}
        , {<<"items">>, Items}
        ]
    end, Source).

routes(WorkerPid, Source) ->
    WorkerPid ! {update, routes, routes(Source)}.
routes(Source) ->
    lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Items = lists:map(fun({ID, _Number}) ->
            BinaryID = ?l2b(ID),
            Route = case request_route(RouteID, ID) of
                {error, _} ->
                    [];
                {ok, R} ->
                    lists:map(fun([Lat, Lon | _]) ->
                        [?to_num(Lat), ?to_num(Lon)]
                    end, R)
            end,
            [ {<<"id">>, BinaryID}
            , {<<"route">>, Route}
            ]
        end, Numbers),
        [ {<<"type">>, RouteName}
        , {<<"items">>, Items}
        ]
    end, Source).

stations(WorkerPid, Source) ->
    WorkerPid ! {update, stations, stations(Source)}.
stations(Source) ->
    lists:map(fun({{RouteName, RouteID}, Numbers}) ->
        Response = request_stations(RouteID,
            transee_util:extract_transport_ids(Numbers)),
        Items = case Response of
            {error, _} ->
                [];
            {ok, Stations} ->
                lists:map(fun([ID, Lat, Lon | _]) ->
                    [ {<<"id">>, ID}
                    , {<<"position">>, [?to_num(Lat), ?to_num(Lon)]}
                    ]
                end, Stations)
        end,
        [ {<<"type">>, RouteName}
        , {<<"items">>, Items}
        ]
    end, Source).

station_info(ID, _Stations, _Source) ->
    HTML = win1251_to_utf8(request_station_info(ID)),
    [Name | Transports] = binary:split(HTML, <<"<br>">>, [global]),
    [ {<<"name">>, binary:replace(Name,
        [<<"<b>">>, <<"</b>">>, <<$">>], <<>>, [global])}
    , {<<"transports">>, Transports}
    , {<<"forecasts">>, []}
    ].

transport_info(ID, GosID) ->
    HTML = request_info(type_id(ID), GosID),
    Path = "//table/tr",
    Tree = mochiweb_html:parse(HTML),
    lists:reverse(lists:foldl(fun parse_transport_info/2, [],
        mochiweb_xpath:execute(Path, Tree))).

parse_transport_info({<<"tr">>, [], Content}, Acc) ->
    case match_content(Content) of
        {ok, Station, Time} ->
            [[{<<"station">>, Station}, {<<"time">>, Time}] | Acc];
        undefined ->
            Acc
    end;
parse_transport_info(_, Acc) ->
    Acc.

match_content([{<<"td">>, [], [Station|_]},
        {<<"td">>, [{<<"align">>,<<"right">>}], [Time|_]}]) ->
    {ok, win1251_to_utf8(Station), Time};
match_content(_) ->
    undefined.

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

request_station_info(ID) ->
    URL = create_url("http://www.ot76.ru/getstationinfo.php?id=~s", [ID]),
    {ok, Body} = submit_request(URL), Body.

request_info(ID, GosID) ->
    URL = create_url("http://www.ot76.ru/mob/getpeinfo.php?vt=~s&npe=~s",
        [ID, GosID]),
    {ok, Body} = submit_request(URL), Body.

%%
%% Internal
%%

type_id(<<"autobus">>) -> <<"1">>;
type_id(<<"trolleybus">>) -> <<"2">>;
type_id(<<"tram">>) -> <<"3">>;
type_id(_) -> undefined.

win1251_to_utf8(V) ->
    win1251:decode(unicode:characters_to_list(V, latin1)).

numbers_for_url(Numbers) ->
    transee_util:qs_numbers(Numbers).

create_url(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

submit_request(URL) ->
    transee_http:request(get, URL, [{"Referer", "http://www.ot76.ru/"}]).

parse_json(Body) ->
    try jsx:decode(Body) of
        JSON -> {ok, JSON}
    catch
        _Any:_Error -> {error, Body}
    end.
