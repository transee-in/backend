-module(transee_city_vladimir_test).
-include("test.hrl").
-define(MOD, transee_city_vladimir).

transee_city_vladimir_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun test_transports/0
    , fun test_positions/0
    , fun test_routes/0
    , fun test_stations/0
    , fun test_station_info/0
    , fun test_transport_info/0
    ]}.

start() -> std:start(), qdate:start(), ?mock_time({1427, 283065, 18809}).
stop(_) -> ?unmock_time.

%%
%% Tests
%%

test_transports() ->
    Source = ?open_source(?MOD),
    JSON = ?assert_json(?MOD:transports(Source)),
    ?assertEqual(<<"autobus">>, json:get(<<"/0/type">>, JSON)),
    ?assertEqual(<<"383-0">>,   json:get(<<"/0/items/1/id">>, JSON)),
    ?assertEqual(<<"6с"/utf8>>, json:get(<<"/0/items/1/name">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getVehiclesMarkers.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"33067567">>, json:get(<<"/0/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getRouteNodes.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"423-0,424-0">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([56.121658, 40.36295], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"340">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([56.165423, 40.454991], json:get(<<"/0/position">>, JSON))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD), ?MOD:stations(Source)
    end),
    ?mock_http_with_response(?MOD, "getStationForecasts.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"340">>, Stations, Source)),
        ?assertEqual(<<"Аптека"/utf8>>, json:get(<<"/name">>, JSON)),
        ?assertEqual([], json:get(<<"/transports">>, JSON)),
        ?assertEqual(<<"autobus">>, json:get(<<"/forecasts/0/type">>, JSON)),
        ?assertEqual(<<"384-0,385-0">>, json:get(<<"/forecasts/0/id">>, JSON)),
        ?assertEqual(<<"7c">>, json:get(<<"/forecasts/0/name">>, JSON)),
        ?assertEqual(<<"14:35">>, json:get(<<"/forecasts/0/arrived_after">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getVehicleForecasts.json", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"Пл. Фрунзе"/utf8>>, json:get(<<"/0/station">>, JSON)),
        ?assertEqual(<<"14:41">>, json:get(<<"/4/time">>, JSON))
    end).
