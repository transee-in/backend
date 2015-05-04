-module(transee_city_kostroma_test).
-include("test.hrl").
-define(MOD, transee_city_kostroma).

transee_city_kostroma_test_() -> {setup,
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
    ?assertEqual(<<"autobus">>,    json:get(<<"/0/type">>, JSON)),
    ?assertEqual(<<"5-0,6-0">>,    json:get(<<"/0/items/7/id">>, JSON)),
    ?assertEqual(<<"14"/utf8>>,    json:get(<<"/0/items/7/name">>, JSON)),
    ?assertEqual(<<"35-0,36-0">>,  json:get(<<"/2/items/1/id">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getVehiclesMarkers.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"autobus">>,  json:get(<<"/0/type">>, JSON)),
        ?assertEqual(<<"47008548">>, json:get(<<"/0/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getRouteNodes.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"158-0,159-0">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([57.805574, 41.110602], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"46">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([57.79465, 40.918343], json:get(<<"/0/position">>, JSON))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD), ?MOD:stations(Source)
    end),
    ?mock_http_with_response(?MOD, "getStationForecasts.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"181">>, Stations, Source)),
        ?assertEqual(<<"3-я Горбольница"/utf8>>, json:get(<<"/name">>, JSON)),
        ?assertEqual([], json:get(<<"/transports">>, JSON)),
        ?assertEqual(<<"minibus_taxi">>, json:get(<<"/forecasts/0/type">>, JSON)),
        ?assertEqual(<<"107-0,108-0">>, json:get(<<"/forecasts/0/id">>, JSON)),
        ?assertEqual(<<"24">>, json:get(<<"/forecasts/0/name">>, JSON)),
        ?assertEqual(<<"14:31">>, json:get(<<"/forecasts/0/arrived_after">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getVehicleForecasts.json", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"К-р Дружба"/utf8>>, json:get(<<"/0/station">>, JSON)),
        ?assertEqual(<<"14:35">>, json:get(<<"/4/time">>, JSON))
    end).
