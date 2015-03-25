-module(transee_city_vladivostok_test).
-include("test.hrl").
-define(MOD, transee_city_vladivostok).

transee_city_vladivostok_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun test_transports/0
    , fun test_positions/0
    , fun test_routes/0
    , fun test_stations/0
    , fun test_station_info/0
    , fun test_transport_info/0
    ]}.

start() -> qdate:start(), ?mock_time({1427, 283065, 18809}).
stop(_) -> ?unmock_time.

%%
%% Tests
%%

test_transports() ->
    Source = ?open_source(?MOD),
    JSON = ?assert_json(?MOD:transports(Source)),
    ?assertEqual(<<"autobus">>,   json:get(<<"/0/type">>, JSON)),
    ?assertEqual(<<"20-0,21-0">>, json:get(<<"/0/items/5/id">>, JSON)),
    ?assertEqual(<<"7т"/utf8>>,  json:get(<<"/0/items/5/name">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getVehiclesMarkers.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"861">>, json:get(<<"/0/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getRouteNodes.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"42-0">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([43.119335, 131.883497], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"180">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([43.193382, 131.932733], json:get(<<"/0/position">>, JSON))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD), ?MOD:stations(Source)
    end),
    ?mock_http_with_response(?MOD, "getStationForecasts.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"181">>, Stations, Source)),
        ?assertEqual(<<>>, json:get(<<"/name">>, JSON)),
        ?assertEqual([], json:get(<<"/transports">>, JSON)),
        ?assertEqual(<<"autobus">>, json:get(<<"/forecasts/0/type">>, JSON)),
        ?assertEqual(<<"59-0,60-0">>, json:get(<<"/forecasts/0/id">>, JSON)),
        ?assertEqual(<<"45">>, json:get(<<"/forecasts/0/name">>, JSON)),
        ?assertEqual(<<"21:46">>, json:get(<<"/forecasts/0/arrived_after">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getVehicleForecasts.json", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"Окатовая"/utf8>>, json:get(<<"/0/station">>, JSON)),
        ?assertEqual(<<"21:39">>, json:get(<<"/4/time">>, JSON))
    end).
