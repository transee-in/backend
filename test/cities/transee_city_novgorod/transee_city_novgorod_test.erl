-module(transee_city_novgorod_test).
-include("test.hrl").
-define(MOD, transee_city_novgorod).

transee_city_novgorod_test_() -> {setup,
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
    ?assertEqual(<<"8-0,9-0">>, json:get(<<"/0/items/1/id">>, JSON)),
    ?assertEqual(<<"1а"/utf8>>, json:get(<<"/0/items/1/name">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getVehiclesMarkers.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"861785002400459">>, json:get(<<"/0/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getRouteNodes.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"84-0,85-0">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([58.526982, 31.251791], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"144">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([58.521414, 31.172183], json:get(<<"/0/position">>, JSON))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD), ?MOD:stations(Source)
    end),
    ?mock_http_with_response(?MOD, "getStationForecasts.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"181">>, Stations, Source)),
        ?assertEqual(<<"Лужское шоссе"/utf8>>, json:get(<<"/name">>, JSON)),
        ?assertEqual([], json:get(<<"/transports">>, JSON)),
        ?assertEqual(<<"autobus">>, json:get(<<"/forecasts/0/type">>, JSON)),
        ?assertEqual(<<"19-0,20-0">>, json:get(<<"/forecasts/0/id">>, JSON)),
        ?assertEqual(<<"6">>, json:get(<<"/forecasts/0/name">>, JSON)),
        ?assertEqual(<<"14:42">>, json:get(<<"/forecasts/0/arrived_after">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getVehicleForecasts.json", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"Кооперативная"/utf8>>, json:get(<<"/0/station">>, JSON)),
        ?assertEqual(<<"14:40">>, json:get(<<"/4/time">>, JSON))
    end).
