-module(transee_city_voronezh_test).
-include("test.hrl").
-define(MOD, transee_city_voronezh).

transee_city_voronezh_test_() -> {setup,
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
    ?assertEqual(<<"autobus">>,  json:get(<<"/0/type">>, JSON)),
    ?assertEqual(<<"9-0,10-0">>, json:get(<<"/0/items/5/id">>, JSON)),
    ?assertEqual(<<"11Н"/utf8>>, json:get(<<"/0/items/5/name">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getVehiclesMarkers.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"66533">>, json:get(<<"/0/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getRouteNodes.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"80-0,81-0">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([51.679312, 39.208006], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"575">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([51.614166,39.807812], json:get(<<"/0/position">>, JSON))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD), ?MOD:stations(Source)
    end),
    ?mock_http_with_response(?MOD, "getStationForecasts.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"181">>, Stations, Source)),
        ?assertEqual(<<"Молодогвардейцев"/utf8>>, json:get(<<"/name">>, JSON)),
        ?assertEqual([], json:get(<<"/transports">>, JSON)),
        ?assertEqual(<<"autobus">>, json:get(<<"/forecasts/0/type">>, JSON)),
        ?assertEqual(<<"31-0,32-0">>, json:get(<<"/forecasts/0/id">>, JSON)),
        ?assertEqual(<<"124">>, json:get(<<"/forecasts/0/name">>, JSON)),
        ?assertEqual(<<"14:41">>, json:get(<<"/forecasts/0/arrived_after">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getVehicleForecasts.json", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"Ильича"/utf8>>, json:get(<<"/0/station">>, JSON)),
        ?assertEqual(<<"14:40">>, json:get(<<"/4/time">>, JSON))
    end).
