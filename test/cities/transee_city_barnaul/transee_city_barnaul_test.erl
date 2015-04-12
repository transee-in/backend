-module(transee_city_barnaul_test).
-include("test.hrl").
-define(MOD, transee_city_barnaul).

transee_city_barnaul_test_() -> {setup,
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
    ?assertEqual(<<"99-0,100-0">>, json:get(<<"/0/items/7/id">>, JSON)),
    ?assertEqual(<<"19"/utf8>>,    json:get(<<"/0/items/7/name">>, JSON)),
    ?assertEqual(<<"12-0,13-0">>,  json:get(<<"/2/items/1/id">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getVehiclesMarkers.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"63816">>, json:get(<<"/0/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getRouteNodes.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"77-0,78-0">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([53.326719, 83.793484], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"412">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([53.264995, 83.709619], json:get(<<"/0/position">>, JSON))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD), ?MOD:stations(Source)
    end),
    ?mock_http_with_response(?MOD, "getStationForecasts.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"181">>, Stations, Source)),
        ?assertEqual(<<"Дача БМК"/utf8>>, json:get(<<"/name">>, JSON)),
        ?assertEqual([], json:get(<<"/transports">>, JSON)),
        ?assertEqual(<<"autobus">>, json:get(<<"/forecasts/0/type">>, JSON)),
        ?assertEqual(<<"111-0,112-0">>, json:get(<<"/forecasts/0/id">>, JSON)),
        ?assertEqual(<<"55">>, json:get(<<"/forecasts/0/name">>, JSON)),
        ?assertEqual(<<"17:31">>, json:get(<<"/forecasts/0/arrived_after">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getVehicleForecasts.json", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"Павловский тракт"/utf8>>, json:get(<<"/0/station">>, JSON)),
        ?assertEqual(<<"17:36">>, json:get(<<"/4/time">>, JSON))
    end).
