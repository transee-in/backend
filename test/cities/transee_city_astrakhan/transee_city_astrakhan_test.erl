-module(transee_city_astrakhan_test).
-include("test.hrl").
-define(MOD, transee_city_astrakhan).

transee_city_astrakhan_test_() -> {setup,
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
    ?assertEqual(<<"autobus">>,   json:get(<<"/0/type">>, JSON)),
    ?assertEqual(<<"32-0,33-0">>, json:get(<<"/0/items/7/id">>, JSON)),
    ?assertEqual(<<"20к"/utf8>>,  json:get(<<"/0/items/7/name">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getVehiclesMarkers.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"87523">>, json:get(<<"/0/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getRouteNodes.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"16-0">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([46.347076, 48.025416], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"187">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([46.35965, 48.084712], json:get(<<"/0/position">>, JSON))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getStations.json", fun() ->
        Source = ?open_source(?MOD), ?MOD:stations(Source)
    end),
    ?mock_http_with_response(?MOD, "getStationForecasts.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"181">>, Stations, Source)),
        ?assertEqual(<<"Татар-Базар"/utf8>>, json:get(<<"/name">>, JSON)),
        ?assertEqual([], json:get(<<"/transports">>, JSON)),
        ?assertEqual(<<"autobus">>, json:get(<<"/forecasts/0/type">>, JSON)),
        ?assertEqual(<<"11-0,12-0">>, json:get(<<"/forecasts/0/id">>, JSON)),
        ?assertEqual(<<"30">>, json:get(<<"/forecasts/0/name">>, JSON)),
        ?assertEqual(<<"14:38">>, json:get(<<"/forecasts/0/arrived_after">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getVehicleForecasts.json", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"Микрорайон Бабаевского"/utf8>>, json:get(<<"/0/station">>, JSON)),
        ?assertEqual(<<"14:47">>, json:get(<<"/4/time">>, JSON))
    end).
