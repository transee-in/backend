-module(transee_city_yaroslavl_test).
-include("test.hrl").
-define(MOD, transee_city_yaroslavl).

transee_city_yaroslavl_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun test_transports/0
    , fun test_positions/0
    , fun test_routes/0
    , fun test_stations/0
    , fun test_station_info/0
    , fun test_transport_info/0
    ]}.

start() -> ok.
stop(_) -> ok.

%%
%% Tests
%%

test_transports() ->
    Source = ?open_source(?MOD),
    JSON = ?assert_json(?MOD:transports(Source)),
    ?assertEqual(<<"autobus">>, json:get(<<"/0/type">>, JSON)),
    ?assertEqual(<<"2k">>,      json:get(<<"/0/items/1/id">>, JSON)),
    ?assertEqual(<<"2к"/utf8>>, json:get(<<"/0/items/1/name">>, JSON)).

test_positions() ->
    ?mock_http_with_response(?MOD, "getpe.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:positions(Source)),
        ?assertEqual(<<"27">>, json:get(<<"/1/items/0/items/0/gos_id">>, JSON))
    end).

test_routes() ->
    ?mock_http_with_response(?MOD, "getroute.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:routes(Source)),
        ?assertEqual(<<"2k">>, json:get(<<"/0/items/1/id">>, JSON)),
        ?assertEqual([57.639400, 39.963450], hd(json:get(<<"/0/items/1/route">>, JSON)))
    end).

test_stations() ->
    ?mock_http_with_response(?MOD, "getstations.json", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:stations(Source)),
        ?assertEqual(<<"258">>, json:get(<<"/0/items/0/id">>, JSON)),
        ?assertEqual([57.588407, 39.845435], json:get(<<"/0/items/0/position">>, JSON))
    end).

test_station_info() ->
    ?mock_http_with_response(?MOD, "getstationinfo.html", fun() ->
        JSON = ?assert_json(?MOD:station_info(<<"808">>)),
        ?assertEqual(<<"Station Some Name">>, json:get(<<"/name">>, JSON)),
        ?assertEqual(<<"Au 19k. Ul. Some - Nothin.">>, json:get(<<"/transports/0">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getpeinfo.html", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"12:51">>, json:get(<<"/0/time">>, JSON)),
        ?assertEqual(<<"13:19">>, json:get(<<"/13/time">>, JSON))
    end).
