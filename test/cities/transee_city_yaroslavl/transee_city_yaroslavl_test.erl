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
        ?assertEqual(<<"999">>, json:get(<<"/0/id">>, JSON)),
        ?assertEqual([57.589139, 39.846764], json:get(<<"/0/position">>, JSON)),
        Set = lists:foldl(fun(#{<<"id">> := ID}, Acc) ->
            sets:add_element(ID, Acc)
        end, sets:new(), JSON),
        ?assertEqual(length(JSON), sets:size(Set))
    end).

test_station_info() ->
    Stations = ?mock_http_with_response(?MOD, "getstations.json", fun() -> ok end),
    ?mock_http_with_response(?MOD, "getstationinfo.html", fun() ->
        Source = ?open_source(?MOD),
        JSON = ?assert_json(?MOD:station_info(<<"808">>, Stations, Source)),
        ?assertEqual(<<"Остановка Улица Большая Фёдоровская"/utf8>>, json:get(<<"/name">>, JSON)),
        ?assertEqual(<<"autobus">>,              json:get(<<"/transports/4/type">>, JSON)),
        ?assertEqual(<<"19к"/utf8>>,             json:get(<<"/transports/4/name">>, JSON)),
        ?assertEqual(<<"19k"/utf8>>,             json:get(<<"/transports/4/id">>, JSON)),
        ?assertEqual(<<"Ул. Гудованцева"/utf8>>, json:get(<<"/transports/4/from">>, JSON)),
        ?assertEqual(<<"Красная пл."/utf8>>,     json:get(<<"/transports/4/to">>, JSON)),
        ?assertEqual([], json:get(<<"/forecasts">>, JSON))
    end).

test_transport_info() ->
    ?mock_http_with_response(?MOD, "getpeinfo.html", fun() ->
        JSON = ?assert_json(?MOD:transport_info(<<"autobus">>, <<"123">>)),
        ?assertEqual(<<"12:51">>, json:get(<<"/0/time">>, JSON)),
        ?assertEqual(<<"13:19">>, json:get(<<"/13/time">>, JSON))
    end).
