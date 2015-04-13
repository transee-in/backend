-module(transee_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    IP = {0, 0, 0, 0},
    Port = 8000,
    {ok, _} = cowboy:start_http(http, 100
        , [ {ip, IP}, {port, Port} ]
        , [ {env, [{dispatch, routes()}]} ]
        ),
    lager:info("Server started at http://~s:~p", [format_ip(IP), Port]),
    transee_sup:start_link().

stop(_State) ->
    ok.

routes() -> cowboy_router:compile([{'_', routes_v1()}]).

routes_v1() ->
    [ {"/api/v1/cities", transee_api_cities_handler, []}
    , {"/api/v1/cities/:city/[:method]", transee_api_city_handler, []}
    ].

format_ip({A, B, C, D}) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]).
