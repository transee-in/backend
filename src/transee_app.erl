-module(transee_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = cowboy:start_http(http, 100
        , [ {ip, {127,0,0,1}}, {port, 8000} ]
        , [ {env, [{dispatch, routes()}]} ]
        ),
    transee_sup:start_link().

stop(_State) ->
    ok.

routes() -> cowboy_router:compile([{'_', routes_v1()}]).

routes_v1() ->
    [ {"/api/v1/cities", transee_api_cities_handler, []}
    , {"/api/v1/cities/:city/[:method]", transee_api_city_handler, []}
    ].
