-module(transee_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = cowboy:start_http(http, 100
        , [ {ip, {127,0,0,1}}, {port, 8000} ]
        , [ {env, [{dispatch, routes()}]}
          % {onrequest,  fun on_request/1}
          % {onresponse, fun on_response/4}
          ]
        ),
    transee_sup:start_link().

stop(_State) ->
    ok.

routes() -> cowboy_router:compile([{'_',
    [ {"/",                    transee_index_handler,      []}
    , {"/api/cities",          transee_api_cities_handler, []}
    , {"/api/:city/[:method]", transee_api_city_handler,   []}
    , {"/static/[...]",        cowboy_static, {dir, static_dir()}}
    ]}]).

static_dir() ->
    filename:join([std:priv_dir(transee), "static"]). 
