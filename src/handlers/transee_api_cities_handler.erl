-module(transee_api_cities_handler).
-include("transee.hrl").
-export([init/2]).

init(Req, State) ->
    Body  = jsx:encode(city_list()),
    Reply = cowboy_req:reply(200, [?json_reponse], Body, Req),

    spawn(fun() ->
        influx:post(transee, api_request, [{method, <<"cities">>}])
    end),

    {ok, Reply, State}.

%%
%% Helpers
%%

city_list() ->
    lists:map(fun({_Module, Name}) ->
        Name
    end, transee:config(cities)).
