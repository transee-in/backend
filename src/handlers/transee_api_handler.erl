-module(transee_api_handler).
-include("transee.hrl").
-export([init/2]).

init(Req, State) ->
    City   = city_to_module(cowboy_req:binding(city, Req)),
    Method = cowboy_req:binding(method, Req),

    {Status, Data} = handle(City, Method),
    
    Body  = jsx:encode(Data),
    Reply = cowboy_req:reply(Status, [?json_reponse], Body, Req),

    {ok, Reply, State}.

%%
%% Handlers
%%

handle(undefined, _) ->
    {404, json_error(<<"city_not_found">>)};
handle(City, undefined) ->
    {200, transee_worker:transports(City)};
handle(City, <<"positions">>) ->
    {200, transee_worker:positions(City)};
handle(City, <<"routes">>) ->
    {200, transee_worker:routes(City)};
handle(City, <<"stations">>) ->
    {200, transee_worker:stations(City)};
handle(_City, _Method) ->
    {404, json_error(<<"method_not_found">>)}.

%%
%% Helpers
%%

json_error(Msg) ->
    [{<<"error">>, Msg}].

city_to_module(City) ->
    PName = list_to_atom("transee_city_" ++ binary_to_list(City)),
    try
        sys:get_status(PName), PName
    catch
        _:_ -> undefined
    end.
