-module(transee_api_city_handler).
-include("transee.hrl").
-export([init/2]).

init(Req, State) ->
    City   = city_to_module(cowboy_req:binding(city, Req)),
    Method = cowboy_req:binding(method, Req),
    Params = case params(Req) of
        undefined -> undefined;
        Val       -> qsp:decode(Val)
    end,

    {Status, Data} = handle(City, Method, Params),
    
    Body  = jsx:encode(Data),
    Reply = cowboy_req:reply(Status, [?json_reponse], Body, Req),

    {ok, Reply, State}.

%%
%% Handlers
%%

handle(undefined, _, _) ->
    {404, json_error(<<"city_not_found">>)};
handle(City, undefined, _) ->
    {200, transee_worker:transports(City)};
handle(City, <<"positions">>, #{<<"type">> := T, <<"numbers">> := N}) when is_list(T), is_map(N) ->
    Result = lists:foldl(fun(Type, Acc) ->
        case maps:get(Type, N, undefined) of
            undefined -> Acc;
            Numbers   -> [transee_worker:positions(City, Type, Numbers)|Acc]
        end
    end, [], T),
    {200, Result};
handle(City, <<"positions">>, #{<<"type">> := T}) when is_list(T) ->
    Result = lists:map(fun(Type) ->
        transee_worker:positions(City, Type)
    end, T),
    {200, Result};
handle(City, <<"positions">>, #{<<"type">> := T, <<"numbers">> := N}) when is_list(N) ->
    {200, transee_worker:positions(City, T, N)};
handle(City, <<"positions">>, #{<<"type">> := T}) ->
    {200, transee_worker:positions(City, T)};
handle(City, <<"positions">>, _) ->
    {200, transee_worker:positions(City)};
handle(City, <<"routes">>, _) ->
    {200, transee_worker:routes(City)};
handle(City, <<"stations">>, _) ->
    {200, transee_worker:stations(City)};
handle(_City, _Method, _) ->
    {404, json_error(<<"method_not_found">>)}.

%%
%% Helpers
%%

params(Req) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> params(qs, cowboy_req:qs(Req));
        _         -> params(body, cowboy_req:body(Req))
    end.

params(qs, <<>>) -> undefined;
params(qs, Val) -> Val;
params(body, {ok, Val, _}) -> Val;
params(body, _) -> undefined.

json_error(Msg) ->
    [{<<"error">>, Msg}].

city_to_module(City) ->
    PName = list_to_atom("transee_city_" ++ binary_to_list(City)),
    try
        sys:get_status(PName), PName
    catch
        _:_ -> undefined
    end.
