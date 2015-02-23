-module(transee_api_city_handler).
-include("transee.hrl").
-export([init/2]).

init(Req, State) ->
    City   = city_to_module(cowboy_req:binding(city, Req)),
    Method = cowboy_req:binding(method, Req),
    Params = case params(Req) of
        undefined -> no_args;
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
handle(City, <<"positions">>, no_args) ->
    {200, transee_worker:positions(City)};
handle(City, <<"positions">>, _) ->
    {404, json_error(<<"badly_formed_filter">>)};
handle(City, <<"routes">>, _) ->
    {200, transee_worker:routes(City)};
handle(City, <<"stations">>, _) ->
    {200, transee_worker:stations(City)};
handle(City, <<"station_info">>, #{<<"id">> := ID}) ->
    {200, transee_worker:station_info(City, ID)};
handle(City, <<"transport_info">>, #{<<"type">> := T, <<"gos_id">> := GosID}) ->
    {200, transee_worker:transport_info(City, T, GosID)};
handle(_City, _Method, _) ->
    {404, json_error(<<"method_not_found">>)}.

%%
%% Helpers
%%

params(Req) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> qs_params(cowboy_req:qs(Req));
        _         -> body_params(cowboy_req:body(Req))
    end.

qs_params(<<>>) -> undefined;
qs_params(Val) -> Val.

body_params({ok, Val, _}) -> Val;
body_params(_) -> undefined.

json_error(Msg) ->
    [{<<"error">>, Msg}].

city_to_module(<<"yaroslavl">>) -> transee_city_yaroslavl;
city_to_module(_) -> undefined.
