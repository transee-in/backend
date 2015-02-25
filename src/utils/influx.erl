-module(influx).
-export([post/3]).

post(DB, Name, DataPL) ->
    post_request(make_url(DB),
        [ {<<"name">>, Name}
        , {<<"columns">>, keys(DataPL)}
        , {<<"points">>, [values(DataPL)]}
        ]).

%%
%% Helpers
%%

keys(PL) ->
    proplists:get_keys(PL).

values(PL) ->
    lists:map(fun(K) ->
        proplists:get_value(K, PL)
    end, keys(PL)).

make_url(DB) ->
    "http://localhost:8086/db/" ++ std_cast:to_list(DB) ++ "/series?u=root&p=root".

post_request(URL, Data) ->
    io:format("json: ~s~n", [jsx:encode([Data])]),
    Request = {URL, [], "application/json", jsx:encode([Data])},
    case httpc:request(post, Request, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            ok;
        {ok, {_, _, Reason}} ->
            {error, Reason};
        Error ->
            Error
    end.
