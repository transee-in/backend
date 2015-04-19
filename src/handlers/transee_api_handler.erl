-module(transee_api_handler).
-include("transee.hrl").
-export([init/2]).

init(Req, State) ->
    Body  = jsx:encode(json(version())),
    Reply = cowboy_req:reply(200, [?json_reponse], Body, Req),

    spawn(fun() ->
        influx:post(transee, api_request, [{method, <<"root">>}])
    end),

    {ok, Reply, State}.

%%
%% Helpers
%%

json({Date, Hash}) ->
    [ {<<"updated_at">>, ?to_bin(Date)}
    , {<<"revision">>,   ?to_bin(Hash)}
    ].

version() ->
    std_cache:set(transee_updated_at, fun() ->
        {remove_nl(git_last_commit_date()), remove_nl(git_last_commit_hash())}
    end).

git_last_commit_date() ->
    os:cmd("git log -1 --format=%cd --date=local").

git_last_commit_hash() ->
    os:cmd("git log -1 --format=%h").

remove_nl(V) ->
    re:replace(V, "[\\n]", "").
