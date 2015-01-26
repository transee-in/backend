-module(transee_index_handler).
-export([init/2]).

init(Req, State) ->
    {ok, Body} = index_view:render([]),
    Reply = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/html">>}
    ], Body, Req),
    {ok, Reply, State}.
