-include_lib("eunit/include/eunit.hrl").

-define(mock_http(Data), ((fun() ->
    meck:new(httpc, [unstick, passthrough]),
    meck:expect(httpc, request, fun(_,_,_,_) -> {ok, {s, h, Data}} end)
end)())).

-define(mock_http_with_response(City, Filename), ((fun() ->
    __Resp = ?load_sample_response(City, Filename),
    ?mock_http(__Resp)
end)())).

-define(unload_mock, meck:unload(httpc)).

-define(assert_json(JSON), json:from_binary(jsx:encode(JSON))).
-define(open_source(Mod), transee_worker:open_source(Mod)).

-define(load_sample_response(City, Filename), ((fun() ->
    {ok, __CWD} = file:get_cwd(),
    __Path = filename:join([__CWD, "..", "test", "cities", City, Filename]),
    {ok, __Content} = file:read_file(__Path),
    __Content
end)())).
