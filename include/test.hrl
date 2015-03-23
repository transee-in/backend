-include_lib("eunit/include/eunit.hrl").

-define(mock_http_with_response(City, Filename, Fun), ((fun() ->
    {ok, __CWD} = file:get_cwd(),
    __Path = filename:join([__CWD, "..", "test", "cities", City, Filename]),
    {ok, __Content} = file:read_file(__Path),
    meck:new(transee_http, [unstick, passthrough]),
    meck:expect(transee_http, request, fun(_,_,_) -> {ok, __Content} end),
    try
        Fun()
    after
        meck:unload(transee_http)
    end
end)())).

-define(assert_json(JSON), json:from_binary(jsx:encode(JSON))).
-define(open_source(Mod), transee_worker:open_source(Mod)).
