-module(transee_http).
-export([request/3]).

request(Method, URL, Headers) ->
    case httpc:request(Method, {URL, Headers},
        [], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} -> {ok, Body};
        _ -> undefined
    end.
