-module(transee_http).
-export([request/3]).

request(Method, URL, Headers) ->
    HttpOptions = [{timeout, 1000}],
    Options = [{body_format, binary}],
    case httpc:request(Method, {URL, Headers}, HttpOptions, Options) of
        {ok, {_Status, _Headers, Body}} ->
            {ok, Body};
        Error ->
            lager:error("~p", [Error]),
            undefined
    end.
