-module(transee_http).
-export([request/3, request/4]).

request(Method, URL, Headers) ->
    request(Method, URL, Headers, 10000).

request(Method, URL, Headers, Timeout) ->
    HttpOptions = [{timeout, Timeout}],
    Options = [{body_format, binary}],
    case httpc:request(Method, {URL, Headers}, HttpOptions, Options) of
        {ok, {_Status, _Headers, Body}} ->
            lager:info("transee_http ok ~s ~s~n ~s~n", [Method, URL, Body]),
            {ok, Body};
        {error, timeout} ->
            lager:error("transee_http timeout ~s ~s", [Method, URL]),
            request(Method, URL, Headers, 20000);
        Error ->
            lager:error("transee_http error ~s ~s ~p", [Method, URL, Error]),
            undefined
    end.
