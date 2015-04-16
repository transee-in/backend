-module(transee_http).
-export([request/3, request/4]).
-define(HTTP_REQUEST_TIMEOUT, 10000). % 10 seconds
-define(PROCESS_SLEEP, 1000).         % 1 second

request(Method, URL, Headers) ->
    request(Method, URL, Headers, ?HTTP_REQUEST_TIMEOUT).

request(Method, URL, Headers, Timeout) ->
    HttpOptions = [{timeout, Timeout}],
    Options = [{body_format, binary}],
    case httpc:request(Method, {URL, Headers}, HttpOptions, Options) of
        {ok, {_Status, _Headers, Body}} ->
            {ok, Body};
        {error, timeout} ->
            try_request_after(Method, URL, Headers, 20000);
        {error, {failed_connect, _}} ->
            try_request_after(Method, URL, Headers, 20000);
        {error, socket_closed_remotely} ->
            try_request_after(Method, URL, Headers);
        Error ->
            lager:error("transee_http:request error ~s ~s ~p", [Method, URL, Error]),
            undefined
    end.

try_request_after(Method, URL, Headers) ->
    try_request_after(Method, URL, Headers, ?HTTP_REQUEST_TIMEOUT).

try_request_after(Method, URL, Headers, Timeout) ->
    timer:sleep(?PROCESS_SLEEP),
    request(Method, URL, Headers, Timeout).
