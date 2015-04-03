-module(transee_time).
-export([now/0]).

now() ->
    os:timestamp().
