-include_lib("std/include/std.hrl").

-define(CHILD(Name, Module, Type, Args),
    {Name, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).
-define(dbg(V), io:format("~20s: ~p~n", [??V, V])).

-define(PING_INTERVAL,        60 * 60 * 1000). % 1 hour
-define(INIT_CITY_INTERVAL,   50).             % 50 ms
-define(RELOAD_CITY_INTERVAL, 30 * 1000).      % 30 seconds

-define(to_num(N), std_cast:to_number(N)).

-define(json_reponse, {<<"content-type">>, <<"application/json; charset=utf-8">>}).

-record(worker_state,
    { city            :: atom()
    , transports = [] :: []
    , positions  = [] :: []
    , routes     = [] :: []
    , stations   = [] :: []
    , source          :: []
    , timer           :: reference()
    , info            :: any()
    }).
