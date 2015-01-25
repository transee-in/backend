-define(CHILD(Name, Module, Type, Args),
    {Name, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

-define(INIT_CITY_INTERVAL,   1000). % 1 second
-define(RELOAD_CITY_INTERVAL, 30000). % 30 seconds

-type lat()
   :: binary().
-type lon()
   :: binary().
-type angle()
   :: binary().
-type item_id()
   :: binary().

-type transport_number()
   :: binary().
-type transport_id()
   :: binary().
-type transport_name()
   :: string().
-type transport_type()
   :: {transport_name(), any()}.
-type transport_list()
   :: [{transport_type(), [{transport_id(), transport_number()}]}].
-type transport_list_with_data()
   :: [{transport_type(), [{transport_number(), any()}]}].
-type transport_route()
   :: {lat(), lon()}.
-type transport_station()
   :: {lat(), lon()}.

-record(worker_state,
    { city            :: atom()
    , transports = [] :: transport_list()
    , positions  = [] :: list(term())
    , routes     = [] :: list(term())
    , stations   = [] :: list(term())
    , info            :: any()
    }).

-record(transport_position,
    { lat   :: lat()
    , lon   :: lon()
    , angle :: angle()
    }).
