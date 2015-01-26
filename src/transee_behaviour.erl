-module(transee_behaviour).
-include("transee.hrl").

-callback transports(transport_list())
       -> transport_list_with_data().

-callback positions(transport_list())
       -> transport_list_with_data().

-callback routes(transport_list())
       -> transport_list_with_data().

-callback stations(transport_list())
       -> transport_list_with_data().
