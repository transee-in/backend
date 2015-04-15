-module(transee_city_vladimir).
-include("transee.hrl").
-define(city, <<"vladimir">>).
-define(url,  <<"http://bus33.su/php/">>).
-define(tz, 3 * 60 * 60). % GMT+0300 - 3 hours
-export([ coordinates/0
        , transports/1, transports/2
        , positions/1, positions/2
        , routes/1, routes/2
        , stations/1, stations/2
        , transport_info/2
        , station_info/3
        ]).

%%
%% Behavior
%%

coordinates() ->
    [56.13655, 40.39658].

transports(WorkerPid, Source) ->
    WorkerPid ! {update, transports, transports(Source)}.
transports(Source) ->
    transee_union_kondrahin:transports(Source).

positions(WorkerPid, Source) ->
    WorkerPid ! {update, positions, positions(Source)}.
positions(Source) ->
    transee_union_kondrahin:positions(?city,
        <<?url/binary, "getVehiclesMarkers.php">>, Source).

routes(WorkerPid, Source) ->
    WorkerPid ! {update, routes, routes(Source)}.
routes(Source) ->
    transee_union_kondrahin:routes(?city,
        <<?url/binary, "getRouteNodes.php">>, Source).

stations(WorkerPid, Source) ->
    WorkerPid ! {update, stations, stations(Source)}.
stations(Source) ->
    transee_union_kondrahin:stations(?city,
        <<?url/binary, "getStations.php">>, Source).

station_info(ID, Stations, Source) ->
    transee_union_kondrahin:station_info(?city,
        <<?url/binary, "getStationForecasts.php">>, ID, Stations, Source, ?tz).

transport_info(_ID, GosID) ->
    transee_union_kondrahin:transport_info(?city,
        <<?url/binary, "getVehicleForecasts.php">>, GosID, ?tz).
