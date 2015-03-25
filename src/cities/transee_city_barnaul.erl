-module(transee_city_barnaul).
-include("transee.hrl").
-define(city, <<"barnaul">>).
-define(tz, 6 * 60 * 60). % GMT+0600 - 6 hours
-export([ transports/1, transports/2
        , positions/1, positions/2
        , routes/1, routes/2
        , stations/1, stations/2
        , transport_info/2
        , station_info/3
        ]).

%%
%% Behavior
%%

transports(WorkerPid, Source) ->
    WorkerPid ! {update, transports, transports(Source)}.
transports(Source) ->
    transee_union_kondrahin:transports(Source).

positions(WorkerPid, Source) ->
    WorkerPid ! {update, positions, positions(Source)}.
positions(Source) ->
    transee_union_kondrahin:positions(?city,
        <<"http://traffic22.ru/php/getVehiclesMarkers.php">>, Source).

routes(WorkerPid, Source) ->
    WorkerPid ! {update, routes, routes(Source)}.
routes(Source) ->
    transee_union_kondrahin:routes(?city,
        <<"http://traffic22.ru/php/getRouteNodes.php">>, Source).

stations(WorkerPid, Source) ->
    WorkerPid ! {update, stations, stations(Source)}.
stations(Source) ->
    transee_union_kondrahin:stations(?city,
        <<"http://traffic22.ru/php/getStations.php">>, Source).

station_info(ID, Stations, Source) ->
    transee_union_kondrahin:station_info(?city,
        <<"http://traffic22.ru/php/getStationForecasts.php">>, ID, Stations, Source, ?tz).

transport_info(_ID, GosID) ->
    transee_union_kondrahin:transport_info(?city,
        <<"http://traffic22.ru/php/getVehicleForecasts.php">>, GosID, ?tz).
