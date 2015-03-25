-module(transee_city_vladivostok).
-include("transee.hrl").
-export([transports/1, positions/1, routes/1, stations/1, transport_info/2, station_info/3]).
-define(city, <<"vladivostok">>).
-define(tz, 10 * 60 * 60). % GMT+1000 - 10 hours
-define(to_num(N), (std_cast:to_number(N))).

%%
%% Behavior
%%

transports(Source) ->
    transee_union_kondrahin:transports(Source).

positions(Source) ->
    transee_union_kondrahin:positions(?city,
        <<"http://bus125.ru/php/getVehiclesMarkers.php">>, Source).

routes(Source) ->
    transee_union_kondrahin:routes(?city,
        <<"http://bus125.ru/php/getRouteNodes.php">>, Source).

stations(Source) ->
    transee_union_kondrahin:stations(?city,
        <<"http://bus125.ru/php/getStations.php">>, Source).

station_info(ID, Stations, Source) ->
    transee_union_kondrahin:station_info(?city,
        <<"http://bus125.ru/php/getStationForecasts.php">>, ID, Stations, Source, ?tz).

transport_info(_ID, GosID) ->
    transee_union_kondrahin:transport_info(?city,
        <<"http://bus125.ru/php/getVehicleForecasts.php">>, GosID, ?tz).

%%
%% Helpers
%%
