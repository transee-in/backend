-module(transee_json).
-export([maybe/2, maybe/3]).

maybe(Path, JSON) ->
    maybe(Path, JSON, []).

maybe(Path, JSON, Default) ->
    try
        json:get(Path, JSON)
    catch _:_ ->
        Default
    end.
