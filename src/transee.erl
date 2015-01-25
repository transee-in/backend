-module(transee).
-export([ start/0
        , stop/0
        % helpers
        , config/1, config/2
        , priv_dir/1
        ]).


start() ->
    ibrowse:start(),
    application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE). 

%%
%% Helpers
%%

config(Key) ->
    config(Key, []).
config(Key, Default) ->
    case application:get_env(?MODULE, Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.

priv_dir(App) ->
   Ebin = filename:dirname(code:which(App)),
   filename:join(filename:dirname(Ebin), "priv").
