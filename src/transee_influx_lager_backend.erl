-module(transee_influx_lager_backend).
-behaviour(gen_event).
-include_lib("lager/include/lager.hrl").
-record(state, {db, name, level}).
-export([ init/1
        , handle_call/2
        , handle_event/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).


init(Options) ->
    Level = proplists:get_value(level, Options, error),
    State = #state{ db    = proplists:get_value(db, Options)
                  , name  = proplists:get_value(name, Options)
                  , level = lager_util:level_to_num(Level)
                  },
    {ok, State}.

handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level = lager_util:level_to_num(Level)}};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message}, #state{level = Level} = State) ->
    case lager_util:is_loggable(Message, Level, []) of
        true  -> log(Level, State);
        false -> undefined
    end,
    {ok, State};
handle_event(Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private
%%

log(Level, #state{db = DB, name = Name}) ->
    influx:post(transee, DB, [{Name, error}]).
