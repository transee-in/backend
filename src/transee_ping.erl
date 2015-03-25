-module(transee_ping).
-behaviour(gen_server).
-include("transee.hrl").
-export([ start_link/0
        % gen_server callbacks
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%
%% API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% GenServer callbacks
%%

init(_) ->
    self() ! ping,
    {ok, []}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(ping, State) ->
    init_ping_timer(),
    send_ping(),
    {noreply, State};
handle_info(Info, State) ->
    {reply, Info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%

init_ping_timer() ->
    erlang:send_after(?PING_INTERVAL, self(), ping).

send_ping() ->
    influx:post(transee, ping, [{status, pong}]).
