-module(lighthouse_server).
-include("lighthouse.hrl").

-export([
    start_link/0
]).

-behaviour(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

-record(state, {
    plugins   :: map(),
    timer_ref :: reference()
}).

-type state() :: #state {}.

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    metal:start_link(?MODULE, ?MODULE, undefined).

%% metal callbacks
-spec init(atom(), pid(), undefined) ->
    {ok, state()}.

init(_Name, _Parent, undefined) ->
    {ok, #state {
        plugins = init_plugins(),
        timer_ref = timer()
    }}.

-spec handle_msg(term(), state()) ->
    {ok, state()}.

handle_msg(tick, #state {
        plugins = Plugins
    } = State) ->

    {ok, State#state {
        plugins = update_plugins(Plugins),
        timer_ref = timer()
    }}.

-spec terminate(term(), state()) ->
    ok.

terminate(_Reason, #state {
        timer_ref = TimerRef
    }) ->

    erlang:cancel_timer(TimerRef),
    ok.

%% private
init_plugins() ->
    lists:foldl(fun(Mod, State) ->
        State#{Mod => Mod:init()}
    end, #{}, lighthouse_plugin:active()).

timer() ->
    {_Mega, _Sec, Micro} = os:timestamp(),
    Delay = trunc((1000000 - Micro) / 1000),
    erlang:send_after(Delay, self(), tick).

update_plugins(Plugins) ->
    lists:foldl(fun(Mod, State) ->
        State#{Mod => Mod:update(maps:get(Mod, State))}
    end, Plugins, lighthouse_plugin:active()).
