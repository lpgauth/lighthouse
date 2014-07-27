-module(lighthouse_app).

-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(lighthouse),
    ok.

%% application callbacks
start(_StartType, _StartArgs) ->
    lighthouse_sup:start_link().

stop(_State) ->
    ok.