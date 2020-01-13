-module(lighthouse_app).
-include("lighthouse.hrl").

-export([
    start/0,
    stop/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
-spec start() ->
    {ok, [atom()]} | {error, term()}.

start() ->
    application:ensure_all_started(?APP).

-spec stop() ->
    ok | {error, term()}.

stop() ->
    application:stop(?APP).

%% application callbacks
-spec start(application:start_type(), term()) ->
    {ok, pid()}.

start(_StartType, _StartArgs) ->
    lighthouse_sup:start_link().

-spec stop(term()) ->
    ok.

stop(_State) ->
    lighthouse_metric:terminate(),
    lighthouse_plugin:terminate(),
    ok.
