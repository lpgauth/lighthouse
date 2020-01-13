-module(lighthouse_metric).
-include("lighthouse.hrl").

-export([
    counter/2,
    gauge/2,
    init/0,
    terminate/0,
    timer/2
]).

%% behaviour
-callback counter(metric_key(), metric_value()) ->
    ok.

-callback gauge(metric_key(), metric_value()) ->
    ok.

-callback timer(metric_key(), metric_value()) ->
    ok.

%% public
-spec counter(metric_key(), metric_value()) ->
    ok.

counter(Key, Value) ->
    BaseKey = base_key(Key),
    lists:foreach(fun (Backend) ->
        Backend:counter(BaseKey, Value)
    end, backends()).

-spec gauge(metric_key(), metric_value()) ->
    ok.

gauge(Key, Value) ->
    BaseKey = base_key(Key),
    lists:foreach(fun (Backend) ->
        Backend:gauge(BaseKey, Value)
    end, backends()).

-spec init() ->
    ok.

init() ->
    foil:new(?MODULE),
    Backends = ?GET_ENV(?ENV_BACKENDS, ?DEFAULT_BACKENDS),
    foil:insert(?MODULE, ?ENV_BACKENDS, Backends),
    foil:load(?MODULE).

-spec terminate() ->
    ok.

terminate() ->
    foil:delete(?MODULE).

-spec timer(metric_key(), metric_value()) ->
    ok.

timer(Key, Value) ->
    BaseKey = base_key(Key),
    lists:foreach(fun (Backend) ->
        Backend:timer(BaseKey, Value)
    end, backends()).

%% private
% TODO: make configurable
base_key(Key) ->
    [<<"lighthouse.">>, Key].

backends() ->
    {ok, Backends} = foil:lookup(?MODULE, ?ENV_BACKENDS),
    Backends.
