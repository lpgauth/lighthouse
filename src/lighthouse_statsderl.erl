-module(lighthouse_statsderl).
-include("lighthouse.hrl").

-behaviour(lighthouse_metric).
-export([
    counter/2,
    gauge/2,
    timer/2
]).

-define(SAMPLING_RATE, 1).

%% public
-spec counter(metric_key(), metric_value()) ->
    ok.

counter(Key, Value) ->
    statsderl:counter(Key, Value, ?SAMPLING_RATE).

-spec gauge(metric_key(), metric_value()) ->
    ok.

gauge(Key, Value) ->
    statsderl:gauge(Key, Value, ?SAMPLING_RATE).

-spec timer(metric_key(), metric_value()) ->
    ok.

timer(Key, Value) ->
    statsderl:timing(Key, Value, ?SAMPLING_RATE).
