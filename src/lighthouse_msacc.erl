-module(lighthouse_msacc).
-include("lighthouse.hrl").

-behaviour(lighthouse_plugin).
-export([
    init/0,
    update/1
]).

%% public
-spec init() ->
    map().

init() ->
    erlang:system_flag(microstate_accounting, true),
    sum().

-spec update(map()) ->
    map().

update(MsAcc) ->
    MsAcc2 = sum(),
    Diff = diff(MsAcc, MsAcc2),
    lists:foreach(fun (Key) ->
        Counters = maps:get(Key, Diff),
        lists:foreach(fun (Key2) ->
            Key3 = [atom_to_list(Key), "_", atom_to_list(Key2)],
            ?GAUGE(Key3, maps:get(Key2, Counters))
        end, maps:keys(Counters))
    end, maps:keys(Diff)),

    MsAcc2.

%% private
diff(MsAcc, MsAcc2) ->
    lists:foldl(fun (Key, Acc) ->
        Counters = maps:get(Key, MsAcc),
        Counters2 = maps:get(Key, MsAcc2),
        Diff = diff_counters(Counters, Counters2),
        Acc#{Key => Diff}
    end, #{}, maps:keys(MsAcc)).

diff_counters(Counters, Counters2) ->
    lists:foldl(fun (Key, Acc) ->
        Counter = maps:get(Key, Counters),
        Counter2 = maps:get(Key, Counters2),
        Acc#{Key => (Counter2 - Counter)}
    end, #{}, maps:keys(Counters)).

sum() ->
    sum(erlang:statistics(microstate_accounting), #{}).

sum([], Acc) ->
    Acc;
sum([#{type := Type, counters := Counters} | T], Acc) ->
    case maps:get(Type, Acc, undefined) of
        undefined ->
            sum(T, Acc#{Type => Counters});
        Counters2 ->
            SumCounters = sum_counters(Counters, Counters2),
            sum(T, Acc#{Type => SumCounters})
    end.

sum_counters(Counters, Counters2) ->
    lists:foldl(fun (Key, Acc) ->
        Counter = maps:get(Key, Counters),
        Counter2 = maps:get(Key, Counters2),
        Acc#{Key => Counter + Counter2}
    end, #{}, maps:keys(Counters)).
