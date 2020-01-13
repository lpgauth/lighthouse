% TODO:
% erlang:system_info/1
% erlang:statistics/1
% ets:info/0
% logger?

-module(lighthouse_vm).
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
    #{
        gc_statistics => gc_statistics(),
        io_statistics => io_statistics(),
        timestamp => os:timestamp()
    }.

-spec update(map()) ->
    map().

update(#{
        gc_statistics := {NumberGCs, WordsReclaimed, _},
        io_statistics := {IoInput, IoOutput},
        timestamp := Timestamp
    } = State) ->

    % code
    ?GAUGE(<<"modules">>, length(code:all_loaded())),

    %% memory
    Memory = erlang:memory(),
    ?GAUGE(<<"memory.total">>, lookup_mb(total, Memory)),
    ?GAUGE(<<"memory.procs_used">>, lookup_mb(processes_used, Memory)),
    ?GAUGE(<<"memory.atom_used">>, lookup_mb(atom_used, Memory)),
    ?GAUGE(<<"memory.binary">>, lookup_mb(binary, Memory)),
    ?GAUGE(<<"memory.ets">>, lookup_mb(ets, Memory)),

    %% statistics
    % active_tasks_all
    % context_switches
    GcStatistics = {NumberGCs2, WordsReclaimed2, _} = gc_statistics(),
    ?COUNTER(<<"gc.count">>, NumberGCs2 - NumberGCs),
    ?COUNTER(<<"gc.words_reclaimed">>, WordsReclaimed2 - WordsReclaimed),
    IoStatistics = {IoInput2, IoOutput2} = io_statistics(),
    ?COUNTER(<<"io.bytes_in">>, IoInput2 - IoInput),
    ?COUNTER(<<"io.bytes_out">>, IoOutput2 - IoOutput),
    {_, Reductions} = erlang:statistics(reductions),
    ?COUNTER(<<"reductions">>, Reductions),
    ?GAUGE(<<"total_run_queue_lengths_all">>,
        erlang:statistics(total_run_queue_lengths)),
    ?GAUGE(<<"total_run_queue_lengths_all">>,
            erlang:statistics(total_run_queue_lengths_all)),

    %% system info
    ?GAUGE(<<"atom_count">>, erlang:system_info(atom_count)),
    ?GAUGE(<<"atom_limit">>, erlang:system_info(atom_limit)),
    ?GAUGE(<<"ets_count">>, erlang:system_info(ets_count)),
    ?GAUGE(<<"ets_limit">>, erlang:system_info(ets_limit)),
    ?GAUGE(<<"port_count">>, erlang:system_info(port_count)),
    ?GAUGE(<<"port_limit">>, erlang:system_info(port_limit)),
    ?GAUGE(<<"proc_count">>, erlang:system_info(process_count)),
    ?GAUGE(<<"proc_limit">>, erlang:system_info(process_limit)),

    % uptime
    Timestamp2 = os:timestamp(),
    Uptime = timer:now_diff(Timestamp2, Timestamp) / 60000000,
    ?GAUGE(<<"uptime_minutes">>, Uptime),

    State# {
        gc_statistics => GcStatistics,
        io_statistics => IoStatistics
    }.

%% private
gc_statistics() ->
    erlang:statistics(garbage_collection).

io_statistics() ->
    {{input, IoInput}, {output, IoOutput}} = erlang:statistics(io),
    {IoInput, IoOutput}.

lookup_mb(Key, List) ->
    {_, Value} = lists:keyfind(Key, 1, List),
    Value / 1048576.
