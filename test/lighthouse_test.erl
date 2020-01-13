-module(lighthouse_test).
-include("lighthouse.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(lighthouse_metric).
-export([
    counter/2,
    gauge/2,
    timer/2
]).

-behaviour(lighthouse_plugin).
-export([
    init/0,
    update/1
]).

%% tests
lighthouse_linux_test() ->
    case os:type() of
        {_, linux} ->
            assert_plugin(lighthouse_linux, [
                {gauge, <<"lighthouse.meminfo.buffers">>},
                {gauge, <<"lighthouse.meminfo.cached">>},
                {gauge, <<"lighthouse.meminfo.mem_free">>},
                {gauge, <<"lighthouse.meminfo.mem_total">>},
                {gauge, <<"lighthouse.meminfo.swap_cached">>},
                {gauge, <<"lighthouse.meminfo.swap_free">>},
                {gauge, <<"lighthouse.meminfo.swap_total">>}
            ]);
        _ ->
            assert_plugin(lighthouse_linux, [])
    end.


lighthouse_msacc_test() ->
    assert_plugin(lighthouse_msacc, [
        {gauge, <<"lighthouse.async_aux">>},
        {gauge, <<"lighthouse.async_check_io">>},
        {gauge, <<"lighthouse.async_emulator">>},
        {gauge, <<"lighthouse.async_gc">>},
        {gauge, <<"lighthouse.async_other">>},
        {gauge, <<"lighthouse.async_port">>},
        {gauge, <<"lighthouse.async_sleep">>},
        {gauge, <<"lighthouse.aux_aux">>},
        {gauge, <<"lighthouse.aux_check_io">>},
        {gauge, <<"lighthouse.aux_emulator">>},
        {gauge, <<"lighthouse.aux_gc">>},
        {gauge, <<"lighthouse.aux_other">>},
        {gauge, <<"lighthouse.aux_port">>},
        {gauge, <<"lighthouse.aux_sleep">>},
        {gauge, <<"lighthouse.dirty_cpu_scheduler_aux">>},
        {gauge, <<"lighthouse.dirty_cpu_scheduler_check_io">>},
        {gauge, <<"lighthouse.dirty_cpu_scheduler_emulator">>},
        {gauge, <<"lighthouse.dirty_cpu_scheduler_gc">>},
        {gauge, <<"lighthouse.dirty_cpu_scheduler_other">>},
        {gauge, <<"lighthouse.dirty_cpu_scheduler_port">>},
        {gauge, <<"lighthouse.dirty_cpu_scheduler_sleep">>},
        {gauge, <<"lighthouse.dirty_io_scheduler_aux">>},
        {gauge, <<"lighthouse.dirty_io_scheduler_check_io">>},
        {gauge, <<"lighthouse.dirty_io_scheduler_emulator">>},
        {gauge, <<"lighthouse.dirty_io_scheduler_gc">>},
        {gauge, <<"lighthouse.dirty_io_scheduler_other">>},
        {gauge, <<"lighthouse.dirty_io_scheduler_port">>},
        {gauge, <<"lighthouse.dirty_io_scheduler_sleep">>},
        {gauge, <<"lighthouse.poll_aux">>},
        {gauge, <<"lighthouse.poll_check_io">>},
        {gauge, <<"lighthouse.poll_emulator">>},
        {gauge, <<"lighthouse.poll_gc">>},
        {gauge, <<"lighthouse.poll_other">>},
        {gauge, <<"lighthouse.poll_port">>},
        {gauge, <<"lighthouse.poll_sleep">>},
        {gauge, <<"lighthouse.scheduler_aux">>},
        {gauge, <<"lighthouse.scheduler_check_io">>},
        {gauge, <<"lighthouse.scheduler_emulator">>},
        {gauge, <<"lighthouse.scheduler_gc">>},
        {gauge, <<"lighthouse.scheduler_other">>},
        {gauge, <<"lighthouse.scheduler_port">>},
        {gauge, <<"lighthouse.scheduler_sleep">>}
    ]).

lighthouse_statsderl_test() ->
    statsd_server:start(),
    assert_backend(lighthouse_statsderl, [
        {{counter, <<"lighthouse.test">>}, <<"1">>},
        {{gauge, <<"lighthouse.test">>}, <<"1">>},
        {{timer, <<"lighthouse.test">>}, <<"1">>}
    ]),
    statsd_server:stop().

lighthouse_vm_test() ->
    assert_plugin(lighthouse_vm, [
        {counter, <<"lighthouse.gc.count">>},
        {counter, <<"lighthouse.gc.words_reclaimed">>},
        {counter, <<"lighthouse.io.bytes_in">>},
        {counter, <<"lighthouse.io.bytes_out">>},
        {counter, <<"lighthouse.reductions">>},
        {gauge, <<"lighthouse.atom_count">>},
        {gauge, <<"lighthouse.atom_limit">>},
        {gauge, <<"lighthouse.ets_count">>},
        {gauge, <<"lighthouse.ets_limit">>},
        {gauge, <<"lighthouse.memory.atom_used">>},
        {gauge, <<"lighthouse.memory.binary">>},
        {gauge, <<"lighthouse.memory.ets">>},
        {gauge, <<"lighthouse.memory.procs_used">>},
        {gauge, <<"lighthouse.memory.total">>},
        {gauge, <<"lighthouse.modules">>},
        {gauge, <<"lighthouse.port_count">>},
        {gauge, <<"lighthouse.port_limit">>},
        {gauge, <<"lighthouse.proc_count">>},
        {gauge, <<"lighthouse.proc_limit">>},
        {gauge, <<"lighthouse.total_run_queue_lengths_all">>},
        {gauge, <<"lighthouse.uptime_minutes">>}
    ]).

%% private
assert_backend(Backend, ExpectedKeysValues) ->
    setup([Backend], [lighthouse_test]),
    timer:sleep(timer:seconds(1)),
    KeysValues = ets:tab2list(?MODULE),
    ?assertEqual(ExpectedKeysValues, lists:usort(KeysValues)),
    cleanup().

assert_plugin(Plugin, ExpectedKeys) ->
    setup([lighthouse_test], [Plugin]),
    timer:sleep(timer:seconds(1)),
    Keys = [Key || {Key, _} <- ets:tab2list(?MODULE)],
    ?assertEqual(ExpectedKeys, lists:usort(Keys)),
    io:format("~p~n", [Keys]),
    cleanup().

cleanup() ->
    lighthouse_app:stop(),
    ets:delete(?MODULE).

setup(Backends, Plugins) ->
    application:start(sasl),
    % error_logger:tty(false),
    ets:new(?MODULE, [named_table, public]),
    application:load(?APP),
    application:set_env(?APP, ?ENV_BACKENDS, Backends),
    application:set_env(?APP, ?ENV_PLUGINS, Plugins),
    lighthouse_app:start().

%% lighthouse_metric callbacks
counter(Key, Value) ->
    ets:insert(?MODULE, {{counter, iolist_to_binary(Key)}, Value}).

gauge(Key, Value) ->
    ets:insert(?MODULE, {{gauge, iolist_to_binary(Key)}, Value}).

timer(Key, Value) ->
    ets:insert(?MODULE, {{timer, iolist_to_binary(Key)}, Value}).

%% lighthouse_plugin callbacks
init() ->
    undefined.

update(undefined) ->
    lighthouse_metric:counter(<<"test">>, 1),
    lighthouse_metric:gauge(<<"test">>, 1),
    lighthouse_metric:timer(<<"test">>, 1),
    undefined.
