-module(statsd_server).

-export([
    start/0,
    stop/0
]).

-define(PORT, 8125).

%% public
start() ->
    put(statsd_server_pid, spawn(fun () ->
        loop(open())
    end)).

stop() ->
    get(statsd_server_pid) ! stop.

%% private
loop(Socket) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {{127, 0, 0, 1}, _Port, Request}} ->
            ets:insert(lighthouse_test, metric(Request)),
            loop(Socket);
        {error, closed} ->
            ok
    end,
    receive
        stop -> ok
    after
        0 ->
            loop(Socket)
    end.

metric(Request) ->
    [Key, ValueType] = binary:split(Request, <<":">>),
    case binary:split(ValueType, <<"|">>) of
        [Value, <<"c">>] ->
            {{counter, Key}, Value};
        [Value, <<"g">>] ->
            {{gauge, Key}, Value};
        [Value, <<"ms">>] ->
            {{timer, Key}, Value}
    end.

open() ->
    Options = [
        binary,
        {active, false},
        {reuseaddr, true}
    ],
    {ok, Socket} = gen_udp:open(?PORT, Options),
    Socket.
