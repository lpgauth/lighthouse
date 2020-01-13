-module(lighthouse_linux).
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
    case os:type() of
        {_, linux} ->
            #{};
        _ ->
            undefined
    end.

-spec update(map() | undefined) ->
    map() | undefined.

update(undefined) ->
    undefined;
update(State) ->
    meminfo(),
    State.

%% private
meminfo() ->
    {ok, File} = file:open("/proc/meminfo", [read, binary]),
    meminfo(File).

meminfo(File) ->
    case file:read_line(File) of
        {ok, Line} ->
            Line2 = re:split(Line, "\\s+", [trim]),
            meminfo_parse(Line2),
            meminfo(File);
        eof ->
            ok;
        {error, _} ->
            ok
    end.

meminfo_parse([<<"MemTotal:">>, Val, _]) ->
    ?GAUGE(<<"meminfo.mem_total">>, binary_to_integer(Val));
meminfo_parse([<<"MemFree:">>, Val, _]) ->
    ?GAUGE(<<"meminfo.mem_free">>, binary_to_integer(Val));
meminfo_parse([<<"Buffers:">>, Val, _]) ->
    ?GAUGE(<<"meminfo.buffers">>, binary_to_integer(Val));
meminfo_parse([<<"Cached:">>, Val, _]) ->
    ?GAUGE(<<"meminfo.cached">>, binary_to_integer(Val));
meminfo_parse([<<"SwapCached:">>, Val, _]) ->
    ?GAUGE(<<"meminfo.swap_cached">>, binary_to_integer(Val));
meminfo_parse([<<"SwapTotal:">>, Val, _]) ->
    ?GAUGE(<<"meminfo.swap_total">>, binary_to_integer(Val));
meminfo_parse([<<"SwapFree:">>, Val, _]) ->
    ?GAUGE(<<"meminfo.swap_free">>, binary_to_integer(Val));
meminfo_parse(_) ->
    ok.
