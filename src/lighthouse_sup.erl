-module(lighthouse_sup).
-include("lighthouse.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) ->
    {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.

init([]) ->
    lighthouse_metric:init(),
    lighthouse_plugin:init(),

    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(lighthouse_server)
    ]}}.
