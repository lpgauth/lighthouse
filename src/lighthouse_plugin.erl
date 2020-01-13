-module(lighthouse_plugin).
-include("lighthouse.hrl").

-export([
    active/0,
    init/0,
    terminate/0
]).

%% behaviour
-callback init() ->
    plugin_state().

-callback update(plugin_state()) ->
    plugin_state().

%% public
-spec active() ->
    [plugin()].

active() ->
    {ok, Plugins} = foil:lookup(?MODULE, ?ENV_PLUGINS),
    Plugins.

-spec init() ->
    ok.

init() ->
    foil:new(?MODULE),
    Plugins = ?GET_ENV(?ENV_PLUGINS, ?DEFAULT_PLUGINS),
    Plugins2 = lists:usort(Plugins),
    foil:insert(?MODULE, ?ENV_PLUGINS, Plugins2),
    foil:load(?MODULE).

-spec terminate() ->
    ok.

terminate() ->
    foil:delete(?MODULE).
