%% macros
-define(APP, lighthouse).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(GET_ENV(Key, Default), application:get_env(?APP, Key, Default)).

% metrics
-define(COUNTER(Key, Value), lighthouse_metric:counter(Key, Value)).
-define(GAUGE(Key, Value), lighthouse_metric:gauge(Key, Value)).
-define(TIMER(Key, Value), lighthouse_metric:timer(Key, Value)).

%% types
-type metric_key() :: iodata().
-type metric_value() :: number().
-type plugin() :: module().
-type plugin_state() :: term().

%% defaults
-define(DEFAULT_BACKENDS, [lighthouse_statsderl]).
-define(DEFAULT_PLUGINS, [
    lighthouse_linux,
    lighthouse_msacc,
    lighthouse_vm
]).

%% env
-define(ENV_BACKENDS, backends).
-define(ENV_PLUGINS, plugins).
