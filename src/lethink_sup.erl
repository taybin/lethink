%% @private
-module(lethink_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    lethink_server = ets:new(lethink_server, [
            ordered_set, public, named_table, {read_concurrency, true}]),
    Procs = [
        {lethink_server, {lethink_server, start_link, []},
            permanent, 5000, worker, [lethink_server]}
    ],
    {ok, {{one_for_one, 10, 10}, Procs}}.
