%% @private
-module(lethink_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(atom(), [any()]) -> {error, any()} | {ok, pid()} | {ok, pid(), any()}.
start(_StartType, _StartArgs) ->
    lethink_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
