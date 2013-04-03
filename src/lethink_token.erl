-module(lethink_token).

-export([start/0,
         get/0]).

-spec start() -> ok.
start() ->
    ets:new(?MODULE, [named_table, public]),
    ets:insert(?MODULE, {token, 0}),
    ok.

-spec get() -> integer().
get() ->
    ets:update_counter(?MODULE, token, 1).
