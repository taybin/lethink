-module(lethink).

-export([
        start/0,
        stop/0,
        add_pool/2,
        add_pool/3,
        remove_pool/1,
        use/2,
        query/2
    ]).

-type response() :: success() | error().
-type success() :: {ok, [any()]}.
-type error() :: {error, binary(), atom(), any()}.

-type document() :: {[keyvalue()]}.
-type keyvalue() :: {binary(), json_term()}.
-type json_term() :: null | boolean() | number() | binary() | document() | [json_term()].

-type connect_options() :: {address, inet:ip_address() | inet:hostname()} |
                           {port, inet:port_number()} |
                           {database, binary()}.

-type table_options() :: {datacenter, binary()} |
                         {primary_key, binary()} |
                         {cache_size, pos_integer()}.

-type insert_options() :: {upsert, binary()}.

-export_type([response/0,
              success/0,
              error/0,
              connect_options/0,
              table_options/0,
              insert_options/0,
              document/0,
              keyvalue/0,
              json_term/0]).

-spec start() -> ok.
start() ->
    application:start(lethink),
    ok.

-spec stop() -> ok.
stop() ->
    application:stop(lethink),
    ok.

%% @equiv add_pool(any(), pos_integer(), [{address, "localhost"}, {port, 28015}, {database, <<"test">>}])
-spec add_pool(any(), pos_integer()) -> ok.
add_pool(Ref, NWorkers) when NWorkers > 0 ->
    add_pool(Ref, NWorkers, []).

%% @doc Start a pool of connections to a database.
-spec add_pool(any(), pos_integer(), [connect_options()]) -> ok.
add_pool(Ref, NWorkers, Opts) when NWorkers > 0 ->
    ok = lethink_server:add_pool(Ref),
    {ok, SupPid} = supervisor:start_child(lethink_sup,
        {{lethink_workers_sup, Ref}, {lethink_workers_sup, start_link, []},
            permanent, 5000, supervisor, [lethink_workers_sup]}),
    _ = [begin
                {ok, _} = supervisor:start_child(
                    SupPid, [Ref, Opts])
        end || _ <- lists:seq(1, NWorkers)],
    ok.

%% @doc Stop a pool of connections.
-spec remove_pool(any()) -> ok.
remove_pool(Ref) ->
    case supervisor:terminate_child(lethink_sup, {lethink_workers_sup, Ref}) of
        ok ->
            supervisor:delete_child(lethink_sup, {lethink_workers_sup, Ref});
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Change all connections in pool to use database for queries.
-spec use(any(), binary()) -> ok.
use(Ref, Db) when is_binary(Db) ->
    WorkerPids = lethink_server:get_all_workers(Ref),
    lists:foreach(fun(Pid) ->
                lethink_worker:use(Pid, Db)
        end, WorkerPids).

-spec query(any(), list()) -> response().
query(Ref, OpList) ->
    Term = lethink_ast:build_query(OpList),
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:query(WorkerPid, Term).
