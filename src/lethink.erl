-module(lethink).

-export([
        start/0,
        stop/0,
        add_pool/2,
        add_pool/3,
        remove_pool/1,
        use/2,
        query/2,
        db_create/2,
        db_drop/2,
        db_list/1,
        table_create/2,
        table_create/3,
        table_drop/2,
        table_list/1
    ]).

-type response() :: success() | error().
-type success() :: {ok, [any()]}.
-type error() :: {error, binary(), atom(), any()}.

-type json() :: null | boolean() | integer() | float() | binary() | {binary(), json()} | [json()].

-type connect_options() :: {address, inet:ip_address() | inet:hostname()} |
                           {port, inet:port_number()} |
                           {database, binary()}.

-type table_options() :: {datacenter, binary()} |
                         {primary_key, binary()} |
                         {cache_size, pos_integer()}.

-export_type([response/0,
              success/0,
              error/0,
              connect_options/0,
              table_options/0]).

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

query(Ref, OpList) ->
    Term = lists:foldl(fun lethink_ast:build_query/2, [], OpList),
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:query(WorkerPid, Term).

-spec db_create(any(), binary()) -> response().
db_create(Ref, Db) when is_binary(Db) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:db_create(WorkerPid, Db).

-spec db_drop(any(), binary()) -> response().
db_drop(Ref, Db) when is_binary(Db) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:db_drop(WorkerPid, Db).

-spec db_list(any()) -> response().
db_list(Ref) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:db_list(WorkerPid).

%% @equiv table_create(any(), binary(), [])
-spec table_create(any(), binary()) -> response().
table_create(Ref, Table) when is_binary(Table) ->
    table_create(Ref, Table, []).

-spec table_create(any(), binary(), [table_options()]) -> response().
table_create(Ref, Table, Opts) when is_binary(Table) ->
     WorkerPid = lethink_server:get_worker(Ref),
     lethink_worker:table_create(WorkerPid, Table, Opts).

-spec table_drop(any(), binary()) -> response().
table_drop(Ref, Table) when is_binary(Table) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:table_drop(WorkerPid, Table).

-spec table_list(any()) -> response().
table_list(Ref) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:table_list(WorkerPid).
