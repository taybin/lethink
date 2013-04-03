-module(lethink).

-export([
        start/0,
        stop/0,
        add_pool/2,
        add_pool/3,
        remove_pool/1,
        use/2,
        db_create/2,
        db_drop/2,
        db_list/1
        %% table_create/2,
        %% table_drop/2,
        %% table_list/1
    ]).

-spec start() -> ok.
start() ->
    application:start(lethink),
    ok.

-spec stop() -> ok.
stop() ->
    application:stop(lethink),
    ok.

%% @doc Start a pool of connections to a database.
-spec add_pool(any(), pos_integer()) -> ok.
add_pool(Ref, NWorkers) when NWorkers > 0 ->
    add_pool(Ref, NWorkers, []).

-spec add_pool(any(), pos_integer(), [lethink_worker:opts()]) -> ok.
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

-spec use(any(), binary()) -> ok.
use(Ref, Db) ->
    WorkerPids = lethink_server:get_all_workers(Ref),
    lists:foreach(fun(Pid) ->
                lethink_worker:use(Pid, Db)
        end, WorkerPids).

-spec db_create(any(), binary()) -> {ok, binary()} | lethink_worker:res_error().
db_create(Ref, Db) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:db_create(WorkerPid, Db).

-spec db_drop(any(), binary()) -> {ok, binary()} | lethink_worker:res_error().
db_drop(Ref, Db) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:db_drop(WorkerPid, Db).

-spec db_list(any()) -> [binary()].
db_list(Ref) ->
    WorkerPid = lethink_server:get_worker(Ref),
    lethink_worker:db_list(WorkerPid).

%% -spec table_create(any(), binary()) -> ok.
%% table_create(Ref, Table) ->
%%     WorkerPid = lethink_server:get_worker(Ref),
%%     lethink_worker:table_create(WorkerPid, Table).
%%
%% -spec table_drop(any(), binary()) -> ok.
%% table_drop(Ref, Table) ->
%%     WorkerPid = lethink_server:get_worker(Ref),
%%     lethink_worker:table_drop(WorkerPid, Table).
%%
%% -spec table_list(any()) -> [binary()].
%% table_list(Ref) ->
%%     WorkerPid = lethink_server:get_worker(Ref),
%%     lethink_worker:table_list(WorkerPid).
