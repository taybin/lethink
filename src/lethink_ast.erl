-module(lethink_ast).

-export([build_query/1]).

% @private
-export([db_create/2,
        db_drop/2,
        db_list/1,
        table_create/2, table_create/3,
        table_drop/2,
        table_list/1,
        db/2,
        table/2, table/3,
        insert/2, insert/3,
        get/2]).

-include("ql2_pb.hrl").

-type build_result() :: #term{} | {error, binary()}.

%% @doc Build rethinkdb query from operation list
-spec build_query([tuple()]) -> build_result().
build_query(QueryList) ->
    apply_seq(QueryList, []).

%% @private
%% @doc foldl-inspired poor man's monad
-spec apply_seq([fun()], [] | #term{} | {error, any()}) -> build_result().
apply_seq(_, {error, Reason}) ->
    {error, Reason};
apply_seq([T | Ts], Term) ->
    [Fun | Args] = tuple_to_list(erlang:append_element(T, Term)),
    apply_seq(Ts, apply(?MODULE, Fun, Args));
apply_seq([], Result) -> Result.

-spec db_create(binary(), []) -> build_result().
db_create(Name, []) when is_binary(Name) ->
    #term{
        type = 'DB_CREATE',
        args = ql2_util:datum_term(Name)
    };
db_create(Name, _) when is_list(Name) ->
    {error, <<"db_create name must be binary">>};
db_create(_, _) ->
    {error, <<"db_create stands alone">>}.

-spec db_drop(binary(), []) -> build_result().
db_drop(Name, []) when is_binary(Name) ->
    #term{
        type = 'DB_DROP',
        args = ql2_util:datum_term(Name)
    }.

-spec db_list([]) -> build_result().
db_list([]) ->
    #term{
        type = 'DB_LIST'
    }.

-spec table_create(binary(), #term{} | []) -> build_result().
table_create(Name, Term) ->
    table_create(Name, [], Term).

-spec table_create(binary(), [lethink:table_options()], #term{} | []) -> build_result().
table_create(Name, Options, []) when is_binary(Name) ->
    #term{
        type = 'TABLE_CREATE',
        args = ql2_util:datum_term(Name),
        optargs = [ table_option_term(Opt) || Opt <- Options ]
    };
table_create(Name, Options, #term{ type = 'DB' } = Db) when is_binary(Name) ->
    #term{
        type = 'TABLE_CREATE',
        args = [Db, ql2_util:datum_term(Name)],
        optargs = [ table_option_term(Opt) || Opt <- Options ]
    }.

%% @private
-spec table_option_term(lethink:table_options()) -> #term_assocpair{}.
table_option_term({datacenter, Value}) when is_binary(Value) ->
    ql2_util:term_assocpair(datacenter, Value);
table_option_term({primary_key, Value}) when is_binary(Value) ->
    ql2_util:term_assocpair(primary_key, Value);
table_option_term({cache_size, Value}) when is_integer(Value) ->
    ql2_util:term_assocpair(cache_size, Value).

-spec table_drop(binary(), [] | #term{}) -> build_result().
table_drop(Name, []) when is_binary(Name) ->
    #term{
        type = 'TABLE_DROP',
        args = ql2_util:datum_term(Name)
    };
table_drop(Name, #term{ type = 'DB' } = Db) when is_binary(Name) ->
    #term{
        type = 'TABLE_DROP',
        args = [ Db, ql2_util:datum_term(Name) ]
    }.

-spec table_list([] | #term{}) -> build_result().
table_list([]) ->
    #term{
        type = 'TABLE_LIST'
    };
table_list(#term{ type = 'DB' } = Db) ->
    #term{
        type = 'TABLE_LIST',
        args = Db
    }.

%% @private
%% @doc Specify a DB.  Must be first operation in query list
%% Optional if a default database has been specified via
%% @see lethink:use/2
-spec db(binary(), []) -> build_result().
db(Name, []) when is_binary(Name) ->
    #term {
        type = 'DB',
        args = ql2_util:datum_term(Name)
    };
db(Name, _) when is_list(Name) ->
    {error, <<"Db name must be binary">>};
db(_, _) ->
    {error, <<"Db must be first operation in query list">>}.

-spec table(binary(), [] | #term{}) -> build_result().
table(Name, []) when is_binary(Name) ->
    table(Name, false, []);
table(Name, #term{ type = 'DB' } = Db) when is_binary(Name) ->
    table(Name, false, Db);
table(Name, _) when is_list(Name) ->
    {error, <<"Table name must be binary">>};
table(_, _) ->
    {error, <<"Table can either start or follow db operation">>}.

-spec table(binary(), boolean(), [] | #term{}) -> build_result().
table(Name, UseOutdated, []) when is_binary(Name) ->
    #term {
        type = 'TABLE',
        args = ql2_util:datum_term(Name),
        optargs = [ql2_util:term_assocpair(<<"use_outdated">>, UseOutdated)]
    };
table(Name, UseOutdated, #term{ type = 'DB' } = Db) when is_binary(Name) ->
    #term {
        type = 'TABLE',
        args = [Db] ++ [ql2_util:datum_term(Name)],
        optargs = [ql2_util:term_assocpair(<<"use_outdated">>, UseOutdated)]
    };
table(Name, _, _) when is_list(Name) ->
    {error, <<"Table name must be binary">>};
table(_, _, _) ->
    {error, <<"Table can either start or follow db operation">>}.

-spec insert(lethink:json(), #term{}) -> build_result().
insert(Data, #term{ type = 'TABLE' } = Table) ->
    #term {
        type = 'INSERT',
        args = [Table] ++ [ql2_util:datum_term(Data)]
    };
insert(_, _) ->
    {error, <<"insert must follow table operator">>}.

-spec insert(lethink:json(), [lethink:insert_options()], #term{}) -> build_result().
insert(Data, Options, #term{ type = 'TABLE' } = Table) ->
    #term {
        type = 'INSERT',
        args = [Table] ++ [ql2_util:datum_term(Data)],
        optargs = [ insert_option_term(Opt) || Opt <- Options ]
    };
insert(_, _, _) ->
    {error, <<"insert must follow table operator">>}.

%% @private
-spec insert_option_term(lethink:insert_options()) -> #term_assocpair{}.
insert_option_term({upsert, Value}) when is_binary(Value) ->
    ql2_util:term_assocpair(upsert, Value).

-spec get(binary() | number(), #term{}) -> build_result().
get(Key, #term{ type = 'TABLE' } = Table) when is_binary(Key); is_number(Key) ->
    #term {
        type = 'GET',
        args = [Table] ++ [ql2_util:datum_term(Key)]
    };
get(Key, _) when is_list(Key) ->
    {error, <<"get key must be binary or number">>};
get(_, _) ->
    {error, <<"get must follow table operator">>}.
