-module(lethink_ast).

-export([build_query/1]).

% @private
-export([db_create/2,
        db_drop/2,
        db_list/1,
        table_create/2, table_create/3,
        index_create/2, index_create/3,
        table_drop/2,
        table_list/1,
        db/2,
        row/1,
        get_field/2,
        table/2, table/3,
        insert/2, insert/3,
        get/2,
        between/3,
        between/4,
        filter/2,
        update/2,
        expr/1, expr/2,
        func/1,
        var/2,
        add/2,
        sub/2,
        mul/2,
        div_/2,
        mod/2,
        and_/2,
        or_/2,
        eq/2,
        ne/2,
        gt/2,
        ge/2,
        lt/2,
        le/2,
        not_/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("ql2_pb.hrl").

-type build_result() :: #term{} | {error, binary()}.

%% @doc Build rethinkdb query from operation list
-spec build_query([tuple()]) -> build_result().
build_query(QueryList) ->
    apply_seq(QueryList, []).

%% @private
%% @doc foldl-inspired poor man's monad
-spec apply_seq([tuple()], [] | #term{} | {error, any()}) -> build_result().
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
        args = expr(Name)
    };
db_create(Name, _) when is_list(Name) ->
    {error, <<"db_create name must be binary">>};
db_create(_, _) ->
    {error, <<"db_create stands alone">>}.

-spec db_drop(binary(), []) -> build_result().
db_drop(Name, []) when is_binary(Name) ->
    #term{
        type = 'DB_DROP',
        args = expr(Name)
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
        args = expr(Name),
        optargs = [ table_option_term(Opt) || Opt <- Options ]
    };
table_create(Name, Options, #term{ type = 'DB' } = Db) when is_binary(Name) ->
    #term{
        type = 'TABLE_CREATE',
        args = [Db, expr(Name)],
        optargs = [ table_option_term(Opt) || Opt <- Options ]
    }.

-spec index_create(binary(), #term{} | []) -> build_result().
index_create(Name, Term) ->
    index_create(Name, [], Term).

-spec index_create(binary(), [lethink:table_options()], #term{} | []) -> build_result().
index_create(Name, Options, []) ->
    #term{
        type = 'INDEX_CREATE',
        args = expr(Name),
        optargs = [ index_option_term(Opt) || Opt <- Options ]
        
    };
index_create(Name, Options, #term{ type = 'TABLE' } = Table) when is_binary(Name) ->
    #term{
        type = 'INDEX_CREATE',
        args = [Table, expr(Name)],
        optargs = [ index_option_term(Opt) || Opt <- Options ]
    }.

%% @private
-spec table_option_term(lethink:table_options()) -> #term_assocpair{}.
table_option_term({datacenter, Value}) when is_binary(Value) ->
    term_assocpair(atom_to_binary(datacenter, utf8), Value);
table_option_term({primary_key, Value}) when is_binary(Value) ->
    term_assocpair(atom_to_binary(primary_key, utf8), Value);
table_option_term({index, Value}) when is_binary(Value) ->
    term_assocpair(atom_to_binary(index, utf8), Value);
table_option_term({cache_size, Value}) when is_integer(Value) ->
    term_assocpair(atom_to_binary(cache_size, utf8), Value).

-spec index_option_term(lethink:index_options()) -> #term_assocpair{}.
index_option_term(_Opt) ->
    %% TODO
    #term_assocpair{}.

-spec table_drop(binary(), [] | #term{}) -> build_result().
table_drop(Name, []) when is_binary(Name) ->
    #term{
        type = 'TABLE_DROP',
        args = expr(Name)
    };
table_drop(Name, #term{ type = 'DB' } = Db) when is_binary(Name) ->
    #term{
        type = 'TABLE_DROP',
        args = [ Db, expr(Name) ]
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
        args = expr(Name)
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
        args = expr(Name),
        optargs = [term_assocpair(<<"use_outdated">>, UseOutdated)]
    };
table(Name, UseOutdated, #term{ type = 'DB' } = Db) when is_binary(Name) ->
    #term {
        type = 'TABLE',
        args = [Db, expr(Name)],
        optargs = [term_assocpair(<<"use_outdated">>, UseOutdated)]
    };
table(Name, _, _) when is_list(Name) ->
    {error, <<"Table name must be binary">>};
table(_, _, _) ->
    {error, <<"Table can either start or follow db operation">>}.

-spec insert(lethink:document(), #term{}) -> build_result().
insert(Data, #term{ type = 'TABLE' } = Table) ->
    #term {
        type = 'INSERT',
        args = [Table, expr(Data)]
    };
insert(_, _) ->
    {error, <<"insert must follow table operator">>}.

-spec insert(lethink:document(), [lethink:insert_options()], #term{}) -> build_result().
insert(Data, Options, #term{ type = 'TABLE' } = Table) ->
    #term {
        type = 'INSERT',
        args = [Table, expr(Data)],
        optargs = [ insert_option_term(Opt) || Opt <- Options ]
    };
insert(_, _, _) ->
    {error, <<"insert must follow table operator">>}.

%% @private
-spec insert_option_term(lethink:insert_options()) -> #term_assocpair{}.
insert_option_term({upsert, Value}) when is_binary(Value) ->
    term_assocpair(atom_to_binary(upsert, utf8), Value).

-spec get(binary() | number(), #term{}) -> build_result().
get(Key, #term{ type = 'TABLE' } = Table) when is_binary(Key); is_number(Key) ->
    #term {
        type = 'GET',
        args = [Table, expr(Key)]
    };
get(Key, _) when is_list(Key) ->
    {error, <<"get key must be binary or number">>};
get(_, _) ->
    {error, <<"get must follow table operator">>}.

-spec between(binary() | number(), binary() | number(), #term{}) -> build_result().
between(Value1, Value2, Term) ->
    between(Value1, Value2, [], Term).

-spec between(binary() | number(), binary() | number(), [lethink:table_options()], #term{}) -> build_result().
between(Value1, Value2, Options, #term{ type = 'TABLE' } = Table) ->
   #term {
        type = 'BETWEEN',
        args = [Table, expr(Value1), expr(Value2)],
        optargs = [table_option_term(Opt) || Opt <- Options]
    };
between(_, _, _, _) ->
    {error, "between must follow table operator"}.

-spec filter(lethink:document(), #term{}) -> build_result().
filter(Value, #term{ type = Type } = Selection) when 
        Type == 'TABLE'; Type == 'GET';
        Type == 'BETWEEN' ->
    #term {
        type = 'FILTER',
        args = [Selection, expr(Value)]
    };
filter(_, _) ->
    {error, "filter must follow table, get or between operator"}.

-spec update(lethink:document() | fun(), #term{}) -> build_result().
update(Data, #term{ type = Type } = Selection) when
        Type == 'TABLE'; Type == 'GET';
        Type == 'BETWEEN'; Type == 'FILTER' ->
    #term {
        type = 'UPDATE',
        args = [Selection, func_wrap(Data)]
    }.

-spec row([]) -> build_result().
row([]) ->
    #term {
        type = 'IMPLICIT_VAR'
    }.

-spec get_field(binary(), #term{}) -> build_result().
get_field(Attr, Term) ->
    #term {
        type = 'GET_FIELD',
        args = [Term, expr(Attr)]
    }.

%% Math and Logic Operations

-spec add(number() | binary(), #term{}) -> build_result().
add(Value, Term) when is_number(Value); is_binary(Value) ->
    #term {
        type = 'ADD',
        args = [Term, expr(Value)]
    }.

-spec sub(number(), #term{}) -> build_result().
sub(Value, Term) when is_number(Value) ->
    #term {
           type = 'SUB',
           args = [Term, expr(Value)]
          }.

-spec mul(number(), #term{}) -> build_result().
mul(Value, Term) when is_number(Value) ->
    #term {
           type = 'MUL',
           args = [Term, expr(Value)]
          }.

-spec div_(number(), #term{}) -> build_result().
div_(Value, Term) when is_number(Value) ->
    #term {
           type = 'DIV',
           args = [Term, expr(Value)]
          }.

-spec mod(number(), #term{}) -> build_result().
mod(Value, Term) when is_number(Value) ->
    #term {
           type = 'MOD',
           args = [Term, expr(Value)]
          }.

-spec and_(boolean(), #term{}) -> build_result().
and_(Value, Term) when is_boolean(Value) ->
    #term {
           type = 'AND',
           args = [Term, expr(Value)]
          }.

-spec or_(boolean(), #term{}) -> build_result().
or_(Value, Term) when is_boolean(Value) ->
    #term {
           type = 'OR',
           args = [Term, expr(Value)]
          }.

-spec eq(lethink:json_term(), #term{}) -> build_result().
eq(Value, Term) ->
    #term {
           type = 'EQ',
           args = [Term, expr(Value)]
          }.

-spec ne(lethink:json_term(), #term{}) -> build_result().
ne(Value, Term) ->
    #term {
           type = 'NE',
           args = [Term, expr(Value)]
          }.

-spec gt(lethink:json_term(), #term{}) -> build_result().
gt(Value, Term) ->
    #term {
           type = 'GT',
           args = [Term, expr(Value)]
          }.

-spec ge(lethink:json_term(), #term{}) -> build_result().
ge(Value, Term) ->
    #term {
           type = 'GE',
           args = [Term, expr(Value)]
          }.

-spec lt(lethink:json_term(), #term{}) -> build_result().
lt(Value, Term) ->
    #term {
           type = 'LT',
           args = [Term, expr(Value)]
          }.

-spec le(lethink:json_term(), #term{}) -> build_result().
le(Value, Term) ->
    #term {
           type = 'LE',
           args = [Term, expr(Value)]
          }.

-spec not_(#term{}) -> build_result().
not_(Term) ->
    #term {
           type = 'NOT',
           args = [Term]
          }.

-spec expr(lethink:json_term(), []) -> build_result().
expr(Data, []) ->
    expr(Data).

-spec expr(lethink:keyvalue() | lethink:json_term() | fun() | #term{} | #term_assocpair{}) -> #term{} | #term_assocpair{}.
expr(Item = #term{}) ->
    Item;
expr(Item = #term_assocpair{}) ->
    Item;
expr({Items}) when is_list(Items) ->
    #term {
        type = 'MAKE_OBJ',
        optargs = [ expr({K, V}) || {K, V} <- Items ]
    };
expr({Key, Value}) ->
    term_assocpair(Key, Value);
expr(Items) when is_list(Items) ->
    case lists:all(fun is_json/1, Items) of
        true -> make_array(Items);
        false -> build_query(Items)
    end;
expr(Func) when is_function(Func) ->
    func(Func);
expr(Value) ->
    #term {
        type = 'DATUM',
        datum = datum(Value)
    }.

make_array(Items) when is_list(Items) ->
    #term {
        type = 'MAKE_ARRAY',
        args = [ expr(I) || I <- Items ]
        }.

% @private
% @doc create Datums from the four basic types.  Arrays and objects
% are created via MAKE_ARRAY and MAKE_OBJ on the server since it's
% cheaper that way.
-spec datum(null | boolean() | number() | binary()) -> #datum{}.
datum(null) ->
    #datum {
        type = 'R_NULL'
    };

datum(V) when is_boolean(V) ->
    #datum {
        type = 'R_BOOL',
        r_bool = V
    };

datum(V) when is_number(V) ->
    #datum {
        type = 'R_NUM',
        r_num = V
    };

datum(V) when is_binary(V) ->
    #datum {
        type = 'R_STR',
        r_str = V
    }.

-spec var(integer(), []) -> #term{}.
var(N, []) ->
    #term {
        type = 'VAR',
        args = expr(N)
        }.

-spec func(fun()) -> #term{}.
func(Func) ->
    {_, Arity} = erlang:fun_info(Func, arity),
    Args = [ {var, N} || N <- lists:seq(1, Arity)],
    #term {
        type = 'FUNC',
        args = [make_array(lists:seq(1, Arity)), expr(apply(Func, Args))]
    }.

-spec term_assocpair(binary(), any()) -> #term_assocpair{}.
term_assocpair(Key, Value) ->
    #term_assocpair {
        key = Key,
        val = expr(Value)
    }.

func_wrap(Data) ->
    Value = expr(Data),
    case ivar_scan(Value) of
        true -> func(fun(_X) -> Value end);
        false -> Value
    end.

% Scan for IMPLICIT_VAR or JS
-spec ivar_scan(any()) -> boolean().
ivar_scan(#term{ type = 'IMPLICIT_VAR' }) ->
    true;
ivar_scan(#term{ args = Args, optargs = OptArgs }) ->
    case is_list(Args) of
        true -> lists:any(fun ivar_scan/1, Args);
        false -> ivar_scan(Args)
    end
    orelse
    case is_list(OptArgs) of
        true -> lists:any(fun ivar_scan/1, OptArgs);
        false -> ivar_scan(Args)
    end;
ivar_scan(#term_assocpair{ val = Val }) ->
    ivar_scan(Val);
ivar_scan(_) ->
    false.

-spec is_json(any()) -> boolean().
is_json(null) -> true;
is_json(Item) when is_boolean(Item) -> true;
is_json(Item) when is_number(Item) -> true;
is_json(Item) when is_binary(Item) -> true;
is_json({List}) when is_list(List) -> true;
is_json({Key, _Value}) when is_binary(Key) -> true;
is_json(_) -> false.

%% lethink:query(test, [{table, <<"marvel">>}, {update, {[{<<"age">>, [{row}, {get_field, <<"age">>}, {add, 1}]}]}}])
%% r.table('marvel').update(lambda x: {'age': x['age'] + 1}))
