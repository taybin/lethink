%% @private
-module(ql2_wrapper).

-export([
    db_create/1,
    db_drop/1,
    db_list/0,
    table_create/3,
    table_drop/2,
    table_list/1
]).

-include("ql2_pb.hrl").

-spec db_create(binary()) -> #query{}.
db_create(Name) when is_binary(Name) ->
    #query{
        type = 'START',
        query = #term{
            type = 'DB_CREATE',
            args = ql2_util:datum_term(Name)
        },
        token = lethink_token:get()
    }.

-spec db_drop(binary()) -> #query{}.
db_drop(Name) when is_binary(Name) ->
    #query{
        type = 'START',
        query = #term{
            type = 'DB_DROP',
            args = ql2_util:datum_term(Name)
        },
        token = lethink_token:get()
    }.

-spec db_list() -> #query{}.
db_list() ->
    #query{
        type = 'START',
        query = #term{
            type = 'DB_LIST'
        },
        token = lethink_token:get()
    }.

-spec table_create(binary(), binary(), [lethink:table_options()]) -> #query{}.
table_create(Database, Name, Options) when is_binary(Database), is_binary(Name) ->
    #query{
        type = 'START',
        query = #term{
            type = 'TABLE_CREATE',
            args = [
                #term {
                    type = 'DB',
                    args = ql2_util:datum_term(Database)
                },
                ql2_util:datum_term(Name)
            ],
            optargs = [ table_option_term(Opt) || Opt <- Options ]
        },
        token = lethink_token:get()
    }.

-spec table_option_term(lethink:table_options()) -> #term_assocpair{}.
table_option_term({datacenter, Value}) when is_binary(Value) ->
    ql2_util:term_assocpair(datacenter, Value);
table_option_term({primary_key, Value}) when is_binary(Value) ->
    ql2_util:term_assocpair(primary_key, Value);
table_option_term({cache_size, Value}) when is_integer(Value) ->
    ql2_util:term_assocpair(cache_size, Value).

-spec table_drop(binary(), binary()) -> #query{}.
table_drop(Database, Name) when is_binary(Database), is_binary(Name) ->
    #query{
        type = 'START',
        query = #term{
            type = 'TABLE_DROP',
            args = [
                #term {
                    type = 'DB',
                    args = ql2_util:datum_term(Database)
                },
                ql2_util:datum_term(Name)
            ]
        },
        token = lethink_token:get()
    }.

-spec table_list(binary()) -> #query{}.
table_list(Database) when is_binary(Database) ->
    #query{
        type = 'START',
        query = #term{
            type = 'TABLE_LIST',
            args = #term {
                    type = 'DB',
                    args = ql2_util:datum_term(Database)
            }
        },
        token = lethink_token:get()
    }.
