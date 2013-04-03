-module(ql2_wrapper).

-export([
    db_create/1,
    db_drop/1,
    db_list/0
]).

-include("ql2_pb.hrl").

-spec db_create(string()) -> #query{}.
db_create(Name) ->
    #query{
        type = 'START',
        query = #term{
            type = 'DB_CREATE',
            args = #term {
                type = 'DATUM',
                datum = #datum{
                    type = 'R_STR',
                    r_str = Name
                }
            }
        },
        token = lethink_token:get()
    }.

-spec db_drop(string()) -> #query{}.
db_drop(Name) ->
    #query{
        type = 'START',
        query = #term{
            type = 'DB_DROP',
            args = #term {
                type = 'DATUM',
                datum = #datum{
                    type = 'R_STR',
                    r_str = Name
                }
            }
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
