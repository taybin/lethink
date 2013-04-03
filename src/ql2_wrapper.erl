-module(ql2_wrapper).

-export([
    db_create/2,
    db_drop/2,
    db_list/1
]).

-include("ql2_pb.hrl").

-spec db_create(string(), pos_integer()) -> #query{}.
db_create(Name, Token) ->
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
        token = Token
    }.

-spec db_drop(string(), pos_integer()) -> #query{}.
db_drop(Name, Token) ->
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
        token = Token
    }.

-spec db_list(pos_integer()) -> #query{}.
db_list(Token) ->
    #query{
        type = 'START',
        query = #term{
            type = 'DB_LIST'
        },
        token = Token
    }.
