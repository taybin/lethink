%% @private
-module(ql2_util).

-export([datum_value/1,
         term_assocpair/2,
         global_db/1,
         datum_term/1,
         datum/1]).

-include("ql2_pb.hrl").

-spec datum_value(#datum{}) -> any().
datum_value(#datum{ type = 'R_NULL' }) ->
    null;
datum_value(#datum{ type = 'R_BOOL', r_bool = Bool}) ->
    Bool;
datum_value(#datum{ type = 'R_NUM', r_num = Num}) ->
    Num;
datum_value(#datum{ type = 'R_STR', r_str = Str }) ->
    list_to_binary(Str);
datum_value(#datum{ type = 'R_ARRAY', r_array = Array }) ->
    [ datum_value(D) || D <- Array ];
datum_value(#datum{ type = 'R_OBJECT', r_object = Objects }) ->
    [ datum_assocpair_tuple(Obj) || Obj <- Objects ].

-spec datum_assocpair_tuple(#datum_assocpair{}) -> {binary(), any()}.
datum_assocpair_tuple(Obj) ->
    {list_to_binary(Obj#datum_assocpair.key), datum_value(Obj#datum_assocpair.val)}.

-spec term_assocpair(atom() | binary(), any()) -> #term_assocpair{}.
term_assocpair(Key, Value) when is_atom(Key) ->
    term_assocpair(atom_to_binary(Key, latin1), Value);
term_assocpair(Key, Value) ->
    #term_assocpair {
        key = Key,
        val = datum_term(Value)
    }.

-spec global_db(binary()) -> #query_assocpair{}.
global_db(Value) ->
    #query_assocpair {
        key = <<"db">>,
        val = #term {
            type = 'DB',
            args = datum_term(Value)
        }
    }.

-spec datum_term(lethink:json()) -> #term{}.
datum_term({Items}) when is_list(Items) ->
    #term {
        type = 'MAKE_OBJ',
        optargs = [ datum_term(Pair) || Pair <- Items ]
    };
datum_term({Key, Value}) ->
    term_assocpair(Key, Value);
datum_term(Items) when is_list(Items) ->
    #term {
        type = 'MAKE_ARRAY',
        args = [ datum_term(I) || I <- Items ]
    };
datum_term(Value) ->
    #term {
        type = 'DATUM',
        datum = datum(Value)
    }.

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
