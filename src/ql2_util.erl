%% @private
-module(ql2_util).

-export([datum_value/1,
         term_assocpair/2,
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

-spec term_assocpair(atom(), any()) -> #term_assocpair{}.
term_assocpair(Key, Value) ->
    #term_assocpair {
        key = atom_to_binary(Key, latin1),
        val = datum_term(Value)
    }.

-spec datum_term(any()) -> #term{}.
datum_term(Value) ->
    #term {
        type = 'DATUM',
        datum = datum(Value)
    }.


% @doc create Datums from the four basic types.  Arrays and objects
% are created via MAKE_ARRAY and MAKE_OBJ on the server since it's
% cheaper that way.
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
