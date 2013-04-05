%% @private
-module(ql2_util).

-export([datum_value/1,
         term_assocpair/3,
         datum_term/2,
         datum/2]).

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

-spec term_assocpair(atom(), atom(), any()) -> #term_assocpair{}.
term_assocpair(Key, Type, Value) ->
    #term_assocpair {
        key = atom_to_binary(Key, latin1),
        val = datum_term(Type, Value)
    }.

-spec datum_term(atom(), any()) -> #term{}.
datum_term(Type, Value) ->
    #term {
        type = 'DATUM',
        datum = datum(Type, Value)
    }.

datum('R_STR', Value) ->
    #datum {
        type = 'R_STR',
        r_str = Value
    };

datum('R_NUM', Value) ->
    #datum {
        type = 'R_NUM',
        r_num = Value
    }.


