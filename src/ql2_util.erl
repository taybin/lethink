%% @private
-module(ql2_util).

-export([datum_value/1,
         global_db/1]).

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

-spec global_db(binary()) -> #query_assocpair{}.
global_db(Value) ->
    #query_assocpair {
        key = <<"db">>,
        val = #term {
            type = 'DB',
            args = lethink_ast:expr(Value)
        }
    }.
