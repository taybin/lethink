-module(lethink_ast_tests).

-include_lib("eunit/include/eunit.hrl").

-include("ql2_pb.hrl").

db_test() ->
    ?assertMatch(#term{}, lethink_ast:db(<<"test">>, [])),
    ?assertMatch({error, _}, lethink_ast:db("test", [])),
    ?assertMatch({error, _}, lethink_ast:db(<<"test">>, #term{})).

table_test() ->
    ?assertMatch(#term{}, lethink_ast:table(<<"table">>, [])),
    ?assertMatch(#term{}, lethink_ast:table(<<"table">>, #term{ type = 'DB' })),
    ?assertMatch({error, _}, lethink_ast:table("table", [])),
    ?assertMatch({error, _}, lethink_ast:table(<<"table">>, #term{})).

func_test() ->
    ?assertMatch(#term{}, lethink_ast:func(fun(_N) -> 1 end)).

is_json_test() ->
    true = lethink_ast:is_json(null),
    true = lethink_ast:is_json(1),
    true = lethink_ast:is_json(1.1),
    true = lethink_ast:is_json(<<"test">>),
    true = lethink_ast:is_json({[]}),
    true = lethink_ast:is_json({<<"key">>, false}),
    false = lethink_ast:is_json({key, false}),
    false = lethink_ast:is_json({row}),
    false = lethink_ast:is_json({<<"test">>}).

ivar_scan_test() ->
    ImpVarTerm = #term{ type = 'IMPLICIT_VAR' },
    true = lethink_ast:ivar_scan(ImpVarTerm),
    true = lethink_ast:ivar_scan(#term{ args = ImpVarTerm}),
    true = lethink_ast:ivar_scan(#term{ args = [ImpVarTerm]}),
    false = lethink_ast:ivar_scan(true).

expr_test() ->
    #term{ type = 'DATUM' } = lethink_ast:expr(5),
    #term{ type = 'FUNC' } = lethink_ast:expr(fun(X) -> [X] end).

func_wrap_test() ->
    #term{ type = 'MAKE_ARRAY' } = lethink_ast:func_wrap([1,2,3]),
    #term{ type = 'DATUM' } = lethink_ast:func_wrap(<<"test">>),
    #term{ type = 'FUNC' } = lethink_ast:func_wrap([{row}]),
    #term{ type = 'FUNC' } = lethink_ast:func_wrap(fun(X) -> [X] end).
