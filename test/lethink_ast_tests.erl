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
