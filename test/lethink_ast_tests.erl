-module(lethink_ast_tests).

-include_lib("eunit/include/eunit.hrl").

-include("ql2_pb.hrl").

db_test() ->
    #term{} = lethink_ast:db(<<"test">>, []),
    {error, _} = lethink_ast:db("test", []),
    {error, _} = lethink_ast:db(<<"test">>, #term{}).

table_test() ->
    #term{} = lethink_ast:table(<<"table">>, []),
    #term{} = lethink_ast:table(<<"table">>, #term{ type = 'DB' }),
    {error, _} = lethink_ast:table("table", []),
    {error, _} = lethink_ast:table(<<"table">>, #term{}).
