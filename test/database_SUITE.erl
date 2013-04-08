-module(database_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([databases/1,
         tables/1,
         use/1]).

%% Optional suite settings
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,5}}].

all() -> 
    [databases,
     tables,
     use].

%% Optional suite pre test initialization
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    lethink:start(),
    lethink:add_pool(test_pool, 1),
    lethink:db_drop(test_pool, <<"test_bin">>),
    lethink:db_drop(test_pool, <<"test_str">>),
    lethink:db_drop(test_pool, <<"table_test">>),
    lethink:db_drop(test_pool, <<"use_test1">>),
    lethink:db_drop(test_pool, <<"use_test2">>),
    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    lethink:remove_pool(test_pool),
    lethink:stop(),
    ok.

%% Tests
databases(_Config) ->
    {ok, _} = lethink:db_create(test_pool, <<"test_bin">>),
    {ok, _} = lethink:db_create(test_pool, "test_str"),
    {ok, Dbs} = lethink:db_list(test_pool),
    true = lists:all(fun is_binary/1, Dbs),
    true = lists:member(<<"test_bin">>, Dbs),
    true = lists:member(<<"test_str">>, Dbs),
    {error, _, _, _} = lethink:db_create(test_pool, <<"test_bin">>),
    {error, _, _, _} = lethink:db_create(test_pool, "test_str"),
    {ok, _} = lethink:db_drop(test_pool, <<"test_bin">>),
    {ok, _} = lethink:db_drop(test_pool, "test_str"),
    {ok, Dbs2} = lethink:db_list(test_pool),
    false = lists:member(<<"test_bin">>, Dbs2),
    false = lists:member(<<"test_str">>, Dbs2),
    {error, _, _, _} = lethink:db_drop(test_pool, <<"test_bin">>),
    {error, _, _, _} = lethink:db_drop(test_pool, "test_str").

tables(_Config) ->
    {ok, _} = lethink:db_create(test_pool, <<"table_test">>),
    ok = lethink:use(test_pool, <<"table_test">>),
    {ok, []} = lethink:table_list(test_pool),
    {ok, _} = lethink:table_create(test_pool, <<"table_bin">>),
    {ok, _} = lethink:table_create(test_pool, "table_str"),
    {ok, Tables1} = lethink:table_list(test_pool),
    true = lists:all(fun is_binary/1, Tables1),
    true = lists:member(<<"table_bin">>, Tables1),
    true = lists:member(<<"table_str">>, Tables1),
    {error, _, _, _} = lethink:table_create(test_pool, <<"table_bin">>),
    {error, _, _, _} = lethink:table_create(test_pool, "table_str"),
    {ok, _} = lethink:table_drop(test_pool, <<"table_bin">>),
    {ok, _} = lethink:table_drop(test_pool, "table_str"),
    {ok, Tables2} = lethink:table_list(test_pool),
    false = lists:member(<<"table_bin">>, Tables2),
    false = lists:member(<<"table_str">>, Tables2),
    {error, _, _, _} = lethink:table_drop(test_pool, <<"table_bin">>),
    {error, _, _, _} = lethink:table_drop(test_pool, "table_str"),
    {ok, _} = lethink:db_drop(test_pool, <<"table_test">>).

use(_Config) ->
    {ok, _} = lethink:db_create(test_pool, <<"use_test1">>),
    ok = lethink:use(test_pool, <<"use_test1">>),
    {ok, _} = lethink:table_create(test_pool, <<"table1">>),
    {ok, Tables1} = lethink:table_list(test_pool),
    true = lists:member(<<"table1">>, Tables1),
    {ok, _} = lethink:db_create(test_pool, <<"use_test2">>),
    ok = lethink:use(test_pool, <<"use_test2">>),
    {ok, Tables2} = lethink:table_list(test_pool),
    false = lists:member(<<"table1">>, Tables2),
    {ok, _} = lethink:db_drop(test_pool, <<"use_test1">>),
    {ok, _} = lethink:db_drop(test_pool, <<"use_test2">>).
