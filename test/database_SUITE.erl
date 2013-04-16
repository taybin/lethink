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
    lethink:add_pool(pool, 1),
    lethink:query(pool, [{db_drop, <<"test_bin">>}]),
    lethink:query(pool, [{db_drop, <<"table_test">>}]),
    lethink:query(pool, [{db_drop, <<"use_test1">>}]),
    lethink:query(pool, [{db_drop, <<"use_test2">>}]),
    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    lethink:remove_pool(pool),
    lethink:stop(),
    ok.

%% Tests
databases(_Config) ->
    {ok, _} = lethink:query(pool, [{db_create, <<"test_bin">>}]),
    {ok, Dbs} = lethink:query(pool, [{db_list}]),
    true = lists:all(fun is_binary/1, Dbs),
    true = lists:member(<<"test_bin">>, Dbs),
    {error, _, _, _} = lethink:query(pool, [{db_create, <<"test_bin">>}]),
    {ok, _} = lethink:query(pool, [{db_drop, <<"test_bin">>}]),
    {ok, Dbs2} = lethink:query(pool, [{db_list}]),
    false = lists:member(<<"test_bin">>, Dbs2),
    {error, _, _, _} = lethink:query(pool, [{db_drop, <<"test_bin">>}]).

tables(_Config) ->
    {ok, _} = lethink:query(pool, [{db_create,<<"table_test">>}]),
    ok = lethink:use(pool, <<"table_test">>),
    {ok, []} = lethink:query(pool, [{db, <<"table_test">>}, {table_list}]),
    {ok, _} = lethink:query(pool, [{db, <<"table_test">>}, {table_create, <<"table_bin">>}]),
        {ok, Tables1} = lethink:query(pool, [{db, <<"table_test">>}, {table_list}]),
    true = lists:all(fun is_binary/1, Tables1),
    true = lists:member(<<"table_bin">>, Tables1),
    {error, _, _, _} = lethink:query(pool, [{db, <<"table_test">>}, {table_create, <<"table_bin">>}]),
    {ok, _} = lethink:query(pool, [{db, <<"table_test">>}, {table_drop, <<"table_bin">>}]),
    {ok, Tables2} = lethink:query(pool, [{db, <<"table_test">>}, {table_list}]),
    false = lists:member(<<"table_bin">>, Tables2),
    {error, _, _, _} = lethink:query(pool, [{db, <<"table_test">>}, {table_drop, <<"table_bin">>}]),
    {ok, _} = lethink:query(pool, [{db_drop, <<"table_test">>}]).

use(_Config) ->
    {ok, _} = lethink:query(pool, [{db_create, <<"use_test1">>}]),
    ok = lethink:use(pool, <<"use_test1">>),
    {ok, _} = lethink:query(pool, [{db, <<"use_test1">>}, {table_create, <<"table1">>}]),
    {ok, Tables1} = lethink:query(pool, [{db, <<"use_test1">>}, {table_list}]),
    true = lists:member(<<"table1">>, Tables1),
    {ok, _} = lethink:query(pool, [{db_create, <<"use_test2">>}]),
    ok = lethink:use(pool, <<"use_test2">>),
    {ok, []} = lethink:query(pool, [{db, <<"use_test2">>}, {table_list}]),
    {ok, Tables1} = lethink:query(pool, [{db, <<"use_test1">>}, {table_list}]),
    {ok, _} = lethink:query(pool, [{db_drop, <<"use_test1">>}]),
    {ok, _} = lethink:query(pool, [{db_drop, <<"use_test2">>}]).
