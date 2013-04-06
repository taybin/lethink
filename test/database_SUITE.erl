-module(database_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([create/1,
         drop/1]).

%% Optional suite settings
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{seconds,60}}].

all() -> 
    [create,
     drop].

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
    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    lethink:remove_pool(test_pool),
    ok.

%% Tests
create(_Config) ->
    {ok, _} = lethink:db_create(test_pool, <<"test_bin">>),
    {ok, _} = lethink:db_create(test_pool, "test_str"),
    {ok, Dbs} = lethink:db_list(test_pool),
    true = lists:member(<<"test_bin">>, Dbs),
    true = lists:member(<<"test_str">>, Dbs),
    {error, _, _, _} = lethink:db_create(test_pool, <<"test_bin">>),
    {error, _, _, _} = lethink:db_create(test_pool, "test_str").

drop(_Config) ->
    {ok, _} = lethink:db_drop(test_pool, <<"test_bin">>),
    {ok, _} = lethink:db_drop(test_pool, "test_str"),
    {ok, Dbs} = lethink:db_list(test_pool),
    false = lists:member(<<"test_bin">>, Dbs),
    false = lists:member("test_str", Dbs),
    {error, _, _, _} = lethink:db_drop(test_pool, <<"test_bin">>),
    {error, _, _, _} = lethink:db_drop(test_pool, "test_str").
