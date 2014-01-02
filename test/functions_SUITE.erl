-module(functions_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([update/1]).

%% Optional suite settings
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,5}}].

all() -> 
    [update].

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
    lethink:query(pool, [{db_create, <<"function_db">>}]),
    lethink:query(pool, [{db, <<"function_db">>}, {table_create, <<"marvel">>}]),
    lethink:use(pool, <<"function_db">>),
    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    lethink:query(pool, [{db_drop, <<"function_db">>}]),
    lethink:remove_pool(pool),
    lethink:stop(),
    ok.

%% Tests
update(_Config) ->
    TestData = [{[{<<"id">>, 1}, {<<"hero">>, <<"batman">>}, {<<"age">>, 30}]},
                {[{<<"id">>, 2}, {<<"hero">>, <<"superman">>}, {<<"age">>, 50}]}],
    {ok, _} = lethink:query(pool, [{table, <<"marvel">>}, {insert, TestData}]),
    {ok, _} = lethink:query(pool, [{table, <<"marvel">>},
                                   {update, {[{<<"age">>, [{row}, {get_field, <<"age">>}, {add, 1}]}]}}]),
    {ok, _} = lethink:query(pool, [{table, <<"marvel">>},
                                   {update, fun(Row) -> [Row, {get_field, <<"age">>}, {add, 1}] end}]).
