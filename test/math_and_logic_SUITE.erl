-module(math_and_logic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([math/1,
         logic/1]).
         
         

%% Optional suite settings
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,5}}].

all() ->
    [math,
     logic].

%% Optional suite pre test initialization
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    lethink:start(),
    lethink:add_pool(math_pool, 1),
    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    lethink:remove_pool(math_pool),
    lethink:stop(),
    ok.

%% Tests
math(_Config) ->
    {ok, 3.0} = lethink:query(pool, [{expr, 1}, {add, 2}]),
    {ok, <<"ab">>} = lethink:query(pool, [{expr, <<"a">>}, {add, <<"b">>}]),
    {ok, 2.0} = lethink:query(pool, [{expr, 5}, {sub, 3}]),
    {ok, 2.0} = lethink:query(pool, [{expr, 5.0}, {sub, 3}]),
    {ok, 2.0} = lethink:query(pool, [{expr, 5}, {sub, 3.0}]),
    {ok, 2.0} = lethink:query(pool, [{expr, 5.0}, {sub, 3.0}]),
    {ok, 15.0} = lethink:query(pool, [{expr, 3}, {mul, 5}]),
    {ok, 3.0} = lethink:query(pool, [{expr, 12}, {div_, 4}]),
    {ok, 1.0} = lethink:query(pool, [{expr, 8}, {mod, 7}]).

logic(_Config) ->
    ok.
