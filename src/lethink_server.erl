%% Based off of bank_server by:
%% Copyright (c) 2012, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @private
-module(lethink_server).
-behavior(gen_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([add_pool/1]).
-export([remove_pool/1]).
-export([add_worker/2]).
-export([get_worker/1]).
-export([get_all_workers/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	pools = [] :: [any()]
}).

-define(TAB, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call(?MODULE, stop).

-spec add_pool(any()) -> ok.
add_pool(Ref) ->
	gen_server:cast(?MODULE, {add_pool, Ref}).

-spec remove_pool(any()) -> ok.
remove_pool(Ref) ->
	gen_server:cast(?MODULE, {remove_pool, Ref}).

-spec add_worker(any(), pid()) -> ok.
add_worker(Ref, Pid) ->
	gen_server:cast(?MODULE, {add_worker, Ref, Pid}).

-spec get_worker(any()) -> pid().
get_worker(Ref) ->
	Workers = get_all_workers(Ref),
	{_, _, Micro} = erlang:now(),
	Random = 1 + Micro rem length(Workers),
	lists:nth(Random, Workers).

-spec get_all_workers(any()) -> [pid()].
get_all_workers(Ref) ->
	ets:lookup_element(?TAB, {pool, Ref}, 2).

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([]) ->
	{ok, #state{}}.

-spec handle_call(any(), {pid(), any()}, #state{}) -> {stop, normal, stopped, #state{}} |
                                                      {reply, ignored, #state{}}.
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast({add_pool, Ref}, State=#state{pools=Pools}) ->
	true = ets:insert_new(?TAB, {{pool, Ref}, []}),
	{noreply, State#state{pools=[Ref|Pools]}};
handle_cast({remove_pool, Ref}, State=#state{pools=Pools}) ->
	true = ets:delete(?TAB, {pool, Ref}),
	{noreply, State#state{pools=lists:delete(Ref, Pools)}};
handle_cast({add_worker, Ref, Pid}, State) ->
	Workers = ets:lookup_element(?TAB, {pool, Ref}, 2),
	true = ets:insert(?TAB, {{pool, Ref}, [Pid|Workers]}),
	_ = erlang:monitor(process, Pid),
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', _, process, Pid, _}, State=#state{pools=Pools}) ->
	_ = [begin
		Workers = ets:lookup_element(?TAB, {pool, Ref}, 2),
		case lists:member(Pid, Workers) of
			false ->
				false;
			true ->
				true = ets:insert(?TAB, {{pool, Ref},
					lists:delete(Pid, Workers)})
		end
	end || Ref <- Pools],
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, _State) ->
	ok.

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
