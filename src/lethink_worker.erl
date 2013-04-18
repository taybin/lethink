%% @private
-module(lethink_worker).

-export([start_link/2,
         use/2,
         query/2]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-include("ql2_pb.hrl").

-record(state, {
    socket :: port(),
    database :: binary(),
    token = 1 :: pos_integer()
}).

-define(RETHINKDB_VERSION, 16#3f61ba36). % magic number from ql2.proto

-spec start_link(any(), [lethink:connect_options()]) -> any().
start_link(Ref, Opts) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Opts], []),
    lethink_server:add_worker(Ref, Pid),
    {ok, Pid}.

-spec use(pid(), binary()) -> ok.
use(Pid, Name) when is_binary(Name) ->
    gen_server:cast(Pid, {use, Name}).

-spec query(pid(), #term{}) -> lethink:response().
query(Pid, Query) ->
    Timeout = application:get_env(lethink, timeout, 30000),
    gen_server:call(Pid, {query, Query}, Timeout).

init([Opts]) ->
    Host = proplists:get_value(address, Opts, {127,0,0,1}),
    Port = proplists:get_value(port, Opts, 28015),
    Database = proplists:get_value(database, Opts, <<"test">>),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    ok = gen_tcp:send(Sock, binary:encode_unsigned(?RETHINKDB_VERSION, little)),
    State = #state{
            socket = Sock,
            database = Database
    },
    {ok, State}.

-spec handle_call(tuple(), pid(), #state{}) -> {reply, lethink:response(), #state{}}.
handle_call({query, Term}, _From, State) ->
    Query = #query {
        type = 'START',
        query = Term,
        token = State#state.token,
        global_optargs = [ql2_util:global_db(State#state.database)]
    },
    Reply = send_and_recv(Query, State#state.socket),
    {reply, Reply, State#state{ token = State#state.token + 1 }};

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast({use, Name}, State) ->
    {noreply, State#state{database = Name}};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:fwrite("Info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    io:fwrite("terminating: ~p", [Reason]),
    gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec send_and_recv(#query{}, port()) -> lethink:response().
send_and_recv(Query, Socket) ->
    send(Query, Socket),
    Response = recv(Socket),
    handle_response(ql2_pb:decode_response(Response)).

-spec send(#query{}, port()) -> any().
send(Query, Socket) ->
    Iolist = ql2_pb:encode_query(Query),
    Length = iolist_size(Iolist),
    gen_tcp:send(Socket, [<<Length:32/little-unsigned>>, Iolist]).

-spec recv(port()) -> any().
recv(Socket) ->
    {ok, ResponseLength} = gen_tcp:recv(Socket, 4),
    {ok, Response} = gen_tcp:recv(Socket, binary:decode_unsigned(ResponseLength, little)),
    Response.

-spec handle_response(#response{}) -> lethink:response().
handle_response(#response{ type = 'SUCCESS_ATOM', response = [Datum]}) ->
    {ok, ql2_util:datum_value(Datum)};
handle_response(#response{ type = 'SUCCESS_SEQUENCE', response = [Datum]}) ->
    {ok, ql2_util:datum_value(Datum)};
handle_response(#response{ type = 'SUCCESS_PARTIAL', response = [Datum]}) ->
    {ok, ql2_util:datum_value(Datum)};

handle_response(#response{ type = 'CLIENT_ERROR', response = [Datum]} = Response) ->
    ErrorMsg = ql2_util:datum_value(Datum),
    {error, ErrorMsg, Response#response.type, Response#response.backtrace};
handle_response(#response{ type = 'COMPILE_ERROR', response = [Datum]} = Response) ->
    ErrorMsg = ql2_util:datum_value(Datum),
    {error, ErrorMsg, Response#response.type, Response#response.backtrace};
handle_response(#response{ type = 'RUNTIME_ERROR', response = [Datum]} = Response) ->
    ErrorMsg = ql2_util:datum_value(Datum),
    {error, ErrorMsg, Response#response.type, Response#response.backtrace}.
