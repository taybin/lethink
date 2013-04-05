%% @private
-module(lethink_worker).

-export([start_link/2,
         db_create/2,
         db_drop/2,
         use/2,
         db_list/1,
         table_create/3,
         table_drop/2,
         table_list/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("ql2_pb.hrl").

-record(state, {
    socket :: port(),
    database :: lethink:name()
}).

-define(RETHINKDB_VERSION, 16#3f61ba36). % magic number from ql2.proto

-spec start_link(any(), [lethink:connect_options()]) -> any().
start_link(Ref, Opts) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Opts], []),
    lethink_server:add_worker(Ref, Pid),
    {ok, Pid}.

-spec db_create(pid(), lethink:name()) -> lethink:response().
db_create(Pid, Name) ->
    gen_server:call(Pid, {db_create, Name}).

-spec db_drop(pid(), lethink:name()) -> lethink:response().
db_drop(Pid, Name) ->
    gen_server:call(Pid, {db_drop, Name}).

-spec db_list(pid()) -> lethink:response().
db_list(Pid) ->
    gen_server:call(Pid, {db_list}).

-spec use(pid(), lethink:name()) -> ok.
use(Pid, Name) ->
    gen_server:cast(Pid, {use, Name}).

-spec table_create(pid(), lethink:name(), [lethink:table_options()]) -> lethink:response().
table_create(Pid, Table, Opts) ->
    gen_server:call(Pid, {table_create, Table, Opts}).

-spec table_drop(pid(), lethink:name()) -> lethink:response().
table_drop(Pid, Table) ->
    gen_server:call(Pid, {table_drop, Table}).

-spec table_list(pid()) -> lethink:response().
table_list(Pid) ->
    gen_server:call(Pid, {table_list}).

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

handle_call({db_create, Name}, _From, State) ->
    Query = ql2_wrapper:db_create(Name),
    Reply = send_and_recv(Query, State#state.socket),
    {reply, Reply, State};

handle_call({db_drop, Name}, _From, State) ->
    Query = ql2_wrapper:db_drop(Name),
    Reply = send_and_recv(Query, State#state.socket),
    {reply, Reply, State};

handle_call({db_list}, _From, State) ->
    Query = ql2_wrapper:db_list(),
    Reply = send_and_recv(Query, State#state.socket),
    {reply, Reply, State};

handle_call({table_create, Name, Opts}, _From, State) ->
    Query = ql2_wrapper:table_create(State#state.database, Name, Opts),
    Reply = send_and_recv(Query, State#state.socket),
    {reply, Reply, State};

handle_call({table_drop, Name}, _From, State) ->
    Query = ql2_wrapper:table_drop(State#state.database, Name),
    Reply = send_and_recv(Query, State#state.socket),
    {reply, Reply, State};

handle_call({table_list}, _From, State) ->
    Query = ql2_wrapper:table_list(State#state.database),
    Reply = send_and_recv(Query, State#state.socket),
    {reply, Reply, State};

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

-spec send_and_recv(#query{}, port()) -> {ok, any()} | {error, any()}.
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
