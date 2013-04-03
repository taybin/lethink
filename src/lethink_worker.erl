-module(lethink_worker).

-export([start_link/4,
         db_create/2,
         db_drop/2,
         use/2,
         db_list/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("ql2_pb.hrl").

-type opts() :: [none | {database, string()}].
-type res_success() :: {ok, [#datum{}]}.
-type res_error() :: {error, [#datum{}], [#backtrace{}]}.

-export_type([opts/0]).

-record(state, {
    socket :: port(),
    token :: pos_integer(),
    database = undefined :: undefined | string()
}).

-define(RETHINKDB_VERSION, 16#3f61ba36). % magic number from ql2.proto

-spec start_link(any(), string(), pos_integer(), opts()) -> any().
start_link(Ref, Host, Port, Opts) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Host, Port, Opts], []),
    lethink_server:add_worker(Ref, Pid),
    {ok, Pid}.

-spec db_create(pid(), string()) -> ok.
db_create(Pid, Name) ->
    gen_server:call(Pid, {db_create, Name}).

-spec db_drop(pid(), string()) -> ok.
db_drop(Pid, Name) ->
    gen_server:call(Pid, {db_drop, Name}).

-spec db_list(pid()) -> [string()].
db_list(Pid) ->
    gen_server:call(Pid, {db_list}).

-spec use(pid(), string()) -> ok.
use(Pid, Name) ->
    gen_server:cast(Pid, {use, Name}).

init([Host, Port, Opts]) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    ok = gen_tcp:send(Sock, binary:encode_unsigned(?RETHINKDB_VERSION, little)),
    State = #state{
            socket = Sock,
            token = 1,
            database = proplists:get_value(database, Opts)
    },
    {ok, State}.

handle_call({db_create, Name}, _From, State) ->
    Query = ql2_wrapper:db_create(Name, State#state.token),
    Response = send_and_recv(Query, State#state.socket),
    {reply, Response, State#state{ token = State#state.token + 1 }};

handle_call({db_drop, Name}, _From, State) ->
    Query = ql2_wrapper:db_drop(Name, State#state.token),
    Response = send_and_recv(Query, State#state.socket),
    {reply, Response, State#state{ token = State#state.token + 1 }};

handle_call({db_list}, _From, State) ->
    Query = ql2_wrapper:db_list(State#state.token),
    {ok, [Response]} = send_and_recv(Query, State#state.socket),
    List = [Datum#datum.r_str || Datum <- Response#datum.r_array],
    {reply, List, State#state{ token = State#state.token + 1 }};

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

-spec handle_response(#response{}) -> res_success() | res_error().
handle_response(#response{ type = 'SUCCESS_ATOM'} = Response) ->
    {ok, Response#response.response};
handle_response(#response{ type = 'SUCCESS_SEQUENCE'} = Response) ->
    {ok, Response#response.response};
handle_response(#response{ type = 'SUCCESS_PARTIAL'} = Response) ->
    {ok, Response#response.response};

handle_response(#response{ type = 'CLIENT_ERROR'} = Response) ->
    {error, Response#response.response, Response#response.backtrace};
handle_response(#response{ type = 'COMPILE_ERROR'} = Response) ->
    {error, Response#response.response, Response#response.backtrace};
handle_response(#response{ type = 'RUNTIME_ERROR'} = Response) ->
    {error, Response#response.response, Response#response.backtrace}.
