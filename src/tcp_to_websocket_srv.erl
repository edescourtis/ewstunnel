%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @copyright (C) 2015, Benbria Corporation
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2015 5:19 PM
%%%-------------------------------------------------------------------
-module(tcp_to_websocket_srv).
-author("Eric des Courtis").

-behaviour(gen_server).

-define(PACKET_BACKLOG, 32).
-define(CONNECT_TIMEOUT, 30 * 1000).
-define(TIMEOUT, 30 * 1000).

%% API
-export([
    start_link/4,
    send/2
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {websocket_pid, tcp_socket, log_disconnect_cb, monitor_ref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(pid(), binary(), 1..65535, fun()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(WebSocketPid, Address, Port, LogDisconnectCallback)
    when is_pid(WebSocketPid), is_binary(Address), is_integer(Port),
    (Port >= 1) and (Port =< 65535) ->
    gen_server:start_link(?MODULE, [WebSocketPid, Address, Port, LogDisconnectCallback], []).

-spec(send(pid(), binary()) -> ok).
send(Pid, Binary) when is_pid(Pid), is_binary(Binary) ->
    gen_server:call(Pid, {send, Binary}, ?TIMEOUT).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([WebSocketPid, Address, Port, LogDisconnectCallback]) ->
    TcpSocket = case gen_tcp:connect(
        unicode:characters_to_list(Address),
        Port,
        [
            binary,
            {active,    ?PACKET_BACKLOG},
            {packet,                raw},
            {keepalive,            true},
            {nodelay,              true}
        ],
        ?CONNECT_TIMEOUT
    ) of
        {ok, TSocket} -> TSocket;
        {error, Reason} ->
            LogDisconnectCallback(Reason),
            exit({error, Reason})
    end,
    WebSocketPid ! connected,
    MonitorRef = erlang:monitor(process, WebSocketPid),
    case erlang:is_process_alive(WebSocketPid) of
        false -> self() ! die; true -> ok 
    end,
    {ok, #state{
        websocket_pid = WebSocketPid,
        tcp_socket = TcpSocket,
        log_disconnect_cb = LogDisconnectCallback,
        monitor_ref = MonitorRef
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({send, Binary}, _From, State = #state{tcp_socket = TcpSocket}) ->
    ok = gen_tcp:send(TcpSocket, Binary),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, TcpSocket, Data}, State = #state{tcp_socket = TcpSocket, websocket_pid = WSP}) ->
    WSP ! {binary, Data},
    {noreply, State};
handle_info({tcp_passive, TcpSocket}, State = #state{tcp_socket = TcpSocket}) ->
    ok = inet:setopts(TcpSocket, [{active, ?PACKET_BACKLOG}]),
    {noreply, State};
handle_info({Reason = tcp_closed, TcpSocket}, State = #state{
    tcp_socket = TcpSocket, websocket_pid = WSP, log_disconnect_cb = LDCB
}) ->
    WSP ! disconnected,
    LDCB(Reason),
    {stop, normal, State};
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, State = #state{
    monitor_ref = MonitorRef,
    log_disconnect_cb = LDCB
}) ->
    LDCB(websocket_closed),
    {stop, normal, State};
handle_info(die, State = #state{log_disconnect_cb = LDCB}) ->
    LDCB(websocket_closed),
    {stop, normal, State};
handle_info({tcp_error, TcpSocket, Reason}, State = #state{
    tcp_socket = TcpSocket, log_disconnect_cb = LDCB
}) ->
    LDCB(Reason),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:info("Unexpected message: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
