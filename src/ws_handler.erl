%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @copyright (C) 2015, Benbria Corporation
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2015 9:45 PM
%%%-------------------------------------------------------------------
-module(ws_handler).
-author("Eric des Courtis").
-author("Dave van Rijswijk").

-define(WEBSOCKET_TIMEOUT, 60 * 1000). %%  1 min
-define(PING_INTERVAL,     10 * 1000). %% 10 secs
-record(state, {opts, tcp_srv, host, port}).

%% API
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req0, Opts) ->
    process_flag(trap_exit, true),
    Req1 = cowboy_req:set_resp_header(
        <<"sec-websocket-protocol">>,
        <<"tunnel-protocol">>,
        Req0
    ),
    Host = <<"localhost">>,
    Port = 22,
    Peer = cowboy_req:peer(Req0),
    LogDisconnectCallback = fun() ->
        lager:info("Disconnected ~p <=/=> ~p", [Peer, {Host, Port}])
    end,
    {ok, TcpServPid} = tcp_to_websocket_srv:start_link(self(), Host, Port, LogDisconnectCallback),
    erlang:start_timer(?PING_INTERVAL, self(), ping),
    {cowboy_websocket, Req1, #state{
        opts = Opts, tcp_srv = TcpServPid, host = Host, port = Port
    }}.

websocket_handle({binary, Msg}, Req, State = #state{tcp_srv = TcpServPid}) ->
    ok = tcp_to_websocket_srv:send(TcpServPid, Msg),
    {ok, Req, State};
websocket_handle({pong, Data}, Req, State) ->
    Peer = cowboy_req:peer(Req),
    lager:debug("Received pong from peer ~p with data ~p", [Peer, Data]),
    {ok, Req, State};
websocket_handle(Data, Req, State) ->
    lager:debug("Received Data: ~p for request: ~p", [Data, Req]),
    {ok, Req, State}.

websocket_info({binary, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State};
websocket_info(disconnected, Req, State) ->
    {reply, close, Req, State};
websocket_info(connected, Req, State = #state{host = Host, port = Port}) ->
    Peer = cowboy_req:peer(Req),
    lager:info("Connected    ~p <===> ~p", [Peer, {Host, Port}]),
    {ok, Req, State};
websocket_info({timeout, _TimerRef, ping}, Req, State) ->
    Peer = cowboy_req:peer(Req),
    lager:debug("Sending  ping from peer ~p", [Peer]),
    erlang:start_timer(?PING_INTERVAL, self(), ping),
    {reply, ping, Req, State};
websocket_info(Info, Req, State) ->
    lager:debug("Received Info: ~p for request: ~p", [Info, Req]),
    {ok, Req, State}.
