%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2015 Eric des Courtis
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.

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

-define(WEBSOCKET_TIMEOUT,           60 * 1000). %%  1 min
-define(PING_INTERVAL,               10 * 1000). %% 10 secs
-define(DEFAULT_FORWARDING, <<"localhost:22">>). %% Forward to local ssh daemon


-record(state, {opts, tcp_srv, host, port, log_connect_cb}).

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
    HostPort = proplists:get_value(<<"dst">> , cowboy_req:parse_qs(Req0), ?DEFAULT_FORWARDING),
    [HostStr, PortStr] = string:tokens(unicode:characters_to_list(HostPort), ":"),
    Host = unicode:characters_to_binary(HostStr),
    Port = case (catch erlang:list_to_integer(PortStr)) of
        N when is_integer(N) ->
            N;
        _ ->
            {ok, N} = inet:getservbyname(erlang:list_to_atom(PortStr), tcp),
            N
    end,
    Peer = cowboy_req:peer(Req0),
    true = case is_allowed(HostStr, Port) of
        true -> true;
        false ->
            lager:warning(
                "Cannot forward from ~p to ~p access denied by acl",
                [Peer, {HostStr, PortStr}]
            )
    end,
    HttpHost = cowboy_req:host(Req0),
    HttpPort = cowboy_req:port(Req0),
    HttpPath = cowboy_req:path(Req0),
    LogDisconnectCallback = fun(Extra) ->
        lager:info("Disconnected ~p <=/=> ~p - ~p", [{Peer, {HttpHost, HttpPort, HttpPath}}, {Host, Port}, Extra])
    end,
    LogConnectCallback = fun() ->
        lager:info("Connected    ~p <===> ~p", [{Peer, {HttpHost, HttpPort, HttpPath}}, {Host, Port}])
    end,
    {ok, TcpServPid} = tcp_to_websocket_srv:start_link(self(), Host, Port, LogDisconnectCallback),
    erlang:start_timer(?PING_INTERVAL, self(), ping),
    {cowboy_websocket, Req1, #state{
        opts = Opts, tcp_srv = TcpServPid, host = Host, port = Port, log_connect_cb = LogConnectCallback
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
websocket_info(connected, Req, State = #state{log_connect_cb = LCCB}) ->
    LCCB(),
    {ok, Req, State};
websocket_info({timeout, _TimerRef, ping}, Req, State) ->
    Peer = cowboy_req:peer(Req),
    lager:debug("  Sending  ping from peer ~p", [Peer]),
    erlang:start_timer(?PING_INTERVAL, self(), ping),
    {reply, ping, Req, State};
websocket_info(Info, Req, State) ->
    lager:debug("Received Info: ~p for request: ~p", [Info, Req]),
    {ok, Req, State}.

is_allowed(Host, Port) ->
    is_allowed(Host, Port, get_acl()).
    
is_allowed(_Host, _Port, []) ->
    false; %% default is to deny
is_allowed(Host, Port, [{H, P, R} | Rest]) ->
    VH = valid_host(H, Host),
    VP = valid_port(P, Port),
    case {VH, VP} of
        {true, true} ->
            R =:= allow;
        _ ->
            is_allowed(Host, Port, Rest)
    end.
    
get_acl() ->
    case application:get_env(ewstunnel, acl) of
        {ok, Acl} ->
            Acl;
        _ ->
            [
                %% Hostname regex or literal or IP or tuple for a range or all,
                %% port name or number or tuple for a range or all,
                %% allow or deny
                {"^localhost$",       ssh,      allow }, %% Allow SSH to self
                {all,                 all,      deny  }  %% Deny everything else
            ]
    end.


valid_host(HostSettings, Host) when is_binary(Host) ->
    valid_host(HostSettings, unicode:characters_to_list(Host));
valid_host(HostSettings, Host) when is_list(Host) ->
    valid_host1(HostSettings, Host).

valid_host1(all, Host) when is_list(Host) ->
    true;
valid_host1({StartIP, EndIp}, Host) when is_list(StartIP), is_list(EndIp) ->
    StartIpN = ip_str_to_integer(StartIP),
    EndIpN = ip_str_to_integer(EndIp),
    case valid_host_range(StartIpN, EndIpN) of
        true ->
            valid_host2(StartIpN, EndIpN, Host);
        false ->
            false
    end;
valid_host1(HostSetting, Host) when is_list(HostSetting), is_list(Host) ->
    case re:run(Host, HostSetting) of
        {match, _} ->
            true;
        _ ->
            false
    end;
valid_host1(_, _) ->
    false.

valid_host2(StartIpN, EndIpN, Host) ->
    case inet:gethostbyname(Host) of
        {ok, {hostent, _, _, _, _, IPs}} ->
            lists:all(
                fun(H) ->
                    HN = ip_tuple_to_integer(H),
                    (HN >= StartIpN) and (HN =< EndIpN)
                end,
                IPs
            );
        {error, _Reason} ->
            false
    end.

valid_host_range(StartIpN, EndIpN) 
    when StartIpN =< EndIpN,
    (StartIpN >= 0) and (StartIpN < 4294967296),
    (EndIpN >= 0) and (EndIpN < 4294967296) ->
    true;
valid_host_range(_, _) ->
    false.

ip_tuple_to_integer({A, B, C, D}) ->
    (256 * 256 * 256 * A) + (256 * 256 * B) + (256 * C) + D.

ip_str_to_integer(IPStr) when is_list(IPStr) ->
    case inet:gethostbyname(IPStr) of
        {ok, {hostent, _, _, _, _, [IP = {_, _, _, _} | _]}} ->
            {_, N} = lists:foldr(fun(X, {M, T}) -> {M * 256, X * M + T} end, {1, 0}, tuple_to_list(IP)),
            {ok, N};
        Other ->
            Other
    end.

%% integer_to_ip_str(N) when is_integer(N) ->
%%     integer_to_ip_str(N, []).
%%
%% integer_to_ip_str(0, Acc) ->
%%     string:join(Acc, ".");
%% integer_to_ip_str(N0, Acc) ->
%%     N1 = N0 div 256,
%%     integer_to_ip_str(N1, [N0 rem 256 | Acc]).

valid_port(all, Port) when is_integer(Port),
    (Port >= 1) and (Port =< 65535) ->
    true;
valid_port(PortSetting, Port) when is_atom(PortSetting) ->
    case inet:getservbyname(PortSetting, tcp) of
        {ok, PortSettingNum} ->
            valid_port(PortSettingNum, Port);
        {error, einval} ->
            false
    end;
valid_port(PortSetting, Port) when is_integer(Port),
    (Port >= 1) and (Port =< 65535) ->
    valid_port1(PortSetting, Port);
valid_port(PortSetting, Port) when is_atom(Port) ->
    case inet:getservbyname(Port, tcp) of
        {ok, PortNum} ->
            valid_port(PortSetting, PortNum);
        {error, einval} ->
            false
    end;
valid_port(_PortSetting, _Port) ->
    false.

valid_port1(all, _Port) ->
    true;
valid_port1(Port, Port) ->
    true;
valid_port1({StartPort, EndPort}, Port)
    when StartPort =< EndPort, (Port >= StartPort) and (Port =< EndPort) ->
    true;
valid_port1(_, _) ->
    false.

