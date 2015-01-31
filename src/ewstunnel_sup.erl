%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @copyright (C) 2015, Benbria Corporation
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2015 8:32 PM
%%%-------------------------------------------------------------------
-module(ewstunnel_sup).

-author("Eric des Courtis").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

