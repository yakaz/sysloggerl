%%%-------------------------------------------------------------------
%%% File    : syslogger_sup.erl
%%% Author  : Christopher Faulet <christopher@yakaz.com>
%%% Description :
%%%
%%% Created : 15 Mar 2010 by Christopher Faulet <christopher@yakaz.com>
%%% $Id$
%%%-------------------------------------------------------------------
-module(syslogger_sup).
-vsn('$Revision$ ').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {syslog, {syslog, start_link, []},
              Restart, Shutdown, Type, [syslog]},

    {ok, {SupFlags, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
