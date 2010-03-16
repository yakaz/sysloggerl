%%%-------------------------------------------------------------------
%%% File    : syslogger.erl
%%% Author  : Christopher Faulet <christopher@yakaz.com>
%%% Description :
%%%
%%% Created : 15 Mar 2010 by Christopher Faulet <christopher@yakaz.com>
%%% $Id$
%%%-------------------------------------------------------------------
-module(syslogger).
-verison('$Revision$ ').

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
start() ->
    application:start(syslogger).

stop() ->
    application:stop(syslogger).

start(_Type, _StartArgs) ->
    case syslogger_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
