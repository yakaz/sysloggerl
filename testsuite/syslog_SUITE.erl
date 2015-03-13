%-
% Copyright (c) 2012-2015 Yakaz
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
% notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
% notice, this list of conditions and the following disclaimer in the
% documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

-module(syslog_SUITE).

-include("testsuite.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
         use_default_logger/1,
         use_custom_logger/1
        ]).

all() -> [
          use_default_logger,
          use_custom_logger
         ].


-define(PRIORITY_RE, "<(\\d{1,3})>").
-define(TS_RE,       "(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s\\d{1,2}\\s\\d{2}:\\d{2}:\\d{2}").
-define(HOST_RE,     "[^\\s]+").
-define(PID_RE,      [$\\,$[, pid_to_list(self()), $\\,$]]).
-define(MSG_RE,      "(.+)").

-define(SYSLOG_RE(Ident),
        [$^,?PRIORITY_RE,?TS_RE,$\s,?HOST_RE,$\s,Ident,$\:,$\s,?MSG_RE,$$]).
-define(SYSLOG_RE(Ident, Pid),
        [$^,?PRIORITY_RE,?TS_RE,$\s,?HOST_RE,$\s,Ident,[$\\,$[, pid_to_list(Pid), $\\,$]],$\:,$\s,?MSG_RE,$$]).


%%====================================================================
init_per_suite(Config) ->
    ?INIT_SUITE(Config),
    application:stop(sysloggerl),
    application:unload(sysloggerl),
    eunit:start(),
    Config.

end_per_suite(Config) ->
    ?END_SUITE(Config),
    eunit:stop(),
    ok.

%%====================================================================
init_per_testcase(Test, Config) ->
    ?INIT_TESTCASE(Test, Config),
    Socket     = open_udp_socket(),
    Host       = "localhost",
    {ok, Port} = inet:port(Socket),
    application:load(sysloggerl),
    sysloggerl_app:set_param(default_syslog_host, Host),
    sysloggerl_app:set_param(default_syslog_port, Port),
    sysloggerl_app:set_param(enable_error_logger, false),
    application:start(sysloggerl),
    [{udp_socket, Socket}, {udp_host, Host}, {udp_port, Port} | Config].


end_per_testcase(Test, Config) ->
    ?END_TESTCASE(Test, Config),
    Socket = ?config(udp_socket, Config),
    close_udp_socket(Socket),
    application:stop(sysloggerl),
    application:unload(sysloggerl),
    ok.

%%====================================================================
use_default_logger(Config) ->
    Socket = ?config(udp_socket, Config),

    Logger = syslog:logger(default),
    ?assertMatch(#logger{name=default}, Logger),
    Ident = Logger#logger.ident,
    Prio  = Logger#logger.priority,
    ?assertEqual("sysloggerl", Ident),
    ?assertEqual(user,   syslog:get_facility(Prio)),
    ?assertEqual(notice, syslog:get_loglevel(Prio)),

    check_logger(Socket, Logger),
    ok.

%% ----
use_custom_logger(Config) ->
    Socket = ?config(udp_socket, Config),

    ?assertEqual(not_found, syslog:logger(my_logger)),
    Ident  = "my_logger",
    Prio   = syslog:priority(user, notice),
    SetRes = syslog:set(my_logger, Ident, Prio, []),
    ?assertMatch({ok, #logger{}}, SetRes),
    {ok, Logger} = SetRes,
    ?assertEqual(Logger, syslog:logger(my_logger)),
    ?assertEqual(Ident,  Logger#logger.ident),
    ?assertEqual(Prio,   Logger#logger.priority),

    check_logger(Socket, Logger),

    ?assertEqual(ok, syslog:unset(my_logger)),
    ?assertEqual(not_found, syslog:logger(my_logger)),
    ok.


%%====================================================================
open_udp_socket() ->
    case gen_udp:open(0, [list, {active, true}, {reuseaddr, true}]) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            io:format("Failed to open udp socket: ~p~n",
                      [inet:format_error(Reason)]),
            exit({error, Reason})
    end.

close_udp_socket(Socket) ->
    gen_udp:close(Socket).

read_syslog_message(Socket) ->
    receive
        {udp, Socket, _, _, Packet} ->
            Packet;
        {udp_closed, Socket} ->
            exit({error, udp_closed})
    after 100 ->
            no_udp_packet
    end.


check_logger(Socket, Logger) ->
    Prio   = Logger#logger.priority,
    Levels = [emergency, alert, critical, error, warning, notice, info, debug],

    log(Socket, Logger, log, "Hello World!"),
    log(Socket, Logger, log, "My current prio ~p", [Prio]),
    [begin
         ?assert(syslog:is_loglevel_valid(Lvl)),
         ?assertMatch(
            {ok, #logger{}},
            syslog:set(Logger#logger{priority=syslog:set_loglevel(Lvl, Prio)})
           ),
         Lg = syslog:logger(Logger#logger.name),

         log(Socket, Lg, log, "Hello World!"),
         log(Socket, Lg, log, "My current level ~p", [Lvl]),

         [begin
              Fun = list_to_atom(lists:flatten(io_lib:format("~p_msg", [L]))),
              case syslog:Lvl() >= syslog:L() of
                  true ->
                      log(Socket, Lg, Fun, "Hello World!"),
                      log(Socket, Lg, Fun, "show ~p messages because my current level is ~p", [L, Lvl]);
                  false ->
                      nolog(Socket, Lg, Fun, "Ignored message"),
                      nolog(Socket, Lg, Fun, "do not show ~p messages because my current level is ~p", [L, Lvl])
              end
          end || L <- Levels]

     end || Lvl <- Levels],
    ok.

log(Socket, Logger, Fun, Msg) ->
    FArgs = if
                Logger#logger.name == default -> [Msg];
                Fun == log -> [Logger#logger.name, #priority{}, Msg, []];
                true -> [Logger#logger.name, Msg, []]
            end,
    do_log(Socket, Logger, Fun, FArgs, Msg).

log(Socket, Logger, Fun, Fmt, Args) ->
    FArgs = if
                Logger#logger.name == default -> [Fmt, Args];
                Fun == log -> [Logger#logger.name, #priority{}, Fmt, Args];
                true -> [Logger#logger.name, Fmt, Args]
            end,
    ExepectedMsg = lists:flatten(io_lib:format(Fmt,Args)),
    do_log(Socket, Logger, Fun, FArgs, ExepectedMsg).

nolog(Socket, Logger, Fun, Msg) ->
    FArgs = if
                Logger#logger.name == default -> [Msg];
                Fun == log -> [Logger#logger.name, #priority{}, Msg, []];
                true -> [Logger#logger.name, Msg, []]
            end,
    do_log(Socket, Logger, Fun, FArgs, no_udp_packet).

nolog(Socket, Logger, Fun, Fmt, Args) ->
    FArgs = if
                Logger#logger.name == default -> [Fmt, Args];
                Fun == log -> [Logger#logger.name, #priority{}, Fmt, Args];
                true -> [Logger#logger.name, Fmt, Args]
            end,
    do_log(Socket, Logger, Fun, FArgs, no_udp_packet).

do_log(Socket, Logger, Fun, Args, ExepectedMsg) ->
    apply(syslog, Fun, Args),
    Packet = read_syslog_message(Socket),
    if
        ExepectedMsg == no_udp_packet ->
            ?assertEqual(no_udp_packet, Packet);
        true ->
            RE = case proplists:get_bool(log_pid, Logger#logger.options) of
                     true  -> ?SYSLOG_RE(Logger#logger.ident, self());
                     false -> ?SYSLOG_RE(Logger#logger.ident)
                 end,
            MatchRes = re:run(Packet, RE, [{capture, all_but_first, list}]),
            ?assertMatch({match, _}, MatchRes),
            {match, [_, Msg]} = MatchRes,
            ?assertEqual(ExepectedMsg, Msg)
    end.
