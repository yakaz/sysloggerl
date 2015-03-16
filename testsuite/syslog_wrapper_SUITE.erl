%-
% Copyright (c) 2012 Yakaz
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

-module(syslog_wrapper_SUITE).

-include("testsuite.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
         wrap_logger/1
        ]).

all() -> [
          wrap_logger
         ].


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
    application:load(sysloggerl),
    sysloggerl_app:set_param(enable_error_logger, false),
    application:start(sysloggerl),
    Config.


end_per_testcase(Test, Config) ->
    ?END_TESTCASE(Test, Config),
    application:stop(sysloggerl),
    application:unload(sysloggerl),
    ok.

%%====================================================================
wrap_logger(_) ->
    Ident = "my_logger",
    Prio  = syslog:priority(user, info),
    Opts  = [],

    ?assertNot(code:is_loaded(my_logger)),
    ?assertEqual(ok, syslog_wrapper:create(my_logger, Ident, Prio, Opts)),
    ?assertMatch({file, _}, code:is_loaded(my_logger)),
    ?assertEqual(Prio#priority.facility,  my_logger:get_facility()),
    ?assertEqual(Prio#priority.log_level, my_logger:get_loglevel()),

    Logger = syslog:logger(my_logger:get_name()),
    ?assertMatch(#logger{}, Logger),
    ?assertEqual(Ident,     Logger#logger.ident),
    ?assertEqual(Prio,      Logger#logger.priority),

    ?assertEqual(noop, my_logger:debug_msg("foo")),

    Name = my_logger:get_name(),
    ?assertEqual(ok, syslog_wrapper:destroy(my_logger)),
    ?assertNot(code:is_loaded(my_logger)),
    ?assertEqual(not_found, syslog:logger(Name)),
    ok.


%%====================================================================
