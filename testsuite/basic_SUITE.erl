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

-module(basic_SUITE).

-include("testsuite.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
         load_default_app/1,
         start_stop_default_app/1,
         invalid_config/1
        ]).

all() -> [
          load_default_app,
          start_stop_default_app,
          invalid_config
         ].

%%====================================================================
init_per_suite(Config) ->
    ?INIT_SUITE(Config),
    eunit:start(),
    Config.

end_per_suite(Config) ->
    ?END_SUITE(Config),
    eunit:stop(),
    ok.

%%====================================================================
init_per_testcase(Test, Config) ->
    ?INIT_TESTCASE(Test, Config),
    Config.


end_per_testcase(Test, Config) ->
    ?END_TESTCASE(Test, Config),
    ok.

%%====================================================================
load_default_app(_) ->
    application:load(sysloggerl),

    %% Print default value for mandatory parameters
    sysloggerl_app:show_params(),

    %% Then check their values in accordance with the documentation
    [check_default_param(Param) || Param <- sysloggerl_app:params_list()],

    ok.

check_default_param(default_syslog_host) ->
    ?assertEqual("localhost", sysloggerl_app:get_param(default_syslog_host));
check_default_param(default_syslog_port) ->
    ?assertEqual(514, sysloggerl_app:get_param(default_syslog_port));
check_default_param(default_ident) ->
    ?assertEqual("sysloggerl", sysloggerl_app:get_param(default_ident));
check_default_param(default_facility) ->
    ?assertEqual(user, sysloggerl_app:get_param(default_facility));
check_default_param(default_loglevel) ->
    ?assertEqual(notice, sysloggerl_app:get_param(default_loglevel));
check_default_param(enable_error_logger) ->
    ?assertEqual(true, sysloggerl_app:get_param(enable_error_logger));
check_default_param(error_logger_ident) ->
    ?assertEqual("error_logger", sysloggerl_app:get_param(error_logger_ident));
check_default_param(error_logger_facility) ->
    ?assertEqual(user, sysloggerl_app:get_param(error_logger_facility));
check_default_param(error_logger_loglevel) ->
    ?assertEqual(info, sysloggerl_app:get_param(error_logger_loglevel));
check_default_param(error_logger_depth) ->
    ?assertEqual(-1, sysloggerl_app:get_param(error_logger_depth));
check_default_param(error_logger_line_length) ->
    ?assertEqual(80, sysloggerl_app:get_param(error_logger_line_length));
check_default_param(error_logger_tty) ->
    ?assertEqual(false, sysloggerl_app:get_param(error_logger_tty));
check_default_param(no_crash_report) ->
    ?assertEqual(false, sysloggerl_app:get_param(no_crash_report));
check_default_param(no_supervisor_report) ->
    ?assertEqual(false, sysloggerl_app:get_param(no_supervisor_report));
check_default_param(no_progress_report) ->
    ?assertEqual(false, sysloggerl_app:get_param(no_progress_report)).


%% ----
start_stop_default_app(_) ->
    ?assertEqual(ok, application:start(sysloggerl)),

    %% By default, there are 2 loggers:
    %%   * the default logger
    %%   * the logger dedicated to events reported by the error logger
    ?assertEqual(2, length(syslog:loggers())),
    ?assertMatch(#logger{}, syslog:logger(default)),
    ?assertMatch(#logger{}, syslog:logger(error_logger_syslog)),

    %% Check that the error_logger_syslog handler is registered
    ?assert(lists:member(error_logger_syslog,
                         gen_event:which_handlers(error_logger))),

    %% By default, printout of standard events to the tty are disabled
    ?assertNot(sysloggerl_app:get_param(error_logger_tty)),
    ?assertNot(lists:member(error_logger_tty_h,
                            gen_event:which_handlers(error_logger))),

    ?assertEqual(ok, application:stop(sysloggerl)),
    ok.

%% ----
invalid_config(_) ->
    application:load(sysloggerl),
    sysloggerl_app:set_param(default_syslog_host, invalid_host),

    ?assertMatch({error, _}, application:start(sysloggerl)),

    ?assertEqual(error, sysloggerl_app:check_and_set_param(default_syslog_host,      not_a_string)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(default_syslog_port,      not_a_integer)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(default_ident,            not_a_string)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(default_facility,         not_a_facility)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(default_loglevel,         not_a_loglevel)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(enable_error_logger,      not_a_boolean)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(error_logger_ident,       not_a_string)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(error_logger_facility,    not_a_facility)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(error_logger_loglevel,    not_a_loglevel)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(error_logger_depth,       not_a_integer)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(error_logger_line_length, not_a_integer)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(error_logger_tty,         not_a_boolean)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(no_crash_report,          not_a_boolean)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(no_supervisor_report,     not_a_boolean)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(no_progress_report,       not_a_boolean)),
    ?assertEqual(error, sysloggerl_app:check_and_set_param(unknown_param,            true)),
    ok.
