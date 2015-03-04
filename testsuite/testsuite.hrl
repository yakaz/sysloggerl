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


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("sysloggerl.hrl").


-ifdef(SHOW_LOG).

-define(LOG(Fmt, Args), io:format(standard_error, Fmt, Args)).

-else.

-define(LOG(Fmt, Args), io_lib:format(Fmt, Args)).

-endif.

-define(GET_ENV(VarName), case os:getenv(VarName) of
                              false -> "";
                              _     -> os:getenv(VarName)
                          end).

-define(INIT_SUITE(Config),
        begin
            Bold  = ?GET_ENV("BOLD_COLOR"),
            Std   = ?GET_ENV("STD_COLOR"),
            ?LOG("~n  ~s~p~s...~n", [Bold, ?MODULE, Std])
        end).

-define(END_SUITE(Config),
        begin
            Status  = proplists:get_value(tc_group_result, Config),
            Passed  = proplists:get_value(ok,      Status),
            Failed  = proplists:get_value(failed,  Status),
            Skipped = proplists:get_value(skipped, Status),

            Red   = ?GET_ENV("RED_COLOR"),
            Green = ?GET_ENV("GREEN_COLOR"),
            Std   = ?GET_ENV("STD_COLOR"),
            ?LOG("  Failed: ~s~-5b~sSkipped: ~-5bPassed: ~s~-5b~s~n",
                 [Red, length(Failed), Std,
                  length(Skipped),
                  Green, length(Passed), Std])
        end).

-define(INIT_TESTCASE(Test, Config),
        begin
            ?LOG("    ~p... ", [Test])
        end).

-define(END_TESTCASE(Test, Config),
        begin
            Red   = ?GET_ENV("RED_COLOR"),
            Green = ?GET_ENV("GREEN_COLOR"),
            Std   = ?GET_ENV("STD_COLOR"),
            case proplists:get_value(tc_status, Config) of
                ok -> ?LOG("~sOK~s~n", [Green, Std]);
                _  -> ?LOG("~sKO~s~n", [Red, Std])
            end,
            Test
        end).
