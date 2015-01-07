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

-module(error_logger_syslog).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {ident       :: string(),
                level       :: syslog:loglevel(),
                facility    :: syslog:facility(),
                depth       :: integer(),
                line_length :: pos_integer()}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
init([]) ->
    Level = sysloggerl_app:get_param(error_logger_loglevel),
    S = #state{ident      = sysloggerl_app:get_param(error_logger_ident),
               level      = syslog:Level(),
               facility   = sysloggerl_app:get_param(error_logger_facility),
               depth      = sysloggerl_app:get_param(error_logger_depth),
               line_length= sysloggerl_app:get_param(error_logger_line_length)},

    syslog:add(?MODULE, S#state.ident, S#state.facility, Level, []),
    {ok, S}.

handle_event({error, _Gleader, {Pid, Format, Data}}, State)
  when State#state.level >= 3 ->
    put(logged_pid, Pid),
    Msg = format_message(error, Format, Data),
    syslog:error_msg(?MODULE, Msg, []),
    {ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State)
  when State#state.level >= 4 ->
    put(logged_pid, Pid),
    Msg = format_message(warning, Format, Data),
    syslog:warning_msg(?MODULE, Msg, []),
    {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State)
  when State#state.level >= 6 ->
    put(logged_pid, Pid),
    Msg = format_message(info, Format, Data),
    syslog:info_msg(?MODULE, Msg, []),
    {ok, State};

handle_event({error_report, _Gleader, {Pid, Type, Report}}, State)
  when State#state.level >= 3 ->
    put(logged_pid, Pid),
    Msg = format_report(Type, Report, State),
    syslog:error_msg(?MODULE, Msg, []),
    {ok, State};
handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State)
  when State#state.level >= 4 ->
    put(logged_pid, Pid),
    Msg = format_report(Type, Report, State),
    syslog:warning_msg(?MODULE, Msg, []),
    {ok, State};
handle_event({info_report, _Gleader, {Pid, Type, Report}}, State)
  when State#state.level >= 6 ->
    put(logged_pid, Pid),
    Msg = format_report(Type, Report, State),
    syslog:info_msg(?MODULE, Msg, []),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    syslog:remove(?MODULE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
format_message(Type, Format, Args) ->
    lists:flatten([format_type(Type), io_lib:format(Format, Args)]).

format_report(Type, Report, State) when is_list(Report) ->
    case io_lib:char_list(Report) of
        true  -> lists:flatten([format_type(Type), Report]);
        false -> format_report2(Type, Report, "", State)
    end;
format_report(Type, Report, State) ->
    lists:flatten([format_type(Type), io_lib:print(Report, 1,
                                                   State#state.line_length,
                                                   State#state.depth)]).


format_report2(Type, [{Tag, Data} | Rest], Result, State) ->
    Prefix       = io_lib:format("    ~p: ", [Tag]),
    PrefixLength = iolist_size(Prefix) + 1,
    DataStr      = io_lib:print(Data, PrefixLength,
                                State#state.line_length,
                                State#state.depth),
    Result2      = [Prefix, DataStr, $\n],
    format_report2(Type, Rest, [Result2|Result], State);

format_report2(Type, [Term | Rest], Result, State) ->
    Result2 = ["    ", io_lib:print(Term, 1, State#state.line_length,
                                    State#state.depth), $\n],
    format_report2(Type, Rest, [Result2|Result], State);
format_report2(Type, [], Result, _) ->
    lists:flatten([format_type(Type), lists:reverse(Result)]).


format_type(supervisor_report) ->
    "= SUPERVISOR REPORT ====\n";
format_type(crash_report) ->
    "= CRASH REPORT ====\n";
format_type(progress) ->
    "= PROGRESS REPORT ====\n";
format_type(std_error) ->
    "= ERROR REPORT ====\n";
format_type(std_warning) ->
    "= WARNING REPORT ====\n";
format_type(std_info) ->
    "= INFO REPORT ====\n";
format_type(error) ->
    "ERROR: ";
format_type(warning) ->
    "WARNING: ";
format_type(info) ->
    "INFO: ";
format_type(Type) ->
    io_lib:format("~p: ", [Type]).
