%-
% Copyright (c) 2012-2014 Yakaz
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

-record(state, {}).


%%====================================================================
%% gen_event callbacks
%%====================================================================
init([]) ->
    {ok, #state{}}.

handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
    put(logged_pid, Pid),
    syslog:error_msg(Format, Data),
    {ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
    put(logged_pid, Pid),
    syslog:warning_msg(Format, Data),
    {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
    put(logged_pid, Pid),
    syslog:info_msg(Format, Data),
    {ok, State};

handle_event({error_report, _Gleader, {Pid, _Type, Report}}, State) ->
    put(logged_pid, Pid),
    Format = format_report(Report),
    syslog:error_msg(Format, []),
    {ok, State};
handle_event({warning_report, _Gleader, {Pid, _Type, Report}}, State) ->
    put(logged_pid, Pid),
    Format = format_report(Report),
    syslog:warning_msg(Format, []),
    {ok, State};
handle_event({info_report, _Gleader, {Pid, _Type, Report}}, State) ->
    put(logged_pid, Pid),
    Format = format_report(Report),
    syslog:info_msg(Format, []),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
format_report(Report) when is_list(Report) ->
    case is_string(Report) of
        true  -> Report;
        false -> format_report2(Report, "", 1000)
    end;
format_report(Report) ->
    lists:flatten(io_lib:print(Report, 1, 1000, -1)).


format_report2([{Tag, Data} | Rest], Result, LineLength) ->
    Prefix       = lists:flatten(io_lib:format("    ~p", [Tag])),
    PrefixLength = length(Prefix) + 2,
    CurLength    = LineLength - PrefixLength,
    DataStr      = io_lib:print(Data, PrefixLength, CurLength, -1),
    Result2      = [Prefix, $:, $\s, DataStr, $\n],
    format_report2(Rest, [Result2|Result], LineLength);
format_report2([Term | Rest], Result, LineLength) ->
    Result2 = [io_lib:print(Term, 1, LineLength, -1), $\n],
    format_report2(Rest, [Result2|Result], LineLength);
format_report2([], Result, _LineLength) ->
    lists:flatten(lists:reverse(Result)).

is_string([H | T]) when is_integer(H), H > 1, H < 255 -> is_string(T);
is_string([])                                         -> true;
is_string(_)                                          -> false.
