%%%-------------------------------------------------------------------
%%% File    : error_logger_syslog.erl
%%% Author  : Christopher Faulet <christopher@yakaz.com>
%%% Description :
%%%
%%% Created : 19 Mar 2010 by Christopher Faulet <christopher@yakaz.com>
%%% $Id$
%%%-------------------------------------------------------------------
-module(error_logger_syslog).
-vsn('$Revision$ ').

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
    Prefix = lists:flatten(io_lib:format("    ~p", [Tag])),
    PrefixLength = length(Prefix) + 2,
    CurLength = LineLength - PrefixLength,
    DataStr = lists:flatten(io_lib:print(Data, PrefixLength, CurLength, -1)),
    Result2 = Prefix ++ ": " ++ DataStr ++ "\n",
    format_report2(Rest, Result ++ Result2, LineLength);

format_report2([Term | Rest], Result, LineLength) ->
    Result2 = lists:flatten(io_lib:print(Term, 1, LineLength, -1)) ++ "\n",
    format_report2(Rest, Result ++ Result2, LineLength);

format_report2([], Result, _LineLength) ->
    Result.

is_string([]) -> true;
is_string([H | T]) when is_integer(H), H > 1, H < 255 -> is_string(T);
is_string(_) -> false.
