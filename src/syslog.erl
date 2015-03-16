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

-module(syslog).

-behaviour(gen_server).

-include("sysloggerl.hrl").

%% API
-export([
         start_link/0,
         set/1, set/4,
         unset/1,
         loggers/0,
         logger/1,

         priority/2,
         set_facility/2, get_facility/1,
         set_loglevel/2, get_loglevel/1,
         is_facility_valid/1,
         is_loglevel_valid/1,

         log/1, log/2, log/3, log/4,

         emergency_msg/1, emergency_msg/2, emergency_msg/3, emergency_msg/4,
         alert_msg/1,     alert_msg/2,     alert_msg/3,     alert_msg/4,
         critical_msg/1,  critical_msg/2,  critical_msg/3,  critical_msg/4,
         error_msg/1,     error_msg/2,     error_msg/3,     error_msg/4,
         warning_msg/1,   warning_msg/2,   warning_msg/3,   warning_msg/4,
         notice_msg/1,    notice_msg/2,    notice_msg/3,    notice_msg/4,
         info_msg/1,      info_msg/2,      info_msg/3,      info_msg/4,
         debug_msg/1,     debug_msg/2,     debug_msg/3,     debug_msg/4
        ]).

-export([
         emergency/0,
         alert/0,
         critical/0,
         error/0,
         warning/0,
         notice/0,
         info/0,
         debug/0
        ]).

-export([
         kern/0,
         user/0,
         mail/0,
         daemon/0,
         auth/0,
         syslog/0,
         lpr/0,
         news/0,
         uucp/0,
         cron/0,
         authpriv/0,
         ftp/0,
         local0/0, local1/0, local2/0, local3/0, local4/0, local5/0, local6/0,
         local7/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ----
-export_type([facility/0, loglevel/0, priority/0, logger/0]).

-type facility() :: kern   | user   | mail   | daemon | auth     | syslog
                  | lpr    | news   | uucp   | cron   | authpriv | ftp
                  | local0 | local1 | local2 | local3 | local4   | local5
                  | local6 | local7.

-type loglevel() :: emergency | alert | critical | error | warning | notice
                  | info      | debug.

-type priority() :: #priority{}.

-type logger() :: #logger{}.

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ----
-spec set(Name, Ident, Priority, Options) -> Result when
      Name     :: any(),
      Ident    :: string(),
      Priority :: syslog:priority(),
      Options  :: proplists:proplist(),
      Result   :: {ok, syslog:logger()}
                | {error, invalid_facility | invalid_loglevel}
                | {error, inet:posix()}.

set(Name, Ident, Prio, Opts) ->
    case {is_facility_valid(Prio#priority.facility),
          is_loglevel_valid(Prio#priority.log_level)} of
        {true, true} -> gen_server:call(?MODULE, {set,Name,Ident,Prio,Opts});
        {false, _}   -> {error, invalid_facility};
        {_, false}   -> {error, invalid_loglevel}
    end.


%% ----
-spec set(Logger) -> Result when
      Logger   :: syslog:logger(),
      Result   :: {ok, syslog:logger()}
                | {error, invalid_facility | invalid_loglevel}
                | {error, inet:posix()}.

set(Logger) ->
    set(Logger#logger.name, Logger#logger.ident,
        Logger#logger.priority, Logger#logger.options).

%% ----
-spec unset(any()) -> ok.

unset(Name) ->
    gen_server:call(?MODULE, {unset, Name}).

%% ----
-spec loggers() -> [syslog:logger()].

loggers() ->
    ets:tab2list(syslog_loggers).

%% ----
-spec logger(Name) -> Result when
      Name   :: any(),
      Result :: syslog:logger() | not_found.

logger(Name) ->
    case ets:lookup(syslog_loggers, Name) of
        []       -> not_found;
        [Logger] -> Logger
    end.

%% ----
-spec priority(Facility, LogLevel) -> Result when
      Facility :: syslog:facility() | undefined,
      LogLevel :: syslog:loglevel(),
      Result   :: syslog:priority()
                | {error, invalid_facility | invalid_loglevel}.

priority(undefined, LogLevel) ->
    case is_loglevel_valid(LogLevel) of
        true  -> #priority{log_level=LogLevel};
        false -> {error, invalid_loglevel}
    end;
priority(Facility, LogLevel) ->
    case {is_facility_valid(Facility), is_loglevel_valid(LogLevel)} of
        {true,  true}  -> #priority{facility=Facility, log_level=LogLevel};
        {false, _}     -> {error, invalid_facility};
        {_,     false} -> {error, invalid_loglevel}
    end.

%% ----
-spec set_facility(Facility, Priority) -> Result when
      Priority :: syslog:priority(),
      Facility :: syslog:facility(),
      Result   :: syslog:priority() | {error, invalid_facility}.

-spec get_facility(Priority) -> Facility when
      Priority :: syslog:priority(),
      Facility :: syslog:facility().

set_facility(Facility, Priority) ->
    case is_facility_valid(Facility) of
        true  -> Priority#priority{facility=Facility};
        false -> {error, invalid_facility}
    end.

get_facility(#priority{facility=Facility}) -> Facility.

%% ----
-spec set_loglevel(LogLevel, Priority) -> Result when
      Priority :: syslog:priority(),
      LogLevel :: syslog:loglevel(),
      Result   :: syslog:priority() | {error, invalid_loglevel}.

-spec get_loglevel(Priority) -> LogLevel when
      Priority :: syslog:priority(),
      LogLevel :: syslog:loglevel().

set_loglevel(LogLevel, Priority) ->
    case is_loglevel_valid(LogLevel) of
        true  -> Priority#priority{log_level=LogLevel};
        false -> {error, invalid_loglevel}
    end.

get_loglevel(#priority{log_level=LogLevel}) -> LogLevel.

%% ----
-spec is_facility_valid(any()) -> boolean().

is_facility_valid(kern)     -> true;
is_facility_valid(user)     -> true;
is_facility_valid(mail)     -> true;
is_facility_valid(daemon)   -> true;
is_facility_valid(auth)     -> true;
is_facility_valid(syslog)   -> true;
is_facility_valid(lpr)      -> true;
is_facility_valid(news)     -> true;
is_facility_valid(uucp)     -> true;
is_facility_valid(cron)     -> true;
is_facility_valid(authpriv) -> true;
is_facility_valid(ftp)      -> true;
is_facility_valid(local0)   -> true;
is_facility_valid(local1)   -> true;
is_facility_valid(local2)   -> true;
is_facility_valid(local3)   -> true;
is_facility_valid(local4)   -> true;
is_facility_valid(local5)   -> true;
is_facility_valid(local6)   -> true;
is_facility_valid(local7)   -> true;
is_facility_valid(_)        -> false.


%% ----
-spec is_loglevel_valid(any()) -> boolean().

is_loglevel_valid(emergency) -> true;
is_loglevel_valid(alert)     -> true;
is_loglevel_valid(critical)  -> true;
is_loglevel_valid(error)     -> true;
is_loglevel_valid(warning)   -> true;
is_loglevel_valid(notice)    -> true;
is_loglevel_valid(info)      -> true;
is_loglevel_valid(debug)     -> true;
is_loglevel_valid(_)         -> false.

%%====================================================================
-spec log(Message) -> Result when
      Message         :: string(),
      Result          :: ok | {error, inet:posix()}.
-spec log(Format, Arg) -> Result when
      Format          :: string(),
      Arg             :: list(),
      Result          :: ok | {error, inet:posix()}.
-spec log(PriorityOrLevel, Format, Arg) -> Result when
      PriorityOrLevel :: syslog:priority() | syslog:loglevel(),
      Format          :: string(),
      Arg             :: list(),
      Result          :: ok | {error, inet:posix()}.
-spec log(Name, PriorityOrLevel, Format, Arg) -> Result when
      Name            :: any(),
      PriorityOrLevel :: syslog:priority() | syslog:loglevel(),
      Format          :: string(),
      Arg             :: list(),
      Result          :: ok | {error, inet:posix()}.

log(Message) ->
    log(default, #priority{}, Message, []).

log(Format, Args) ->
    log(default, #priority{}, Format, Args).

log(#priority{}=Priority, Format, Args) ->
    log(default, Priority, Format, Args);
log(LogLevel, Format, Args) ->
    log(default, #priority{log_level=LogLevel}, Format, Args).

log(Name, #priority{}=Priority, Format, Args) ->
    send_syslog_message(Name, Priority, Format, Args);
log(Name, LogLevel, Format, Args) ->
    log(Name, #priority{log_level=LogLevel}, Format, Args).

%% ----
-spec emergency_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec emergency_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec emergency_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec emergency_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

emergency_msg(Message) ->
    log(default, #priority{log_level=emergency}, Message, []).

emergency_msg(Format, Args) ->
    log(default, #priority{log_level=emergency}, Format, Args).

emergency_msg(Name, Format, Args)->
    log(Name, #priority{log_level=emergency}, Format, Args).

emergency_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=emergency}, Format, Args).

%% ----
-spec alert_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec alert_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec alert_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec alert_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

alert_msg(Message) ->
    log(default, #priority{log_level=alert}, Message, []).

alert_msg(Format, Args) ->
    log(default, #priority{log_level=alert}, Format, Args).

alert_msg(Name, Format, Args)->
    log(Name, #priority{log_level=alert}, Format, Args).

alert_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=alert}, Format, Args).

%% ----
-spec critical_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec critical_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec critical_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec critical_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

critical_msg(Message) ->
    log(default, #priority{log_level=critical}, Message, []).

critical_msg(Format, Args) ->
    log(default, #priority{log_level=critical}, Format, Args).

critical_msg(Name, Format, Args)->
    log(Name, #priority{log_level=critical}, Format, Args).

critical_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=critical}, Format, Args).

%% ----
-spec error_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec error_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec error_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec error_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

error_msg(Message) ->
    log(default, #priority{log_level=error}, Message, []).

error_msg(Format, Args) ->
    log(default, #priority{log_level=error}, Format, Args).

error_msg(Name, Format, Args)->
    log(Name, #priority{log_level=error}, Format, Args).

error_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=error}, Format, Args).


%% ----
-spec warning_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec warning_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec warning_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec warning_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

warning_msg(Message) ->
    log(default, #priority{log_level=warning}, Message, []).

warning_msg(Format, Args) ->
    log(default, #priority{log_level=warning}, Format, Args).

warning_msg(Name, Format, Args)->
    log(Name, #priority{log_level=warning}, Format, Args).

warning_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=warning}, Format, Args).

%% ----
-spec notice_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec notice_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec notice_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec notice_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

notice_msg(Message) ->
    log(default, #priority{log_level=notice}, Message, []).

notice_msg(Format, Args) ->
    log(default, #priority{log_level=notice}, Format, Args).

notice_msg(Name, Format, Args)->
    log(Name, #priority{log_level=notice}, Format, Args).

notice_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=notice}, Format, Args).

%% ----
-spec info_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec info_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec info_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec info_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

info_msg(Message) ->
    log(default, #priority{log_level=info}, Message, []).

info_msg(Format, Args) ->
    log(default, #priority{log_level=info}, Format, Args).

info_msg(Name, Format, Args)->
    log(Name, #priority{log_level=info}, Format, Args).

info_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=info}, Format, Args).

%% ----
-spec debug_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.
-spec debug_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec debug_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec debug_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

debug_msg(Message) ->
    log(default, #priority{log_level=debug}, Message, []).

debug_msg(Format, Args) ->
    log(default, #priority{log_level=debug}, Format, Args).

debug_msg(Name, Format, Args)->
    log(Name, #priority{log_level=debug}, Format, Args).

debug_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=debug}, Format, Args).


%%====================================================================
%% Convenient routines for specifying levels.
emergency() -> 0. %% system is unusable
alert()     -> 1. %% action must be taken immediately
critical()  -> 2. %% critical conditions
error()     -> 3. %% error conditions
warning()   -> 4. %% warning conditions
notice()    -> 5. %% normal but significant condition
info()      -> 6. %% informational
debug()     -> 7. %% debug-level messages

%% Convenient routines for specifying facility codes.
kern()      ->  0. %% kernel messages
user()      ->  1. %% random user-level messages
mail()      ->  2. %% mail system
daemon()    ->  3. %% system daemons
auth()      ->  4. %% security/authorization messages
syslog()    ->  5. %% messages generated internally by syslogd
lpr()       ->  6. %% line printer subsystem
news()      ->  7. %% network news subsystem
uucp()      ->  8. %% UUCP subsystem
cron()      ->  9. %% clock daemon
authpriv()  -> 10. %% security/authorization messages (private)
ftp()       -> 11. %% ftp daemon
local0()    -> 16. %% reserved for local use
local1()    -> 17. %% reserved for local use
local2()    -> 18. %% reserved for local use
local3()    -> 19. %% reserved for local use
local4()    -> 20. %% reserved for local use
local5()    -> 21. %% reserved for local use
local6()    -> 22. %% reserved for local use
local7()    -> 23. %% reserved for local use


%%====================================================================
%% gen_server callbacks.
%%====================================================================
init([]) ->
    ets:new(syslog_loggers, [named_table, protected, {keypos, 2}]),

    %% Add default syslog ident to catch messages without ident
    case gen_udp:open(0) of
        {ok, Socket} ->
            DefaultIdent    = sysloggerl_app:get_param(default_ident),
            DefaultFacility = sysloggerl_app:get_param(default_facility),
            DefaultLevel    = sysloggerl_app:get_param(default_loglevel),
            DefaultPriority = #priority{facility=DefaultFacility,
                                        log_level=DefaultLevel},
            DefaultOptions  = [
                               log_pid,
                               {host, get_host([])},
                               {port, get_port([])}
                              ],

            Logger = #logger{name       = default,
                             ident      = DefaultIdent,
                             udp_socket = Socket,
                             priority   = DefaultPriority,
                             options    = DefaultOptions},
            ets:insert(syslog_loggers, Logger),
            {ok, []};
        {error, Reason} ->
            {stop, Reason}
    end.

%% ----
handle_call({set, Name, Ident, Priority, Options}, _From, State) ->
    Reply = case ets:lookup(syslog_loggers, Name) of
                [] ->
                    case gen_udp:open(0) of
                        {ok, Socket} ->
                            Logger = #logger{name       = Name,
                                             ident      = Ident,
                                             udp_socket = Socket,
                                             priority   = Priority,
                                             options    = Options},
                            ets:insert(syslog_loggers, Logger),
                            {ok, Logger};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                [Logger] ->
                    NewLogger = Logger#logger{ident    = Ident,
                                              priority = Priority,
                                              options  = Options},
                    ets:insert(syslog_loggers, NewLogger),
                    {ok, NewLogger}
            end,
    {reply, Reply, State};

handle_call({unset, Name}, _From, State) ->
    case ets:lookup(syslog_loggers, Name) of
        [Logger] ->
            ets:delete(syslog_loggers, Name),
            gen_udp:close(Logger#logger.udp_socket);
        [] ->
            ok
    end,
    {reply, ok, State}.

%% ----
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ----
handle_info(_Info, State) ->
    {noreply, State}.

%% ----
terminate(_Reason, _State) ->
    ets:foldl(fun(Logger, Acc) ->
                      gen_udp:close(Logger#logger.udp_socket),
                      Acc
              end, [], syslog_loggers),
    ets:delete(syslog_loggers),
    ok.

%% ----
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
get_host(Opts) ->
    proplists:get_value(host, Opts,
                        sysloggerl_app:get_param(default_syslog_host)).

get_port(Opts) ->
    proplists:get_value(port, Opts,
                        sysloggerl_app:get_param(default_syslog_port)).

get_hostname() ->
    case node() of
        nonode@nohost ->
            {ok, Hostname} = inet:gethostname(),
            Hostname;
        Node ->
            string:sub_word(atom_to_list(Node), 2, $@)
    end.

get_timestamp() ->
    {{_, Mo, D}, {H, Mi, S}} = calendar:now_to_local_time(os:timestamp()),
    [month(Mo), $\s, day(D), $\s, digit(H), $:, digit(Mi), $:, digit(S)].

month( 1) -> "Jan";
month( 2) -> "Feb";
month( 3) -> "Mar";
month( 4) -> "Apr";
month( 5) -> "May";
month( 6) -> "Jun";
month( 7) -> "Jul";
month( 8) -> "Aug";
month( 9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

day(1) -> " 1";
day(2) -> " 2";
day(3) -> " 3";
day(4) -> " 4";
day(5) -> " 5";
day(6) -> " 6";
day(7) -> " 7";
day(8) -> " 8";
day(9) -> " 9";
day(N) -> integer_to_list(N).

digit(0) -> "00";
digit(1) -> "01";
digit(2) -> "02";
digit(3) -> "03";
digit(4) -> "04";
digit(5) -> "05";
digit(6) -> "06";
digit(7) -> "07";
digit(8) -> "08";
digit(9) -> "09";
digit(N) -> integer_to_list(N).

%% ----
format_prefix(#logger{priority=DPriority, ident=Ident, options=Opts},
              #priority{facility=Facility, log_level=LogLevel}) ->
    F = case Facility of
            undefined -> DPriority#priority.facility;
            _         -> Facility
        end,
    L = case LogLevel of
            undefined -> DPriority#priority.log_level;
            _         -> LogLevel
        end,
    Pri = (?MODULE:F() bsl 3) bor ?MODULE:L(),
    TS  = get_timestamp(),
    HN  = get_hostname(),
    case proplists:get_bool(log_pid, Opts) of
        true  ->
            Pid = case erase(logged_pid) of
                      undefined -> io_lib:print(self());
                      P         -> io_lib:print(P)
                  end,
            [$<,integer_to_list(Pri),$>,TS,$\s,HN,$\s,Ident,$[,Pid,$],$:,$\s];
        false ->
            [$<,integer_to_list(Pri),$>,TS,$\s,HN,$\s,Ident,$:,$\s]
    end.

%% ----
prepend_line_nb(Prefix, [Line]) ->
    [truncate_line(lists:flatten(Prefix, Line))];
prepend_line_nb(Prefix, Lines) ->
    Length = length(Lines),
    Width  = length(integer_to_list(Length)),
    prepend_line_nb2(Prefix, Lines, 1, length(Lines), Width, []).

prepend_line_nb2(Prefix, [Line | Rest], Current, Total, Width, Result) ->
    Str0 = io_lib:format("~s[~*..0b/~b] ~s", [Prefix,Width,Current,Total,Line]),
    Str1 = truncate_line(Str0),
    prepend_line_nb2(Prefix, Rest, Current + 1, Total, Width, [Str1|Result]);
prepend_line_nb2(_Prefix, [], _Current, _Total, _Width, Result) ->
    lists:reverse(Result).

%% ----
truncate_line(Line) ->
    N = iolist_size(Line),
    if
        N  > 1024 -> string:substr(lists:flatten(Line), 1, 1024);
        true      -> Line
    end.

%%====================================================================
send_syslog_message(Name, #priority{log_level=LogLevel}=Priority,
                    Format, Args) ->
    case ets:lookup(syslog_loggers, Name) of
        [] ->
            ok;
        [Logger] when LogLevel == undefined ->
            Message = format_message(Logger, Priority, Format, Args),
            [send_packet(Logger, Line) || Line <- Message];
        [#logger{priority=P}=Logger] ->
            Min = ?MODULE:(P#priority.log_level)(),
            Cur = ?MODULE:LogLevel(),
            if
                Cur =< Min ->
                    Message = format_message(Logger, Priority, Format, Args),
                    [send_packet(Logger, Line) || Line <- Message];
                true ->
                    ok
            end
    end.

format_message(Logger, Priority, Message, []) ->
    format_message(Logger, Priority, Message);
format_message(Logger, Priority, Format, Args) ->
    format_message(Logger, Priority,
                   lists:flatten(io_lib:format(Format, Args))).

format_message(Logger, Priority, Message) ->
    Prefix = format_prefix(Logger, Priority),
    prepend_line_nb(Prefix, string:tokens(Message, "\n")).

send_packet(#logger{udp_socket=Socket, options=Opts}, Packet) ->
    Host = get_host(Opts),
    Port = get_port(Opts),
    gen_udp:send(Socket, Host, Port, Packet).
