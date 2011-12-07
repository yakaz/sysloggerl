%%%-------------------------------------------------------------------
%%% File    : syslog.erl
%%% Author  : Christopher Faulet <christopher@yakaz.com>
%%% Description :
%%%
%%% Created : 15 Mar 2010 by Christopher Faulet <christopher@yakaz.com>
%%% $Id$
%%%-------------------------------------------------------------------
-module(syslog).
-vsn('$Revision$ ').

%% API
-export([
         start_link/0,
         stop/0,
         add/5,
         remove/1,
         update/5,
         priority/2,
         get_facility/1,
         get_loglevel/1,
         is_facility_valid/1,
         is_loglevel_valid/1,

         log/3, log/4,

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
         local0/0,
         local1/0,
         local2/0,
         local3/0,
         local4/0,
         local5/0,
         local6/0,
         local7/0
        ]).



%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
-record(syslog, {name, ident, udp_socket, facility, log_level, options}).
-record(priority, {facility, log_level}).
%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


add(Name, Ident, Facility, LogLevel, Options) ->
    gen_server:call(?MODULE, {add, Name, Ident, Facility, LogLevel, Options}).

remove(Name) ->
    gen_server:call(?MODULE, {remove, Name}).

update(Name, Ident, Facility, LogLevel, Options) ->
    gen_server:call(?MODULE, {update, Name, Ident, Facility, LogLevel, Options}).

priority(Facility, LogLevel) when is_atom(Facility), is_atom(LogLevel) ->
    %% FIXME: check facility and log_level
    #priority{facility=Facility,log_level=LogLevel}.

get_facility(#priority{facility=Facility}) -> Facility.
get_loglevel(#priority{log_level=LogLevel}) -> LogLevel.

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
%% FIXME: check priority
log(Priority, Format, Args) ->
    log(default, Priority, Format, Args).

log(Name, #priority{}=Priority, Format, Args) when is_list(Format),
                                                   is_list(Args) ->
    send_syslog_message(Name, Priority, Format, Args).


%%====================================================================
emergency_msg(Format) ->
    log(#priority{log_level=emergency}, Format, []).

emergency_msg(Format, Args) ->
    log(#priority{log_level=emergency}, Format, Args).

emergency_msg(Name, Format, Args)->
    log(Name, #priority{log_level=emergency}, Format, Args).

emergency_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=emergency}, Format, Args).


%%====================================================================
alert_msg(Format) ->
    log(default, #priority{log_level=alert}, Format, []).

alert_msg(Format, Args) ->
    log(default, #priority{log_level=alert}, Format, Args).

alert_msg(Name, Format, Args)->
    log(Name, #priority{log_level=alert}, Format, Args).

alert_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=alert}, Format, Args).


%%====================================================================
critical_msg(Format) ->
    log(default, #priority{log_level=critical}, Format, []).

critical_msg(Format, Args) ->
    log(default, #priority{log_level=critical}, Format, Args).

critical_msg(Name, Format, Args)->
    log(Name, #priority{log_level=critical}, Format, Args).

critical_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=critical}, Format, Args).


%%====================================================================
error_msg(Format) ->
    log(default, #priority{log_level=error}, Format, []).

error_msg(Format, Args) ->
    log(default, #priority{log_level=error}, Format, Args).

error_msg(Name, Format, Args)->
    log(Name, #priority{log_level=error}, Format, Args).

error_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=error}, Format, Args).



%%====================================================================
warning_msg(Format) ->
    log(default, #priority{log_level=warning}, Format, []).

warning_msg(Format, Args) ->
    log(default, #priority{log_level=warning}, Format, Args).

warning_msg(Name, Format, Args)->
    log(Name, #priority{log_level=warning}, Format, Args).

warning_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=warning}, Format, Args).


%%====================================================================
notice_msg(Format) ->
    log(default, #priority{log_level=notice}, Format, []).

notice_msg(Format, Args) ->
    log(default, #priority{log_level=notice}, Format, Args).

notice_msg(Name, Format, Args)->
    log(Name, #priority{log_level=notice}, Format, Args).

notice_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=notice}, Format, Args).


%%====================================================================
info_msg(Format) ->
    log(default, #priority{log_level=info}, Format, []).

info_msg(Format, Args) ->
    log(default, #priority{log_level=info}, Format, Args).

info_msg(Name, Format, Args)->
    log(Name, #priority{log_level=info}, Format, Args).

info_msg(Name, Facility, Format, Args) ->
    log(Name, #priority{facility=Facility, log_level=info}, Format, Args).

%%====================================================================
debug_msg(Format) ->
    log(default, #priority{log_level=debug}, Format, []).

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

    ets:new(syslog, [named_table, protected, {keypos, 2}]),

    %% Add default syslog ident to catch messages without ident
    case gen_udp:open(0) of
        {ok, Socket} ->
            Default_Ident = syslogger_app:get_param(default_ident),
            Default_Facility = syslogger_app:get_param(default_facility),
            Default_Level = syslogger_app:get_param(default_loglevel),
            Syslog = #syslog{name       = default,
                             ident      = Default_Ident,
                             udp_socket = Socket,
                             facility   = Default_Facility,
                             log_level  = Default_Level,
                             options    = [
                                           log_pid,
                                           {host, get_host([])},
                                           {port, get_port([])}
                                          ]},
            ets:insert(syslog, Syslog),
            error_logger:add_report_handler(error_logger_syslog),
            {ok, #state{}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({add, Name, Ident, Facility, LogLevel, Options}, _From, State) ->
    %% FIXME: check facility and loglevel
    Reply = case ets:lookup(syslog, Name) of
                [] ->
                    case gen_udp:open(0) of
                        {ok, Socket} ->
                            Syslog = #syslog{name       = Name,
                                             ident      = Ident,
                                             udp_socket = Socket,
                                             facility   = Facility,
                                             log_level  = LogLevel,
                                             options    = Options},
                            ets:insert(syslog, Syslog),
                            {ok, Socket};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                [_] ->
                    {error, already_exists}
            end,
    {reply, Reply, State};

handle_call({remove, Name}, _From, State) ->
    case ets:lookup(syslog, Name) of
        [Syslog] ->
            ets:delete(syslog, Name),
            gen_udp:close(Syslog#syslog.udp_socket);
        [] -> ok
    end,
    {reply, ok, State};

handle_call({update, Name, Ident, Facility, LogLevel, Options}, _From, State) ->
    %% FIXME: check facility and loglevel
    Reply = case ets:lookup(syslog, Name) of
                [] ->
                    {error, not_found};
                [Syslog] ->
                    NewSyslog = Syslog#syslog{ident     = Ident,
                                              facility  = Facility,
                                              log_level = LogLevel,
                                              options   = Options},
                    ets:insert(syslog, NewSyslog),
                    {ok, Syslog#syslog.udp_socket}
            end,
    {reply, Reply, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% FIXME: handle message from udp socket
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:delete_report_handler(error_logger_syslog),
    ets:foldl(fun(Syslog, Acc) ->
                      gen_udp:close(Syslog#syslog.udp_socket),
                      Acc
              end, [], syslog),
    ets:delete(syslog),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
get_host(Args) when is_list(Args) ->
    proplists:get_value(host, Args, syslogger_app:get_param(syslogd_host));
get_host(_) ->
    syslogger_app:get_param(syslogd_host).

get_port(Args) when is_list(Args) ->
    proplists:get_value(port, Args, syslogger_app:get_param(syslogd_port));
get_port(_) ->
    syslogger_app:get_param(syslogd_port).

%%====================================================================
format_prefix(Syslog) ->
    case proplists:get_bool(log_pid, Syslog#syslog.options) of
        true  ->
            %% FIXME: good idea ?
            Pid = case get(logged_pid) of
                      undefined -> self();
                      P -> P
                  end,
            Syslog#syslog.ident ++ "[" ++ io_lib:print(Pid) ++ "]: ";
        false ->
            Syslog#syslog.ident ++ ": "
    end.

prepend_line_nb(Prefix, [Line]) ->
    [Prefix ++ Line];
prepend_line_nb(Prefix, Lines) ->
    Length = length(Lines),
    Width = length(integer_to_list(Length)),
    prepend_line_nb2(Prefix, Lines, 1, length(Lines), Width, []).

prepend_line_nb2(Prefix, [Line | Rest], Current, Total, Width, Result) ->
    Str = lists:flatten(io_lib:format("~s[~*..0b/~b] ",
                                      [Prefix, Width, Current, Total])),
    prepend_line_nb2(Prefix, Rest, Current + 1, Total, Width,
                     Result ++ [Str ++ Line]);
prepend_line_nb2(_Prefix, [], _Current, _Total, _Width, Result) ->
    Result.

truncate_lines(Lines) ->
    truncate_lines2(Lines, []).

truncate_lines2([Line | Rest], Result) when length(Line) > 1019 ->
    truncate_lines2(Rest, Result ++ [string:substr(Line, 1, 1019)]);
truncate_lines2([Line | Rest], Result) ->
    truncate_lines2(Rest, Result ++ [Line]);
truncate_lines2([], Result) ->
    Result.

%%====================================================================
send_syslog_message(Name, Priority, Format, []) ->
    send_syslog_message(Name, Priority, Format);
send_syslog_message(Name, Priority, Format, Args) ->
    Message = lists:flatten(io_lib_format:fwrite(Format, Args)),
    send_syslog_message(Name, Priority, Message).

send_syslog_message(#syslog{}=Syslog, Priority, Message) ->
    Prefix = format_prefix(Syslog),
    Lines = string:tokens(Message, "\n"),
    Prefixed_Lines = prepend_line_nb(Prefix, Lines),
    Truncated_Lines = truncate_lines(Prefixed_Lines),
    send_syslog_lines(Syslog, Priority, Truncated_Lines);
send_syslog_message(Name, #priority{log_level=LogLevel}=Priority, Message) ->
    case ets:lookup(syslog, Name) of
        [] ->
            ok;
        [Syslog] ->
            Min = Syslog#syslog.log_level,
            Cur = ?MODULE:LogLevel(),
            if
                Cur =< Min -> send_syslog_message(Syslog, Priority, Message);
                true -> ok
            end
    end.


send_syslog_lines(Syslog, Priority, Lines) ->
    lists:foreach(fun(L) -> send_syslog_packet(Syslog, Priority, L) end, Lines).


send_syslog_packet(#syslog{udp_socket=Socket, facility=DFacility, options=Opts},
                   #priority{facility=Facility, log_level=LogLevel}, Message) ->
    Priority =
        case Facility of
            undefined -> (?MODULE:DFacility() bsl 3) bor ?MODULE:LogLevel();
            _ -> (?MODULE:Facility() bsl 3) bor ?MODULE:LogLevel()
        end,
    Packet = "<" ++ integer_to_list(Priority) ++ ">" ++ Message,
    Host = get_host(Opts),
    Port = get_port(Opts),
    gen_udp:send(Socket, Host, Port, Packet).

