%%%-------------------------------------------------------------------
%%% File    : syslog_wrapper.erl
%%% Author  : Christopher Faulet <christopher@yakaz.com>
%%% Description :
%%%
%%% Created : 19 Mar 2010 by Christopher Faulet <christopher@yakaz.com>
%%% $Id$
%%%-------------------------------------------------------------------
-module(syslog_wrapper).
-vsn('$Revision$ ').

%% API
-export([
         create/3,
         parse_transform/2,

         get_loglevel/0,
         log/3,

         emergency_msg/2, emergency_msg/3,
         alert_msg/2,     alert_msg/3,
         critical_msg/2,  critical_msg/3,
         error_msg/2,     error_msg/3,
         warning_msg/2,   warning_msg/3,
         notice_msg/2,    notice_msg/3,
         info_msg/2,      info_msg/3,
         debug_msg/2,     debug_msg/3
        ]).

%%====================================================================
%% API
%%====================================================================
create(ModName, SyslogName, LogLevel) when is_atom(ModName) ->
    Options = [
               binary,
               {parse_transform, ?MODULE},
               {d, 'MODULE_NAME', ModName},
               {d, 'SYSLOG_NAME', SyslogName},
               {d, 'SYSLOG_LOGLEVEL', syslog:LogLevel()}
              ],
    case get_srcfile() of
        {error, notfound} -> error;
        SrcFile ->
            case compile:file(SrcFile, Options) of
                {ok, ModName, Binary} ->
                    case code:load_binary(ModName, [], Binary) of
                        {module, ModName} -> ok;
                        {error, _What}    -> error
                    end;
                {error, _Errors, _Warnings} -> error;
                error -> error
            end
    end.


parse_transform(Forms, Options) ->
    case lists:keysearch('MODULE_NAME', 2, Options) of
        {value, {d, 'MODULE_NAME', ModName}} ->
            case lists:keysearch(module, 3, Forms) of
                {value, {attribute,Line,module,_}} ->
                    lists:keyreplace(module, 3, Forms,
                                     {attribute,Line,module,ModName});
                false ->
                    [{attribute,1,module,ModName}|Forms]
            end;
        false ->
            Forms
    end.


%%====================================================================
-ifdef(SYSLOG_LOGLEVEL).

-ifndef(SYSLOG_NAME).
-define(SYSLOG_NAME, default).
-endif.

get_loglevel() ->
    ?SYSLOG_LOGLEVEL.

log(Priority, Format, Args) ->
    LogLevel = syslog:get_loglevel(),
    case syslog:LogLevel() =< ?SYSLOG_LOGLEVEL of
        true  -> syslog:log(?SYSLOG_NAME, Priority, Format, Args);
        false -> ok
    end.

emergency_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 0 ->
    syslog:emergency_msg(?SYSLOG_NAME, Format, Args);
emergency_msg(_,_) -> ok.

emergency_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 0 ->
    syslog:emergency_msg(?SYSLOG_NAME, Facility, Format, Args);
emergency_msg(_,_,_) -> ok.

%% --
alert_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 1 ->
    syslog:alert_msg(?SYSLOG_NAME, Format, Args);
alert_msg(_,_) -> ok.

alert_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 1 ->
    syslog:alert_msg(?SYSLOG_NAME, Facility, Format, Args);
alert_msg(_,_,_) -> ok.

%% --
critical_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 2 ->
    syslog:critical_msg(?SYSLOG_NAME, Format, Args);
critical_msg(_,_) -> ok.

critical_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 2 ->
    syslog:critical_msg(?SYSLOG_NAME, Facility, Format, Args);
critical_msg(_,_,_) -> ok.

%% --
error_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 3 ->
    syslog:error_msg(?SYSLOG_NAME, Format, Args);
error_msg(_,_) -> ok.

error_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 3 ->
    syslog:error_msg(?SYSLOG_NAME, Facility, Format, Args);
error_msg(_,_,_) -> ok.

%% --
warning_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 4 ->
    syslog:warning_msg(?SYSLOG_NAME, Format, Args);
warning_msg(_,_) -> ok.

warning_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 4 ->
    syslog:warning_msg(?SYSLOG_NAME, Facility, Format, Args);
warning_msg(_,_,_) -> ok.

%% --
notice_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 5 ->
    syslog:notice_msg(?SYSLOG_NAME, Format, Args);
notice_msg(_,_) -> ok.

notice_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 5 ->
    syslog:notice_msg(?SYSLOG_NAME, Facility, Format, Args);
notice_msg(_,_,_) -> ok.

info_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 6 ->
    syslog:info_msg(?SYSLOG_NAME, Format, Args);
info_msg(_,_) -> ok.

info_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 6 ->
    syslog:info_msg(?SYSLOG_NAME, Facility, Format, Args);
info_msg(_,_,_) -> ok.

debug_msg(Format, Args) when ?SYSLOG_LOGLEVEL >= 7 ->
    syslog:debug_msg(?SYSLOG_NAME, Format, Args);
debug_msg(_,_) -> ok.

debug_msg(Facility, Format, Args) when ?SYSLOG_LOGLEVEL >= 7 ->
    syslog:debug_msg(?SYSLOG_NAME, Facility, Format, Args);
debug_msg(_,_,_) -> ok.

-else.

get_loglevel() ->
    undefined.

log(_,_,_) -> ok.

emergency_msg(_,_) -> ok.
emergency_msg(_,_,_) -> ok.

alert_msg(_,_) -> ok.
alert_msg(_,_,_) -> ok.

critical_msg(_,_) -> ok.
critical_msg(_,_,_) -> ok.

error_msg(_,_) -> ok.
error_msg(_,_,_) -> ok.

warning_msg(_,_) -> ok.
warning_msg(_,_,_) -> ok.

notice_msg(_,_) -> ok.
notice_msg(_,_,_) -> ok.

info_msg(_,_) -> ok.
info_msg(_,_,_) -> ok.

debug_msg(_,_) -> ok.
debug_msg(_,_,_) -> ok.

-endif.



%%====================================================================
%% Internal functions
%%====================================================================
get_srcfile() ->
    case code:lib_dir(syslogger, src) of
        {error, bad_name} ->
            SrcFile = proplists:get_value(source,
                                          ?MODULE:module_info(compile)),
            case filelib:is_file(SrcFile) of
                true  -> SrcFile;
                false -> {error, notfound}
            end;
        SrcDir ->
            File =  filename:join(SrcDir, "syslog_wrapper.erl"),
            case filelib:is_file(File) of
                true ->
                    File;
                false ->
                    SrcFile = proplists:get_value(source,
                                                  ?MODULE:module_info(compile)),
                    case filelib:is_file(SrcFile) of
                        true  -> SrcFile;
                        false -> {error, notfound}
                    end
            end
    end.
