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

-module(syslog_wrapper).

-ifdef(LOGGER_NAME).

%% API
-export([
         get_name/0,
         get_loglevel/0,
         get_facility/0,
         get_logger/0,

         log/1, log/2, log/3,

         emergency_msg/1, emergency_msg/2,
         alert_msg/1,     alert_msg/2,
         critical_msg/1,  critical_msg/2,
         error_msg/1,     error_msg/2,
         warning_msg/1,   warning_msg/2,
         notice_msg/1,    notice_msg/2,
         info_msg/1,      info_msg/2,
         debug_msg/1,     debug_msg/2
        ]).

%%====================================================================
%% API
%%====================================================================
-spec get_name() -> Name when
      Name :: {syslog_wrapper, atom()}.

get_name() ->
    ?LOGGER_NAME.

%% ----
-spec get_loglevel() -> Level when
      Level :: syslog:loglevel().

get_loglevel() ->
    syslog:get_loglevel(?LOGGER_PRIORITY).

%% ----
-spec get_facility() -> Facility when
      Facility :: syslog:facility().

get_facility() ->
    syslog:get_facility(?LOGGER_PRIORITY).

%% ---
-spec get_logger() -> Logger when
      Logger :: syslog:logger().

get_logger() ->
    syslog:logger(?LOGGER_NAME).

%% ----
-spec log(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec log(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
-spec log(LogLevel, Format, Args) -> Result when
      LogLevel :: syslog:loglevel(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

log(Message) ->
    syslog:log(?LOGGER_NAME, ?LOGGER_PRIORITY, Message, []).

log(Format, Args) ->
    syslog:log(?LOGGER_NAME, ?LOGGER_PRIORITY, Format, Args).

log(LogLevel, Format, Args) ->
    case syslog:LogLevel() =< ?LOGGER_LOGLEVEL_INT of
        true  -> syslog:log(?LOGGER_NAME, LogLevel, Format, Args);
        false -> ok
    end.



%% ----
-ifdef(LOG_EMERGENCY_MSG).

-spec emergency_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec emergency_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

emergency_msg(Message) ->
    syslog:emergency_msg(?LOGGER_NAME, Message, []).

emergency_msg(Format, Args) ->
    syslog:emergency_msg(?LOGGER_NAME, Format, Args).

-else.

emergency_msg(_) -> noop.
emergency_msg(_,_) -> noop.

-endif.

%% ----
-ifdef(LOG_ALERT_MSG).

-spec alert_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec alert_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

alert_msg(Message) ->
    syslog:alert_msg(?LOGGER_NAME, Message, []).

alert_msg(Format, Args) ->
    syslog:alert_msg(?LOGGER_NAME, Format, Args).

-else.

alert_msg(_) -> noop.
alert_msg(_,_) -> noop.

-endif.

%% ----
-ifdef(LOG_CRITIAL_MSG).

-spec critical_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec critical_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

critical_msg(Message) ->
    syslog:critical_msg(?LOGGER_NAME, Message, []).

critical_msg(Format, Args) ->
    syslog:critical_msg(?LOGGER_NAME, Format, Args).

-else.

critical_msg(_) -> noop.
critical_msg(_,_) -> noop.

-endif.

%% ----
-ifdef(LOG_ERROR_MSG).

-spec error_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec error_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

error_msg(Message) ->
    syslog:error_msg(?LOGGER_NAME, Message, []).

error_msg(Format, Args) ->
    syslog:error_msg(?LOGGER_NAME, Format, Args).

-else.

error_msg(_) -> noop.
error_msg(_,_) -> noop.

-endif.

%% ----
-ifdef(LOG_WARNING_MSG).

-spec warning_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec warning_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

warning_msg(Message) ->
    syslog:warning_msg(?LOGGER_NAME, Message, []).

warning_msg(Format, Args) ->
    syslog:warning_msg(?LOGGER_NAME, Format, Args).

-else.

warning_msg(_) -> noop.
warning_msg(_,_) -> noop.

-endif.

%% ----
-ifdef(LOG_NOTICE_MSG).

-spec notice_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec notice_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

notice_msg(Message) ->
    syslog:notice_msg(?LOGGER_NAME, Message, []).

notice_msg(Format, Args) ->
    syslog:notice_msg(?LOGGER_NAME, Format, Args).

-else.

notice_msg(_) -> noop.
notice_msg(_,_) -> noop.

-endif.

%% ----
-ifdef(LOG_INFO_MSG).

-spec info_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec info_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

info_msg(Message)  ->
    syslog:info_msg(?LOGGER_NAME, Message, []).

info_msg(Format, Args)  ->
    syslog:info_msg(?LOGGER_NAME, Format, Args).

-else.

info_msg(_) -> noop.
info_msg(_,_) -> noop.

-endif.

%% ----
-ifdef(LOG_DEBUG_MSG).

-spec debug_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
-spec debug_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.

debug_msg(Message) ->
    syslog:debug_msg(?LOGGER_NAME, Message, []).

debug_msg(Format, Args) ->
    syslog:debug_msg(?LOGGER_NAME, Format, Args).

-else.

debug_msg(_) -> noop.
debug_msg(_,_) -> noop.

-endif.

-else.

-include("sysloggerl.hrl").

%% API
-export([
         create/4,
         destroy/1,
         parse_transform/2
        ]).

%%====================================================================
%% API
%%====================================================================
-spec create(ModName, Ident, Priority, Options) -> Result when
      ModName  :: any(),
      Ident    :: string(),
      Priority :: syslog:priority(),
      Options  :: proplists:proplist(),
      Result   :: ok | error.

create(ModName, Ident, Priority, Options) ->
    LoggerName = {?MODULE, ModName},
    CompileOpts= get_module_options(ModName, LoggerName, Priority),

    case get_srcfile() of
        {error, notfound} ->
            error;
        SrcFile ->
            case compile(SrcFile, CompileOpts) of
                ok ->
                    case syslog:set(LoggerName, Ident, Priority, Options) of
                        {ok, _} -> ok;
                        _       -> destroy(ModName)
                    end;
                error ->
                    error
            end
    end.

%% ----
-spec destroy(ModName) -> ok when
      ModName :: atom().

destroy(ModName) ->
    LoggerName = {?MODULE, ModName},
    syslog:unset(LoggerName),
    code:delete(ModName),
    ok.

%% ----
-spec parse_transform(Forms, Options) -> Forms when
      Forms   :: [erl_parse:abstract_form()],
      Options :: [compile:option()].

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
%% Internal functions
%%====================================================================
get_srcfile() ->
    case code:lib_dir(sysloggerl, src) of
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

get_module_options(ModName, LoggerName, Priority) ->
    LogLevel = Priority#priority.log_level,
    N        = syslog:LogLevel(),
    [
     binary,
     return,
     warnings_as_errors,
     {parse_transform,          ?MODULE},
     {d, 'MODULE_NAME',         ModName},
     {d, 'LOGGER_NAME',         LoggerName},
     {d, 'LOGGER_PRIORITY',     Priority},
     {d, 'LOGGER_LOGLEVEL_INT', N}
    ] ++
        if N >= ?SYSLOG_LOGLEVEL_EMERGENCY -> [{d, 'LOG_EMERGENCY_MSG'}];
           true                            -> []
        end ++
        if N >= ?SYSLOG_LOGLEVEL_ALERT     -> [{d, 'LOG_ALERT_MSG'}];
           true                            -> []
        end ++
        if N >= ?SYSLOG_LOGLEVEL_CRITICAL  -> [{d, 'LOG_CRITIAL_MSG'}];
           true                            -> []
        end ++
        if N >= ?SYSLOG_LOGLEVEL_ERROR     -> [{d, 'LOG_ERROR_MSG'}];
           true                            -> []
        end ++
        if N >= ?SYSLOG_LOGLEVEL_WARNING   -> [{d, 'LOG_WARNING_MSG'}];
           true                            -> []
        end ++
        if N >= ?SYSLOG_LOGLEVEL_NOTICE    -> [{d, 'LOG_NOTICE_MSG'}];
           true                            -> []
        end ++
        if N >= ?SYSLOG_LOGLEVEL_INFO      -> [{d, 'LOG_INFO_MSG'}];
           true                            -> []
        end ++
        if N >= ?SYSLOG_LOGLEVEL_DEBUG     -> [{d, 'LOG_DEBUG_MSG'}];
           true                            -> []
        end.


compile(File, Options) ->
    case compile:file(File, Options) of
        {ok, ModName, Binary}    -> load(ModName, Binary);
        {ok, ModName, Binary, _} -> load(ModName, Binary);
        _                        -> error
    end.

load(ModName, Binary) ->
    case code:load_binary(ModName, [], Binary) of
        {module, ModName} -> ok;
        {error, _What}    -> error
    end.

-endif.
