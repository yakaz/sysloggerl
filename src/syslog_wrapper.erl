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

-module(syslog_wrapper).

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
-spec create(atom(), string(), syslog:loglevel()) -> ok | error.

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
-spec get_loglevel() -> Level when
      Level :: syslog:loglevel().

-spec log(Priority, Format, Args) -> Result when
      Priority :: syslog:priority(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec emergency_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec emergency_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec alert_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec alert_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec critical_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec critical_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec error_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec error_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec warning_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec warning_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec notice_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec notice_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec info_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec info_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

-spec debug_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
-spec debug_msg(Facility, Format, Args) -> Result when
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

%%====================================================================
-ifdef(SYSLOG_LOGLEVEL).

-ifndef(SYSLOG_NAME).
-define(SYSLOG_NAME, default).
-endif.

get_loglevel() ->
    ?SYSLOG_LOGLEVEL.

%% ----
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
    notice.

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
