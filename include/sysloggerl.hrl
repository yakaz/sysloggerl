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

-record(priority, {facility  :: syslog:facility(),
                   log_level :: syslog:loglevel()}).

-record(logger,   {name       :: any(),
                   ident      :: string(),
                   udp_socket :: inet:socket(),
                   priority   :: syslog:priority(),
                   options    :: proplists:proplist()}).

%% Expose facilities/levels macros to use it in clauses.

-define(SYSLOG_FACILITY_KERN,      0).
-define(SYSLOG_FACILITY_USER,      1).
-define(SYSLOG_FACILITY_MAIL,      2).
-define(SYSLOG_FACILITY_DAEMON,    3).
-define(SYSLOG_FACILITY_AUTH,      4).
-define(SYSLOG_FACILITY_SYSLOG,    5).
-define(SYSLOG_FACILITY_LPR,       6).
-define(SYSLOG_FACILITY_NEWS,      7).
-define(SYSLOG_FACILITY_UUCP,      8).
-define(SYSLOG_FACILITY_CRON,      9).
-define(SYSLOG_FACILITY_AUTHPRIV, 10).
-define(SYSLOG_FACILITY_FTP,      11).
-define(SYSLOG_FACILITY_LOCAL0,   16).
-define(SYSLOG_FACILITY_LOCAL1,   17).
-define(SYSLOG_FACILITY_LOCAL2,   18).
-define(SYSLOG_FACILITY_LOCAL3,   19).
-define(SYSLOG_FACILITY_LOCAL4,   20).
-define(SYSLOG_FACILITY_LOCAL5,   21).
-define(SYSLOG_FACILITY_LOCAL6,   22).
-define(SYSLOG_FACILITY_LOCAL7,   23).

-define(SYSLOG_LOGLEVEL_EMERGENCY, 0).
-define(SYSLOG_LOGLEVEL_ALERT,     1).
-define(SYSLOG_LOGLEVEL_CRITICAL,  2).
-define(SYSLOG_LOGLEVEL_ERROR,     3).
-define(SYSLOG_LOGLEVEL_WARNING,   4).
-define(SYSLOG_LOGLEVEL_NOTICE,    5).
-define(SYSLOG_LOGLEVEL_INFO,      6).
-define(SYSLOG_LOGLEVEL_DEBUG,     7).
