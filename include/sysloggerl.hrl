-record(priority, {facility  :: syslog:facility(),
                   log_level :: syslog:loglevel()}).

-record(logger,   {name       :: atom(),
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
