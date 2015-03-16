# Module syslog

- [Description](#description)
- [Exported types](#exported-types)
- [Function index](#functions-index)
- [Function details](#functions-details)

**Behaviours**: gen_server.  
**Authors**: Christopher Faulet (christopher.faulet@capflam.org).


## Description

TODO

## Exported types

<a name="facility"></a>
<a name="loglevel"></a>
<a name="priority"></a>
<a name="logger"></a>
```erlang
facility() :: kern   | user   | mail   | daemon | auth     | syslog
            | lpr    | news   | uucp   | cron   | authpriv | ftp
            | local0 | local1 | local2 | local3 | local4   | local5
            | local6 | local7.

loglevel() :: emergency | alert | critical | error | warning | notice
            | info      | debug.

priority() :: #priority{}. %% Defined in 'sysloggerl.hrl'

logger() :: #logger{}.     %% Defined in 'sysloggerl.hrl'
```

## Functions index

| Function name                               | Description
| ------------------------------------------- | -----------
| *Loggers management functions*
| [set/1,4](#set14)                           | Creates a new logger or update an existing one.
| [unset/1](#unset1)                          | Removes an existing logger.
| [loggers/0](#loggers0)                      | Retrieves all existing loggers.
| [logger/1](#logger1)                        | Retrieves a logger given its name.
| *Logging functions*
| [log/1,2,3,4](#log1234)                     | Basic functions to log messages.
| [emergency_msg/1,2,3,4](#emergency_msg1234) | Functions to log messages with the `emergency` log level.
| [alert_msg/1,2,3,4](#alert_msg1234)         | Functions to log messages with the `alert` log level.
| [critical_msg/1,2,3,4](#critical_msg1234)   | Functions to log messages with the `critical` log level.
| [error_msg/1,2,3,4](#error_msg1234)         | Functions to log messages with the `error` log level.
| [warning_msg/1,2,3,4](#warning_msg1234)     | Functions to log messages with the `warning` log level.
| [notice_msg/1,2,3,4](#notice_msg1234)       | Functions to log messages with the `notice` log level.
| [info_msg/1,2,3,4](#info_msg1234)           | Functions to log messages with the `info` log level.
| [debug_msg/1,2,3,4](#debug_msg1234)         | Functions to log messages with the `debug` log level.
| *Functions to handle `#priority{}` record*  
| [priority/2](#priority2)                    | Builds a `#priority{}` record.
| [set_facility/2](#set_facility2)            | Sets the facility of a `#priority{}` record.
| [get_facility/1](#get_facility1)            | Gets the facility of a `#priority{}` record.
| [set_loglevel/2](#set_loglevel2)            | Sets the log level of a `#priority{}` record.
| [get_loglevel/1](#get_loglevel1)            | Gets the log level of a `#priority{}` record.
| [is_facility_valid/1](#is_facility_valid1)  | Tests if the given facility is valid.
| [is_loglevel_valid/1](#is_loglevel_valid1)  | Tests if the given log level is valid.
| *Convenient routines specifying log levels codes*
| [emergency/0](#emergency0)                  | Returns the code corresponding to the `emergency` log level.
| [alert/0](#alert0)                          | Returns the code corresponding to the `alert` log level.
| [critical/0](#critical0)                    | Returns the code corresponding to the `critical` log level.
| [error/0](#error0)                          | Returns the code corresponding to the `error` log level.
| [warning/0](#warning0)                      | Returns the code corresponding to the `warning` log level.
| [notice/0](#notice0)                        | Returns the code corresponding to the `notice` log level.
| [info/0](#info0)                            | Returns the code corresponding to the `info` log level.
| [debug/0](#debug0)                          | Returns the code corresponding to the `debug` log level.
| *Convenient routines specifying facility codes*
| [kern/0](#kern0)                            | Returns the code corresponding to the `kern` facilty.
| [user/0](#user0)                            | Returns the code corresponding to the `user` facilty.
| [mail/0](#mail0)                            | Returns the code corresponding to the `mail` facilty.
| [daemon/0](#daemon0)                        | Returns the code corresponding to the `daemon` facilty.
| [auth/0](#auth0)                            | Returns the code corresponding to the `auth` facilty.
| [syslog/0](#syslog0)                        | Returns the code corresponding to the `syslog` facilty.
| [lpr/0](#lpr0)                              | Returns the code corresponding to the `lpr` facilty.
| [news/0](#news0)                            | Returns the code corresponding to the `news` facilty.
| [uucp/0](#uucp0)                            | Returns the code corresponding to the `uucp` facilty.
| [cron/0](#cron0)                            | Returns the code corresponding to the `cron` facilty.
| [authpriv/0](#authpriv0)                    | Returns the code corresponding to the `authpriv` facilty.
| [ftp/0](#ftp0)                              | Returns the code corresponding to the `ftp` facilty.
| [local0/0](#local00)                        | Returns the code corresponding to the `local0` facilty.
| [local1/0](#local10)                        | Returns the code corresponding to the `local1` facilty.
| [local2/0](#local20)                        | Returns the code corresponding to the `local2` facilty.
| [local3/0](#local30)                        | Returns the code corresponding to the `local3` facilty.
| [local4/0](#local40)                        | Returns the code corresponding to the `local4` facilty.
| [local5/0](#local50)                        | Returns the code corresponding to the `local5` facilty.
| [local6/0](#local60)                        | Returns the code corresponding to the `local6` facilty.
| [local7/0](#local70)                        | Returns the code corresponding to the `local7` facilty.


## Functions details

### Loggers management functions

#### set/1,4

```erlang
set(Name, Ident, Priority, Options) -> Result when
      Name     :: any(),
      Ident    :: string(),
      Priority :: syslog:priority(),
      Options  :: proplists:proplist(),
      Result   :: {ok, syslog:logger()}
                | {error, invalid_facility | invalid_loglevel}
                | {error, inet:posix()}.

set(Logger) -> Result when
      Logger   :: syslog:logger(),
      Result   :: {ok, syslog:logger()}
                | {error, invalid_facility | invalid_loglevel}
                | {error, inet:posix()}.
```
TODO

#### unset/1

```erlang
unset(any()) -> ok.
```
TODO

#### loggers/0

```erlang
loggers() -> [syslog:logger()].
```
TODO

#### logger/1

```erlang
logger(Name) -> Result when
      Name   :: any(),
      Result :: syslog:logger() | not_found.
```
TODO

### Logging functions

#### log/1,2,3,4

```erlang
log(Message) -> Result when
      Message         :: string(),
      Result          :: ok | {error, inet:posix()}.

log(Format, Arg) -> Result when
      Format          :: string(),
      Arg             :: list(),
      Result          :: ok | {error, inet:posix()}.

log(PriorityOrLevel, Format, Arg) -> Result when
      PriorityOrLevel :: syslog:priority() | syslog:loglevel(),
      Format          :: string(),
      Arg             :: list(),
      Result          :: ok | {error, inet:posix()}.

log(Name, PriorityOrLevel, Format, Arg) -> Result when
      Name            :: any(),
      PriorityOrLevel :: syslog:priority() | syslog:loglevel(),
      Format          :: string(),
      Arg             :: list(),
      Result          :: ok | {error, inet:posix()}.
```
TODO

#### emergency_msg/1,2,3,4

```erlang
emergency_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

emergency_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

emergency_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

emergency_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### alert_msg/1,2,3,4

```erlang
alert_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

alert_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

alert_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

alert_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```

#### critical_msg/1,2,3,4

```erlang
critical_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

critical_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

critical_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

critical_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### error_msg/1,2,3,4

```erlang
error_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

error_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

error_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

error_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```

#### warning_msg/1,2,3,4

```erlang
warning_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

warning_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

warning_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

warning_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### notice_msg/1,2,3,4

```erlang
notice_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

notice_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

notice_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

notice_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```

#### info_msg/1,2,3,4

```erlang
info_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

info_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

info_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

info_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### debug_msg/1,2,3,4

```erlang
debug_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

debug_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

debug_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

debug_msg(Name, Facility, Format, Args) -> Result when
      Name     :: any(),
      Facility :: syslog:facility(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

### Functions to handle `#priority{}` record

#### priority/2

```erlang
priority(Facility, LogLevel) -> Result when
      Facility :: syslog:facility() | undefined,
      LogLevel :: syslog:loglevel(),
      Result   :: syslog:priority()
                | {error, invalid_facility | invalid_loglevel}.
```

#### set_facility/2

```erlang
set_facility(Facility, Priority) -> Result when
      Priority :: syslog:priority(),
      Facility :: syslog:facility(),
      Result   :: syslog:priority() | {error, invalid_facility}.
```
TODO

#### get_facility/1

```erlang
get_facility(Priority) -> Facility when
      Priority :: syslog:priority(),
      Facility :: syslog:facility().
```
TODO

#### set_loglevel/2

```erlang
set_loglevel(LogLevel, Priority) -> Result when
      Priority :: syslog:priority(),
      LogLevel :: syslog:loglevel(),
      Result   :: syslog:priority() | {error, invalid_loglevel}.
```
TODO

#### get_loglevel/1

```erlang
get_loglevel(Priority) -> LogLevel when
      Priority :: syslog:priority(),
      LogLevel :: syslog:loglevel().
```

#### is_facility_valid/1

```erlang
is_facility_valid(any()) -> boolean().
```
TODO

#### is_loglevel_valid/1

```erlang
is_loglevel_valid(any()) -> boolean().
```
TODO

### Convenient routines specifying log levels codes

The log level determines the importance of the message.  The levels are, in
order of decreasing importance:

#### emergency/0

```erlang
emergency() -> 0.
```
System is unusable.

#### alert/0

```erlang
alert() -> 1.
```
Action must be taken immediately.

#### critical/0

```erlang
critical() -> 2.
```
Critical conditions.

#### error/0

```erlang
error() -> 3.
```
Error conditions.

#### warning/0

```erlang
warning() -> 4
```
Warning conditions.

#### notice/0

```erlang
notice() -> 5.
```
Normal, but significant, condition.

#### info/0

```erlang
info() -> 6.
```
Informational message.

#### debug/0

```erlang
debug() -> 7.
```
Debug-level message.

### Convenient routines specifying facility codes

The facility is used to specify what type of program is logging the
message. This lets the syslog daemon to specify that messages from different
facilities will be handled differently.

#### kern/0

```erlang
kern() -> 0.
```
Kernel messages.

#### user/0

```erlang
user() -> 1.
```
Generic user-level messages.

#### mail/0

```erlang
mail() -> 2.
```
Mail subsystem.

#### daemon/0

```erlang
daemon() -> 3.
```
System daemons without separate facility value.

#### auth/0

```erlang
auth() -> 4.
```
Security/authorization messages.

#### syslog/0

```erlang
syslog() -> 5.
```
Messages generated internally by syslogd(8).

#### lpr/0

```erlang
lpr() -> 6.
```
Line printer subsystem.

#### news/0

```erlang
news() -> 7.
```
USENET news subsystem.

#### uucp/0

```erlang
uucp() -> 8.
```
UUCP subsystem.

#### cron/0

```erlang
cron() -> 9.
```
Clock daemon (cron and at).

#### authpriv/0

```erlang
authpriv() -> 10.
```
Security/authorization messages (private).

#### ftp/0

```erlang
ftp() -> 11.
```
Ftp daemon.

#### local0/0

```erlang
local0() -> 16.
```
Reserved for local use.

#### local1/0

```erlang
local1() -> 17.
```
Reserved for local use.

#### local2/0

```erlang
local2() -> 18.
```
Reserved for local use.

#### local3/0

```erlang
local3() -> 19.
```
Reserved for local use.

#### local4/0

```erlang
local4() -> 20.
```
Reserved for local use.

#### local5/0

```erlang
local5() -> 21.
```
Reserved for local use.

#### local6/0

```erlang
local6() -> 22.
```
Reserved for local use.

#### local7/0

```erlang
local7() -> 23.
```
Reserved for local use.
