[**README**](/README.md) &#10143; **syslog**

# Module syslog

- [Description](#description)
- [Exported types](#exported-types)
- [Function index](#functions-index)
- [Function details](#functions-details)

**Behaviours**: gen_server.  
**Authors**: Christopher Faulet (christopher.faulet@capflam.org).


## Description

Main module of the **sysloggerl** application. It contains functions
[to manage loggers](#loggers-management-functions) and
[to log messages](#logging-functions) to syslog daemons.

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
| [set/1,4](#set14)                           | Creates a new logger or updates an existing one.
| [unset/1](#unset1)                          | Removes an existing logger.
| [loggers/0](#loggers0)                      | Returns all existing loggers.
| [logger/1](#logger1)                        | Returns a logger given its name.
| *Logging functions*
| [log/1,2,3](#log123)                        | Basic functions to log messages.
| [*LOGLEVEL*_msg/1,2,3](#loglevel_msg123)    | Functions to log messages with the `LOGLEVEL` log level.
| *Functions to handle `#priority{}` record*
| [priority/2](#priority2)                    | Creates a `#priority{}` record.
| [set_facility/2](#set_facility2)            | Sets the facility of a `#priority{}` record.
| [get_facility/1](#get_facility1)            | Gets the facility of a `#priority{}` record.
| [set_loglevel/2](#set_loglevel2)            | Sets the log level of a `#priority{}` record.
| [get_loglevel/1](#get_loglevel1)            | Gets the log level of a `#priority{}` record.
| [is_facility_valid/1](#is_facility_valid1)  | Tests if the given facility is valid.
| [is_loglevel_valid/1](#is_loglevel_valid1)  | Tests if the given log level is valid.
| *Convenient routines specifying log levels and facility codes*
| [*LOGLEVEL*/0](#loglevel0)                  | Returns the code corresponding to the `LOGLEVEL` log level.
| [*FACILITY*/0](#facility0)                  | Returns the code corresponding to the `FACILITY` facilty.


## Functions details

### Loggers management functions

#### set/1,4

```erlang
set(Name, Ident, Priority, Options) -> Result when
      Name     :: any(),
      Ident    :: string(),
      Priority :: priority(),
      Options  :: proplists:proplist(),
      Result   :: {ok, logger()}
                | {error, invalid_facility | invalid_loglevel}.
```
Creates a new logger or updates an existing one. The available options are:

  * `log_pid`: Include the caller's PID with each message.
  * `{host, string()}`: Send messages to the specified syslog server.
  * `{port, pos_integer()}`: Use the specified port.

If the logger creation is successful, the function should return `{ok, Logger}`,
where `Logger` is the created logger.
If the `Priority` is malformed, the function should return `{error,
invalid_facility}` or `{error, invalid_loglevel}` depending on which field is
invalid.


```erlang
set(Logger) -> Result when
      Logger   :: logger(),
      Result   :: {ok, logger()}
                | {error, invalid_facility | invalid_loglevel}.
```
Equivalent to `set(Logger#logger.name, Logger#logger.ident, Logger#logger.priority, Logger#logger.options)`.

#### unset/1

```erlang
unset(Name) -> ok when
      Name :: any().
```
Removes the logger with the name `Name`. This function always returns `ok`,
regardless the logger is found or not.

#### loggers/0

```erlang
loggers() -> [logger()].
```
Returns all existing loggers.

#### logger/1

```erlang
logger(Name) -> Result when
      Name   :: any(),
      Result :: logger() | not_found.
```
Returns the logger with the name `Name`, or `not_found` if such logger does not
exist.

### Logging functions

#### log/1,2,3

```erlang
log(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
```
Same as `log(Message, [])`.

```erlang
log(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```
Same as `log(undefined, Format, Args)`.

```erlang
log(LogLevel, Format, Args) -> Result when
      LogLevel :: loglevel() | undefined,
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```
Same as `log(default, LogLevel, Format, Args)`.

```erlang
log(Name, LogLevel, Format, Args) -> Result when
      Name     :: any(),
      LogLevel :: loglevel() | undefined,
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```
Sends a message using the logger with the name `Name` and the specified
`LogLevel`. If `LogLevel` is `undefined`, the minimum log level of the logger
will be used (defined during its creation).  
This function never fails.

#### *LOGLEVEL*_msg/1,2,3

**Here `LOGLEVEL` should be replaced by one of the log levels defined
[below](#loglevel0).**

```erlang
LOGLEVEL_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
```
Same as `LOGLEVEL_msg(Message, [])`.

```erlang
LOGLEVEL_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```
Same as `LOGLEVEL_msg(default, Format, Args)`.

```erlang
LOGLEVEL_msg(Name, Format, Args) -> Result when
      Name     :: any(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```
Sends a message using the logger with the name `Name` and the log level
`LOGLEVEL`.  
This function never fails.

```erlang
1> syslog:error_msg("An error occurred").
ok
```


### Functions to handle `#priority{}` record

#### priority/2

```erlang
priority(Facility, LogLevel) -> Result when
      Facility :: facility() | undefined,
      LogLevel :: loglevel(),
      Result   :: priority()
                | {error, invalid_facility | invalid_loglevel}.
```

Creates a `#priority{}` record where the facility is set to `Facility` and the
log level to `LogLevel`.

```erlang
2> syslog:priority(user, info).
#priority{facility = user, log_level = info}
3> syslog:priority(daemon, warning).
#priority{facility = daemon, log_level = warning}
```

#### set_facility/2

```erlang
set_facility(Facility, Priority) -> Result when
      Priority :: priority(),
      Facility :: facility(),
      Result   :: priority() | {error, invalid_facility}.
```
Update the given `#priority{}` record by setting the facility to `Facility`.

#### get_facility/1

```erlang
get_facility(Priority) -> Facility when
      Priority :: priority(),
      Facility :: facility().
```
Returns the facility of the priority `Priority`.

#### set_loglevel/2

```erlang
set_loglevel(LogLevel, Priority) -> Result when
      Priority :: priority(),
      LogLevel :: loglevel(),
      Result   :: priority() | {error, invalid_loglevel}.
```
Update the given `#priority{}` record by setting the log level to `LogLevel`.

#### get_loglevel/1

```erlang
get_loglevel(Priority) -> LogLevel when
      Priority :: priority(),
      LogLevel :: loglevel().
```
Returns the log level of the priority `Priority`.

#### is_facility_valid/1

```erlang
is_facility_valid(any()) -> boolean().
```
Returns `true` if the parameter is a valid facility. Else it returns `false`.

#### is_loglevel_valid/1

```erlang
is_loglevel_valid(any()) -> boolean().
```
Returns `true` if the parameter is a valid log level. Else it returns `false`.

### Convenient routines specifying log levels and facility codes

#### *LOGLEVEL*/0

**Here `LOGLEVEL` should be replaced by one of the following log levels.**

```erlang
LOGLEVEL() -> 0.
```
Returns the code corresponding to the `LOGLEVEL` log level.

```erlang
4> syslog:emergency().
0
5> syslog:debug().
7
```

The log level determines the importance of the message.  The levels are, in
order of decreasing importance:

* `emergency`: System is unusable.
* `alert`: Action must be taken immediately.
* `critical`: Critical conditions.
* `error`: Error conditions.
* `warning`: Warning conditions.
* `notive`: Normal, but significant, condition.
* `info`: Informational message.
* `debug`: Debug-level message.

#### *FACILITY*/0

**Here `FACILITY` should be replaced by one of the following facilities.**

```erlang
FACILITY() -> 0.
```
Returns the code corresponding to the `FACILITY` facility.

```erlang
6> syslog:daemon().
3
7> syslog:local3().
19
```

The facility is used to specify what type of program is logging the
message. This lets the syslog daemon to specify that messages from different
facilities will be handled differently. The facilities are:

* `kern`: Kernel messages.
* `user`: Generic user-level messages.
* `mail`: Mail subsystem.
* `daemon`: System daemons without separate facility value.
* `auth`: Security/authorization messages.
* `syslog`: Messages generated internally by syslogd(8).
* `lpr`: Line printer subsystem.
* `news`: USENET news subsystem.
* `uucp`: UUCP subsystem.
* `cron`: Clock daemon (cron and at).
* `authpriv`: Security/authorization messages (private).
* `ftp`: Ftp daemon.
* `local0-7`: Reserved for local use.
