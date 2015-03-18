# Module syslog_wrapper

- [Description](#description)
- [Function index](#function-index)
- [Function details](#function-details)

**Authors**: Christopher Faulet (christopher.faulet@capflam.org).


## Description

Module to wrap a logger in a *logger module*.

```erlang
1> Prio = syslog:priority(user, info).
#priority{facility = user, log_level = info}
2> syslog_wrapper:create(my_app_logger, "my_app", Prio, []).
ok
3> my_app_logger:get_name().
{syslog_wrapper, my_app_logger}
4> my_app_logger:error_msg("Serious error in my application"). %% This message will be sent
ok
5> my_app_logger:debug_msg("Dump debug information"). %% This one will be ignored
ok
```

## Functions index


| Function name                                      | Description
| -------------------------------------------------- | -----------
| *Functions to handle generated logger modules*
| [create/4](#create4)                               | Creates or updates a *logger module*.
| [destroy/1](#destroy1)                             | Destroys a *logger module*.
| *Functions exported by generated logger modules*
| [Module:get_name/0](#moduleget_name0)              | Gets the name of the logger used by the *logger module*.
| [Module:get_loglevel/0](#moduleget_loglevel0)      | Gets the minimum log level used by the *logger module*.
| [Module:get_facility/0](#moduleget_facility0)      | Gets the log facility used by the *logger module*.
| [Module:get_logger/0](#moduleget_logger0)          | Gets the logger used by the *logger module*.
| [Module:log/1,2,3](#modulelog123)                  | Basic functions to log messages.
| [Module:*LOGLEVEL*_msg/1,2](#moduleloglevel_msg12) | Functions to log messages with the `LOGLEVEL` log level.

## Functions details

### Functions to handle generated logger modules

#### create/4

```erlang
create(Module, Ident, Priority, Options) -> Result when
      Module   :: any(),
      Ident    :: string(),
      Priority :: syslog:priority(),
      Options  :: proplists:proplist(),
      Result   :: ok | error.
```

Creates or updates a *logger module*. `Module` will be the name of this
module. A logger will be created/updated to handle all messages sent through
this *logger module* by calling `syslog:set(..., Ident, Priority, Options)`.

#### destroy/1

```erlang
destroy(Module) -> ok when
      Module :: atom().
```
Destroys the *logger module* `Module`. This will also remove the associated
logger by calling `syslog:unset(Module:get_name())`.

### Functions exported by generated logger modules

#### Module:get_name/0

```erlang
Module:get_name() -> Name when
      Name :: {syslog_wrapper, atom()}.
```
Returns the logger name created when the *logger module* `Module` was created.

#### Module:get_loglevel/0

```erlang
Module:get_loglevel() -> Level when
      Level :: syslog:loglevel().
```
Returns the minimum log level required to log messages with the *logger module*
`Module`.

#### Module:get_facility/0

```erlang
Module:get_facility() -> Facility when
      Facility :: syslog:facility().
```
Return the facility used to log messages with the *logger module* `Module`.

#### Module:get_logger/0

```erlang
Module:get_logger() -> Logger when
      Logger :: syslog:logger().
```
Returns the logger created when the *logger module* `Module` was created.

#### Module:log/1,2,3

```erlang
Module:log(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
```
Same as `Module:log(Message, [])`.

```erlang
Module:log(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```
Same as `Module:log(Module:get_loglevel(), Format, Args)`.

```erlang
Module:log(LogLevel, Format, Args) -> Result when
      LogLevel :: syslog:loglevel(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```

Sends a message using the logger associated with the *logger module* `Module`
and the log level `LogLevel`. If the log level `LogLevel` has a lower importance
than the minimum level used to create the *logger module* `Module`, nothing
happens.  
This function never fails.

#### Module:LOGLEVEL_msg/1,2

**Here `LOGLEVEL` should be replaced by one of the log levels defined
[here](doc/syslog_wrapper.md#loglevel0).**

```erlang
Module:LOGLEVEL_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok.
```
Same as `Module:LOGLEVEL_msg(Message, [])`.

```erlang
Module:LOGLEVEL_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok.
```

Sends a message using the logger associated with the *logger module* `Module`
and the log level `LOGLEVEL`. If the log level corresponding to `LOGLEVEL` has a
lower importance than the minimum level used to create the *logger module*
`Module`, nothing happens.  
This function never fails.
