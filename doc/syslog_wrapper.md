# Module syslog_wrapper

- [Description](#description)
- [Function index](#function-index)
- [Function details](#function-details)

**Authors**: Christopher Faulet (christopher.faulet@capflam.org).


## Description

TODO


## Functions index


| Function name                                      | Description
| -------------------------------------------------- | -----------
| *Functions to handle generated logger modules*  
| [create/4](#create4)                               | Creates or Re-create a logger module.
| [destroy/1](#destroy1)                             | Destroys a logger module.
| *Functions exported by generated logger modules*  
| [Module:get_name/0](#moduleget_name0)              | Gets name of the logger used by the logger module.
| [Module:get_loglevel/0](#moduleget_loglevel0)      | Gets the minimum log level used by the logger module.
| [Module:get_facility/0](#moduleget_facility0)      | Gets the log facility used by the logger module.
| [Module:get_logger/0](#moduleget_logger0)          | Gets the logger used by the logger module.
| [Module:log/1,2,3](#modulelog123)                  | Basic functions to log messages.
| [Module:emergency_msg/1,2](#moduleemergency_msg12) | Functions to log messages with the `emergency` log level.
| [Module:alert_msg/1,2](#modulealert_msg12)         | Functions to log messages with the `alert` log level.
| [Module:critical_msg/1,2](#modulecritical_msg12)   | Functions to log messages with the `critical` log level.
| [Module:error_msg/1,2](#moduleerror_msg12)         | Functions to log messages with the `error` log level.
| [Module:warning_msg/1,2](#modulewarning_msg12)     | Functions to log messages with the `warning` log level.
| [Module:notice_msg/1,2](#modulenotice_msg12)       | Functions to log messages with the `notice` log level.
| [Module:info_msg/1,2](#moduleinfo_msg12)           | Functions to log messages with the `info` log level.
| [Module:debug_msg/1,2](#moduledebug_msg12)         | Functions to log messages with the `debug` log level.

## Functions details

### Functions to handle generated logger modules

#### create/4

```erlang
create(ModName, Ident, Priority, Options) -> Result when
      ModName  :: any(),
      Ident    :: string(),
      Priority :: syslog:priority(),
      Options  :: proplists:proplist(),
      Result   :: ok | error.
```
TODO

#### destroy/1

```erlang
destroy(ModName) -> ok when
      ModName :: atom().
```
TODO

### Functions exported by generated logger modules

#### Module:get_name/0

```erlang
Module:get_name() -> Name when
      Name :: {syslog_wrapper, atom()}.
```
TODO

#### Module:get_loglevel/0

```erlang
Module:get_loglevel() -> Level when
      Level :: syslog:loglevel().
```
TODO

#### Module:get_facility/0

```erlang
Module:get_facility() -> Facility when
      Facility :: syslog:facility().
```
TODO

#### Module:get_logger/0

```erlang
Module:get_logger() -> Logger when
      Logger :: syslog:logger().
```
TODO

#### Module:log/1,2,3

```erlang
Module:log(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:log(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.

Module:log(LogLevel, Format, Args) -> Result when
      LogLevel :: syslog:loglevel(),
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:emergency_msg/1,2

```erlang
Module:emergency_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:emergency_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:alert_msg/1,2

```erlang
Module:alert_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:alert_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:critical_msg/1,2

```erlang
Module:critical_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:critical_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:error_msg/1,2

```erlang
Module:error_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:error_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:warning_msg/1,2

```erlang
Module:warning_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:warning_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:notice_msg/1,2

```erlang
Module:notice_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:notice_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:info_msg/1,2

```erlang
Module:info_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:info_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO

#### Module:debug_msg/1,2

```erlang
Module:debug_msg(Message) -> Result when
      Message  :: string(),
      Result   :: ok | {error, inet:posix()}.

Module:debug_msg(Format, Args) -> Result when
      Format   :: string(),
      Args     :: list(),
      Result   :: ok | {error, inet:posix()}.
```
TODO
