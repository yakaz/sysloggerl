# Module syslog

- [Description](#description)
- [Function index](#functions-index)
- [Function details](#functions-details)

**Behaviours**: application.  
**Authors**: Christopher Faulet (christopher.faulet@capflam.org).


## Description

Module to manage the **sysloggerl** application. It contains functions to
manipulate configuration parameters handled by it. It also offer a way to safely
reload the application and apply changes made on its environment.

```erlang
1> application:start(sysloggerl).
ok
2> application:get_param(default_loglevel).
notice
...
10> sysloggerl_app:check_and_set_param(default_loglevel, debug).
ok
11> sysloggerl_app:reload().
ok
```


## Functions index

| Function name                                  | Description
| ---------------------------------------------- | -----------
| *Configuration management functions*
| [params_list/0](#params_list0)                 | Returns the list of all configuration parameters handled by the **sysloggerl** application.
| [get_param/1](#get_param1)                     | Returns the value of a given configuration parameter.
| [set_param/2](#set_param2)                     | Sets the value of a given configuration parameter.
| [check_and_set_param/2](#check_and_set_param2) | Checks and sets the value of a given configuration parameter.
| [show_params/0](#show_params0)                 | Prints out configuration parameters handled by the **sysloggerl** application, with their value.
| [check_params/0](#check_params0)               | Checks the values validity of all configuration parameters handled by the **sysloggerl** application.
| *Application API*
| [reload/0](#reload0)                           | Reloads the **sysloggerl** application.

## Functions details

### params_list/0

```erlang
params_list() -> Params when
      Params :: [atom()].
```
Returns the list of all parameters handled by the **sysloggerl** application.

### get_param/1

```erlang
get_param(Param) -> Value when
      Param :: atom(),
      Value :: any().
```
Returns the value of the configuration parameter `Param`.

### set_param/2

```erlang
set_param(Param, Value) -> ok when
      Param :: atom(),
      Value :: any().
```
Sets the value of the configuration parameter `Param`.

### check_and_set_param/2

```erlang
check_and_set_param(Param, Value) -> Result when
      Param  :: atom(),
      Value  :: any(),
      Result :: ok | error.
```

Same as `set_param(Param, Value)` but only if `Value` is a valid value for the
configuration parameter `Param`.

### show_params/0

```erlang
show_params() -> ok.
```
Prints out configuration parameters handled by the **sysloggerl** application,
with their value.

### check_params/0

```erlang
check_params() -> boolean().
```
Checks the values validity of all configuration parameters handled by the
**sysloggerl** application.

### reload/0

```erlang
reload() -> Result when
      Result :: ok
              | {error, invalid_configuration}
              | {error, {not_started, sysloggerl}}.
```
Reloads the **sysloggerl** application. It should be started and the
configuration should be valid to succeed.
