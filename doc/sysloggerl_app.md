# Module syslog

- [Description](#description)
- [Function index](#functions-index)
- [Function details](#functions-details)

**Behaviours**: application.  
**Authors**: Christopher Faulet (christopher.faulet@capflam.org).


## Description

TODO


## Functions index

| Function name                                  | Description
| ---------------------------------------------- | -----------
| *Configuration management functions*  
| [params_list/0](#params_list0)                 | Returns the list of all parameters handled by the **sysloggerl** application.
| [get_param/1](#get_param1)                     | Gets the value of a given parameter in the application environment.
| [set_param/2](#set_param2)                     | Sets the value of a given parameter in the application environment.
| [check_and_set_param/2](#check_and_set_param2) | Checks and sets the value of a given parameter in the application environment.
| [show_params/0](#show_params0)                 | Prints out parameters handled by the **sysloggerl** application, with their value.
| [check_params/0](#check_params0)               | Checks the values validity of all parameters handled by the **sysloggerl** application.
| *Application API*  
| [reload/0](#reload0)                           | Reloads the **sysloggerl** application.

## Functions details

### params_list/0

```erlang
params_list() -> Params when
      Params :: [atom()].
```
TODO

### get_param/1

```erlang
get_param(Param) -> Value when
      Param :: atom(),
      Value :: any().
```
TODO

### set_param/2

```erlang
set_param(Param, Value) -> ok when
      Param :: atom(),
      Value :: any().
```
TODO

### check_and_set_param/2

```erlang
check_and_set_param(Param, Value) -> Result when
      Param  :: atom(),
      Value  :: any(),
      Result :: ok | error.
```
TODO

### show_params/0

```erlang
show_params() -> ok.
```
TODO

### check_params/0

```erlang
check_params() -> boolean().
```
TODO

### reload/0

```erlang
reload() -> Result when
      Result :: ok
              | {error, invalid_configuration}
              | {error, {not_started, sysloggerl}}.
```
TODO
