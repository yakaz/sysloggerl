# sysloggerl: A syslog logger for Erlang

**sysloggerl** is a pure [Erlang/OTP](http://www.erlang.org/) application for
logging messages to syslog daemons, using UDP sockets. It provides a
customizable error_logger report handler and can handles several loggers with
different logging options. For now, it implements only the
[RFC 3164](http://tools.ietf.org/html/rfc3164) protocol.

**sysloggerl** is distributed under the terms of the **2-clause BSD license**;
see `COPYING`.


[![Build Status](https://travis-ci.org/yakaz/sysloggerl.svg?branch=master)](https://travis-ci.org/yakaz/sysloggerl)

Table of contents
-----------------
 - [Installation](#installation)
  - [Rebar](#rebar)
  - [Autotools](#autotools)
 - [How it works!](#how-it-works)
  - [Play with loggers](#play-with-loggers)
  - [Log messages](#log-messages)
  - [Syslog `error_logger` handler](#error_logger-handler)
 - [Configuration](#configuration)
 - [Advanced feature: The syslog wrappers](#advanced-feature-the-syslog-wrappers)
 - [API](#api)


## Installation

### Rebar

If you use rebar, you can run the following command to build the application:

```bash
rebar compile
```

### Autotools

If you use the Autotools and `make(1)`, run the following commands to build the
application:

```bash
# Generate Autotools files.
autoreconf -vif

# Build the application.
./configure
make

# Install it.
sudo make install
```

The default installation path is your Erlang's distribution libraries directory
(see `code:lib_dir()`).


## How it works!

First of all, before using **sysloggerl**, you must start it:

```erlang
1> application:start(sysloggerl).
```

Then, you can use [syslog](doc/syslog.md) module to log messages, to set new
loggers or to retrieve info about existing ones.  

To ease its use, the **sysloggerl** application
[exports useful types](doc/syslog.md#exported-types) and two records (see
[sysloggerl.hrl](include/sysloggerl.hrl) file):


```erlang
-record(priority, {facility  :: syslog:facility(),
                   log_level :: syslog:loglevel()}).

-record(logger,   {name       :: any(),
                   ident      :: string(),
                   udp_socket :: inet:socket(),
                   priority   :: syslog:priority(),
                   options    :: proplists:proplist()}).
```

### Play with loggers

The **sysloggerl** application can handle several loggers, with different
logging options. So it is possible to send messages with different
identification strings, different [priorities](doc/syslog.md#priority), to
different syslog servers.

At any time, you can add a new logger or update an existing one:

```erlang
2> rr("./include/sysloggerl.hrl").
3> Prio = syslog:priority(user, info).
4> Opts = [{host, "192.168.200.15"}, {port, 514}].
5> {ok, Logger} = syslog:set(my_logger, "test", Prio, Opts).
6> NewPrio = syslog:set_loglevel(debug, Prio).
7> {ok, _} = syslog:set(Logger#logger{priority=NewPrio}).
```

All existing loggers (or a specific one using its name) can be retrieved:

```erlang
8> syslog:loggers().
[#logger{name = my_logger,ident = "test",
         udp_socket = #Port<0.802>,
         priority = #priority{facility = user,log_level = debug},
         options = [{host,"192.168.200.15"},{port,514}]},
 #logger{name = error_logger_syslog,ident = "error_logger",
         udp_socket = #Port<0.756>,
         priority = #priority{facility = user,log_level = info},
         options = []},
 #logger{name = default,ident = "sysloggerl",
         udp_socket = #Port<0.748>,
         priority = #priority{facility = user,log_level = notice},
         options = [log_pid]}]

9> syslog:logger(my_logger).
#logger{name = my_logger,ident = "test",
        udp_socket = #Port<0.802>,
        priority = #priority{facility = user,log_level = debug},
        options = [{host,"192.168.200.15"},{port,514}]}
```

By default, **sysloggerl** is started with a default logger and, if enabled, a
logger dedicated to messages reported by the error logger. their names are,
respectively, `default` and `error_logger_syslog`.

To remove a logger, you should simply call `syslog:unset/1`.

### Log messages

The easiest way to log messages is to use the default logger, _i.e_ by calling
the logging API without any logger name:

```erlang
10> syslog:log("Hello World!").
11> syslog:log(debug, "Hello World!").
12> syslog:warning_msg("I am on ~p", [node()]).
```

To use a specific logger, you should use the logging API with the corresponding
logger name:

```erlang
13> syslog:log(my_logger, info, "Hello World!", []).
14> syslog:log(my_logger, debug, "Hello World!", []).
15> syslog:warning_msg(my_logger, "I am on ~p", [node()]).
```

**Note**: any Erlang term can be used as logger name.

#### Syslog `error_logger` handler

By default, an event handler is added to log events reported by the error
logger, named [`error_logger_syslog`](src/error_logger_syslog.erl):

```erlang
16> gen_event:which_handlers(error_logger).
[error_logger_syslog,error_logger].
```

It can be disabled by setting `enable_error_logger` flag to `false` in the
application environment.

*Standard events* sent to the error logger (using `error_logger:*_msg/1,2`
functions) are logged on one line, with the level as prefix:

```text
Mar 17 13:56:46 myrkr error_logger: INFO: I'm <0.33.0>
Mar 17 13:56:46 myrkr error_logger: WARNING: be careful
Mar 17 13:56:46 myrkr error_logger: ERROR: This is an error
```

On their side, *Standard report events* (sent using `error_logger:*_report/1`
functions) are logged with a format similar to the standard event handler:

```text
Mar 17 14:47:16 myrkr error_logger: [1/4] = INFO REPORT ====
Mar 17 14:47:16 myrkr error_logger: [2/4]     tag1: data1
Mar 17 14:47:16 myrkr error_logger: [3/4]     a_term
Mar 17 14:47:16 myrkr error_logger: [4/4]     tag2: data
Mar 17 14:47:26 myrkr error_logger: [1/2] = ERROR REPORT ====
Mar 17 14:47:26 myrkr error_logger: [2/2] Serious error in my module
```

Note that *User defined reports* are also logged, following the same format.

By default, the [`error_logger_syslog`](src/error_logger_syslog.erl) handler
will also log *supervisor reports*, *crash reports* and *progress
reports*. These logs can be removed by switching on, respectively,
`no_supervisor_report`, `no_crash_report` or `no_progress_report` parameters in
the application environment.

## Configuration

You can configure the default behaviour of the **sysloggerl** by setting
following parameters in the application environment:

* `{default_syslog_host, string()}`

  Specifies the host of the Syslog daemon to which messages will be sent by
  default. Default value: `"localhost"`.

* `{default_syslog_port, pos_integer()}`

  Specifies the port of the Syslog daemon to which messages will be sent by
  default. Default value: `514`.

* `{default_ident, string()}`

  Specifies the identification string which will be prepended by default to
  logged messages. Default value: `sysloggerl`.

* `{default_facility, syslog:facility()}`

  Specifies the default Syslog [facility](doc/syslog.md#facility) used to log
  messages. Default value: `user`.

* `{default_loglevel, syslog:loglevel()}`

  Specifies the default minimum [level](doc/syslog.md#loglevel) to log
  messages. Default value: `notice`.

By default, **sysloggerl** logs events reported by the error logger using a
dedicated logger (Standard and SASL's events are logged). There are additional
parameters to configure this logger:

* `{enable_error_logger, boolean()}`

  Enables/disables the logger dedicated to messages reported by the error
  logger. Default value: `true`.

* `{error_logger_ident, string()}`

  Overwrites the default identification string for messages received from the
  error logger. Default value: `"error_logger"`.

* `{error_logger_facility, syslog:facility()}`

  Specifies the Syslog [facility](doc/syslog.md#facility) used to log error
  logger events. Default value: `user`.

* `{error_logger_loglevel, syslog:loglevel()}`

  Sets the minimum [level](doc/syslog.md#loglevel) to log error logger
  events. Default value: `info`.

* `{error_logger_depth, -1 | non_neg_integer()}`

  Controls the depth of the structures written when a report event is
  received. When the specified depth is reached, everything below this level is
  replaced by "...". Default value: `-1`.

* `{error_logger_line_length, non_neg_integer()}`

  Specifies the maximum line length of the structures written when a report
  event is received. Default value: `80`.

* `{error_logger_tty, boolean()}`

  Enables/disables printout of *standard events* to the tty. See
  `error_logger:tty/1`. It also enables/disables *supervisor reports*, *crash
  reports* and *progress reports*. Default value: `false`.

* `{no_crash_report, boolean()}`

  Enables/disables filtering out *crash reports*. Default value: `false`.

* `{no_supervisor_report, boolean()}`

  Enables/disables filtering out *supervisor reports*. Default value: `false`.

* `{no_progress_report, boolean()}`

  Enables/disables filtering out *progress reports*. Default value: `false`.

[sysloggerl_app](doc/sysloggerl_app.md) module can be used to dynamically
changed the value of these parameters. To take effet, the **sysloggerl**
application must be reloaded:

```erlang
17> sysloggerl_app:check_and_set_param(default_ident, "my_default_logger").
18> sysloggerl_app:reload().
19> syslog:logger(default).
#logger{name = default,ident = "my_default_logger",
        udp_socket = #Port<0.748>,
        priority = #priority{facility = user,log_level = notice},
        options = [log_pid]}]
```


## Advanced feature: The syslog wrappers

For performance reasons, **sysloggerl** application tries to filter unwanted log
messages by comparing their log level with the minimum level of the used logger.
So, for a logger with a minimum log level set to `notice`, all `debug` and
`info` messages will be dropped by the application. This avoids unnecessary UDP
traffic.  
Of course this is not a mandatory. You could always set the minimum level of a
logger to debug and let the syslog daemon do its job with respect of its
configuration.  
Internally, loggers are stored in an ETS table. So a lookup is required for each
messages, regardless if it will be sent or not. This is not really huge but on
an heavily used application, such calls could be not negligible.

Next, a typical and encouraged usage for applications that log their messages
through **sysloggerl** is to define a dedicated logger. But it would seem
annoying to use the [syslog API](doc/syslog.md) by repeating the logger name for each log
messages.

To solve these problems, you can wrap your logger in a logger module, using the
[syslog_wrapper API](doc/syslog_wrapper.md):

```erlang
20> Prio = syslog:priority(user, info).
21> Opts = [{host, "192.168.200.15"}, {port, 514}].
22> syslog_wrapper:create(my_app_logger, "my_app", Prio, Opts).
ok
23> my_app_logger:get_name().
{syslog_wrapper, my_app_logger}
24> my_app_logger:debug_msg("Just a test!").
ok
```

When you call `syslog_wrapper:create/4`, a module is compiled *on the fly*.  
Such created logger modules can be updated by re-calling
`syslog_wrapper:create/4`. So, like with sysloggerl loggers, it is possible to
dynamically adapt its log level to your needs. And at any time you can remove it
by calling `syslog_wrapper:destroy/1` using the module name as argument:

```erlang
25> syslog_wrapper:destroy(my_app_logger).
26> code:is_loaded(my_app_logger).
false
```

**Important**: To do its job, `syslog_wrapper` needs to have access to its own
source file. It starts to search it in the `src` subdirectory directly under the
top directory of the **sysloggerl** application (by calling
`code:lib_dir(sysloggerl, src)`. Then, if not found, it retrieves the `source`
attribute returned by `syslog_wrapper:module_info(compile)`.

## API

| Modules
| ---------------------------------------
| [sysloggerl_app](doc/sysloggerl_app.md)
| [syslog](doc/syslog.md)
| [syslog_wrapper](doc/syslog_wrapper.md)
