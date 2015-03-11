# sysloggerl: A syslog logger for Erlang

**sysloggerl** is a pure [Erlang/OTP](http://www.erlang.org/) application for
logging messages to syslog daemons, using UDP sockets. It provides a
customizable error_logger report handler and can handle several loggers with
different logging options. For now, it implements only the [RFC
3164](http://tools.ietf.org/html/rfc3164) protocol.

**sysloggerl** is distributed under the terms of the **2-clause BSD license**;
see `COPYING`.


[![Build Status](https://travis-ci.org/yakaz/sysloggerl.svg?branch=master)](https://travis-ci.org/yakaz/sysloggerl)

Table of contents
-----------------
 - [Installation](#installation)
  - [Rebar](#rebar)
  - [Autotools](#autotools)
 - [Getting started](#getting-started)
  - [Play with loggers](#play-with-loggers)
  - [Log messages](#log-messages)
  - [Advanced feature: The syslog wrappers](#advanced-feature-the-syslog-wrappers)
 - [Configuration](#configuration)
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


## Getting started

First of all, before using **sysloggerl**, you must start it:

```erlang
1> application:start(sysloggerl).
```

Then, you can use `syslog` module to log messages, to set new loggers or to
retrieve info about existing ones.  
To ease its use, the **sysloggerl** application exports two records (see
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
identification strings, different priorities, to different syslog servers.

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
         options = [log_pid,{host,"localhost"},{port,514}]}]

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

### Advanced feature: The syslog wrappers

TODO


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

  Specifies the default Syslog facility used to log messages. Default value:
  `user`.

* `{default_loglevel, syslog:loglevel()}`

  Specifies the default minimum level to log messages. Default value: `notice`.

By default, **sysloggerl** logs events reported by the error logger using a
dedicated logger. There are additional parameters to configure this logger:

* `{enable_error_logger, boolean()}`

  Enables/disables the logger dedicated to messages reported by the error
  logger. Default value: `true`.

* `{error_logger_ident, string()}`

  Overwrites the default identification string for messages received from the
  error logger. Default value: `"error_logger"`.

* `{error_logger_facility, syslog:facility()}`

  Specifies the Syslog facility used to log error logger events. Default value:
  `user`.

* `{error_logger_loglevel, syslog:loglevel()}`

  Sets the minimum level to log error logger events. Default value: `info`.

* `{error_logger_depth, -1 | non_neg_integer()}`

  Controls the depth of the structures written when a report event is
  received. When the specified depth is reached, everything below this level is
  replaced by "...". Default value: `-1`.

* `{error_logger_line_length, non_neg_integer()}`

  Specifies the maximum line length of the structures written when a report
  event is received. Default value: `80`.

* `{error_logger_tty, boolean()}`

  Enables/disables printout of standard events to the tty. See
  `error_logger:tty/1`. Default value: `false`.


## API

| Modules
| ---------------------------------------
| [syslog](doc/syslog.md)
| [syslog_wrapper](doc/syslog_wrapper.md)
