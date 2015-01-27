# sysloggerl: A syslog logger for Erlang

**sysloggerl** is a pure [Erlang/OTP](http://www.erlang.org/) application for
logging messages to syslog daemons, using UDP sockets. It provides a
customizable error_logger report handler and supports several loggers with
different logging options. For now, it implements only the [RFC
3164](http://tools.ietf.org/html/rfc3164) protocol.

**sysloggerl** is distributed under the terms of the **2-clause BSD license**;
see `COPYING`.

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

First of all, before using **syslogger**, you must start it:

```erlang
> application:start(syslogger).
```

Then, you can use `syslog` module to log messages, to set new loggers or to
retrieve info about existing loggers.

### Play with loggers

TODO

### Log messages

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

TODO

## Advanced feature: The syslog wrappers

TODO
