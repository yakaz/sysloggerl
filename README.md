# sysloggerl: A syslog logger for Erlang

**sysloggerl** is pure an [Erlang/OTP](http://www.erlang.org/) application for
logging messages to syslog daemons, using UDP sockets. It provides a
customizable error_logger report handler and supports several loggers with
different logging options. For now, it implements only the
[RFC 3164](http://tools.ietf.org/html/rfc3164) protocol.

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
