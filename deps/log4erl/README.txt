log4erl Manual:
===============

TOC:
====
1. Features
2. Installation
3. Usage
4. API
5. Configuration
6. Known issues
7. Future development
8. License

1. FEATURES:
============
- Multiple logs
- Currently, only size-based log rotation of files for file appender
- Support default logger if no logger specified
- 5 predifined log levels (debug, info, warn, error, fatal)
- A log handler for error_logger
- Support for user-specified log levels
- Support for a log formatter (similar to Layouts in Log4J)
- Support for console log
- Support for smtp formatter
- Support for XML logs
- Support for syslog 
- Support for changing format and level of appender during run-time

2. INSTALLATION:
================
To compile & install log4erl, download source from google code's website
(http://code.google.com/p/log4erl/) or  from svn:
$> svn checkout http://log4erl.googlecode.com/svn/trunk/ log4erl

or from github public repository (http://github.com/ahmednawras/log4erl/).

$> cd log4erl
$> make

or you can run the below from erlang shell:

$> cd src
$> erl
1> make:all([{outdir, "../ebin"}]).

3. USAGE:
=========
1- In order to use log4erl, you need to first include it in the path. There
are 2 ways to do this:
a) include the "log4erl" directory in erlang's "lib" directory in the target
machine (cp -Rf log4erl /where/erlang/is/lib).
$> cp -Rf log4erl /usr/local/lib/erlang/lib/

b) include the "log4erl" ebin directory in the path when running you program
$> erl -pz /path/to/log4erl ...

2- Once the log4erl directory is included, you can use its API as described in section "API". but before, 
you need to run:
> application:start(log4erl).

3- Create a configuration file and load it using log4erl:conf(File)
> log4erl:conf("priv/log4erl.conf").

4- Alternatevly, you can create loggers & add appenders to them programmatically as appropriate. 
You can do this as per the API below.
> log4erl:add_logger(messages_log).
> log4erl:add_console_appender(messages_log, cmd_logs, {warn, "[%L] %l%n"}).
  where Conf is the erlang term describing how the log is to be handled
  You can also add more types of appenders, which is explained in API.txt and Appneders_API.txt.

5- Now, you can log whatever messages you like as per logging functions described in API.
> log4erl:info("Information message").

Precedance of log levels are:
all = debug < info < warn < error < fatal < none
User defined levels are always written except when none level is specified in the logger specification
(See below).

4. API:
=======
Please look at API.txt file for more information about the API.

5. CONFIGURATION:
=================
Please look at CONFIGURATION.txt for more information about how to configure log4erl.

6. KNOWN ISSUES:
================
- Name of both loggers & appenders should be unique and not registered since log4erl will try and register
  their names. If the name is already registered, nothing will happen. This will be fixed soon.
- If you run change_log_format/1,2 and appender crashed, a restart from the supervisor will not record the latest
  format used. It will only use either the default format or the format used in the argument is supplied.  

7. FUTURE DEVELOPMENT:
======================
- Add support for different log persistance methods (e.g files, XML, console, DB, SNMP, syslog...etc)
- Add support for time-based log rotation
- Multiple configuration format (Erlang terms, XML?, properties files?)
- Add support for NDC & MDC ???

Please send your suggestion to ahmed.nawras <at @ at> gmail <dot . dot> com

8. LICENSE:
===========
This software is subject to "Mozilla Public License 1.1". You can find the license terms
in the file 'LICENSE.txt' shipping along with the source code. You may also get a copy
of the license term from the URL: "http://www.mozilla.org/MPL/MPL-1.1.html".
