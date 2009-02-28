%% This is the application resource file (.app file) for the 'base'
%% application.
{application, log4erl,
[{description, "Logger for erlang in the spirit of Log4J"},
 {vsn, "0.8.3"},
 {modules, [log4erl]},
 {registered,[log4erl]},
 {applications, [kernel,stdlib]},
 {mod, {log4erl,[default_logger]}},
 {start_phases, []}
]}.
