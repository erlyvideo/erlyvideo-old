-module(ems).

-compile(export_all).


start() -> 
	io:format("Starting EMS ...~n"),
	application:start(ems).

stop() ->
	io:format("Stopping EMS ...~n"),
	application:stop(ems).

reload() ->
	io:format("Compiling and reloading EMS Modules ...~n"),
	make:all([load]).
