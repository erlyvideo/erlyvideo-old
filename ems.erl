-module(ems).

-compile(export_all).


start() -> 
	io:format("Starting EMS ...~n"),
	application:start(ems).

stop() ->
	io:format("Stopping EMS ...~n"),
	application:stop(ems).

reload() ->
	io:format("Reloading EMS Modules ...~n"),
	reload:reload([
		ems_app,
		ems_server,ems_fsm,
		ems_amf,ems_rtmp,ems_flv,
		ems_test,
		gen_rtmp
		]).
	
restart() ->
	stop(),
	reload(),
	io:format("Reloading Castini Modules ...~n"),
	castini:reload(),
	start().
