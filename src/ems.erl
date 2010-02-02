%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        Helper module for easy application start, stop, reloading , etc.
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('max@maxidoors.ru').
% -include("../include/ems.hrl").
-include_lib("erlyvideo/include/rtmp_session.hrl").

-export([start/0,stop/0,restart/0,rebuild/0,reload/0]).
-export([get_var/2, get_var/3, check_app/3, try_method_chain/3, respond_to/3]).
-export([start_modules/0, stop_modules/0]).
-export([call_modules/2]).
-export([host/1, load_config/0, reconfigure/0]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Starts ErlMedia
%% @end 
%%--------------------------------------------------------------------
start() -> 
	io:format("Starting ErlMedia ...~n"),
	ems:load_config(),
  application:start(crypto),
  application:start(rtsp),
  application:start(rtmp),
  start_rtmp(),
  start_rtsp(),
  ems_log:start(),
	application:start(erlmedia),
	ems:start_modules().
  

start_rtmp() ->
  case ems:get_var(rtmp_port, 1935) of
    undefined -> 
      ok;
    RTMP when is_integer(RTMP) -> 
      rtmp_socket:start_server(RTMP, rtmp_listener1, rtmp_session)
  end.
  

start_rtsp() ->
  case ems:get_var(rtsp_port, undefined) of
    undefined -> 
      ok;
    RTSP when is_integer(RTSP) -> 
      rtsp:start_server(RTSP, rtsp_listener1, ems_rtsp)
  end.
    

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Stops ErlMedia
%% @end 
%%--------------------------------------------------------------------
stop() ->
	io:format("Stopping ErlMedia ...~n"),
	ems:stop_modules(),
	application:stop(erlmedia),
	application:unload(erlmedia),
  ems_log:stop(),
	application:stop(rtsp),
	application:unload(rtsp),
	application:stop(rtmp),
	application:unload(rtmp).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Stops, Compiles , Reloads and starts ErlMedia
%% @end 
%%--------------------------------------------------------------------
restart() ->
	stop(),
	rebuild(),
	reload(),
	start().

reconfigure() ->
  RTMP = ems:get_var(rtmp_port, undefined),
  RTSP = ems:get_var(rtsp_port, undefined),
  load_config(),
  case {RTMP, ems:get_var(rtmp_port, undefined)} of
    {undefined, _} -> ok;
    {RTMP, RTMP} -> ok;
    _ -> 
      supervisor:terminate_child(rtmp_sup, rtmp_listener1),
      supervisor:delete_child(rtmp_sup, rtmp_listener1)
  end,
  case {RTSP, ems:get_var(rtsp_port, undefined)} of
    {undefined, _} -> ok;
    {RTSP, RTSP} -> ok;
    _ -> 
      supervisor:terminate_child(rtsp_sup, rtsp_listener1),
      supervisor:delete_child(rtsp_sup, rtsp_listener1)
  end,
  {ok, _} = start_rtmp(),
  {ok, _} = start_rtsp(),
  ok.
  
load_config() ->
  case file:consult("priv/erlmedia.conf") of
    {ok, Env} -> [application:set_env(erlmedia, Key, Value) || {Key, Value} <- Env];
    _ -> ok
  end.
      

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Compiles ErlMedia
%% @end 
%%--------------------------------------------------------------------
rebuild() ->
	io:format("Recompiling EMS Modules ...~n"),
	make:all([load]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Compiles and reloads ErlMedia Modules
%% @end
%%--------------------------------------------------------------------
reload() ->
	application:load(erlmedia),
	case application:get_key(erlmedia,modules) of
		undefined    -> 
			application:load(erlmedia),
			reload();
		{ok,Modules} -> 
			io:format("Reloading EMS Modules ...~n"),
			reload(lists:usort(Modules))
	end.
reload(Module) when is_atom(Module) ->
	code:purge(Module),
	code:delete(Module),
	code:load_file(Module),
	true;
reload([]) -> ok;
reload([?MODULE | T]) -> reload(T);
reload([H|T]) ->
	reload(H),
	reload(H),
	reload(T).
	
	
	
	
%%--------------------------------------------------------------------
%% @spec (Opt::atom(), Default::any()) -> any()
%% @doc Gets application enviroment variable. Returns Default if no 
%% varaible named Opt is found. User defined varaibles in .config file
%% override application default varabiles.
%% @end 
%%--------------------------------------------------------------------
get_var(Opt, Default) ->
	case lists:keysearch(erlmedia, 1, application:loaded_applications()) of
		false -> application:load(erlmedia);
		_ -> ok
	end,
	case application:get_env(erlmedia, Opt) of
	{ok, Val} -> Val;
	_ ->
		case init:get_argument(Opt) of
		{ok, [[Val | _] | _]} -> Val;
		error		-> Default
		end
	end.


get_var(Key, Host, Default) ->
  case ets:match_object(vhosts, {{host(Host), Key}, '$1'}) of
    [{{_Hostname, Key}, Value}] -> Value;
    [] -> Default
  end.


respond_to(Module, Command, Arity) ->
  case code:ensure_loaded(Module) of
		{module, Module} -> 
		  lists:member({Command, Arity}, Module:module_info(exports));
		_ -> false
	end.
  
  
host(Hostname) when is_binary(Hostname) -> host(binary_to_list(Hostname));
host(Hostname) when is_atom(Hostname) -> Hostname;
host(Hostname) -> 
  case ets:match_object(vhosts, {Hostname, '$1'}) of
    [{Hostname, Host}] -> Host;
    [] -> default
  end.
  

%%--------------------------------------------------------------------
%% @spec () -> ok | {error, Reason}
%% @doc Initializes all modules
%% @end 
%%--------------------------------------------------------------------
start_modules() -> call_modules(start, []).

call_modules(Function, Args) -> call_modules(Function, Args, ems:get_var(applications, [])).

call_modules(_, _, []) -> 
  ok;
call_modules(Function, Args, [Module|Modules]) ->
  case respond_to(Module, Function, length(Args)) of
    true -> 
      ok = erlang:apply(Module, Function, Args);
    _ ->
      ok
  end,
  call_modules(Function, Args, Modules).
  

%%--------------------------------------------------------------------
%% @spec () -> ok | {error, Reason}
%% @doc Shutdown all modules
%% @end 
%%--------------------------------------------------------------------
stop_modules() -> call_modules(stop, []).



%%--------------------------------------------------------------------
%% @spec (Opt::atom(), Command::atom(), Arity::integer()) -> any()
%% @doc Try to launch methods one by one in modules
%% @end 
%%--------------------------------------------------------------------

try_method_chain(Host, Method, Args) when is_atom(Host) ->
  try_method_chain(ems:get_var(applications, Host, ['apps_rtmp']), Method, Args);

try_method_chain([], _Method, _Args) ->
  {unhandled};

try_method_chain([Module | Applications], Method, Args) ->
  case respond_to(Module, Method, length(Args)) of
    true -> case apply(Module, Method, Args) of
      {unhandled} -> try_method_chain(Applications, Method, Args);
      Else -> Else
    end;
    false -> try_method_chain(Applications, Method, Args)
  end.  


%%--------------------------------------------------------------------
%% @spec (Opt::atom(), Command::atom(), Arity::integer()) -> any()
%% @doc Look whan module in loaded plugins can handle required method
%% @end 
%%--------------------------------------------------------------------

check_app(_, connect, _) ->
  apps_rtmp;
  
check_app([], _Command, _Arity) ->
  unhandled;

check_app([Module | Applications], Command, Arity) ->
  case respond_to(Module, Command, Arity) of
    true -> Module;
    false -> check_app(Applications, Command, Arity)
  end;


check_app(#rtmp_session{host = Host} = _State, Command, Arity) ->
  Applications = ems:get_var(applications, Host, ['apps_rtmp']),
  check_app(Applications, Command, Arity).



	