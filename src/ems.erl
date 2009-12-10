%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Helper module for easy application start, stop, reloading , etc.
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
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
-include("../include/ems.hrl").

-export([start/0,stop/0,restart/0,rebuild/0,reload/0]).
-export([get_var/1,get_var/2, check_app/3, try_method_chain/2]).
-export([start_modules/0, stop_modules/0]).
-export([call_modules/2]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Starts ErlMedia
%% @end 
%%--------------------------------------------------------------------
start() -> 
	io:format("Starting ErlMedia ...~n"),
	application:start(?APPLICATION).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Stops ErlMedia
%% @end 
%%--------------------------------------------------------------------
stop() ->
	io:format("Stopping ErlMedia ...~n"),
	application:stop(?APPLICATION),
	application:unload(?APPLICATION).

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
	application:load(?APPLICATION),
	case application:get_key(?APPLICATION,modules) of
		undefined    -> 
			application:load(?APPLICATION),
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
%% @spec (Opt::atom()) -> any()
%% @doc Gets application enviroment variable. User defined varaibles in 
%% .config file override application default varabiles. Default [].
%% @end 
%%--------------------------------------------------------------------
get_var(Opt) -> get_var(Opt, []).
%%--------------------------------------------------------------------
%% @spec (Opt::atom(), Default::any()) -> any()
%% @doc Gets application enviroment variable. Returns Default if no 
%% varaible named Opt is found. User defined varaibles in .config file
%% override application default varabiles.
%% @end 
%%--------------------------------------------------------------------
get_var(Opt, Default) ->
	case lists:keysearch(?APPLICATION, 1, application:loaded_applications()) of
		false -> application:load(?APPLICATION);
		_ -> ok
	end,
	case application:get_env(?APPLICATION, Opt) of
	{ok, Val} -> Val;
	_ ->
		case init:get_argument(Opt) of
		{ok, [[Val | _] | _]} -> Val;
		error		-> Default
		end
	end.


respond_to(Module, Command, Arity) ->
  case code:ensure_loaded(Module) of
		{module, Module} -> 
		  lists:member({Command, Arity}, Module:module_info(exports));
		_ -> false
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

try_method_chain(Method, Args) ->
  try_method_chain(ems:get_var(applications, ['apps_rtmp']), Method, Args).

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
	
check_app([], _Command, _Arity) ->
  unhandled;

check_app([Module | Applications], Command, Arity) ->
  case respond_to(Module, Command, Arity) of
    true -> Module;
    false -> check_app(Applications, Command, Arity)
  end;


check_app(#rtmp_session{} = _State, Command, Arity) ->
  Applications = ems:get_var(applications, ['apps_rtmp']),
  check_app(Applications, Command, Arity).



	