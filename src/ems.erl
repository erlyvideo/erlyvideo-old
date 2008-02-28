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
-include("../include/ems.hrl").

-export([start/0,stop/0,restart/0,rebuild/0,reload/0]).


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
	application:stop(?APPLICATION).

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