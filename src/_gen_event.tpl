%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Central point of erlyvideo events
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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
-module(_gen_event).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_event).


%% External API

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).




%%%------------------------------------------------------------------------
%%% Callback functions from gen_event
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (InitArgs) -> {ok, State}           |
%%                     {ok, State, hibernate}
%%
%% @doc Called by gen_event framework at process startup.
%% @end
%%----------------------------------------------------------------------

init([]) ->
  {ok, state}.

%%-------------------------------------------------------------------------
%% @spec (Event, State) -> {ok, NewState}            |
%%                         {ok, NewState, hibernate} |
%%                         {swap_handler,Args1,NewState,Handler2,Args2} |
%%                         remove_handler
%% @doc Callback for events.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_event(_Msg, State) ->
  io:format("Nice event ~p~n", [_Msg]),
  {ok, State}.

%%-------------------------------------------------------------------------
%% @spec (Request, State) -> {ok, Reply, NewState}            |
%%                           {ok, Reply, NewState, hibernate} |
%%                           {swap_handler,Reply,Args1,NewState,Handler2,Args2} |
%%                           {remove_handler,Reply}
%% @doc Callback for synchronous events.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, State) ->
  {ok, Request, State}.



%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{ok, NewState}          |
%%                      {ok, NewState, hibernate} 
%%                      {swap_handler,Args1,NewState,Handler2,Args2} |
%%                      remove_handler
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(_Info, State) ->
  {ok, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
