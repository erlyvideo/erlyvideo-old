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
-module(ems_event).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_event).


%% External API
-export([start_link/0, notify/1, add_handler/2, add_sup_handler/2, remove_handler/1]).

-export([user_connected/2, user_disconnected/2, user_play/4]).
-export([stream_started/3, stream_source_lost/3, stream_stopped/3]).

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  gen_event:add_handler(?MODULE, ?MODULE, []),
  {ok, Pid}.


%%--------------------------------------------------------------------
%% @spec (Event::any()) -> ok
%%
%% @doc Send event to ems_event subscribers
%% @end
%%----------------------------------------------------------------------
notify(Event) ->
  gen_event:notify(?MODULE, Event).

  
%%--------------------------------------------------------------------
%% @spec (Handler::any(), Args::[any()]) -> ok
%%
%% @doc Subscribe to ems_event
%% @end
%%----------------------------------------------------------------------
add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).

%%--------------------------------------------------------------------
%% @spec (Handler::any(), Args::[any()]) -> ok
%%
%% @doc Subscribe to ems_event
%% @end
%%----------------------------------------------------------------------
add_sup_handler(Handler, Args) ->
  gen_event:add_sup_handler(?MODULE, Handler, Args).
  
%%--------------------------------------------------------------------
%% @spec (Handler::any()) -> ok
%%
%% @doc Unsubscribe from ems_event
%% @end
%%----------------------------------------------------------------------
remove_handler(Handler) ->
  gen_event:remove_handler(?MODULE, Handler).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc send event that user has connected
%% @end
%%----------------------------------------------------------------------
user_connected(Host, Session) ->
  gen_event:notify(?MODULE, {user_connected, Host, Session}).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc send event that user has disconnected
%% @end
%%----------------------------------------------------------------------
user_disconnected(Host, Session) ->
  gen_event:notify(?MODULE, {user_disconnected, Host, Session}).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc send event that user has disconnected
%% @end
%%----------------------------------------------------------------------
user_play(Host, User, Name, Media) ->
  gen_event:notify(?MODULE, {user_play, Host, User, Name, Media}).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc send event that stream has started
%% @end
%%----------------------------------------------------------------------
stream_started(Host, Name, Stream) ->
  gen_event:notify(?MODULE, {stream_started, Host, Name, Stream}).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc send event that stream source was temporarily lost
%% @end
%%----------------------------------------------------------------------
stream_source_lost(Host, Name, Stream) ->
  gen_event:notify(?MODULE, {stream_source_lost, Host, Name, Stream}).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc send event that stream was completely stopped
%% @end
%%----------------------------------------------------------------------
stream_stopped(Host, Name, Stream) ->
  gen_event:notify(?MODULE, {stream_stopped, Host, Name, Stream}).



%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([]) ->
  {ok, state}.

%%-------------------------------------------------------------------------
%% @spec (Request, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, State) ->
  {ok, Request, State}.


%%-------------------------------------------------------------------------
%% @spec (Event, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, State) ->
  error_logger:info_msg("ems_event: ~p~n", [Event]),
  {ok, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
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
