%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Central point of erlyvideo events
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_event).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_event).
-include("ems.hrl").
-include("../include/erlyvideo.hrl").

%% External API
-export([start_link/0, notify/1, add_handler/2, subscribe_to_events/1, add_sup_handler/2, remove_handler/1]).

-export([user_connected/3, user_disconnected/3, user_play/4, user_stop/4]).
-export([stream_started/4, stream_source_lost/3, stream_stopped/3]).

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
subscribe_to_events(Pid) ->
  add_sup_handler(ems_event_consumer, [Pid]).

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
%% @spec (Host, Session) -> ok
%%
%% @doc send event that user has connected
%% @end
%%----------------------------------------------------------------------
user_connected(Host, Session, Stats) ->
  UserId = proplists:get_value(user_id, Stats),
  SessionId = proplists:get_value(session_id, Stats),
  gen_event:notify(?MODULE, #erlyvideo_event{event = user_connected, host = Host, stats = Stats,
                                             user = Session, user_id = UserId, session_id = SessionId}).

%%--------------------------------------------------------------------
%% @spec (Host, Session) -> ok
%%
%% @doc send event that user has disconnected
%% @end
%%----------------------------------------------------------------------
user_disconnected(Host, Session, Stats) ->
  UserId = proplists:get_value(user_id, Stats),
  SessionId = proplists:get_value(session_id, Stats),
  gen_event:notify(?MODULE, #erlyvideo_event{event = user_disconnected, host = Host, stats = Stats,
                                             user = Session, user_id = UserId, session_id = SessionId}).

%%--------------------------------------------------------------------
%% @spec (Host, User, Name) -> ok
%%
%% @doc send event that user has started playing
%% @end
%%----------------------------------------------------------------------
user_play(Host, User, Stream, Options) ->
  Name = proplists:get_value(name, Options),
  gen_event:notify(?MODULE, #erlyvideo_event{event = user_play, host = Host, user = User, 
                                             stream_name = Name, stream = Stream, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Host, User, Name, Stats) -> ok
%%
%% @doc send event that user has finished playing
%% @end
%%----------------------------------------------------------------------
user_stop(Host, User, Stream, Options) ->
  Name = proplists:get_value(name, Options),
  gen_event:notify(?MODULE, #erlyvideo_event{event = user_stop, host = Host, user = User, stream = Stream, stream_name = Name, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Stream, Options) -> ok
%%
%% @doc send event that stream has started
%% @end
%%----------------------------------------------------------------------
stream_started(Host, Name, Stream, Options) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_started, host = Host, name = Name, stream = Stream, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Stream) -> ok
%%
%% @doc send event that stream source was temporarily lost
%% @end
%%----------------------------------------------------------------------
stream_source_lost(Host, Name, Stream) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_source_lost, host = Host, name = Name, stream = Stream}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Stream) -> ok
%%
%% @doc send event that stream was completely stopped
%% @end
%%----------------------------------------------------------------------
stream_stopped(Host, Name, Stream) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_stopped, host = Host, name = Name, stream = Stream}).



%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec ([]) -> {ok, State}           |
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
  ?D({ems_event, Event}),
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
