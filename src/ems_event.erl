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
-include("log.hrl").
-include("jsonerl.hrl").
-include("../include/erlyvideo.hrl").

%% External API
-export([start_link/0, notify/1, add_handler/2, subscribe_to_events/1, add_sup_handler/2, remove_handler/1]).
-export([start_handlers/0, stop_handlers/0]).

-export([user_connected/3, user_disconnected/3, user_play/4, user_stop/4]).
-export([stream_created/4, stream_started/4, stream_source_lost/3, stream_source_requested/3, stream_stopped/3]).
-export([slow_media/2]).

-export([to_json/1, to_xml/1]).

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  start_handlers(),
  {ok, Pid}.



start_handlers() ->
  gen_event:add_handler(?MODULE, ?MODULE, []),
  Hosts = proplists:get_keys(ems:get_var(vhosts,[])),
  lists:foreach(fun(Host) ->
    [gen_event:add_handler(?MODULE, ems_event_hook, [Host, Event, Handler]) || {Event, Handler} <- ems:get_var(event_handlers, Host, [])]
  end, Hosts),
  ok.

stop_handlers() ->
  [gen_event:delete_handler(?MODULE, Handler, []) || Handler <- gen_event:which_handlers(?MODULE)].

%%--------------------------------------------------------------------
%% @spec (Event::any()) -> ok
%%
%% @doc Convert event to JSON
%% @end
%%----------------------------------------------------------------------
to_json(Event) ->
  Map = fun
    (V) when is_pid(V) -> list_to_binary(erlang:pid_to_list(V));
    (V) when is_reference(V) -> list_to_binary(erlang:ref_to_list(V));
    (V) -> V
  end,
  Clean = [{K,Map(V)} || {K,V} <- tuple_to_list(?record_to_struct(erlyvideo_event, Event))],
  JSON = mochijson2:encode(Clean),
  iolist_to_binary(JSON).

to_xml(Event) ->
  Content = xmlize(tuple_to_list(?record_to_struct(erlyvideo_event, Event)), []),
  XML = xmerl:export_simple([{event, [], Content}], xmerl_xml),
  iolist_to_binary(XML).

xmlize([{K,V}|Attr], Acc) when is_binary(K) ->
  xmlize([{binary_to_atom(K,utf8),V}|Attr], Acc);


xmlize([{K,undefined}|Attr], Acc) ->
  xmlize(Attr, [{K, [], ["null"]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_atom(V) ->
  xmlize(Attr, [{K, [], [atom_to_list(V)]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_binary(V) ->
  xmlize(Attr, [{K, [], [io_lib:format("~s", [V])]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_number(V) ->
  xmlize(Attr, [{K, [], [io_lib:format("~p", [V])]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_pid(V) ->
  xmlize(Attr, [{K, [], [erlang:pid_to_list(V)]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_reference(V) ->
  xmlize(Attr, [{K, [], [erlang:ref_to_list(V)]}|Acc]);

xmlize([{K,[{_,_}|_] = V}|Attr], Acc) ->
  xmlize(Attr, [{K, [], xmlize(V, [])}|Acc]);

xmlize([{K,V}|Attr], Acc) ->
  xmlize(Attr, [{K, [], [V]}|Acc]);
  
xmlize([], Acc) ->
  lists:reverse(Acc).

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
%% @doc send event that stream has been created
%% @end
%%----------------------------------------------------------------------
stream_created(Host, Name, Stream, Options) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_created, host = Host, stream_name = Name, stream = Stream, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Stream, Options) -> ok
%%
%% @doc send event that stream has been really started. Maybe sent many times per stream, as soon as source is appearing.
%% @end
%%----------------------------------------------------------------------
stream_started(Host, Name, Stream, Options) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_started, host = Host, stream_name = Name, stream = Stream, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Stream) -> ok
%%
%% @doc send event that stream source was temporarily lost
%% @end
%%----------------------------------------------------------------------
stream_source_lost(Host, Name, Stream) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_source_lost, host = Host, stream_name = Name, stream = Stream}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Options) -> ok
%%
%% @doc send event that stream is trying to restore source
%% @end
%%----------------------------------------------------------------------
stream_source_requested(Host, Name, Options) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_source_requested, host = Host, stream_name = Name, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Stream) -> ok
%%
%% @doc send event that stream was completely stopped
%% @end
%%----------------------------------------------------------------------
stream_stopped(Host, Name, Stream) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = stream_stopped, host = Host, stream_name = Name, stream = Stream}).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Stream) -> ok
%%
%% @doc media cannot read frames at required speed
%% @end
%%----------------------------------------------------------------------
slow_media(Stream, Delay) when is_pid(Stream) andalso is_number(Delay) ->
  gen_event:notify(?MODULE, #erlyvideo_event{event = slow_media, stream = Stream, options = [{delay,Delay}]}).


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
handle_event(_Event, State) ->
  %?D({ems_event, Event}),
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
