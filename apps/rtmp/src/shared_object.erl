%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        shared object support
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(shared_object).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/rtmp.hrl").
-include("shared_object.hrl").
-define(SAVE_TIMEOUT, 1000).

-behaviour(gen_server).

-record(so_state, {
  host,
  name,
  version = 0,
  file_path,
  table_name,
  persistent,
  event_count,
  data = [],
  clients = []
}).

-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
         
-export([message/2]).

%%--------------------------------------------------------------------
%% @spec (Host, Name, Persistent::boolean()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Starts new shared object.
%% @private
%% @end
%%----------------------------------------------------------------------
start_link(Host, Name, Persistent)  ->
   gen_server:start_link(?MODULE, [Host, Name, Persistent], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec message(Object::pid(), Message::any()) -> ok
%%
%% @doc Send message to shared object
%% @end
%%----------------------------------------------------------------------
message(Object, Message) ->
  gen_server:cast(Object, {message, Message, self()}).

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
init([Host, Name, Persistent]) ->
  % ?D({"Loaded", Host, Name, Data}),
  FilePath = file_path(Host, Name),
  TableName = case Persistent of
    true -> 
      DETSName = table_name(Host, Name),
      {ok, DETSName} = dets:open_file(DETSName, [{file,FilePath},{auto_save,1000},{keypos,#shared_object.key}]),
      DETSName;
    _ -> 
      undefined
  end,
  State = #so_state{host = Host, name = Name, persistent = Persistent, table_name = TableName, file_path = FilePath},
  {Data, Version} = load(State),
  {ok, State#so_state{data = Data, version = Version, event_count = 1}}.
  

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
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

handle_call(data, _From, #so_state{data = Data} = State) ->
  {reply, {ok, Data}, State};

handle_call(Request, _From, State) ->
 {stop, {unknown_call, Request}, State}.


handle_event([], _, State) ->
  State;

handle_event([connect | Events], Client, #so_state{clients = Clients, data = _Data, host = _Host} = State) ->
  erlang:monitor(process, Client),
  connect_notify(Client, State),
  handle_event(Events, Client, State#so_state{clients = [Client | Clients]});

handle_event([{set_attribute, {Key, Value}} | Events], Client, #so_state{name = Name, version = Version, persistent = P, data = Data, clients = Clients} = State) ->
  
  NewState = State#so_state{data = lists:keystore(Key, 1, Data, {Key, Value}), version = Version+1},
  AuthorReply = #so_message{name = Name, version = Version, persistent = P, events = [{update_attribute, Key}]},
  rtmp_session:send(Client, #rtmp_message{type = shared_object, body = AuthorReply}),
  
  OtherReply = #so_message{name = Name, version = Version+1, persistent = P, events = [{update_data, [{Key, Value}]}]},
  Message = #rtmp_message{type = shared_object, body = OtherReply},
  ClientList = lists:delete(Client, Clients),
  [rtmp_session:send(C, Message) || C <- ClientList],
  save(NewState),
  handle_event(Events, Client, NewState);


handle_event([{delete_attribute, Key} | Events], Client, #so_state{name = Name, version = Version, persistent = P, data = Data, clients = Clients} = State) ->

  NewState = State#so_state{data = lists:keydelete(Key, 1, Data), version = Version+1},
  AuthorReply = #so_message{name = Name, version = Version, persistent = P, events = [{update_attribute, Key}]},
  rtmp_session:send(Client, #rtmp_message{type = shared_object, body = AuthorReply}),

  OtherReply = #so_message{name = Name, version = Version+1, persistent = P, events = [{delete_data, Key}]},
  Message = #rtmp_message{type = shared_object, body = OtherReply},
  ClientList = lists:delete(Client, Clients),
  [rtmp_session:send(C, Message) || C <- ClientList],
  save(NewState),
  handle_event(Events, Client, NewState);


handle_event([{send_message, {Function, Args}} | Events], Client, #so_state{name = Name, version = Version, persistent = P, clients = Clients} = State) ->
  Reply = #so_message{name = Name, version = Version, persistent = P, events = [{send_message, {Function, Args}}]},
  Message = #rtmp_message{type = shared_object, body = Reply},
  [rtmp_session:send(C, Message) || C <- Clients],
  handle_event(Events, Client, State);
  
handle_event([{Event, EventData} | Events], Client, State) ->
  ?D({"Unknown event", Event, EventData}),
  handle_event(Events, Client, State);

handle_event([Event | Events], Client, State) ->
  ?D({"Unknown event", Event}),
  handle_event(Events, Client, State).
  

connect_notify(Client, #so_state{name = Name, version = Version, persistent = P, data = []}) ->
  Reply = #so_message{name = Name, version = Version, persistent = P, events = [initial_data]},
  rtmp_session:send(Client, #rtmp_message{type = shared_object, body = Reply});

connect_notify(Client, #so_state{name = Name, version = Version, persistent = P, data = Data}) ->
  Updates = [{update_data, [Entry]} || Entry <- Data],
  Reply = #so_message{name = Name, version = Version, persistent = P, events = [initial_data | Updates]},
  rtmp_session:send(Client, #rtmp_message{type = shared_object, body = Reply}).

save(#so_state{persistent = false}) -> ok;
save(#so_state{host = Host, name = Name, data = Data, version = Version, table_name = Table}) -> 
  % ?D({"Saving", Host, Name, Data}),
  dets:insert(Table, #shared_object{key={Host, Name}, version=Version, data=Data}).

load(#so_state{persistent = false}) -> {[], 0};
load(#so_state{name = Name, host = Host, persistent = true, table_name = Table}) ->
  case dets:lookup(Table, {Host, Name}) of
    [#shared_object{data = Data, version = Version}] -> {Data, Version};
    _ -> {[], 0}
  end.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast({message, #so_message{events = Events}, Client}, #so_state{event_count = Count} = State) ->
  State1 = handle_event(Events, Client, State),
  case Count rem 10000 of
    1 -> statistics(wall_clock);
    0 -> 
      {_, Time} = statistics(wall_clock),
      io:format("~pK sync/sec ~p~n", [round(10000/Time), Count]);
    _ -> ok
  end,
  {noreply, State1#so_state{event_count = Count+1}};
  

handle_cast(_Msg, State) ->
  {noreply, State}.

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
% 

file_path(Host, _Name) -> "/tmp/"++atom_to_list(Host)++".db".
table_name(Host, _Name) -> list_to_atom("shared_objects_"++atom_to_list(Host)).


handle_info({'DOWN', _, process, Client, _Reason}, #so_state{name = Name, clients = Clients} = State) ->
  NewClients = lists:delete(Client, Clients),
  % ?D({"Client diconnected from", State#so_state.name, Client}),
  case length(NewClients) of
    0 -> ?D({"Stopping SO", Name}), {stop, normal, State};
    _ -> {noreply, State#so_state{clients = NewClients}}
  end;

handle_info(_Info, State) ->
  ?D({"Unknown message", _Info}),
  {noreply, State}.



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

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

