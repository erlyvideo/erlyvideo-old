-module(shared_object).
-author(max@maxidoors.ru).
-include("../../include/ems.hrl").
-include("../../include/shared_objects.hrl").


-behaviour(gen_server).

-record(shared_object, {
  name,
  version = 0,
  persistent,
  data = [],
  clients = []
}).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
         
-export([message/2]).

%%--------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(SharedObject, Persistent)  ->
   gen_server:start_link(?MODULE, [SharedObject, Persistent], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------


message(Object, Message) ->
  gen_server:call(Object, {message, Message}).

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
init([Name, Persistent]) ->
  process_flag(trap_exit, true),
  {ok, #shared_object{name = Name, persistent = Persistent, data = [{a, 1}, {b, <<"zz">>}]}}.
  

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

handle_call({message, #so_message{events = Events} = Message}, {Client, _Ref}, State) ->
  {State1, Replies} = parse_event(Events, Client, State, []),
  
  Reply = Message#so_message{events = Replies},
  gen_fsm:send_event(Client, {send, {#channel{id = 12, timestamp = 0, type = ?RTMP_TYPE_SO_AMF0, stream_id = 0}, Reply}}),
  {reply, ok, State1};

handle_call(Request, _From, State) ->
 {stop, {unknown_call, Request}, State}.


parse_event([], _, State, Reply) ->
  {State, Reply};

parse_event([?SO_CONNECT | Events], Client, #shared_object{clients = Clients, data = _Data} = State, Replies) ->
  link(Client),
  ?D({"Client connected to", State#shared_object.name, Client}),
  
  parse_event(Events, Client, State#shared_object{clients = [Client | Clients]}, 
              [?SO_INITIAL_DATA, ?SO_CLEAR_DATA |Replies]).
  

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
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

handle_info({'EXIT', Client, _Reason}, #shared_object{clients = Clients} = State) ->
  NewClients = lists:delete(Client, Clients),
  ?D({"Client diconnected from", State#shared_object.name, Client}),
  case length(NewClients) of
    0 -> {stop, normal, State};
    _ -> {noreply, State#shared_object{clients = NewClients}}
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
  ?D({"Shared object stop", self(), _State#shared_object.name}),
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

