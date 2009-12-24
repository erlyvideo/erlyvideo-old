-module(rtmpt_sessions).
-author(max@maxidoors.ru).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
         
-export([create/2]).
         
-record(rtmpt_sessions, {
  sessions
}).

-record(rtmpt_session, {
  key,
  session
}).


%%--------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link()  ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

open(SessionId, IP) ->
  {ok, Object} = gen_server:call(?MODULE, {open, SessionId, IP}),
  Object.

create(SessionId, IP) ->
  {ok, Object} = gen_server:call(?MODULE, {create, SessionId, IP}),
  Object.

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
  process_flag(trap_exit, true),
  Sessions = ets:new(rtmpt_sessions, [set, {keypos, #rtmpt_session.key}]),
  {ok, #rtmpt_sessions{sessions = Sessions}}.
  

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

handle_call({create, SessionID, IP}, _From, #rtmpt_sessions{sessions = Sessions} = State) ->
  case ets:lookup(Sessions, {SessionID, IP}) of
    [{Key, Session}] -> 
      {reply, {error, exists}, State};
    _ -> 
      {ok, RTMPT} = rtmp_sup:start_rtmpt(SessionID, IP),
      ets:insert(Sessions, {{SessionID, IP}, RTMPT}),
      {reply, {ok, RTMPT}, State}
  end;
  

handle_call({open, SessionID, IP}, _From, #rtmpt_sessions{sessions = Sessions} = State) ->
  case ets:lookup(Sessions, {SessionID, IP}) of
    [{Key, RTMPT}] -> {reply, {ok, RTMPT}, State};
    _ -> {reply, {error, notfound}, State}
  end;

handle_call(Request, _From, State) ->
 {stop, {unknown_call, Request}, State}.

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

handle_info({'EXIT', RTMPT, _Reason}, #rtmpt_sessions{sessions = Sessions} = State) ->
  ets:match_delete(Sessions, {'_', RTMPT}),
  io:format("[RTMPT] stopped rtmpt session: ~p", [RTMPT]),
  {noreply, State};
  

handle_info(_Info, State) ->
  io:format("[RTMPT] Unknown message: ~p", [_Info]),
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

