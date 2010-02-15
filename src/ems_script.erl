%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Supervisor module
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
-module(ems_script).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).


%% External API
-export([start_link/0, reload/0, stop/0, call/3, send/1, port/0]).

-export([rtmp_method_missing/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/ems.hrl").
-include_lib("erlyvideo/include/rtmp_session.hrl").
% -export([rtmp_method_missing/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(ems_script, {
  path,
  state = starting,
  port
}).


reload() ->
  gen_server:cast(?MODULE, reload).

stop() ->
  gen_server:cast(?MODULE, stop).

call(M, F, A) ->
  gen_server:call(?MODULE, {call, M, F, A}).
  
send(Message) ->
  gen_server:cast(?MODULE, {send, Message}).

port() ->
  gen_server:call(?MODULE, port).

rtmp_method_missing(#rtmp_session{host = Host, player_info = PlayerInfo} = Session, #rtmp_funcall{args = [_ | Args], command = Command} = AMF) ->
  case gen_server:call(?MODULE, {call, application, Command, Args}) of
    {ok, {bert, nil}} -> 
      Session;
    {ok, Reply} -> 
      ?D({"Returning", Reply}),
      rtmp_session:reply(Session, AMF#rtmp_funcall{args = [Reply]}),
      Session;
    {error, {server, 0, _, _, _}} ->
      unhandled;
    {error, Reason} ->
      Dump = lists:flatten(io_lib:format("~p", [Reason])),
      rtmp_session:fail(Session, AMF#rtmp_funcall{args = Dump}),
      Session
  end.    
      

  % rtmp_session:accept_connection(NewState, AMF),

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
  Path = "priv/script.rb",
  process_flag(trap_exit, true),
  self() ! init_script,
  {ok, #ems_script{path = Path}}.



open_script(#ems_script{path = Path} = State) ->
  Port = open_port({spawn, Path}, [{packet, 4}, nouse_stdio, exit_status, binary]),
  ?D({"Spawned external script", Path, Port}),
  State#ems_script{port = Port, state = started}.

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

handle_call({call, _M, _F, _A} = Command, _From, #ems_script{port = Port} = State) when is_port(Port) ->
  port_command(Port, term_to_binary(Command)),
  receive
    {Port, {data, Data}} ->
      Response = case binary_to_term(Data) of
        {reply, Reply} -> {ok, Reply};
        Else -> Else
      end,
      {reply, Response, State}
    after 10000 ->
      {reply, timeout, State}
  end;

handle_call(port, _, #ems_script{port = Port} = State) ->
  {reply, {ok, Port}, State};

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
handle_cast(reload, #ems_script{port = Port} = State) ->
  port_close(Port),
  {noreply, open_script(State)};

handle_cast(stop, #ems_script{port = Port} = State) ->
  port_close(Port),
  {noreply, State#ems_script{state = stopped}};

handle_cast({send, Message}, #ems_script{port = Port} = State) ->
  ?D({"Sending message", Message}),
  Port ! Message,
  {noreply, State};

handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

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
handle_info({'EXIT', Port, _}, #ems_script{port = Port, state = started} = Server) ->
  ?D({"Script failed, restarting"}),
  timer:send_after(1000, self(), init_script),
  {noreply, Server};

handle_info({'EXIT', Port, _}, #ems_script{port = Port, state = stopped} = Server) ->
  {noreply, Server};
  
handle_info(init_script, State) ->
  {noreply, open_script(State)};

handle_info(_Info, State) ->
  ?D({"Unknown script message", _Info}),
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
