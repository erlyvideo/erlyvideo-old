-module(rtsp_connection).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-include_lib("ertsp/include/rtsp.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

% -include("../include/rtsp.hrl").
% -include("../../../include/ems.hrl").
% -define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
% -define(TIMEOUT, 1000).

-export([start_link/1, set_socket/2]).

%% rtsp_socket callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(rtsp_connection, {
  socket,
  rtsp,
  
  media,
  callback,
  frames = [],
  addr,
  port
}).


start_link(Callback) ->
  gen_server:start_link(?MODULE, [Callback], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  gen_tcp:controlling_process(Socket, Pid),
  gen_server:cast(Pid, {socket_ready, Socket}).


%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([Callback]) ->
  random:seed(now()),
  {ok, #rtsp_connection{callback = Callback}}.



%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_cast({socket_ready, Socket}, State) ->
  {ok, {IP, Port}} = inet:peername(Socket),
  {ok, RTSP} = rtsp_socket:start_link(),
  erlang:monitor(process, RTSP),
  rtsp_socket:accept(RTSP, Socket, self()),
  {noreply, State#rtsp_connection{socket=Socket, addr=IP, port = Port, rtsp = RTSP}}.


handle_info({'DOWN', _, process, RTSP, _Reason}, #rtsp_connection{rtsp = RTSP} = State) ->
  {stop, normal, State};

handle_info(#video_frame{} = Frame, #rtsp_connection{frames = Frames} = State) ->
  {noreply, State#rtsp_connection{frames = Frames ++ [Frame]}};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info(timeout, State) ->
  {stop, normal, State}.


handle_call({record, URL}, _From, #rtsp_connection{callback = Callback, frames = Frames} = State) ->
  {ok, Media} = Callback:record(URL),
  lists:foreach(fun(Frame) -> 
    Media ! Frame
  end, Frames),
  {reply, {ok, Media}, State#rtsp_connection{frames = []}};

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ?D({"RTSP stopping"}),
  ok.


%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



  

