%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Special module, that can publish FLV file to erlyvideo. 
%%% Required only for console tool ``contrib/rtmp_publish''
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%% @hidden
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
-module(rtmp_publish).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/flv.hrl").
-include_lib("rtmp/include/rtmp.hrl").


%% External API
-export([start_link/2, start_link/3, publish/3, continue_publish/1, wait_for_end/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(publisher, {
  path,
  file,
  offset = 0,
  url,
  host,
  port,
  server_path,
  socket,
  rtmp,
  frame,
  stream,
  counter = 0,
  no_timeout = false
}).


publish(Path, URL, Options) ->
  start_link(Path, URL, Options).
    
continue_publish(Publisher) ->
  Publisher ! timeout.  

wait_for_end(Pid) ->
  erlang:monitor(process, Pid),
  receive
    {'DOWN', _Ref, process, Pid, normal} -> ok;
    {'DOWN', _Ref, process, Pid, Reason} -> {error, Reason}
  end.

start_link(Path, URL) ->
  start_link(Path, URL, []).

start_link(Path, URL, Options) ->
  gen_server:start_link(?MODULE, [Path, URL, Options], []).




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


init([Path, URL, Options]) ->
	{ok, File} = file:open(Path, [read, binary, {read_ahead, 100000}, raw]),
	{#flv_header{} = _Header, Offset} = flv:read_header(File),
	{rtmp, _UserInfo, Host, Port, [$/ | ServerPath], _Query} = http_uri2:parse(URL),
	Publisher = #publisher{path = Path, file = File, offset = Offset, url = URL, host = Host, port = Port, server_path = ServerPath},
	
	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}]),
  {ok, RTMP} = rtmp_socket:connect(Socket),
  io:format("Connecting to ~s~n", [URL]),
  
  {ok, Publisher#publisher{socket = Socket, rtmp = RTMP, counter = 1, no_timeout = proplists:get_value(no_timeout, Options, false)}}.


read_frame(#publisher{file = File, offset = Offset} = Publisher) ->
  case flv:read_frame(File, Offset) of
    #video_frame{next_id = NextOffset} = Frame ->
		  {Frame, Publisher#publisher{frame = Frame, offset = NextOffset}};
    eof -> eof;
    {error, Reason} -> {error, Reason}
  end.

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
handle_info({rtmp, RTMP, connected}, #publisher{server_path = Path} = Publisher) ->
  rtmp_socket:setopts(RTMP, [{active, true}]),
  rtmp_lib:connect(RTMP, [{app, <<"live">>}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
  Stream = rtmp_lib:createStream(RTMP),
  rtmp_lib:publish(RTMP, Stream, Path),
  {Frame1, Publisher1} = read_frame(Publisher),
  {Frame2, Publisher2} = read_frame(Publisher1),
  {Frame3, Publisher3} = read_frame(Publisher2),
  {_Frame, Publisher4} = read_frame(Publisher3),
  [send_frame(RTMP, Stream, Frame) || Frame <- [Frame1, Frame2, Frame3]],
  io:format("Connected, publishing to ~s (~p)~n", [Path, Stream]),
  {noreply, Publisher4#publisher{stream = Stream}};

handle_info(timeout, #publisher{frame = Frame1, stream = Stream, rtmp = RTMP, counter = Counter, no_timeout = NoTimeout} = Server) ->
  send_frame(RTMP, Stream, Frame1),
  
  case read_frame(Server) of
    {Frame2, Server2} ->
      Timeout = case NoTimeout of
        true -> 0;
        _ -> Frame2#video_frame.dts - Frame1#video_frame.dts
      end,
  
      case Counter rem 1000 of
        0 -> io:format("Publishing second ~p~n", [round(Frame1#video_frame.dts/1000)]);
        _ -> ok
      end,
      {noreply, Server2#publisher{counter = Counter + 1}, Timeout};
    eof ->
      % io:format("Stopping on ~p~n", [Server#publisher.offset]),
      {stop, normal, Server}
  end;

handle_info({rtmp, _Socket, disconnect}, State) ->
  {stop, normal, State};

handle_info({rtmp, _Socket, _Message}, State) ->
  io:format("RMTP: ~p~n", [_Message]),
  self() ! timeout,
  {noreply, State};

handle_info(_Info, State) ->
  {stop, {unknown_message,_Info}, State}.

rtmp_message(#video_frame{dts = DTS, content = Type} = Frame, StreamId) ->
  #rtmp_message{
    channel_id = channel_id(Frame), 
    timestamp = DTS,
    type = Type,
    stream_id = StreamId,
    body = flv_video_frame:encode(Frame)}.

send_frame(RTMP, Stream, Frame) ->
  Message = rtmp_message(Frame, Stream),
	rtmp_socket:send(RTMP, Message).
  

channel_id(#video_frame{content = metadata}) -> 4;
channel_id(#video_frame{content = audio}) -> 5;
channel_id(#video_frame{content = video}) -> 6.

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
