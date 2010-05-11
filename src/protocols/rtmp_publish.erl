%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Example of gen_server
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%% @hidden
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
-module(rtmp_publish).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/flv.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").


%% External API
-export([start_link/2]).

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
  counter = 0
}).

start_link(Path, URL) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Path, URL], []).




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


init([Path, URL]) ->
	{ok, File} = file:open(Path, [read, binary, {read_ahead, 100000}, raw]),
	{#flv_header{} = _Header, Offset} = flv:read_header(File),
	{rtmp, _UserInfo, Host, Port, [$/ | ServerPath], _Query} = http_uri2:parse(URL),
	self() ! tick,
	Publisher = #publisher{path = Path, file = File, offset = Offset, url = URL, host = Host, port = Port, server_path = ServerPath},
	{_Frame, Publisher1} = read_frame(Publisher),
	
	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}]),
  {ok, RTMP} = rtmp_socket:connect(Socket),
  io:format("Connecting to ~s~n", [URL]),
  
  {ok, Publisher1#publisher{socket = Socket, rtmp = RTMP, counter = 1}}.


read_frame(#publisher{file = File, offset = Offset} = Publisher) ->
  case flv:read_tag(File, Offset) of
    #flv_tag{next_tag_offset = NextOffset} = Tag ->
		  Frame = flv_video_frame:tag_to_video_frame(Tag),
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
handle_info({rtmp, RTMP, connected}, #publisher{server_path = Path} = Server) ->
  rtmp_socket:setopts(RTMP, [{active, true}]),
  rtmp_lib:connect(RTMP, [{app, <<"live">>}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
  Stream = rtmp_lib:createStream(RTMP),
  rtmp_lib:publish(RTMP, Stream, Path),
  self() ! timeout,
  io:format("Connected, publishing to ~s~n", [Path]),
  {noreply, Server#publisher{stream = Stream}};

handle_info(timeout, #publisher{frame = Frame1, stream = Stream, rtmp = RTMP, counter = Counter} = Server) ->
  Message = rtmp_message(Frame1, Stream),
	rtmp_socket:send(RTMP, Message),
  
  {Frame2, Server2} = read_frame(Server),
  
  Timeout = Frame2#video_frame.dts - Frame1#video_frame.dts,
  
  case Counter rem 1000 of
    0 -> io:format("Publishing second ~p~n", [round(Frame1#video_frame.dts/1000)]);
    _ -> ok
  end,
  {noreply, Server2#publisher{counter = Counter + 1}, Timeout};

handle_info(_Info, State) ->
  {noreply, State}.

rtmp_message(#video_frame{dts = DTS, type = Type} = Frame, StreamId) ->
  #rtmp_message{
    channel_id = channel_id(Frame), 
    timestamp = DTS,
    type = Type,
    stream_id = StreamId,
    body = flv_video_frame:encode(Frame)}.
  

channel_id(#video_frame{type = metadata}) -> 4;
channel_id(#video_frame{type = audio}) -> 5;
channel_id(#video_frame{type = video}) -> 6.

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
