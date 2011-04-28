%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        RTP standalone server for single RTP channel
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
-module(rtp_server).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-include("log.hrl").

%% External API
-export([start_link/1]).
-export([
         play/2,
         media_info_loc/1,
         listen_ports/3,
         add_stream/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).



play(RTP, Fun) ->
  gen_server:call(RTP, {play, Fun}).

media_info_loc(RTP) ->
  gen_server:call(RTP, media_info_loc).

listen_ports(RTP, StreamId, Transport) ->
  gen_server:call(RTP, {listen_ports, StreamId, Transport}).

add_stream(RTP, Location, Stream)
  when Location =:= local orelse
       Location =:= remote ->
  gen_server:call(RTP, {add_stream, Location, Stream}).


-record(rtp_server, {
  consumer,
  media_info_loc,
  rtp_loc,
  chan_loc,
  media_info_rmt,
  rtp_rmt,
  chan_rmt,
  transcoder_in,
  transcoder_out
}).

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


init([Options]) ->
  MediaIn = proplists:get_value(media_info_loc, Options),
  RTP_LOC = if MediaIn == undefined -> undefined; true -> rtp:init(in, MediaIn) end,
  MediaOut = proplists:get_value(media_info_rmt, Options),
  RTP_RMT = if MediaOut == undefined -> undefined; true -> rtp:init(out, MediaOut) end,

  ?DBG("RTP State Init:~nIN:~n~p~nOut:~n~p", [MediaIn, MediaOut]),

  %% MediaIn = proplists:get_value(media_info_in, Options),
  %% RTP_LOC = rtp:init(in, MediaIn),

  %% MediaOut = #media_info{audio = [#stream_info{options = StreamOpts} = StreamInfo], options = SessOpts} = proplists:get_value(media_info_out, Options),
  %% RTP_RMT = rtp:init(out, MediaOut),

  %% RPort1 = proplists:get_value(port, StreamOpts),
  %% RPort2 = RPort1 + 1,
  %% RAddr = proplists:get_value(remote_addr, SessOpts),

  %% {ok, RTP_LOC1, _} = rtp:setup_channel(RTP_LOC, 1, [{proto,udp},{remote_rtp_port,RPort1},{remote_rtcp_port,RPort2},{remote_addr,RAddr}]),

  %% {ok, RTP_RMT1, Reply} = rtp:setup_channel(RTP_RMT, 1, [{proto,udp},{remote_rtp_port,RPort1},{remote_rtcp_port,RPort2},{remote_addr,RAddr}]),
  %% [{local_rtp_port, LPort1}, {local_rtcp_port, _LPort2}, {local_addr, _LAddr}] = Reply,
  %% StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, LPort1})},
  %% MediaOut1 = MediaOut#media_info{audio = [StreamInfo1]},

  Consumer = proplists:get_value(consumer, Options),
  erlang:monitor(process, Consumer),

  TrCreateFun =
    fun(Key) ->
        case proplists:get_value(Key, Options) of
          {{_, _} = From, {_, _} = To} ->
            ems_sound:init([{from, From}, {to, To}]);
          _Other ->
            undefined
        end
    end,

  TranscoderIn = TrCreateFun(transcode_in),
  TranscoderOut = TrCreateFun(transcode_out),

  {ok, #rtp_server{
    media_info_loc = MediaIn,
    rtp_loc = RTP_LOC,
    media_info_rmt = MediaOut,
    rtp_rmt = RTP_RMT,
    consumer = Consumer,
    transcoder_in = TranscoderIn,
    transcoder_out = TranscoderOut
  }}.

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
handle_call({play, Fun}, _From, #rtp_server{} = Server) ->
  Fun(),
  {reply, ok, Server};

handle_call(media_info_loc, _From, #rtp_server{media_info_loc = MediaInfo} = Server) ->
  ?DBG("RTP State:~n~p", [Server]),
  {reply, MediaInfo, Server};

handle_call({listen_ports, StreamId, Transport},
            _From, #rtp_server{rtp_loc = RTP_LOC,
                               media_info_loc = MediaLoc} = Server) ->
  {ok, NewRTP_LOC, ChanOpts} = rtp:setup_channel(RTP_LOC, StreamId, Transport),
  PortRTP = proplists:get_value(local_rtp_port, ChanOpts),
  PortRTCP = proplists:get_value(local_rtcp_port, ChanOpts),

  #media_info{audio = [#stream_info{options = StreamOpts} = StreamInfo], options = _SessOpts} = MediaLoc,
  StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, PortRTP})},
  MediaLoc1 = MediaLoc#media_info{audio = [StreamInfo1]},

  ?DBG("RTP State Listen:~nNewRTP_LOC:~n~p~nOpts:~n~p", [NewRTP_LOC, ChanOpts]),
  {reply, {ok, {PortRTP, PortRTCP}},
   Server#rtp_server{media_info_loc = MediaLoc1,
                     rtp_loc = NewRTP_LOC,
                     chan_loc = ChanOpts}};

handle_call({add_stream, Location, MediaInfo}, _From,
            #rtp_server{} = Server)
  when Location =:= local orelse
       Location =:= remote ->

  case Location of
    local ->
      RTP_LOC = rtp:init(in, MediaInfo),
      %%{ok, RTP_LOC1, _} = rtp:setup_channel(RTP_LOC, 1, [{proto,udp},{remote_rtp_port,RPort1},{remote_rtcp_port,RPort2},{remote_addr,RAddr}]),
      {ok, RTP_LOC1, ChanOpts} = rtp:setup_channel(RTP_LOC, 1, [{proto,udp}]),

      NewServer =
        Server#rtp_server{
          media_info_loc = MediaInfo,
          rtp_loc = RTP_LOC1,
          chan_loc = ChanOpts
         };
    remote ->
      #media_info{audio = [#stream_info{options = StreamOpts} = _StreamInfo], options = SessOpts} = MediaInfo,
      RTP_RMT = rtp:init(out, MediaInfo),
      RPort1 = proplists:get_value(port, StreamOpts),
      RPort2 = RPort1 + 1,
      RAddr = proplists:get_value(remote_addr, SessOpts),
      ?DBG("RTP Net: ~p, ~p, ~p", [RAddr, RPort1, RPort2]),
      {ok, RTP_RMT1, ChanOpts} = rtp:setup_channel(RTP_RMT, 1,
                                                   [{proto,udp},
                                                    {remote_rtp_port,RPort1},
                                                    {remote_rtcp_port,RPort2},
                                                    {remote_addr,RAddr}]),
      ?DBG("RTP State: RTP_RMT1:~n~p~nChanOpts:~n~p", [RTP_RMT1, ChanOpts]),
      %% [{local_rtp_port, LPort1}, {local_rtcp_port, _LPort2}, {local_addr, _LAddr}] = Reply,
      %% StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, LPort1})},
      %% MediaInfo1 = MediaInfo#media_info{audio = [StreamInfo1]},

      NewServer =
        Server#rtp_server{
          media_info_rmt = MediaInfo,
          rtp_rmt = RTP_RMT1
         }
  end,


  {reply, ok, NewServer};

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
handle_info({'DOWN', _, process, Client, _Reason}, #rtp_server{consumer = Client} = Server) ->
  {stop, normal, Server};


handle_info({udp, _Socket, Addr, Port, Bin},
            #rtp_server{rtp_rmt = RTPState,
                        consumer = Consumer,
                        transcoder_in = Transcoder} = State) ->
  %%?DBG("RTP Handle Data: ~p, ~p,~n~p~n~p", [Addr, Port, RTPState, Bin]),
  {ok, NewRTPState, Frames} = rtp:handle_data(RTPState, {Addr, Port}, Bin),
  %%?DBG("Frames:~n~p", [Frames]),
  NewTranscoder = transcode_in(Consumer, Frames, Transcoder),
  %%[Consumer ! transcode(Frame, Transcoder) || Frame <- Frames],
  {noreply, State#rtp_server{rtp_loc = NewRTPState, transcoder_in = NewTranscoder}};

handle_info(#video_frame{} = Frame,
            #rtp_server{rtp_loc = RTPState,
                        transcoder_out = Transcoder} = State) ->
  {NewFrame, NewTranscoder} = transcode_out(Frame, Transcoder),
  {ok, NewRTPState} = rtp:handle_frame(RTPState, NewFrame),
  {noreply, State#rtp_server{rtp_rmt = NewRTPState,
                             transcoder_out = NewTranscoder}};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

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

%% Internal functions
transcode_in(Consumer, Frames, undefined) ->
  [Consumer ! Frame || Frame <- Frames];
transcode_in(_, [], Transcoder) ->
  Transcoder;
transcode_in(Consumer, [H|Tail], Transcoder) ->
  {ok, TrFrame, NewTranscoder} = ems_sound:transcode(H, Transcoder),
  Consumer ! TrFrame,
  transcode_in(Consumer, Tail, NewTranscoder).

transcode_out(Frame, undefined) ->
  {Frame, undefined};
transcode_out(Frame, Transcoder) ->
  {ok, NewFrame, NewTranscoder} = ems_sound:transcode(Frame, Transcoder),
  {NewFrame, NewTranscoder}.
