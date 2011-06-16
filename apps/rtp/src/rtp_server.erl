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

-include("rtp.hrl").
-include("log.hrl").

%% External API
-export([start_link/1]).
-export([
         play/2,
         stop/1,
         media_info_loc/1,
         listen_ports/3,
         add_stream/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).



play(RTP, Fun) ->
  ?DBG("Play: ~p, ~p", [RTP, Fun]),
  gen_server:call(RTP, {play, Fun}).

stop(RTP) ->
  ?DBG("Stop: ~p", [RTP]),
  gen_server:call(RTP, {stop}).

media_info_loc(RTP) ->
  gen_server:call(RTP, media_info_loc).

listen_ports(RTP, Content, Transport) ->
  gen_server:call(RTP, {listen_ports, Content, Transport}).

add_stream(RTP, Location, Stream)
  when Location =:= local orelse
       Location =:= remote ->
  gen_server:call(RTP, {add_stream, Location, Stream}).

-record(rtp_server, {
  consumer,

  media_info_loc,
  rtp_loc,
  chan_loc = [],

  media_info_rmt,
  rtp_rmt,
  chan_rmt,

  transcoder_in,
  transcoder_out,

  udp_conn,

  file
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
  MediaLoc = proplists:get_value(media_info_loc, Options),
  RTP_LOC = if MediaLoc == undefined -> undefined; true -> rtp:init(local, MediaLoc) end,
  MediaRmt = proplists:get_value(media_info_rmt, Options),
  RTP_RMT = if MediaRmt == undefined -> undefined; true -> rtp:init(remote, MediaRmt) end,

  ?DBG("RTP State Init:~nLOC:~n~p~nRMT:~n~p", [MediaLoc, MediaRmt]),

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

  {ok, TranscoderIn} = TrCreateFun(transcode_in),
  {ok, TranscoderOut} = TrCreateFun(transcode_out),

  ?DBG("TranscoderIn: ~p~nTranscoderOut: ~p", [TranscoderIn, TranscoderOut]),

  %%{ok, File} = file:open("/tmp/rtp.wav", [append]),

  {ok, #rtp_server{
    media_info_loc = MediaLoc,
    rtp_loc = RTP_LOC,
    media_info_rmt = MediaRmt,
    rtp_rmt = RTP_RMT,
    consumer = Consumer,
    transcoder_in = TranscoderIn,
    transcoder_out = TranscoderOut
    %%file = File
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
  Res = Fun(),
  ?DBG("Play Fun: ~p", [Res]),
  {reply, ok, Server};

handle_call({stop}, _From, #rtp_server{} = Server) ->
  {stop, normal, ok, Server};

handle_call(media_info_loc, _From, #rtp_server{media_info_loc = MediaInfo} = Server) ->
  ?DBG("RTP State:~n~p", [Server]),
  {reply, MediaInfo, Server};

handle_call({listen_ports, Content, Transport},
            _From, #rtp_server{rtp_loc = RTP_LOC,
                               chan_loc = ChanOpts,
                               media_info_loc = MediaLoc} = Server) ->
  ?DBG("Listen ports: ~p, ~p", [Content, Transport]),
  StreamId = stream_id(Content),
  {ok, NewRTP_LOC, NewChanOpts} = rtp:setup_channel(RTP_LOC, StreamId, Transport),
  UDPConn = NewRTP_LOC#rtp_state.udp,
  ?DBG("UDPConn: ~p", [UDPConn]),
  PortRTP = proplists:get_value(local_rtp_port, NewChanOpts),
  PortRTCP = proplists:get_value(local_rtcp_port, NewChanOpts),

  MediaLoc1 =
    case Content of
      audio ->
        #media_info{audio = [#stream_info{options = StreamOpts} = StreamInfo], options = _SessOpts} = MediaLoc,
        StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, PortRTP})},
        MediaLoc#media_info{audio = [StreamInfo1]};
      video ->
        #media_info{video = [#stream_info{options = StreamOpts} = StreamInfo], options = _SessOpts} = MediaLoc,
        StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, PortRTP})},
        MediaLoc#media_info{video = [StreamInfo1]}
    end,


  ?DBG("RTP State Listen:~nNewRTP_LOC:~n~p~nOpts:~n~p", [NewRTP_LOC, NewChanOpts]),
  {reply, {ok, {PortRTP, PortRTCP}},
   Server#rtp_server{media_info_loc = MediaLoc1,
                     rtp_loc = NewRTP_LOC,
                     chan_loc = lists:keystore(Content, 1, ChanOpts, {Content, NewChanOpts}),
                     udp_conn = UDPConn}};

handle_call({add_stream, Location,
             #media_info{audio = _Audio,
                         video = _Video} = MediaInfo}, _From,
            #rtp_server{udp_conn = UDPConn} = Server)
  when Location =:= local orelse
       Location =:= remote ->

  case Location of
    local ->
      RTP_LOC = rtp:init(local, MediaInfo),
      %%{ok, RTP_LOC1, _} = rtp:setup_channel(RTP_LOC, 1, [{proto,udp},{remote_rtp_port,RPort1},{remote_rtcp_port,RPort2},{remote_addr,RAddr}]),
      Transport = [{proto,udp}],


      {NewRTP_LOC, ChanOpts} =
        lists:foldl(fun(C, {RL, O}) ->
                        SId = stream_id(C),
                        {ok, RTP_LOC1, CO} = rtp:setup_channel(RL, SId, Transport),
                        {RTP_LOC1, lists:keystore(SId, 1, O, {SId, CO})}
                    end, {RTP_LOC, []}, [audio
                                         %%, video
                                        ]),


      %% {ok, RTP_LOC1, AudioChanOpts} = rtp:setup_channel(RTP_LOC, audio, Transport),
      %% {ok, RTP_LOC2, VideoChanOpts} = rtp:setup_channel(RTP_LOC1, video, Transport),

      NewServer =
        Server#rtp_server{
          media_info_loc = MediaInfo,
          rtp_loc = NewRTP_LOC,
          chan_loc = ChanOpts
         };
    remote ->
      RTP_RMT = rtp:init(remote, MediaInfo),


      {NewRTP_RMT, ChanOpts, NewMediaInfo} =
        lists:foldl(fun(C, {RR, O, Mi}) ->
                        case C of
                          audio ->
                            #media_info{audio = [#stream_info{options = StreamOpts} = StreamInfo], options = SessOpts} = Mi;
                          video ->
                            #media_info{video = [#stream_info{options = StreamOpts} = StreamInfo], options = SessOpts} = Mi
                        end,
                        RPort1 = proplists:get_value(port, StreamOpts),
                        RPort2 = RPort1 + 1,
                        RAddr = proplists:get_value(remote_addr, SessOpts),
                        ?DBG("RTP Net: ~p, ~p, ~p", [RAddr, RPort1, RPort2]),
                        Transport = [{proto,udp},
                                     {remote_rtp_port,RPort1},
                                     {remote_rtcp_port,RPort2},
                                     {remote_addr,RAddr}],
                        %%Transport = [{proto,udp}],
                        SId = stream_id(C),
                        {ok, RTP_RMT1, CO} = rtp:setup_channel(RR#rtp_state{udp = UDPConn}, SId, Transport),
                        StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, RPort1})},
                        NewMi = Mi#media_info{audio = [StreamInfo1]},

                        {RTP_RMT1, lists:keystore(SId, 1, O, {SId, CO}), NewMi}
                    end, {RTP_RMT, [], MediaInfo}, [audio
                                         %%, video
                                        ]),

      ?DBG("RTP State: NewRTP_RMT:~n~p~nChanOpts:~n~p", [NewRTP_RMT, ChanOpts]),

      %% FIXME: Store remote ports in local state
      RTP_LOC = Server#rtp_server.rtp_loc,
      {UDPRmt, undefined} = NewRTP_RMT#rtp_state.udp,
      {UDPLoc, undefined} = RTP_LOC#rtp_state.udp,
      UDPLoc1 = UDPLoc#rtp_udp{
                  remote_rtp_port = UDPRmt#rtp_udp.remote_rtp_port,
                  remote_rtcp_port = UDPRmt#rtp_udp.remote_rtcp_port,
                  remote_addr = UDPRmt#rtp_udp.remote_addr
                 },
      NewRTP_LOC = RTP_LOC#rtp_state{udp = {UDPLoc1, undefined}},
      ?DBG("RTP State: NewRTP_LOC:~n~p", [NewRTP_LOC]),

      %% [{local_rtp_port, LPort1}, {local_rtcp_port, _LPort2}, {local_addr, _LAddr}] = Reply,
      %% StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, RPort1})},
      %% MediaInfo1 = MediaInfo#media_info{audio = [StreamInfo1]},

      NewServer =
        Server#rtp_server{
          media_info_rmt = NewMediaInfo,
          rtp_rmt = NewRTP_RMT,
          rtp_loc = NewRTP_LOC
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
            #rtp_server{rtp_loc = RTPState,
                        consumer = Consumer,
                        transcoder_in = Transcoder} = State) ->
  %%?DBG("RTP Handle Data: ~p, ~p,~n~p~n~p", [Addr, Port, RTPState, Bin]),
  {ok, NewRTPState, Frames} = rtp:handle_data(RTPState, {Addr, Port}, Bin),
  %%?DBG("Frames:~n~p", [Frames]),
  NewTranscoder = transcode_in(Consumer, Frames, Transcoder),
  %%[Consumer ! transcode(Frame, Transcoder) || Frame <- Frames],
  {noreply, State#rtp_server{rtp_loc = NewRTPState, transcoder_in = NewTranscoder}};

handle_info(#video_frame{content = audio} = Frame,
            #rtp_server{rtp_rmt = RTPState,
                        transcoder_out = Transcoder,
                        file = _File} = State) ->
  %% {#video_frame{body = <<Body:160/binary,_/binary>>} = NewFrame, NewTranscoder} =
  %%   transcode_out(Frame, Transcoder),

  {#video_frame{body = Body} = NewFrame, NewTranscoder} =
    transcode_out(Frame, Transcoder),
  Bodies = split_pack(Body),
  NewRTPState =
    lists:foldl(fun(B, St) ->
                    {ok, NewSt} = rtp:handle_frame(St, NewFrame#video_frame{body = B}),
                    NewSt
                end, RTPState, Bodies),

  %%?DBG("Transcoder: ~p~nFrameIn:~n~p~nFrameOut:~n~p", [Transcoder, Frame, NewFrame]),
  %%file:write(File, NewFrame#video_frame.body),
  %%{ok, NewRTPState} = rtp:handle_frame(RTPState, NewFrame#video_frame{body = NewBody}),
  {noreply, State#rtp_server{rtp_rmt = NewRTPState,
                             transcoder_out = NewTranscoder}};

handle_info(#video_frame{content = video} = _Frame,
            #rtp_server{rtp_rmt = _RTPState} = State) ->
  %%?DBG("V:~n~p", [Frame]),
  %%rtp:handle_frame(RTPState, Frame),
  {noreply, State};

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
  ?DBG("To ~p:~n~p", [Consumer, Frames]),
  [Consumer ! Frame || Frame <- Frames];
transcode_in(_, [], Transcoder) ->
  Transcoder;
transcode_in(Consumer, [H|Tail], Transcoder) ->
  {ok, TrFrame, NewTranscoder} = ems_sound:transcode(H, Transcoder),
  %%?DBG("Transcoder: ~p~nFrameIn:~n~p~nFrameOut:~n~p", [Transcoder, H, TrFrame]),
  Consumer ! TrFrame,
  transcode_in(Consumer, Tail, NewTranscoder).

transcode_out(Frame, undefined) ->
  {Frame, undefined};
transcode_out(Frame, Transcoder) ->
  {ok, NewFrame, NewTranscoder} = ems_sound:transcode(Frame, Transcoder),
  {NewFrame, NewTranscoder}.

split_pack(Data) ->
  split_pack(Data, 160).

split_pack(Data, Size) ->
  split_pack(Data, Size, []).

split_pack(Data, Size, Acc) when size(Data) >= Size ->
  <<Head:Size/binary, Tail/binary>> = Data,
  split_pack(Tail, Size, [Head | Acc]);
split_pack(Data, _Size, []) ->
  [Data];
split_pack(Data, _Size, Acc) ->
  lists:reverse([Data | Acc]).

stream_id(audio) -> 1;
stream_id(video) -> 2.

