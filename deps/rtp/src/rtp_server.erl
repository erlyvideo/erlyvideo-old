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
         media_info_out/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).



play(RTP, Fun) ->
  gen_server:call(RTP, {play, Fun}).

media_info_out(RTP) ->
  gen_server:call(RTP, media_info_out).


-record(rtp_server, {
  consumer,
  media_info_in,
  rtp_in,
  media_info_out,
  rtp_out
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
  MediaIn = proplists:get_value(media_info_in, Options),
  RTP_IN = rtp:init(in, MediaIn),

  MediaOut = #media_info{audio = [#stream_info{options = StreamOpts} = StreamInfo], options = SessOpts} = proplists:get_value(media_info_out, Options),
  RTP_OUT = rtp:init(out, MediaOut),
  RPort1 = proplists:get_value(port, StreamOpts),
  RPort2 = RPort1 + 1,
  RAddr = proplists:get_value(remote_addr, SessOpts),
  {ok, RTP_IN1, _} = rtp:setup_channel(RTP_IN, 1, [{proto,udp},{remote_rtp_port,RPort1},{remote_rtcp_port,RPort2},{remote_addr,RAddr}]),

  {ok, RTP_OUT1, Reply} = rtp:setup_channel(RTP_OUT, 1, [{proto,udp},{remote_rtp_port,RPort1},{remote_rtcp_port,RPort2},{remote_addr,RAddr}]),
  [{local_rtp_port, LPort1}, {local_rtcp_port, _LPort2}, {local_addr, _LAddr}] = Reply,
  StreamInfo1 = StreamInfo#stream_info{options = lists:keystore(port, 1, StreamOpts, {port, LPort1})},
  MediaOut1 = MediaOut#media_info{audio = [StreamInfo1]},

  Consumer = proplists:get_value(consumer, Options),
  erlang:monitor(process, Consumer),

  {ok, #rtp_server{
    media_info_in = MediaIn,
    rtp_in = RTP_IN1,
    media_info_out = MediaOut1,
    rtp_out = RTP_OUT1,
    consumer = Consumer
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

handle_call(media_info_out, _From, #rtp_server{media_info_out = MediaInfo} = Server) ->
  {reply, MediaInfo, Server};

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
            #rtp_server{rtp_in = RTPState,
                        consumer = Consumer} = State) ->
  {ok, NewRTPState, Frames} = rtp:handle_data(RTPState, {Addr, Port}, Bin),
  %%?DBG("Frames:~n~p", [Frames]),
  [Consumer ! Frame || Frame <- Frames],
  {noreply, State#rtp_server{rtp_in = NewRTPState}};

handle_info(#video_frame{} = Frame, #rtp_server{rtp_out = RTPState} = State) ->
  {ok, NewRTPState} = rtp:handle_frame(RTPState, Frame),
  {noreply, State#rtp_server{rtp_out = NewRTPState}};

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
