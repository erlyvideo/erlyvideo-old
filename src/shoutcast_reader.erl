%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Shoutcast module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-shoutcast.
%%% 
%%% erlang-shoutcast is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-shoutcast is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-shoutcast.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(shoutcast_reader).
-author('Max Lapshin <max@maxidoors.ru>').
-export([start_link/1]).
-behaviour(gen_server).

-define(D(X), ems_log:debug(3, shoutcast, "~p:~p ~p",[?MODULE, ?LINE, X])).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/aac.hrl").

-record(shoutcast, {
  consumer,
  audio_config = undefined,
  state,
  sync_count = 0,
  format = aac,
  buffer = <<>>,
  timestamp,
  sample_rate = 44100,
  headers = [],
  byte_counter = 0
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% AAC+ example
% {ok, Pid1} = ems_sup:start_shoutcast_media("http://91.121.132.237:8052/").
% MP3 example
% {ok, Pid2} = ems_sup:start_shoutcast_media("http://205.188.215.230:8002").

start_link(Consumer) ->
  gen_server:start_link(?MODULE, [Consumer], []).


init([Consumer]) ->
  erlang:monitor(process, Consumer),
  {ok, #shoutcast{state = request, consumer = Consumer}}.


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
  ?D({"Undefined call", Request, _From}),
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
  ?D({"Undefined cast", _Msg}),
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

handle_info({data, Bin}, #shoutcast{buffer = <<>>} = State) ->
  {noreply, decode(State#shoutcast{buffer = Bin})};

handle_info({data, Bin}, #shoutcast{buffer = Buffer} = State) ->
  {noreply, decode(State#shoutcast{buffer = <<Buffer/binary, Bin/binary>>})};

handle_info(#video_frame{flavor = config, content = audio} = Frame, State) ->
  {noreply, send_frame(Frame, State#shoutcast{audio_config = Frame})};

handle_info(#video_frame{} = Frame, State) ->
  {noreply, send_frame(Frame, State)};


handle_info({'DOWN', _, process, Consumer, _Reason}, #shoutcast{consumer = Consumer} = State) ->
  ?D({"Shoutcast consumer died"}),
  {stop, normal, State};

handle_info(Message, State) ->
  ?D({"Unknown message", Message, State}),
  {stop, {unhandled, Message}, State}.


decode(#shoutcast{state = request, buffer = <<"ICY 200 OK\r\n", Rest/binary>>} = State) ->
  decode(State#shoutcast{state = headers, buffer = Rest});

decode(#shoutcast{state = request, buffer = <<"HTTP/1.0 200 OK\r\n", Rest/binary>>} = State) ->
  decode(State#shoutcast{state = headers, buffer = Rest});

decode(#shoutcast{state = request, buffer = <<"HTTP/1.1 200 OK\r\n", Rest/binary>>} = State) ->
  decode(State#shoutcast{state = headers, buffer = Rest});

decode(#shoutcast{state = headers, buffer = Buffer, headers = Headers} = State) ->
  case erlang:decode_packet(httph_bin, Buffer, []) of
    {more, undefined} -> 
      State;
    {ok, {http_header, _, Name, _, Value}, Rest} ->
      ?D({Name, Value}),
      decode(State#shoutcast{headers = [{Name, Value} | Headers], buffer = Rest});
    {ok, http_eoh, Rest} ->
      decode(State#shoutcast{state = unsynced_body, format = format(State), buffer = Rest})
  end;

% decode(#shoutcast{state = metadata, buffer = <<Length, Data/binary>>} = State) when size(Data) >= Length*16 ->
%   MetadataLength = Length*16,
%   <<Metadata:MetadataLength/binary, Rest/binary>> = Data,
%   % ?D({"Metadata", Length, Metadata}),
%   decode(State#shoutcast{state = body, buffer = Rest});
% 
% decode(#shoutcast{state = metadata} = State) ->
%   State;
%

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount, format = mp3} = State) when SyncCount == 50 ->
  decode(State#shoutcast{format = mp3, sync_count = SyncCount + 1});

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount, format = aac} = State) when SyncCount == 50 ->
  decode(State#shoutcast{format = aac});

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount}) when SyncCount == 100 ->
  error;

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount, format = mp3, buffer = <<_, Rest/binary>>} = State) ->
  case mp3:decode(State#shoutcast.buffer) of
    {ok, _, _} ->
      ?D({"Sync MP3"}),
      decode(State#shoutcast{state = body, timestamp = 0});
    {more, undefined} ->
      ?D({"Want more MP3 for sync"}),
      State#shoutcast{sync_count = SyncCount + 1};
    {error, unknown} ->
      decode(State#shoutcast{buffer = Rest, sync_count = SyncCount + 1})
  end;


decode(#shoutcast{state = unsynced_body, format = aac, sync_count = SyncCount, buffer = <<_, Rest/binary>>} = State) ->
  case aac:decode(State#shoutcast.buffer) of
    {ok, _Frame, Second} ->
      ?D({"Presync AAC"}),
      case aac:decode(Second) of
        {more, undefined} ->
          ?D({"Want more AAC for second frame"}),
          State;
        {error, unknown} ->
          ?D({"Presync failed"}),
          decode(State#shoutcast{buffer = Rest, sync_count = SyncCount + 1});
        {ok, _, _} ->
          ?D({"Synced AAC"}),
          AACConfig = aac:config(Second),
          AudioConfig = #video_frame{       
           	content          = audio,
           	flavor = config,
        		dts           = 0,
        		pts           = 0,
        		body          = AACConfig,
        	  codec	    = aac,
        	  sound	  = {stereo, bit16, rate44}
        	},
        	Config = aac:decode_config(AACConfig),
        	SampleRate = Config#aac_config.frequency / 1000,
        	send_frame(AudioConfig, State),
          decode(State#shoutcast{buffer = Second, state = body, audio_config = AudioConfig, timestamp = 0, sample_rate = SampleRate})
      end;
    {more, undefined} ->
      ?D({"Want more AAC for first frame"}),
      State;
    {error, unknown} ->
      decode(State#shoutcast{buffer = Rest})
  end;

% decode(#shoutcast{state = unsynced_body, format = aac, buffer = <<16#FFF:12, _:18, Length1:13, _:13, _:Length1, 16#FFF:12, _:18, Length2:13, _:13, _:Length2, _/binary>>} = State) ->
%   {_First, Buffer} = split_binary(State#shoutcast.buffer, 7+Length1),
%   decode(State#shoutcast{state = body, buffer = Buffer});
% 

decode(#shoutcast{state = unsynced_body, buffer = <<>>} = State) ->
  State;

decode(#shoutcast{state = body, format = aac, buffer = Data, timestamp = Timestamp, sample_rate = SampleRate} = State) ->
  % ?D({"Decode"}),
  case aac:decode(Data) of
    {ok, Packet, Rest} ->
      Frame = #video_frame{       
        content          = audio,
        dts           = Timestamp / SampleRate,
        pts           = Timestamp / SampleRate,
        body          = Packet,
    	  codec      = aac,
    	  sound	  = {stereo, bit16, rate44}
      },
      send_frame(Frame, State),
      decode(State#shoutcast{buffer = Rest, timestamp = Timestamp + 1024});
    {error, unknown} -> 
      <<_, Rest/binary>> = Data,
      ?D({"sync aac"}),
      decode(State#shoutcast{buffer = Rest});
    {more, undefined} -> 
      % ?D(size(Data)),
      State
  end;

decode(#shoutcast{state = body, format = mp3, buffer = Data, timestamp = Timestamp} = State) ->
  % ?D({"Decode"}),
  case mp3:decode(Data) of
    {ok, Packet, Rest} ->
      Frame = #video_frame{       
        content          = audio,
        dts           = Timestamp,
        pts           = Timestamp,
        body          = Packet,
    	  codec	    = mp3,
    	  sound	  = {stereo, bit16, rate44}
      },
      send_frame(Frame, State),
      decode(State#shoutcast{buffer = Rest, timestamp = Timestamp + 1024});
    {error, unknown} -> 
      <<_, Rest/binary>> = Data,
      ?D({"sync mp3"}),
      decode(State#shoutcast{buffer = Rest});
    {more, undefined} -> 
      State
  end.
      
format(#shoutcast{headers = Headers}) ->
  case proplists:get_value('Content-Type', Headers) of
    <<"audio/mpeg">> -> mp3;
    <<"audio/aac">> -> aac;
    <<"audio/aacp">> -> aac
  end.



send_frame(Frame, #shoutcast{consumer = Consumer} = State) ->
  Consumer ! Frame,
  State.



%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(normal, _State) ->
  ok;
  
terminate(_Reason, _State) ->
  ?D({"Shoutcast client terminating", _Reason}),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
