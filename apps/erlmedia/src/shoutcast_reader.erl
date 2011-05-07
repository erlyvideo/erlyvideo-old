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

-include("../include/video_frame.hrl").
-include("../include/aac.hrl").
-include("log.hrl").

-record(shoutcast, {
  consumer,
  audio_config = undefined,
  state,
  url,
  socket,
  options,
  sync_count = 0,
  format = aac,
  buffer = <<>>,
  timestamp,
  sample_rate = 44.1,
  headers = [],
  byte_counter = 0
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% AAC+ example
% {ok, Pid1} = ems_sup:start_shoutcast_media("http://91.121.132.237:8052/").
% MP3 example
% {ok, Pid2} = ems_sup:start_shoutcast_media("http://205.188.215.230:8002").

start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).


init([Options]) ->
  Consumer = proplists:get_value(consumer, Options),
  URL = proplists:get_value(url, Options),

  {_, _, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {_HostPort, Path} = http_uri2:extract_path_with_query(URL),
  
  ?D({shout_connect, Host, Port, Path}),
  Timeout = proplists:get_value(timeout, Options, 3000),

  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, once}], Timeout),
  ok = gen_tcp:send(Socket, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"),

  erlang:monitor(process, Consumer),
  {ok, #shoutcast{state = request, consumer = Consumer, url = URL, socket = Socket, options = Options}}.


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
handle_call(connect, _From, State) ->
  {reply, ok, State};

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

handle_info({tcp, Socket, Bin}, #shoutcast{buffer = <<>>} = State) ->
  inet:setopts(Socket, [{active,once}]),
  {noreply, decode(State#shoutcast{buffer = Bin})};

handle_info(#video_frame{flavor = config, content = audio} = Frame, State) ->
  {noreply, send_frame(Frame, State#shoutcast{audio_config = Frame})};

handle_info({tcp, Socket, Bin}, #shoutcast{buffer = Buffer} = State) ->
  inet:setopts(Socket, [{active,once}]),
  {noreply, decode(State#shoutcast{buffer = <<Buffer/binary, Bin/binary>>})};

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

decode(#shoutcast{state = unsynced_body, format = Format, sync_count = SyncCount}) when SyncCount == 10000 ->
  erlang:error({shoutcast_unsynced, Format, SyncCount});

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
   %?D({"Decode"}),
  case aac:unpack_adts(State#shoutcast.buffer) of
    {ok, _Frame, Second} ->
      ?D({"Presync AAC"}),
      case aac:unpack_adts(Second) of
        {more, undefined} ->
          ?D({"Want more AAC for second frame"}),
          State;
        {error, unknown} ->
          ?D({"Presync failed"}),
          decode(State#shoutcast{buffer = Rest, sync_count = SyncCount + 1});
        {ok, _, _} ->
          ?D({"Synced AAC"}),
          AACConfig = aac:adts_to_config(Second),
          AudioConfig = #video_frame{       
           	content   = audio,
           	flavor    = config,
        		dts       = 0,
        		pts       = 0,
        		body      = AACConfig,
        	  codec	    = aac,
        	  sound	    = {stereo, bit16, rate44}
        	},
        	Config = aac:decode_config(AACConfig),
        	SampleRate = Config#aac_config.sample_rate / 1000,
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
   %?D({"Decode"}),
  case aac:unpack_adts(Data) of
    {ok, Packet, Rest} ->
      Frame = #video_frame{       
        content    = audio,
        dts        = Timestamp / SampleRate,
        pts        = Timestamp / SampleRate,
        body       = Packet,
        flavor     = frame,
    	  codec      = aac,
    	  sound	     = {stereo, bit16, rate44}
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

decode(#shoutcast{state = body, format = mp3, buffer = Data, timestamp = Timestamp, sample_rate = SampleRate} = State) ->
  % ?D({"Decode"}),
  case mp3:decode(Data) of
    {ok, Packet, Rest} ->
      Frame = #video_frame{       
        content = audio,
        dts     = Timestamp / SampleRate,
        pts     = Timestamp / SampleRate,
        body    = Packet,
        flavor  = frame,
    	  codec	  = mp3,
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


-include_lib("eunit/include/eunit.hrl").

shoutcast_aac_config_test () ->
  {ok,Dev} = file:open("test/files/shoutcast_aac.dmp",[read,raw,binary]),
  {ok,Data} = file:pread(Dev,0,1024),
  decode(#shoutcast{state = unsynced_body,buffer = Data, consumer = self()}),
  Guard = #video_frame{content = audio,dts = 0,pts = 0,codec = aac,flavor = config, sound = {stereo,bit16,rate44},body = <<18,16>>},
  ?assertEqual(Guard, get_frame(config)).

shoutcast_aac_frame_test () ->
  {ok,Dev} = file:open("test/files/shoutcast_aac.dmp",[read,raw,binary]),
  {ok,Data} = file:pread(Dev,0,1024),
  decode(#shoutcast{state = unsynced_body,buffer = Data, consumer = self()}),
  Frame = get_frame(frame),
  ?assertMatch(<<33,_/binary>>, Frame#video_frame.body).

get_frame(Match) ->
  receive 
   #video_frame{flavor = Match} = Frame -> Frame;
   #video_frame{} -> get_frame(Match)
  end.  

count_frame_aac_test () ->
  TrueCount = 142,
  {ok,Dev} = file:open("test/files/shoutcast_aac.dmp",[read,raw,binary]),
  {ok,Data} = file:pread(Dev,0,1024),
  decode(#shoutcast{state = unsynced_body,buffer = Data, consumer = self()}),
  CurCount = count_frame(0),
  ?assertEqual(TrueCount,CurCount).

dts_play_test () ->
  {ok,Dev} = file:open("test/files/shoutcast_aac.dmp",[read,raw,binary]),
  {ok,Data} = file:pread(Dev,0,1024),
  decode(#shoutcast{state = unsynced_body,buffer = Data, consumer = self()}),
  Frames = aac_frame([]),
  delta_dts(Frames).

delta_dts([]) ->
  ok;

delta_dts([#video_frame{}]) ->
  ok;

delta_dts([Frame|Frames]) ->
  [NextFrame|_] = Frames,
  Delta = NextFrame#video_frame.dts - Frame#video_frame.dts,
  ?D(Delta),
  true = Delta < 24 andalso Delta > 22,
  delta_dts(Frames).
  
aac_frame(Frames) ->
  receive 
   #video_frame{flavor = frame} = Frame -> aac_frame([Frame|Frames]);
   _Else -> aac_frame(Frames)
   after 20 -> lists:reverse(Frames)
  end.
  

count_frame (Count) ->
  receive 
    #video_frame{} -> count_frame(Count+1)
    after 20 -> Count
  end.
