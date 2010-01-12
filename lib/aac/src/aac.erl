-module(aac).
-author(max@maxidoors.ru).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(aac_config, {
  type,
  frequency,
  channels,
  samples_per_frame
}).

-export([decode_config/1, decode/1, config/3, config/1]).


%
%     0         1             2           3         4           5         6
% ||__sync__||sync|___e||pr|rate|_|c||hn|____|fr||ame_len_||gth|_____||______|cn||
%
  
decode(<<16#FFF:12, _ID:1, _Layer:2, 1:1, _Profile:2, SampleRate:4,
         _Private:1, Channels:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
         FrameLength:13, _ADTS:11, _Count:2, Frame:FrameLength/binary, Rest/binary>>) ->
  ?D({noerr, _ID, _Layer, _Profile, frequency(SampleRate), _Private, channels(Channels), _Original, _Home, _Copyright, _CopyrightStart, FrameLength, _Count}),
  {ok, Frame, Rest};

decode(<<16#FFF:12, _ID:1, _Layer:2, 0:1, _Profile:2, SampleRate:4,
           _Private:1, Channels:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
           FrameLength:13, _ADTS:11, _Count:2, _CRC32:16, Frame:FrameLength/binary, Rest/binary>>) ->
  ?D({err, _ID, _Layer, _Profile, frequency(SampleRate), _Private, channels(Channels), _Original, _Home, _Copyright, _CopyrightStart, FrameLength, _Count}),
  {ok, Frame, Rest};


decode(<<16#FFF:12, _:4, _/binary>>) ->
  {more, undefined};

decode(<<16#FF>>) ->
  {more, undefined};
  
decode(<<>>) ->
  {more, undefined};
  
decode(_) ->
  {error, unknown}.

decode_config(<<ObjectType:5, 2#1111:4, FrequencyIndex:24, ChannelConfig:4, FrameLength:1, _DependsCore:1, _Extension:1, _/binary>>) ->
  unpack_config(ObjectType, FrequencyIndex, ChannelConfig, FrameLength);
decode_config(<<ObjectType:5, FrequencyIndex:4, ChannelConfig:4, FrameLength:1, _DependsCore:1, _Extension:1, _/binary>>) ->
  unpack_config(ObjectType, FrequencyIndex, ChannelConfig, FrameLength).
  
unpack_config(ObjectType, Frequency, ChannelConfig, FrameLength) ->
  #aac_config{samples_per_frame = samples_per_frame(FrameLength), type = object_type(ObjectType), frequency = frequency(Frequency), channels = channels(ChannelConfig)}.


config(<<16#FFF:12, _ID:1, _Layer:2, _:1, _Profile:2, SampleRate:4,
           _Private:1, Channels:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
           FrameLength:13, _ADTS:11, _Count:2, _/binary>>) ->
  config(_Profile, SampleRate,Channels).


config(Profile, Frequency, Channels) ->
  _DependsCore = 1,
  _Extension = 0,
  _Samples = 0,
  <<Profile:5, Frequency:4, Channels:4, _Samples:1, _DependsCore:1, _Extension:1>>.

samples_per_frame(0) -> 1024;
samples_per_frame(1) -> 960.

object_type(0) -> null;
object_type(1) -> aac_main;
object_type(2) -> aac_lc;
object_type(3) -> aac_ssr;
object_type(4) -> aac_ltp;
object_type(5) -> aac_sbr;
object_type(Other) -> Other.

frequency(0) -> 96000;
frequency(1) -> 88200;
frequency(2) -> 64000;
frequency(3) -> 48000;
frequency(4) -> 44100;
frequency(5) -> 32000;
frequency(6) -> 24000;
frequency(7) -> 22050;
frequency(8) -> 16000;
frequency(9) -> 12000;
frequency(10) -> 11025;
frequency(11) -> 8000;
frequency(12) -> 7350;
frequency(Other) -> Other.

channels(0) -> specific;
channels(1) -> fc;
channels(2) -> flr;
channels(3) -> flcr;
channels(4) -> flcr_bc;
channels(5) -> flcr_blr;
channels(6) -> flcr_blr_lfe;
channels(7) -> flcr_slr_blr_lfe;
channels(Other) -> Other.
