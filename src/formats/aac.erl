-module(aac).
-author(max@maxidoors.ru).

-record(aac_config, {
  type,
  frequency,
  channels,
  samples_per_frame
}).

-export([decode_config/1]).

decode_config(<<ObjectType:5, 2#1111:4, FrequencyIndex:24, ChannelConfig:4, FrameLength:1, _DependsCore:1, _Extension:1, _/binary>>) ->
  unpack_config(ObjectType, FrequencyIndex, ChannelConfig, FrameLength);
decode_config(<<ObjectType:5, FrequencyIndex:4, ChannelConfig:4, FrameLength:1, _DependsCore:1, _Extension:1, _/binary>>) ->
  unpack_config(ObjectType, FrequencyIndex, ChannelConfig, FrameLength).
  
unpack_config(ObjectType, Frequency, ChannelConfig, FrameLength) ->
  #aac_config{samples_per_frame = samples_per_frame(FrameLength), type = object_type(ObjectType), frequency = frequency(Frequency), channels = channels(ChannelConfig)}.


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
