%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        AAC unpacking module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlmedia.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(aac).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/aac.hrl").

-export([decode_config/1, encode_config/1, unpack_adts/1, pack_adts/2, adts_to_config/1, to_fmtp/1]).

%%--------------------------------------------------------------------
%% @spec (Body::binary(), Config::aac_config()) -> ADTS::binary()
%% @doc Packs AAC frame into ADTS frame, suitable for transmitting in MPEG-TS PES or Shoutcast
%% @end 
%%--------------------------------------------------------------------
pack_adts(Frame, #aac_config{type = ObjectType, sample_rate = Frequency, channels = ChannelConfig}) ->
  ID = 0,
  Layer = 0,
  ProtectionAbsent = 1,
  Profile = encode_object_type(ObjectType) - 1,
  SampleRate = encode_sample_rate(Frequency),
  Private = 0,
  Channels = encode_channels(ChannelConfig),
  Original = 0,
  Home = 0,
  Copyright = 0,
  CopyrightStart = 0,
  FrameLength = size(Frame) + 7,
  % ADTS = 2#10001010101,
  ADTS = 16#7ff,
  Count = 0,
  <<16#FFF:12, ID:1, Layer:2, ProtectionAbsent:1,  % byte 1-2
    Profile:2, SampleRate:4, Private:1, Channels:3,  % byte 3, 2 bits left
    Original:1, Home:1, Copyright:1, CopyrightStart:1, % byte 4 and 2 bits follow
    FrameLength:13, % byte 5, 3 bits follow
    ADTS:11, Count:2, % byte 6,7
    Frame/binary>>.




%%--------------------------------------------------------------------
%% @spec (ADTS::binary()) -> {ok, Frame::binary(), Rest::binary()} | {more, undefined} | {error, unknown}
%% @doc Unpacks ADTS into AAC frame. Returns {more, undefined} if data is not enough
%%
%%     0         1             2           3         4           5         6
%% ||__sync__||sync|___e||pr|rate|_|c||hn|____|fr||ame_len_||gth|_____||______|cn||
%% 
%% @end 
%%--------------------------------------------------------------------
unpack_adts(<<16#FFF:12, _ID:1, _Layer:2, 1:1, _Profile:2, _SampleRate:4,
         _Private:1, _Channels:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
         FrameLength:13, _ADTS:11, _Count:2, Data/binary>>) when size(Data) >= FrameLength - 7 ->
  {Frame, Rest} = split_binary(Data, FrameLength - 7),
  % ?D({noerr, _ID, _Layer, _Profile, frequency(SampleRate), _Private, channels(Channels), _Original, _Home, _Copyright, _CopyrightStart, FrameLength, _Count}),
  {ok, Frame, Rest};

unpack_adts(<<16#FFF:12, _ID:1, _Layer:2, 0:1, _Profile:2, _SampleRate:4,
           _Private:1, _Channels:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
           FrameLength:13, _ADTS:11, _Count:2, _CRC32:16, Data/binary>>) when size(Data) >= FrameLength - 9 ->
  {Frame, Rest} = split_binary(Data, FrameLength - 9),
  % ?D({err, _ID, _Layer, _Profile, frequency(SampleRate), _Private, channels(Channels), _Original, _Home, _Copyright, _CopyrightStart, FrameLength, _Count}),
  {ok, Frame, Rest};

unpack_adts(<<16#FFF:12, _:4, _Rest/binary>>) ->
  {more, undefined};

unpack_adts(<<16#FF>>) ->
  {more, undefined};
  
unpack_adts(<<>>) ->
  {more, undefined};
  
unpack_adts(_) ->
  {error, unknown}.

%%--------------------------------------------------------------------
%% @spec (Body::binary()) -> Config::aac_config()
%% @doc Unpack binary AAC config into #aac_config{}
%%
%% The rest may be once read from ffmpeg mpeg4audio.c
%% @end 
%%--------------------------------------------------------------------
decode_config(AAC) ->
  extract_object_type(AAC, #aac_config{}).
  
extract_object_type(<<2#11111:5, ObjectType:6, AAC/bitstring>>, #aac_config{} = Config) ->
  extract_sample_rate(AAC, Config#aac_config{type = decode_object_type(ObjectType)});

extract_object_type(<<ObjectType:5, AAC/bitstring>>, #aac_config{} = Config) ->
  extract_sample_rate(AAC, Config#aac_config{type = decode_object_type(ObjectType)}).
  
extract_sample_rate(<<2#1111:4, SampleRate:24, AAC/bitstring>>, Config) ->
  extract_channels(AAC, Config#aac_config{sample_rate = SampleRate});

extract_sample_rate(<<SampleRate:4, AAC/bitstring>>, Config) ->
  extract_channels(AAC, Config#aac_config{sample_rate = decode_sample_rate(SampleRate)}).

extract_channels(<<Channels:4, FrameLength:1, _DependsCore:1, _Extension:1, _/binary>>, Config) ->
  Config#aac_config{channels = decode_channels(Channels), channel_count = Channels, samples_per_frame = decode_samples_per_frame(FrameLength)}.


%%--------------------------------------------------------------------
%% @spec (Config::aac_config()) -> Body::binary()
%% @doc Encode #aac_config{} to binary AAC config
%% @end 
%%--------------------------------------------------------------------
encode_config(Config) ->
  {ObjectType, FrequencyIndex, ChannelConfig, FrameLength} = pack_config(Config),
  DependsCore = 0,
  Extension = 0,
  <<ObjectType:5, FrequencyIndex:4, ChannelConfig:4, FrameLength:1, DependsCore:1, Extension:1>>.

pack_config(#aac_config{samples_per_frame = FrameLength, type = ObjectType, sample_rate = Frequency, channels = ChannelConfig}) ->
  {encode_object_type(ObjectType), encode_sample_rate(Frequency), encode_channels(ChannelConfig), encode_samples_per_frame(FrameLength)}.
  


%%--------------------------------------------------------------------
%% @spec (ADTS::binary()) -> Config::binary()
%% @doc Convert ADTS frame into AAC config
%% @end 
%%--------------------------------------------------------------------
adts_to_config(<<16#FFF:12, _ID:1, _Layer:2, _:1, Profile:2, SampleRate:4,
           _Private:1, Channels:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
           _FrameLength:13, _ADTS:11, _Count:2, _/binary>>) ->
  _Samples = 0,
  _DependsCore = 0,
  _Extension = 0,
  <<(Profile+1):5, SampleRate:4, Channels:4, _Samples:1, _DependsCore:1, _Extension:1>>.



decode_samples_per_frame(0) -> 1024;
decode_samples_per_frame(1) -> 960.

encode_samples_per_frame(1024) -> 0;
encode_samples_per_frame(960) -> 1.

decode_object_type(0) -> null;
decode_object_type(1) -> aac_main;
decode_object_type(2) -> aac_lc;
decode_object_type(3) -> aac_ssr;
decode_object_type(4) -> aac_ltp;
decode_object_type(5) -> aac_sbr.

encode_object_type(null) -> 0;
encode_object_type(aac_main) -> 1;
encode_object_type(aac_lc) -> 2;
encode_object_type(aac_ssr) -> 3;
encode_object_type(aac_ltp) -> 4;
encode_object_type(aac_sbr) -> 5.


decode_sample_rate(0) -> 96000;
decode_sample_rate(1) -> 88200;
decode_sample_rate(2) -> 64000;
decode_sample_rate(3) -> 48000;
decode_sample_rate(4) -> 44100;
decode_sample_rate(5) -> 32000;
decode_sample_rate(6) -> 24000;
decode_sample_rate(7) -> 22050;
decode_sample_rate(8) -> 16000;
decode_sample_rate(9) -> 12000;
decode_sample_rate(10) -> 11025;
decode_sample_rate(11) -> 8000;
decode_sample_rate(12) -> 7350.

encode_sample_rate(96000) -> 0;
encode_sample_rate(88200) -> 1;
encode_sample_rate(64000) -> 2;
encode_sample_rate(48000) -> 3;
encode_sample_rate(44100) -> 4;
encode_sample_rate(32000) -> 5;
encode_sample_rate(24000) -> 6;
encode_sample_rate(22050) -> 7;
encode_sample_rate(16000) -> 8;
encode_sample_rate(12000) -> 9;
encode_sample_rate(11025) -> 10;
encode_sample_rate(8000) -> 11;
encode_sample_rate(7350) -> 12.

decode_channels(0) -> specific;
decode_channels(1) -> fc;
decode_channels(2) -> flr;
decode_channels(3) -> flcr;
decode_channels(4) -> flcr_bc;
decode_channels(5) -> flcr_blr;
decode_channels(6) -> flcr_blr_lfe;
decode_channels(7) -> flcr_slr_blr_lfe.

encode_channels(specific) -> 0;
encode_channels(fc) -> 1;
encode_channels(flr) -> 2;
encode_channels(flcr) -> 3;
encode_channels(flcr_bc) -> 4;
encode_channels(flcr_blr) -> 5;
encode_channels(flcr_blr_lfe) -> 6;
encode_channels(flcr_slr_blr_lfe) -> 7.


to_fmtp(#aac_config{} = Config) ->
  to_fmtp(encode_config(Config));

to_fmtp(Config) when is_binary(Config) ->
  Encoded = [io_lib:format("~2.16.0B", [N]) || N <- binary_to_list(Config)],
  lists:flatten(["profile-level-id=1;mode=AAC-hbr;sizelength=13;indexlength=3;indexdeltalength=3;config=", Encoded]).
  

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

% config_test() ->
%   ?assertEqual(ok, aac:decode_config(<<255,240>>)).

fmtp_test() ->
  ?assertEqual("profile-level-id=1;mode=AAC-hbr;sizelength=13;indexlength=3;indexdeltalength=3;config=11B056E500", aac:to_fmtp(<<17,176,86,229,0>>)).






