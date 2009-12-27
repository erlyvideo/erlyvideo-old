-module(h264).

-author('max@maxidoors.ru').

-include("../../include/ems.hrl").
-include("../../include/h264.hrl").
-include("../../include/video_frame.hrl").

-export([decode_nal/2, video_config/1]).
-export([profile_name/1, exp_golomb_read_list/2, exp_golomb_read_list/3, exp_golomb_read_s/1]).
-export([open_dump/0, dump_nal/2, fake_open_dump/0, fake_dump_nal/2]).


fake_open_dump() -> ok.
fake_dump_nal(_File, _Nal) -> ok.

open_dump() ->
  {ok, File} = file:open("out.x264", [write, binary]),
  File.
  
dump_nal(File, NAL) ->
  file:write(File, <<0,0,1, NAL/binary>>).


-ifdef(dump_h264).
-define(OPEN_H264_OUT, open_dump).
-define(DUMP_H264, dump_nal).
-else.
-define(OPEN_H264_OUT, fake_open_dump).
-define(DUMP_H264, fake_dump_nal).
-endif.

video_config(H264) ->
  case decoder_config(H264) of
    ok -> {H264, []};
    DecoderConfig when is_binary(DecoderConfig) ->
      Frame = #video_frame{       
       	type          = video,
       	decoder_config = true,
    		timestamp     = 0,
    		body          = DecoderConfig,
    		frame_type    = keyframe,
    		codec_id      = avc
    	},
    	{H264, [Frame]}
  end.
      

decoder_config(#h264{sps = undefined}) -> ok;
decoder_config(#h264{pps = undefined}) -> ok;
decoder_config(#h264{pps = PPS, sps = SPS, profile = Profile, profile_compat = ProfileCompat, level = Level}) ->
  LengthSize = 4-1,
  Version = 1,
  SPSBin = iolist_to_binary(SPS),
  PPSBin = iolist_to_binary(PPS),
  <<Version, Profile, ProfileCompat, Level, 2#111111:6, LengthSize:2, 
    2#111:3, (length(SPS)):5, (size(SPSBin)):16, SPSBin/binary,
    (length(PPS)), (size(PPSBin)):16, PPSBin/binary>>.


decode_nal(NAL, #h264{dump_file = undefined} = H264) ->
  File = ?OPEN_H264_OUT(),
  decode_nal(NAL, H264#h264{dump_file = File});

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SINGLE:5, _/binary>> = Data, #h264{dump_file = File} = H264) ->
  % ?D("P-frame"),
  (catch slice_header(Data)),
  ?DUMP_H264(File, Data),
  VideoFrame = #video_frame{
   	type          = video,
		body          = nal_with_size(Data),
		frame_type    = frame,
		codec_id      = avc
  },
  {H264, [VideoFrame]};


decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SLICE_A:5, _Rest/binary>> = _Data, H264) ->
  io:format("Coded slice data partition A~n"),
  % slice_header(Rest, H264);
  {H264, []};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SLICE_B:5, _Rest/binary>> = _Data, H264) ->
  io:format("Coded slice data partition B~n"),
  % slice_header(Rest, H264);
  {H264, []};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SLICE_C:5, _Rest/binary>> = _Data, H264) ->
  io:format("Coded slice data partition C~n"),
  % slice_header(Rest, H264);
  {H264, []};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_IDR:5, _/binary>> = Data, #h264{dump_file = File} = H264) ->
  % ?D("I-frame"),
  (catch slice_header(Data)),
  ?DUMP_H264(File, Data),
  VideoFrame = #video_frame{
   	type          = video,
		body          = nal_with_size(Data),
		frame_type    = keyframe,
		codec_id      = avc
  },
  {H264, [VideoFrame]};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SEI:5, _/binary>> = Data, #h264{dump_file = File} = H264) ->
  ?DUMP_H264(File, Data),
  _VideoFrame = #video_frame{
   	type          = video,
		body          = nal_with_size(Data),
		frame_type    = frame,
		codec_id      = avc
  },
  
  {H264, []};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SPS:5, Profile, _:8, Level, _/binary>> = SPS, #h264{dump_file = File} = H264) ->
  io:format("Sequence parameter set ~p ~p~n", [profile_name(Profile), Level/10]),
  % io:format("log2_max_frame_num_minus4: ~p~n", [Log2MaxFrameNumMinus4]),
  ?DUMP_H264(File, SPS),
  video_config(H264#h264{profile = Profile, level = Level, sps = [remove_trailing_zero(SPS)]});

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_PPS:5, Bin/binary>> = PPS, #h264{dump_file = File} = H264) ->
  ?DUMP_H264(File, PPS),
  {_PPSId, Rest1} = exp_golomb_read(Bin),
  {_SPSId, _Rest} = exp_golomb_read(Rest1),
  video_config(H264#h264{pps = [remove_trailing_zero(PPS)]});

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_DELIM:5, _PrimaryPicTypeId:3, _:5, _/binary>> = Delimiter, #h264{dump_file = File} = H264) ->
  % PrimaryPicType = case PrimaryPicTypeId of
  %     0 -> "I";
  %     1 -> "I, P";
  %     2 -> "I, P, B";
  %     3 -> "SI";
  %     4 -> "SI, SP";
  %     5 -> "I, SI";
  %     6 -> "I, SI, P, SP";
  %     7 -> "I, SI, P, SP, B"
  % end,
  ?DUMP_H264(File, Delimiter),
  VideoFrame = #video_frame{
   	type          = video,
		body          = nal_with_size(Delimiter),
		frame_type    = frame,
		codec_id      = avc
  },
  % io:format("Access unit delimiter, PPT = ~p~n", [PrimaryPicType]),
  {H264, [VideoFrame]};


  
decode_nal(<<0:1, _NRI:2, ?NAL_STAP_A:5, Rest/binary>>, H264) ->
  decode_stapa(Rest, [], H264);

decode_nal(<<0:1, _NRI:2, ?NAL_STAP_B:5, _/binary>>, _H264) ->
  erlang:error(h264_star_b_unsupported);

decode_nal(<<0:1, _NRI:2, ?NAL_MTAP16:5, _/binary>>, _H264) ->
  erlang:error(h264_mtap16_unsupported);

decode_nal(<<0:1, _NRI:2, ?NAL_MTAP24:5, _/binary>>, _H264) ->
  erlang:error(h264_mtap24_unsupported);

decode_nal(<<0:1, _NRI:2, ?NAL_FUB:5, _/binary>>, _H264) ->
  erlang:error(h264_fub_unsupported);


%          <<0:1, _NRI:2, ?NAL_FUA:5, Start:1, End:1, R:1, Type:1,  _Rest/binary>>
decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 1:1, _End:1, 0:1, _Type:5, Rest/binary>>, H264) ->
  {H264#h264{buffer = Rest}, []};

decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 0:1, 0:1, 0:1, _Type:5, Rest/binary>>, #h264{buffer = Buf} = H264) ->
  {H264#h264{buffer = <<Buf/binary, Rest/binary>>}, []};

decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 0:1, 1:1, 0:1, Type:5, Rest/binary>>, #h264{buffer = Buf} = H264) ->
  decode_nal(<<0:1, _NRI:2, Type:5, Buf/binary, Rest/binary>>, H264#h264{buffer = <<>>});


decode_nal(<<0:1, _NalRefIdc:2, _NalUnitType:5, _/binary>>, H264) ->
  io:format("Unknown NAL unit type ~p~n", [_NalUnitType]),
  {H264, []}.
  
decode_stapa(<<Size:16, NAL:Size/binary, Rest/binary>>, Frames, H264) ->
  {H264_1, NewFrames} = decode_nal(NAL, H264),
  decode_stapa(Rest, Frames ++ NewFrames, H264_1);
  
decode_stapa(<<>>, Frames, H264) ->
  {H264, Frames}.
  
nal_with_size(NAL) -> <<(size(NAL)):32, NAL/binary>>.

remove_trailing_zero(Bin) ->
  Size = size(Bin) - 1,
  case Bin of
    <<Smaller:Size/binary, 0>> -> remove_trailing_zero(Smaller);
    _ -> Bin
  end.

profile_name(66) -> "Baseline";
profile_name(77) -> "Main";
profile_name(88) -> "Extended";
profile_name(100) -> "High";
profile_name(110) -> "High 10";
profile_name(122) -> "High 4:2:2";
profile_name(144) -> "High 4:4:4";
profile_name(Profile) -> "Unknown "++integer_to_list(Profile).


slice_header(Bin) ->
    {_FirstMbInSlice, Rest} = exp_golomb_read(Bin),
    {SliceTypeId, Rest2 } = exp_golomb_read(Rest),
    {_PicParameterSetId, Rest3 } = exp_golomb_read(Rest2),
    <<_FrameNum:5, _FieldPicFlag:1, _BottomFieldFlag:1, _/bitstring>> = Rest3,
    _SliceType = slice_type(SliceTypeId),
    % case _PicParameterSetId of
    %   0 -> ok;
    %   _ -> io:format("~s ~p~n", [_SliceType, _PicParameterSetId])
    % end,
    % io:format("~s~p:~p:~p:~p ~n", [_SliceType, _FrameNum, _PicParameterSetId, _FieldPicFlag, _BottomFieldFlag]),
    ok.

slice_type(0) -> 'P';
slice_type(1) -> 'B';
slice_type(2) -> 'I';
slice_type(3) -> 'p';
slice_type(4) -> 'i';
slice_type(5) -> 'P';
slice_type(6) -> 'B';
slice_type(7) -> 'I';
slice_type(8) -> 'p';
slice_type(9) -> 'i';
slice_type(_) -> undefined.


exp_golomb_read_list(Bin, List) ->
  exp_golomb_read_list(Bin, List, []).
  
exp_golomb_read_list(Bin, [], Results) -> {Results, Bin};
exp_golomb_read_list(Bin, [Key | Keys], Results) ->
  {Value, Rest} = exp_golomb_read(Bin),
  exp_golomb_read_list(Rest, Keys, [{Key, Value} | Results]).

exp_golomb_read_s(Bin) ->
  {Value, _Rest} = exp_golomb_read(Bin),
  case Value band 1 of
    1 -> (Value + 1)/2;
    _ -> - (Value/2)
  end.

exp_golomb_read(Bin) ->
  exp_golomb_read(Bin, 0).
  
exp_golomb_read(<<0:1, Rest/bitstring>>, LeadingZeros) ->
  exp_golomb_read(Rest, LeadingZeros + 1);

exp_golomb_read(<<1:1, Data/bitstring>>, LeadingZeros) ->
  <<ReadBits:LeadingZeros, Rest/bitstring>> = Data,
  CodeNum = (1 bsl LeadingZeros) -1 + ReadBits,
  {CodeNum, Rest}.

