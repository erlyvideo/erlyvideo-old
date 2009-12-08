-module(h264).

-author('max@maxidoors.ru').

-include("../../include/ems.hrl").
-include("../../include/h264.hrl").

-export([decode_nal/2]).
-export([profile_name/1, exp_golomb_read_list/2, exp_golomb_read_list/3, exp_golomb_read_s/1]).

video_config(H264) ->
  case decoder_config(H264) of
    ok -> {H264, []};
    DecoderConfig when is_binary(DecoderConfig) ->
      Frame = #video_frame{       
       	type          = ?FLV_TAG_TYPE_VIDEO,
       	decoder_config = true,
    		timestamp     = 0,
    		body          = DecoderConfig,
    		frame_type    = ?FLV_VIDEO_FRAME_TYPE_KEYFRAME,
    		codec_id      = ?FLV_VIDEO_CODEC_AVC
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



decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SINGLE:5, _/binary>> = Data, H264) ->
  VideoFrame = #video_frame{
   	type          = ?FLV_TAG_TYPE_VIDEO,
		body          = nal_with_size(Data),
		frame_type    = ?FLV_VIDEO_FRAME_TYPEINTER_FRAME,
		codec_id      = ?FLV_VIDEO_CODEC_AVC
  },
  {H264, [VideoFrame]};


decode_nal(<<0:1, _NalRefIdc:2, 2:5, Rest/binary>>, H264) ->
  % io:format("Coded slice data partition A     :: "),
  slice_header(Rest, H264);

decode_nal(<<0:1, _NalRefIdc:2, 3:5, Rest/binary>>, H264) ->
  % io:format("Coded slice data partition B     :: "),
  slice_header(Rest, H264);

decode_nal(<<0:1, _NalRefIdc:2, 4:5, Rest/binary>>, H264) ->
  % io:format("Coded slice data partition C     :: "),
  slice_header(Rest, H264);

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_IDR:5, _/binary>> = Data, H264) ->
  VideoFrame = #video_frame{
   	type          = ?FLV_TAG_TYPE_VIDEO,
		body          = nal_with_size(Data),
		frame_type    = ?FLV_VIDEO_FRAME_TYPE_KEYFRAME,
		codec_id      = ?FLV_VIDEO_CODEC_AVC
  },
  {H264, [VideoFrame]};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_PPS:5, _/binary>> = PPS, H264) ->
  % io:format("Picture parameter set: ~p~n", [PPS]),
  video_config(H264#h264{pps = [remove_trailing_zero(PPS)]});

decode_nal(<<0:1, _NalRefIdc:2, 9:5, PrimaryPicTypeId:3, _:5, _/binary>>, H264) ->
  PrimaryPicType = case PrimaryPicTypeId of
      0 -> "I";
      1 -> "I, P";
      2 -> "I, P, B";
      3 -> "SI";
      4 -> "SI, SP";
      5 -> "I, SI";
      6 -> "I, SI, P, SP";
      7 -> "I, SI, P, SP, B"
  end,
  io:format("Access unit delimiter, PPT = ~p~n", [PrimaryPicType]),
  {H264, []};


decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SPS:5, Profile, _:8, Level, _/binary>> = SPS, H264) ->
  % {_SeqParameterSetId, _Data2} = exp_golomb_read(Data1),
  % {Log2MaxFrameNumMinus4, Data3} = exp_golomb_read(Data2),
  % {PicOrderCntType, Data4} = exp_golomb_read(Data3),
  % case PicOrderCntType of
  %   0 ->
  %     {Log2MaxPicOrder, Data5} = exp_golomb_read(Data4);
  %   1 ->
  %     <<DeltaPicAlwaysZero:1, Data4_1/bitstring>> = Data4,
      
  % _ProfileName = profile_name(Profile),
  % io:format("~nSequence parameter set ~p ~p~n", [ProfileName, Level/10]),
  % io:format("log2_max_frame_num_minus4: ~p~n", [Log2MaxFrameNumMinus4]),
  video_config(H264#h264{profile = Profile, level = Level, sps = [remove_trailing_zero(SPS)]});
  
decode_nal(<<0:1, _NRI:2, ?NAL_STAR_A:5, Rest/binary>>, H264) ->
  decode_stara(Rest, [], H264);

decode_nal(<<0:1, _NRI:2, ?NAL_STAR_B:5, Rest/binary>>, H264) ->
  error(h264_star_b_unsupported);

decode_nal(<<0:1, _NRI:2, ?NAL_MTAP16:5, Rest/binary>>, H264) ->
  error(h264_mtap16_unsupported);

decode_nal(<<0:1, _NRI:2, ?NAL_MTAP24:5, Rest/binary>>, H264) ->
  error(h264_mtap24_unsupported);

decode_nal(<<0:1, _NRI:2, ?NAL_FUB:5, Rest/binary>>, H264) ->
  error(h264_fub_unsupported);


%          <<0:1, _NRI:2, ?NAL_FUA:5, Start:1, End:1, Type:6,  _Rest/binary>>
decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 1:1, _End:1, _Type:6, Rest/binary>>, H264) ->
  {H264#h264{buffer = Rest}, []};

decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 0:1, 0:1, _Type:6, Rest/binary>>, #h264{buffer = Buf} = H264) ->
  {H264#h264{buffer = <<Buf/binary, Rest/binary>>}, []};

decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 0:1, 1:1, _Type:6, Rest/binary>>, #h264{buffer = Buf} = H264) ->
  Data = <<Buf/binary, Rest/binary>>,
  VideoFrame = #video_frame{
   	type          = ?FLV_TAG_TYPE_VIDEO,
		body          = nal_with_size(Data),
		frame_type    = ?FLV_VIDEO_FRAME_TYPEINTER_FRAME,
		codec_id      = ?FLV_VIDEO_CODEC_AVC
  },
  {H264#h264{buffer = <<>>}, [VideoFrame]};


decode_nal(<<0:1, _NalRefIdc:2, _NalUnitType:5, _/binary>>, H264) ->
  % io:format("Unknown NAL unit type ~p~n", [NalUnitType]),
  {H264, []}.
  
decode_stara(<<Size:16, NAL:Size/binary, Rest/binary>>, Frames, H264) ->
  {H264_1, NewFrames} = decode_nal(NAL, H264),
  decode_stara(Rest, Frames ++ NewFrames, H264_1);
  
decode_stara(Rest, Frames, H264) ->
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


slice_header(Bin, H264) ->
    {_FirstMbInSlice, Rest} = exp_golomb_read(Bin),
    {SliceTypeId, Rest2 } = exp_golomb_read(Rest),
    {_PicParameterSetId, Rest3 } = exp_golomb_read(Rest2),
    <<_FrameNum:5, _FieldPicFlag:1, _BottomFieldFlag:1, _/bitstring>> = Rest3,
    _SliceType = slice_type(SliceTypeId),
    % io:format("~s~p:~p:~p:~p ~n", [_SliceType, _FrameNum, _PicParameterSetId, _FieldPicFlag, _BottomFieldFlag]),
    {H264, undefined}.

slice_type(0) -> 'P';
slice_type(1) -> 'B';
slice_type(2) -> 'I';
slice_type(3) -> 'p';
slice_type(4) -> 'i';
slice_type(5) -> 'P';
slice_type(6) -> 'B';
slice_type(7) -> 'I';
slice_type(8) -> 'p';
slice_type(9) -> 'i'.


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

