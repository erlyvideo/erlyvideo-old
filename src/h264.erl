-module(h264).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-author('Max Lapshin <max@maxidoors.ru>').

% -include("../../include/ems.hrl").
-include_lib("erlmedia/include/h264.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-export([decode_nal/2, video_config/1, has_config/1, unpack_config/1, metadata/1]).
-export([profile_name/1, exp_golomb_read_list/2, exp_golomb_read_list/3, exp_golomb_read_s/1]).
-export([open_dump/0, dump_nal/2, fake_open_dump/0, fake_dump_nal/2]).
-export([parse_sps/1]).

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
  case has_config(H264) of
    false -> undefined;
    true ->
      #video_frame{       
       	type          = video,
       	decoder_config = true,
    		dts           = 0,
    		pts           = 0,
    		body          = decoder_config(H264),
    		frame_type    = keyframe,
    		codec_id      = h264
    	}
  end.

metadata(Config) ->
  {_, [SPSBin, _]} = unpack_config(Config),
  #h264_sps{width = Width, height = Height} = parse_sps(SPSBin),
  #video_frame{       
   	type          = metadata,
		dts           = 0,
		pts           = 0,
		body          = [<<"onMetaData">>, {object, [{width, Width}, {height, Height}]}]
	}.
  
      
%% Look at vlc/modules/demux/mp4/libmp4.c:1022
%%
unpack_config(<<_Version, _Profile, _ProfileCompat, _Level, 2#111111:6, LengthSize:2, 2#111:3, 
                SPSCount:5, Rest/binary>>) ->
  {SPS, <<PPSCount, Rest1/binary>>} = parse_h264_config(Rest, SPSCount, []),
  {PPS, <<>>} = parse_h264_config(Rest1, PPSCount, SPS),
  {LengthSize + 1, lists:reverse(PPS)}.
  
  
  
parse_h264_config(Rest, 0, List) -> {List, Rest};
parse_h264_config(<<Length:16, NAL:Length/binary, Rest/binary>>, Count, List) -> 
  parse_h264_config(Rest, Count - 1, [NAL|List]).

  
has_config(#h264{sps = SPS, pps = PPS}) when length(SPS) > 0 andalso length(PPS) > 0 -> true;
has_config(_) -> false.

decoder_config(#h264{sps = undefined}) -> undefined;
decoder_config(#h264{pps = undefined}) -> undefined;
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
		codec_id      = h264
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
  % io:format("I-frame ~p ~p~n", [size(Data), element(1, split_binary(Data, 40))]),
  (catch slice_header(Data)),
  ?DUMP_H264(File, Data),
  VideoFrame = #video_frame{
   	type          = video,
		body          = nal_with_size(Data),
		frame_type    = keyframe,
		codec_id      = h264
  },
  {H264, [VideoFrame]};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SEI:5, _/binary>> = Data, #h264{dump_file = File} = H264) ->
  ?DUMP_H264(File, Data),
  _VideoFrame = #video_frame{
   	type          = video,
		body          = nal_with_size(Data),
		frame_type    = frame,
		codec_id      = h264
  },
  
  {H264, []};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_SPS:5, Profile, _:8, Level, _/binary>> = SPS, #h264{dump_file = File} = H264) ->
  % io:format("log2_max_frame_num_minus4: ~p~n", [Log2MaxFrameNumMinus4]),
  ?DUMP_H264(File, SPS),
  % ?D({"Parsing SPS", SPS}),
  % _SPSInfo = parse_sps(SPS),
  % io:format("SPS ~p ~p ~px~p~n", [profile_name(Profile), Level/10, SPSInfo#h264_sps.width, SPSInfo#h264_sps.height]),
  {H264#h264{profile = Profile, level = Level, sps = [SPS]}, []};

decode_nal(<<0:1, _NalRefIdc:2, ?NAL_PPS:5, Bin/binary>> = PPS, #h264{dump_file = File} = H264) ->
  ?DUMP_H264(File, PPS),
  {_PPSId, Rest1} = exp_golomb_read(Bin),
  {_SPSId, _Rest} = exp_golomb_read(Rest1),
  % video_config(H264#h264{pps = [PPS]});
  {H264#h264{pps = [PPS]}, []};

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
		codec_id      = h264
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


%          <<0:1, _NRI:2, ?NAL_FUA:5, Start:1, End:1, R:1, Type:1,  _Rest/binary>>, R === 0
decode_nal(<<0:1, NRI:2, ?NAL_FUA:5, 1:1, _End:1, 0:1, Type:5, Rest/binary>>, H264) ->
  {H264#h264{buffer = <<0:1, NRI:2, Type:5, Rest/binary>>}, []};

decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 0:1, 0:1, 0:1, _Type:5, Rest/binary>>, #h264{buffer = Buf} = H264) ->
  {H264#h264{buffer = <<Buf/binary, Rest/binary>>}, []};

decode_nal(<<0:1, NRI:2, ?NAL_FUA:5, 1:1, 1:1, 0:1, Type:5, Rest/binary>>, H264) ->
  decode_nal(<<0:1, NRI:2, Type:5, Rest/binary>>, H264#h264{buffer = <<>>});

decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, 0:1, 1:1, 0:1, _Type:5, Rest/binary>>, #h264{buffer = Buf} = H264) ->
  decode_nal(<<Buf/binary, Rest/binary>>, H264#h264{buffer = <<>>});


% decode_nal(<<0:1, _NRI:2, ?NAL_FUA:5, S:1, E:1, R:1, _Type:5, Rest/binary>>, #h264{buffer = Buf} = H264) ->
%   ?D({"Unknonw FUA", S, E, R, _Type}),
%   {H264#h264{buffer = <<>>}, []};


decode_nal(<<0:1, _NalRefIdc:2, _NalUnitType:5, _/binary>> = NAL, H264) ->
  ?D({"Unknown NAL unit type", _NalUnitType, size(NAL)}),
  {H264, []}.
  
decode_stapa(<<Size:16, NAL:Size/binary, Rest/binary>>, Frames, H264) ->
  {H264_1, NewFrames} = decode_nal(NAL, H264),
  decode_stapa(Rest, Frames ++ NewFrames, H264_1);
  
decode_stapa(<<>>, Frames, H264) ->
  {H264, Frames}.
  
nal_with_size(NAL) -> <<(size(NAL)):32, NAL/binary>>.


parse_sps(<<0:1, _NalRefIdc:2, ?NAL_SPS:5, Profile, _:8, Level, Data/binary>>) when Profile >= 100 ->
  {SPS_ID, Rest} = exp_golomb_read(Data),
  {ChromaFormat, Rest1} = exp_golomb_read(Rest),
  case ChromaFormat of
    3 -> <<_:1, Rest2/bitstring>> = Rest1;
    _ -> Rest2 = Rest1
  end,
  {_BitDepthLuma, Rest3} = exp_golomb_read(Rest2),
  {_BitDepthChroma, Rest4} = exp_golomb_read(Rest3),
  <<_:2, Rest5/bitstring>> = Rest4,
  SPS = #h264_sps{profile = Profile, level = Level, sps_id = SPS_ID},
  parse_sps_data(Rest5, SPS);

parse_sps(<<0:1, _NalRefIdc:2, ?NAL_SPS:5, Profile, _:8, Level, Data/binary>>) ->
  {SPS_ID, Rest1} = exp_golomb_read(Data),
  % ?D({"SPS ID", SPS_ID}),
  SPS = #h264_sps{profile = Profile, level = Level, sps_id = SPS_ID},
  parse_sps_data(Rest1, SPS).
  
  
parse_sps_data(Data, SPS) ->
  {Log2FrameNum, Rest2} = exp_golomb_read(Data),
  {PicOrder, Rest3} = exp_golomb_read(Rest2),
  % ?D({"Pic order", PicOrder}),
  parse_sps_pic_order(Rest3, PicOrder, SPS#h264_sps{max_frame_num = Log2FrameNum+4}).
  
parse_sps_pic_order(Data, 0, SPS) ->
  {_Log2PicOrder, Rest} = exp_golomb_read(Data),
  parse_sps_ref_frames(Rest, SPS);
  
parse_sps_pic_order(<<_AlwaysZero:1, Data/bitstring>>, 1, SPS) ->
  {_OffsetNonRef, Rest1} = exp_golomb_read_s(Data),
  {_OffsetTopBottom, Rest2} = exp_golomb_read_s(Rest1),
  {NumRefFrames, Rest3} = exp_golomb_read_s(Rest2),
  NumRefFrames = 0,
  parse_sps_ref_frames(Rest3, SPS);

parse_sps_pic_order(Data, 2, SPS) ->
  parse_sps_ref_frames(Data, SPS).
  
parse_sps_ref_frames(Data, SPS) ->
  {_NumRefFrames, <<_Gaps:1, Rest1/bitstring>>} = exp_golomb_read(Data),
  {PicWidth, Rest2} = exp_golomb_read(Rest1),
  Width = (PicWidth + 1)*16,
  {PicHeight, <<_FrameMbsOnly:1, _Rest3/bitstring>>} = exp_golomb_read(Rest2),
  Height = (PicHeight + 1)*16,
  SPS#h264_sps{width = Width, height = Height}.
  

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

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

parse_sps_for_high_profile_test() ->
  ?assertEqual(#h264_sps{profile = 100, level = 50, sps_id = 0, max_frame_num = 9, width = 1280, height = 720}, parse_sps(<<103,100,0,50,172,52,226,192,80,5,187,1,16,0,0,62,144,0,11,184,8,241,131,24,184>>)).

parse_sps_for_low_profile_test() ->
  ?assertEqual(#h264_sps{profile = 77, level = 51, sps_id = 0, max_frame_num = 8, width = 512, height = 384}, parse_sps(<<103,77,64,51,150,99,1,0,99,96,34,0,0,3,0,2,0,0,3,0,101,30,48,100,208>>)).

parse_sps_for_rtsp_test() ->
  ?assertEqual(#h264_sps{profile = 66, level = 20, sps_id = 0, max_frame_num = 4, width = 352, height = 288}, parse_sps(<<103,66,224,20,218,5,130,81>>)).

unpack_config_1_test() ->
  Config = <<1,66,192,21,253,225,0,23,103,66,192,21,146,68,15,4,127,88,8,128,0,1,244,0,0,97,161,71,139,23,80,1,0,4,104,206,50,200>>,
  Result = [<<103,66,192,21,146,68,15,4,127,88,8,128,0,1,244,0,0,97,161,71,139,23,80>>, <<104,206,50,200>>],
  LengthSize = 2,
  ?assertEqual({LengthSize, Result}, unpack_config(Config)).
  
unpack_config_2_test() ->
  Config = <<1,77,64,30,255,224,0>>,
  ?assertEqual({4, []}, unpack_config(Config)).
  
unpack_config_3_test() ->
  Config = <<1,66,224,11,255,225,0,19,39,66,224,11,169,24,96,157,128,53,6,1,6,182,194,181,239,124,4,1,0,4,40,222,9,136>>,
  ?assertEqual({4,[<<39,66,224,11,169,24,96,157,128,53,6,1,6,182,194,181,239,124,4>>, <<40,222,9,136>>]}, unpack_config(Config)).






  
  