%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        RTMP encoding/decoding and command handling module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_flv).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").

-export([init/1,read_frame/1,to_tag/2,header/1, parse_meta/1, encode/1]).



%%--------------------------------------------------------------------
%% @spec (IoDev::iodev()) -> {ok, IoSize::integer(), Header::header()} | {error,Reason::atom()}
%% @doc Starts ErlMedia
%% @end 
%%--------------------------------------------------------------------
init(#video_player{device = IoDev} = State) -> 
  case file:read(IoDev, ?FLV_HEADER_LENGTH) of
    {ok, Data} -> 
      {ok, State#video_player{pos = iolist_size(Data), header = header(Data)}};
    eof -> 
      {error, unexpected_eof};
    {error, Reason} -> {error, Reason}           
  end.


% Reads a tag from IoDev for position Pos.
% @param IoDev
% @param Pos
% @return a valid video_frame record type
read_frame(#video_player{device = IoDev, pos = Pos} = State) ->
	case file:pread(IoDev,Pos, ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH) of
		{ok, IoList} ->
			case iolist_to_binary(IoList) of
			  <<PrevTagSize:32/integer, Type:8, BodyLength:24/big, TimeStamp:24/big, TimeStampExt:8, StreamId:24/big>> ->				
					case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, BodyLength) of
						{ok,IoList2} -> 
						    <<TimeStampAbs:32/big>> = <<TimeStampExt:8, TimeStamp:24/big>>,
						    Body = iolist_to_binary(IoList2),
					    	TagData = #video_frame{       
					    	  prev_tag_size = PrevTagSize,
				         	type          = Type,
						 			timestamp_abs = TimeStampAbs,
						 			streamid      = StreamId,
						 			pos           = Pos,
						   		nextpos       = Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + size(Body),
						 			body          = Body
								},
							case Type of
								
								 ?FLV_TAG_TYPE_AUDIO -> 
								  {SoundType, SoundSize, SoundRate, SoundFormat} =  extractAudioHeader(IoDev, Pos),
								 	{ok, TagData#video_frame{
										  sound_type	= SoundType,
										  sound_size	= SoundSize,
										  sound_rate	= SoundRate,
										  sound_format	= SoundFormat
										  }, State};
								 ?FLV_TAG_TYPE_VIDEO -> 
								  {FrameType, CodecID, Width, Height} = extractVideoHeader(IoDev, Pos),
								 	{ok, TagData#video_frame{
										  frame_type	= FrameType,
										  codec_id	= CodecID,
										  width		= Width,
										  height	= Height
										  }, State};
								?FLV_TAG_TYPE_META -> 
								  AmfData = ems_amf:decode(iolist_to_binary(IoList2)),
								  {ok, TagData#video_frame{
										   amf_data      = AmfData
										   }, State}
								end;
	
						eof -> 
							{ok, done, State};
						{error, Reason} -> 
							{error, Reason}
					end;
				%where has the error handling gone :)?
				_ ->
					{ok, done}
			end;		
        eof -> 
			{error, unexpected_eof};
        {error, Reason} -> 
			{error, Reason}
	end.
	
% Extracts width and height from video frames.
% TODO: add to video_frame, not done yet
% @param IoDev
% @param Pos
% @param codecID
% @return {Width, Height}
getWidthHeight(IoDev, Pos, CodecID)->
	case CodecID of
		?FLV_VIDEO_CODEC_SORENSEN 	-> decodeSorensen(IoDev, Pos);
		?FLV_VIDEO_CODEC_SCREENVIDEO 	-> decodeScreenVideo(IoDev, Pos);
		?FLV_VIDEO_CODEC_ON2VP6 	-> decodeVP6(IoDev, Pos);
		%not sure if FLV_VIDEO_CODEC_ON2VP6 == FLV_VIDEO_CODEC_ON2VP6_ALPHA: needs to be tested...
		?FLV_VIDEO_CODEC_ON2VP6_ALPHA 	-> decodeVP6(IoDev, Pos);
		%FLV_VIDEO_CODEC_SCREENVIDEO2 doesn't seem to be widely used yet, no decoding method available
		?FLV_VIDEO_CODEC_SCREENVIDEO2 	-> {undefined, undefined}
	end.

				


% Extracts video header information for a tag.
% @param IoDev
% @param Pos
% @return {FrameType, CodecID}
extractVideoHeader(IoDev, Pos) ->	
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, 1) of   
	       {ok,IoList3} ->
		     case iolist_to_binary(IoList3) of
			     %type= 9 :: read video headers:
				 <<FrameType:4, CodecID:4>> ->
				 	{Width, Height} = getWidthHeight(IoDev, Pos, CodecID),
					{FrameType, CodecID, Width, Height}
		      end;
		eof -> 
			 {ok, done};
		{error, Reason} -> 
			 {error, Reason}
end.



decodeScreenVideo(IoDev, Pos) ->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 4) of
		{ok, IoList4} -> 
			case iolist_to_binary(IoList4) of
				<<_Offset:4, Width:12, Height:12, _Rest:4>> -> {Width, Height}
			end
	end.
	
decodeSorensen(IoDev, Pos) ->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 9) of
		{ok, IoList4} ->
			case iolist_to_binary(IoList4) of
				<<_Offset:30, Info:3, _Rest:39>> -> 
				case Info of
					
					0 ->  case iolist_to_binary(IoList4) of
							<<_Offset1:30, _Info1:3, Width1:8, Height1:8, _Rest1:23>> -> {Width1, Height1}
					      end;
					1 -> case iolist_to_binary(IoList4) of
							<<_Offset2:30, _Info2:3, Width2:16, Height2:16, _Rest2:7>> -> {Width2, Height2}
					     end;
					2 -> {352, 288};
					3 -> {176, 144};
					4 -> {128, 96};
					5 -> {320, 240};
					6 -> {160, 120}
				end
			end
	end.

decodeVP6(IoDev, Pos)->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 7) of
			{ok, IoList4} -> 
				case iolist_to_binary(IoList4) of
					<<HeightHelper:4, WidthHelper:4, _Offset:24, Width:8, Height:8>> -> 
						{Width*16-WidthHelper, Height*16-HeightHelper}
				end
	end.
% Extracts audio header information for a tag.
% @param IoDev
% @param Pos
% @return {SoundType, SoundSize, SoundRate, SoundFormat}
extractAudioHeader(IoDev, Pos) ->	
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, 1) of   
	       {ok,IoList3} ->
		     case iolist_to_binary(IoList3) of
			     %type= 9 :: read video headers:
				 %<<SoundType:1, SoundSize:1, SoundRate:2, SoundFormat: 4>> ->
				 <<SoundFormat:4, SoundRate:2, SoundSize:1, SoundType:1>> ->
					 {SoundType, SoundSize, SoundRate, SoundFormat}
		      end;
		eof -> 
			 {ok, done};
		{error, Reason} -> 
			 {error, Reason}
end.


encode(#video_frame{type = ?FLV_TAG_TYPE_AUDIO,
                    decoder_config = true,
                    sound_format = ?FLV_AUDIO_FORMAT_AAC,
                	  sound_type	= SoundType,
                	  sound_size	= SoundSize,
                	  sound_rate	= SoundRate,
                    body = Body}) when is_binary(Body) ->
  <<?FLV_AUDIO_FORMAT_AAC:4, SoundRate:2, SoundSize:1, SoundType:1, 
    ?FLV_AUDIO_AAC_SEQUENCE_HEADER:8, Body/binary>>;


encode(#video_frame{type = ?FLV_TAG_TYPE_AUDIO,
                    sound_format = ?FLV_AUDIO_FORMAT_AAC,
                	  sound_type	= SoundType,
                	  sound_size	= SoundSize,
                	  sound_rate	= SoundRate,
                    body = Body}) when is_binary(Body) ->
	<<?FLV_AUDIO_FORMAT_AAC:4, SoundRate:2, SoundSize:1, SoundType:1, 
	  ?FLV_AUDIO_AAC_RAW:8, Body/binary>>;


encode(#video_frame{type = ?FLV_TAG_TYPE_VIDEO,
                    frame_type = FrameType,
                   	decoder_config = true,
                   	codec_id = CodecId,
                    body = Body}) when is_binary(Body) ->
  CompositionTime = 0,
	<<FrameType:4/integer, CodecId:4/integer, ?FLV_VIDEO_AVC_SEQUENCE_HEADER:8/integer, CompositionTime:24/big-integer, Body/binary>>;

encode(#video_frame{type = ?FLV_TAG_TYPE_VIDEO,
                    frame_type = FrameType,
                   	codec_id = CodecId,
                    body = Body}) when is_binary(Body) ->
  CompositionTime = 0,
	<<FrameType:4/integer, CodecId:4/integer, ?FLV_VIDEO_AVC_NALU:8/integer, CompositionTime:24/big-integer, Body/binary>>;

encode(#video_frame{timestamp_abs = TimeStampAbs, 
                    prev_tag_size = PrevTagSize,
                    type = Type,
                    streamid = StreamId,
                    body = Body}) when is_binary(Body) ->
	<<PrevTagSize:32/big, Type:8, (size(Body)):24/big, TimeStampAbs:32/big, StreamId:24, Body/binary>>;
	
encode(_) ->
  ?D("Request to encode undefined").
	

header(#flv_header{version = Version, audio = Audio, video = Video}) -> 
	Reserved = 0,
	Offset = 9,
	PrevTag = 0,
	<<70,76,86,Version:8,Reserved:5,Audio:1,Reserved:1,Video:1,Offset:32,PrevTag:32>>;
header(Bin) when is_binary(Bin) ->
	<<70,76,86, Ver:8, _:5, Audio:1, _:1, Video:1, 0,0,0,9>> = Bin,
	#flv_header{version=Ver,audio=Audio,video=Video};
header(IoList) when is_list(IoList) -> header(iolist_to_binary(IoList)).
		

to_tag(#channel{msg = Msg,timestamp = FullTimeStamp, type = Type, stream = StreamId}, PrevTimeStamp) ->
	BodyLength = size(Msg),	
	{TimeStampExt, TimeStamp} = case PrevTimeStamp of
		<<TimeStampExt1:8,TimeStamp1:32>> -> 
			{TimeStampExt1, TimeStamp1};
		_ ->
			{0, PrevTimeStamp}
	end,			
	PrevTagSize = size(Msg) + 11,
	{<<Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Msg/binary,PrevTagSize:32>>,
	 FullTimeStamp + PrevTimeStamp}.


parse_meta(Bin) ->
	file:write_file("/sfe/temp/meta.txt",Bin),
	?D(Bin),
	{Type,String,Next} = ems_amf:parse(Bin),
%	?D(String),
%	?D(Next),
	{Type,Array,_Next} = ems_amf:parse(Next),
	{String,Array}.
	
