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

-export([read_header/1,read_tag/2,to_tag/2,header/1, parse_meta/1, encodeTag/2]).

read_header(IoDev) -> 
    case file:read(IoDev, ?FLV_HEADER_LENGTH) of
        {ok, Data} -> 

			{ok, iolist_size(Data), header(Data)};
        eof -> {error, unexpected_eof};
        {error, Reason} -> {error, Reason}           
    end.


% Reads a tag from IoDev for position Pos, place this method in ems_flv or leave it here???
% @param IoDev
% @param Pos
% @return a valid flv_tag record type
read_tag(IoDev, Pos) ->
	case file:pread(IoDev,Pos, ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH) of
		{ok, IoList} ->
			case iolist_to_binary(IoList) of
			  	<<PrevTagSize:32/integer,Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24>> ->				
					case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, BodyLength) of
						{ok,IoList2} -> 
						    <<TimeStampAbs:32>> = <<TimeStampExt:8, TimeStamp:24>>,
						    	TagData = #flv_tag{       prev_tag_size = PrevTagSize,
					         			          type          = Type,
							 			  body_length   = BodyLength,
							 			  timestamp_abs = TimeStampAbs,
							 			  streamid      = StreamId,
							 			  pos           = Pos,
							   			  nextpos       = Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + BodyLength,
							 			  body          = iolist_to_binary(IoList2)
										  },
							case Type of
								
								 8 -> {SoundType, SoundSize, SoundRate, SoundFormat} =  extractAudioHeader(IoDev, Pos),
								     
								 	{ok, TagData#flv_tag{
										  sound_type	= SoundType,
										  sound_size	= SoundSize,
										  sound_rate	= SoundRate,
										  sound_format	= SoundFormat
										  }};
								 9 -> {FrameType, CodecID, Width, Height} = extractVideoHeader(IoDev, Pos),
								 	{ok, TagData#flv_tag{
										  frame_type	= FrameType,
										  codec_id	= CodecID,
										  width		= Width,
										  height	= Height
										  }};
								18 -> AmfData = ems_amf:decode(iolist_to_binary(IoList2)),
								       {ok, TagData#flv_tag{
										   amf_data      = AmfData
										   }}
				
								end;
	
						eof -> 
							{ok, done};
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

% Decodes a tag from binary
% @param EncodedTag
% @return a valid flv_tag record type
read_tag(EncodedTag) ->

	<<PrevTagSize:32/integer,Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24, Body/binary>> = EncodedTag,		

	    <<TimeStampAbs:32>> = <<TimeStampExt:8, TimeStamp:24>>,
		case Type of
			18 ->  
				      {ok, #flv_tag{prev_tag_size = PrevTagSize,
						  type          = Type,
						  body_length   = BodyLength,
						  timestamp_abs = TimeStampAbs,
						  streamid      = StreamId,
						  body          = Body
						  }};
			_ -> {error}

			end.

% Extracts width and height from video frames.
% TODO: add to flv_tag, not done yet
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
				<<Offset:4, Width:12, Height:12, Rest:4>> -> {Width, Height}
			end
	end.
	
decodeSorensen(IoDev, Pos) ->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 9) of
		{ok, IoList4} ->
			case iolist_to_binary(IoList4) of
				<<Offset:30, Info:3, Rest:39>> -> 
				case Info of
					
					0 ->  case iolist_to_binary(IoList4) of
							<<Offset1:30, Info1:3, Width1:8, Height1:8, Rest1:23>> -> {Width1, Height1}
					      end;
					1 -> case iolist_to_binary(IoList4) of
							<<Offset2:30, Info2:3, Width2:16, Height2:16, Rest2:7>> -> {Width2, Height2}
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
					<<HeightHelper:4, WidthHelper:4, Offset:24, Width:8, Height:8>> -> 
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


encodeTag(Tag, Body) when is_record(Tag, flv_tag) ->
	TimeStampAbs = Tag#flv_tag.timestamp_abs,
	PrevTagSize = Tag#flv_tag.prev_tag_size,
	Type = Tag#flv_tag.type,
	BodyLength = Tag#flv_tag.body_length,
	StreamId = Tag#flv_tag.streamid,
	<<PrevTagSize:32, Type:8, BodyLength:24,TimeStampAbs:32,
	StreamId:24, Body/binary>>.
	

header(#flv_header{version = Version, audio = Audio, video = Video} = FLVHeader) when is_record(FLVHeader,flv_header) -> 
	Reserved = 0,
	Offset = 9,
	PrevTag = 0,
	<<70,76,86,Version:8,Reserved:5,Audio:1,Reserved:1,Video:1,Offset:32,PrevTag:32>>;
header(Bin) when is_binary(Bin) ->
	<<70,76,86, Ver:8, _:5, Audio:1, _:1, Video:1, 0,0,0,9>> = Bin,
	#flv_header{version=Ver,audio=Audio,video=Video};
header(IoList) when is_list(IoList) -> header(iolist_to_binary(IoList)).
		

to_tag(#channel{msg = Msg,timestamp = FullTimeStamp, type = Type, stream = StreamId} = Channel, PrevTimeStamp) when is_record(Channel,channel) ->
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
	
