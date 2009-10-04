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
-include("../include/flv.hrl").

-export([init/1,read_frame/1,to_tag/2,header/1, parse_meta/1, encode/1]).



%%--------------------------------------------------------------------
%% @spec (IoDev::iodev()) -> {ok, IoSize::integer(), Header::header()} | {error,Reason::atom()}
%% @doc Read flv file and load its frames in memory ETS
%% @end 
%%--------------------------------------------------------------------
init(#video_player{device = IoDev} = Player) -> 
  case file:read(IoDev, ?FLV_HEADER_LENGTH) of
    {ok, Data} -> 
      ?D("Going to build FLV frame list"),
      read_frame_list(Player#video_player{
        pos = iolist_size(Data),
        frames = ets:new(frames, [ordered_set, private, {keypos, 2}]),
        header = header(Data)});
    eof -> 
      {error, unexpected_eof};
    {error, Reason} -> {error, Reason}           
  end.

read_frame_list(#video_player{device = IoDev, pos = Pos, frames = FrameTable} = Player) ->
  % We need to bypass PreviousTagSize and read header.
	case file:pread(IoDev,Pos + ?FLV_PREV_TAG_SIZE_LENGTH, ?FLV_TAG_HEADER_LENGTH) of
		{ok, IoList} ->
		  <<Type, Length:24/big, TimeStamp:24/big, TimeStampExt, _StreamId:24/big>> = iolist_to_binary(IoList),
		  <<TimeStampAbs:32/big>> = <<TimeStampExt, TimeStamp:24/big>>,
		  
		  PreparedFrame = #file_frame{
		    timestamp = TimeStampAbs, 
		    offset = Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, 
		    size = Length,
		    type = Type
		  },
		  
			Frame = case Type of
			  ?FLV_TAG_TYPE_VIDEO ->
				  {FrameType, _CodecID, _Width, _Height} = extractVideoHeader(IoDev, Pos),
				  KeyFrame = case FrameType of
				    1 -> true;
				    _ -> false
			    end,
			    PreparedFrame#file_frame{
			      id = TimeStampAbs*3,
    		    keyframe = KeyFrame
			    };
        ?FLV_TAG_TYPE_AUDIO -> 
          % {SoundType, SoundSize, SoundRate, SoundFormat} =  extractAudioHeader(IoDev, Pos),
          PreparedFrame#file_frame{
            id = TimeStampAbs*3 + 1
          };
			  ?FLV_TAG_TYPE_META ->
			    PreparedFrame#file_frame{
			      id = TimeStampAbs*3 + 2
			    }
			end,
			
			ets:insert(FrameTable, Frame),
			read_frame_list(Player#video_player{pos = Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + Length});
    eof -> 
      ?D({"Read frames", ets:info(FrameTable)}),
      {ok, Player#video_player{pos = undefined}};
    {error, Reason} -> 
      {error, Reason}
  end.
  


% Reads a tag from IoDev for position Pos.
% @param IoDev
% @param Pos
% @return a valid video_frame record type
read_frame(#video_player{pos = undefined, frames = FrameTable} = Player) ->
  read_frame(Player#video_player{pos = ets:first(FrameTable)});
  
read_frame(#video_player{device = IoDev, pos = Key, frames = FrameTable} = Player) ->
  [Frame] = ets:lookup(FrameTable, Key),
  #file_frame{type = Type, offset = Offset, size = Size, timestamp = Timestamp} = Frame,
	case file:pread(IoDev, Offset, Size) of
		{ok, Data} ->
      {ok, #video_frame{type = Type, raw_body = true, body = iolist_to_binary(Data), timestamp_abs = Timestamp, nextpos = ets:next(FrameTable, Key)}, Player};
    eof ->
      {ok, done};
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


encode(_Frame) ->
  ?D({"Request to encode undefined", _Frame}).
	

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
	
