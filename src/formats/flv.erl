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
-module(flv).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../include/ems.hrl").
-include("../../include/flv.hrl").
-include("../../include/media_info.hrl").

-export([init/1, read_frame/2, codec_config/2, read_frame_list/3, header/1]).
-behaviour(gen_format).



%%--------------------------------------------------------------------
%% @spec (IoDev::iodev()) -> {ok, IoSize::integer(), Header::header()} | {error,Reason::atom()}
%% @doc Read flv file and load its frames in memory ETS
%% @end 
%%--------------------------------------------------------------------
init(#media_info{device = IoDev} = MediaInfo) -> 
  case file:read(IoDev, ?FLV_HEADER_LENGTH) of
    {ok, Data} -> 
      read_frame_list(MediaInfo#media_info{
          frames = ets:new(frames, [ordered_set, public, {keypos, #file_frame.id}]),
          header = header(Data)}, size(Data), 0);
    eof -> 
      {error, unexpected_eof};
    {error, Reason} -> {error, Reason}           
  end.

read_frame_list(#media_info{} = MediaInfo, Offset, FrameCount) when FrameCount == 10 ->
  spawn_link(?MODULE, read_frame_list, [MediaInfo, Offset, FrameCount + 1]),
  {ok, MediaInfo};
  

read_frame_list(#media_info{device = Device, frames = FrameTable} = MediaInfo, Offset, FrameCount) ->
  % We need to bypass PreviousTagSize and read header.
	case file:pread(Device,Offset + ?FLV_PREV_TAG_SIZE_LENGTH, ?FLV_TAG_HEADER_LENGTH) of
		{ok, <<Type, Length:24/big, TimeStamp:24/big, TimeStampExt, _StreamId:24/big>>} ->
		  <<TimeStampAbs:32/big>> = <<TimeStampExt, TimeStamp:24/big>>,
		  
		  PreparedFrame = #file_frame{
		    timestamp = TimeStampAbs, 
		    offset = Offset + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, 
		    size = Length,
		    type = Type
		  },
		  
			Frame = case Type of
			  ?FLV_TAG_TYPE_VIDEO ->
				  {FrameType, _CodecID, _Width, _Height} = extractVideoHeader(Device, Offset),
				  KeyFrame = case FrameType of
				    ?FLV_VIDEO_FRAME_TYPE_KEYFRAME -> true;
				    _ -> false
			    end,
			    PreparedFrame#file_frame{
			      id = TimeStampAbs*3 + 1,
    		    keyframe = KeyFrame
			    };
        ?FLV_TAG_TYPE_AUDIO -> 
          {_SoundType, _SoundSize, _SoundRate, _SoundFormat} =  extractAudioHeader(Device, Offset),
          PreparedFrame#file_frame{
            id = TimeStampAbs*3 + 2
          };
			  ?FLV_TAG_TYPE_META ->
			    PreparedFrame#file_frame{
			      id = TimeStampAbs*3
			    }
			end,
			
			ets:insert(FrameTable, Frame),
			read_frame_list(MediaInfo, Offset + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + Length, FrameCount + 1);
    eof -> 
      {ok, MediaInfo};
    {error, Reason} -> 
      {error, Reason}
  end.
  
codec_config(_, _) -> undefined.

% Reads a tag from IoDev for position Pos.
% @param IoDev
% @param Pos
% @return a valid video_frame record type
read_frame(#media_info{device = IoDev, frames = FrameTable}, Key) ->
  [Frame] = ets:lookup(FrameTable, Key),
  #file_frame{offset = Offset, size = Size} = Frame,
	case file:pread(IoDev, Offset, Size) of
		{ok, Data} -> video_frame(Frame, Data);
    eof -> done;
    {error, Reason} -> {error, Reason}
  end.
  
video_frame(#file_frame{type = Type, timestamp = Timestamp}, Data) ->
  #video_frame{type = Type, raw_body = true, body = Data, timestamp = Timestamp}.

	
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
   {ok, <<FrameType:4, CodecID:4>>} ->
				{Width, Height} = getWidthHeight(IoDev, Pos, CodecID),
				{FrameType, CodecID, Width, Height};
		eof -> 
			 {ok, done};
		{error, Reason} -> 
			 {error, Reason}
end.



decodeScreenVideo(IoDev, Pos) ->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 4) of
		{ok, <<_Offset:4, Width:12, Height:12, _Rest:4>>} -> {Width, Height}
	end.
	
decodeSorensen(IoDev, Pos) ->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 9) of
		{ok, IoList} ->
		  <<_Offset:30, Info:3, _Rest:39>> = IoList,
			case Info of
				
				0 -> <<_:30, _:3, Width1:8, Height1:8, _Rest1:23>> = IoList, {Width1, Height1};
				1 -> <<_:30, _:3, Width2:16, Height2:16, _Rest2:7>> = IoList, {Width2, Height2};
				2 -> {352, 288};
				3 -> {176, 144};
				4 -> {128, 96};
				5 -> {320, 240};
				6 -> {160, 120}
			end
	end.

decodeVP6(IoDev, Pos)->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 6) of
			{ok, <<HeightHelper:4, WidthHelper:4, _Offset:24, Width:8, Height:8>>} -> {Width*16-WidthHelper, Height*16-HeightHelper}
	end.
% Extracts audio header information for a tag.
% @param IoDev
% @param Pos
% @return {SoundType, SoundSize, SoundRate, SoundFormat}
extractAudioHeader(IoDev, Pos) ->	
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, 1) of   
	  {ok, <<SoundFormat:4, SoundRate:2, SoundSize:1, SoundType:1>>} -> {SoundType, SoundSize, SoundRate, SoundFormat};
		eof -> {ok, done};
		{error, Reason} -> {error, Reason}
  end.



-spec header(#flv_header{} | <<_:72>>) -> #flv_header{version::byte(),audio::0 | 1,video::0 | 1}.

header(#flv_header{version = Version, audio = Audio, video = Video}) -> 
	Reserved = 0,
	Offset = 9,
	PrevTag = 0,
	<<70,76,86,Version:8,Reserved:5,Audio:1,Reserved:1,Video:1,Offset:32,PrevTag:32>>;
header(Bin) when is_binary(Bin) ->
	<<70,76,86, Ver:8, _:5, Audio:1, _:1, Video:1, 0,0,0,9>> = Bin,
	#flv_header{version=Ver,audio=Audio,video=Video}.
		


