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
-compile(export_all).

-export([read_header/1,read_tag/2,write_header/1,write_header/2,write_tag/2,header/1,write/2,to_tag/2,tag/1]).

header(#flv_header{version = Version, audio = Audio, video = Video} = FLVHeader) when is_record(FLVHeader,flv_header) -> 
	Reserved = 0,
	Offset = 9,
	PrevTag = 0,
	<<70,76,86,Version:8,Reserved:5,Audio:1,Reserved:1,Video:1,Offset:32,PrevTag:32>>;
header(Bin) when is_binary(Bin) ->
	<<70,76,86, Ver:8, _:5, Audio:1, _:1, Video:1, 0,0,0,9>> = Bin,
	#flv_header{version=Ver,audio=Audio,video=Video};
header(IoList) when is_list(IoList) -> header(iolist_to_binary(IoList)).




read_header(IoDev) -> 
    case file:read(IoDev, ?FLV_HEADER_LENGTH) of
        {ok, Data} -> 

			{ok, iolist_size(Data), header(Data)};
        eof -> {error, unexpected_eof};
        {error, Reason} -> {error, Reason}           
    end.

read_tag(IoDev, Pos) ->
	case file:pread(IoDev,Pos, ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH) of
		{ok, IoList} ->
			case iolist_to_binary(IoList) of
			  	<<PrevTagSize:32/integer,Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24>> ->				
					case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, BodyLength) of
						{ok,IoList2} -> 
						    <<TimeStampAbs:32>> = <<TimeStampExt:8, TimeStamp:24>>,
							{ok, #flv_tag{prev_tag_size = PrevTagSize,
					         			  type          = Type,
							 			  body_length   = BodyLength,
							 			  timestamp_abs = TimeStampAbs,
							 			  streamid      = StreamId,
							 			  pos           = Pos,
							   			  nextpos       = Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + BodyLength,
							 			  body          = iolist_to_binary(IoList2)}};
						eof -> 
							{ok, done};
						{error, Reason} -> 
							{error, Reason}
					end;
				_ ->
					{error, unexpected_eof};
			end;		
        eof -> 
			{error, unexpected_eof};
        {error, Reason} -> 
			{error, Reason}
	end.


write_header(IoDev) ->
	write_header(IoDev,#flv_header{audio=1,video=1}).
write_header(IoDev, FLVHeader) ->
	Header = header(FLVHeader),
	case file:write(IoDev,Header) of
		ok -> 
			?D("Writing Header"),
			ok;
		{error,Reason} -> {error,Reason}
	end.

write_tag(IoDev, #flv_tag{type= Type, timestamp=TimeStamp, streamid = StreamId, body = Body} = _FLVTag) ->
	BodyLength = size(Body),
	TimeStampExt = 0,
	PrevTagSize = size(Body) + 11,
	Tag = <<Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Body/binary,PrevTagSize:32>>,
	case file:write(IoDev, Tag) of
        ok ->
            ?D("Writing Tag"),
			ok;
        {errror, Reason} -> {error,Reason}
	end.

to_tag(#channel{msg = Msg,timestamp = FullTimeStamp, type = Type, stream = StreamId} = Channel, PrevTimeStamp) when is_record(Channel,channel) ->
	BodyLength = size(Msg),
	<<TimeStampExt:8,TimeStamp:32>> = FullTimeStamp,
	TimeStampExt = 0,
	PrevTagSize = size(Msg) + 11,
	NewTimeStamp = case PrevTimeStamp of
		0 -> 0;
		_ -> TimeStamp + PrevTimeStamp
	end,
	{<<Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Msg/binary,PrevTagSize:32>>,NewTimeStamp}.


write(FileName,List) when is_list(List) -> 
	case list_to_binary(List) of
		Bin when is_binary(Bin) -> write(FileName,Bin);
		_ -> {error,not_binary}
	end;
write(FileName,Bin)  when is_binary(Bin) -> file:write_file(FileName,Bin).








tag(List) when is_list(List) -> tag(iolist_to_binary(List));
tag(<<PrevTagSize:32/integer,Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Rest/binary>>) ->
	case Rest of 
	<<Body:BodyLength/binary,Next/binary>> ->
		<<TimeStamp_Abs:32/integer>> = <<TimeStampExt:8, TimeStamp:24>>, 
		Tag = #flv_tag{prev_tag_size = PrevTagSize,
								 type          = Type,
								 body_length   = BodyLength,
								 timestamp     = TimeStamp,
								 timestamp_ext = TimeStampExt,
								 timestamp_abs = TimeStamp_Abs,
								 streamid      = StreamId,
								 pos = pos,
								 nextpos = nextpos,
								 body          = Body},
		{Tag,Next};
		_ -> {error,tag_error}
	end.



parse_meta(Bin) ->
	file:write_file("/sfe/temp/meta.txt",Bin),
	?D(Bin),
	{Type,String,Next} = ems_amf:parse(Bin),
%	?D(String),
%	?D(Next),
	{Type,Array,_Next} = ems_amf:parse(Next),
	{String,Array}.
	


















