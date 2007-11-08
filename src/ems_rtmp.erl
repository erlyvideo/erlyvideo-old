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
-module(ems_rtmp).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").

-compile(export_all).



handshake(Bin) when is_binary(Bin) -> <<Bin/binary,Bin/binary>>.


encode(Channel,AMF)	when is_record(Channel,channel), is_record(AMF,amf) -> 
	?D({"Encode", Channel, AMF}),
	encode(Channel,ems_amf:encode(AMF));
encode(Channel,Data) when is_record(Channel,channel), is_binary(Data) -> 
	encode(Channel,Data,<<>>).

encode(_Channel, <<>>, Packet) -> Packet;
encode(#channel{id = Id, timestamp = TimeStamp, type= Type, stream = StreamId} = Channel, Data, <<>>) -> 
	Length = size(Data),
	{Chunk,Rest} = chunk(Data),
	BinId = encode_id(?RTMP_HDR_NEW,Id),
	NextPacket = <<BinId/binary,TimeStamp:24,Length:24,Type:8,StreamId:32/little,Chunk/binary>>,
	encode(Channel, Rest, NextPacket);
encode(#channel{id = Id} = Channel, Data, Packet) -> 
	{Chunk,Rest} = chunk(Data),
	BinId = encode_id(?RTMP_HDR_CONTINUE, Id),
	NextPacket = <<Packet/binary,BinId/binary,Chunk/binary>>,
	encode(Channel, Rest, NextPacket).

encode_id(Type, Id) when Id > 319 -> 
	NewId = Id - 64,
	<<Type:2,?RTMP_HDR_LRG_ID:6,NewId:16>>;
encode_id(Type, Id) when Id > 63 -> 
	NewId = Id - 64,
	<<Type:2,?RTMP_HDR_MED_ID:6,NewId:8>>;
encode_id(Type, Id) -> <<Type:2, Id:6>>.


chunk(Data) -> chunk(Data,?RTMP_DEF_CHUNK_SIZE).
chunk(Data,ChunkSize) ->
	if
		size(Data) < ChunkSize -> {Data,<<>>};
		true ->
			<<Chunk:ChunkSize/binary,Rest/binary>> = Data,
			{Chunk,Rest}
	end.
		




decode(<<>>,State) -> State;
decode(Bin,State) ->
%	?D("Decode"),
	case State#ems_fsm.complete of
		true ->
			{Channel,Rest} = header(Bin),
			{NewChannel,NewChannelList} = channel_merge(Channel,State),
			{Chunk,Next} = get_chunk(Channel,State,Rest);
		false ->
			NewChannelList = State#ems_fsm.channels,
			NewChannel = lists:nth(1,NewChannelList),
			Prev = State#ems_fsm.prev_buff,
			Rest = <<Prev/binary,Bin/binary>>,
			{Chunk,Next} = get_chunk(NewChannel,State,Rest)
	end,
	
	case NewChannel#channel.msg of
		Msg when is_binary(Msg) ->
			Message = <<Msg/binary,Chunk/binary>>,
			NextChannel = NewChannel#channel{msg=Message},
			case check_message(NextChannel,State,Next) of
				complete  -> 
%					?D("Comlpete"),
					NewState = command(NextChannel,State), % Perform Commands here
					NextChannelList = channel_put(NextChannel#channel{msg = <<>>},NewChannelList),
					NextState = NewState#ems_fsm{channels=NextChannelList,complete = true, prev_buff = <<>>},
					decode(Next,NextState); 
				next_packet     -> % add case to determine if the processing is done even though the binary is not empty.
%					?D("Next Packet"),
					NextChannelList = channel_put(NewChannel,NewChannelList),
					State#ems_fsm{channels=NextChannelList, prev_buff = Chunk, complete = false};
				continue -> 
%					?D({"Continue",NextChannel#channel.length,size(Message),size(Next),State#ems_fsm.chunk_size}),
					NextChannelList = channel_put(NextChannel,NewChannelList),
					NextState = State#ems_fsm{channels=NextChannelList, complete = true, prev_buff = <<>>},
					decode(Next,NextState)
			end;
		undefined -> 
			?D({"Bad Channel: ",NewChannel#channel.id}),
			gen_fsm:send_event(self(), {stop}),
			decode(<<>>, State)
	end.

check_message(#channel{length = Length, msg = Msg} = _Channel,_State,_Bin) 
	when Length == size(Msg) -> complete;
check_message(#channel{length = Length, msg = Msg} = _Channel,_State,Bin) 
	when Length > size(Msg), size(Bin) == 0 -> next_packet;
check_message(_Channel,_State,_Bin) -> continue.

get_chunk(Channel,State,Bin) ->
	ChunkSize = chunk_size(Channel,State,size(Bin)),
	<<Chunk:ChunkSize/binary,Next/binary>> = Bin,
	{Chunk,Next}.


command(#channel{type = ?RTMP_TYPE_CHUNK_SIZE} = Channel, State) ->
	<<ChunkSize:32>> = Channel#channel.msg,
	?D({"Change Chunk Size",Channel,ChunkSize}),
	State#ems_fsm{chunk_size=ChunkSize};

command(#channel{type = ?RTMP_TYPE_PING} = _Channel, State) ->
	?D("Ping - ignoring"),
	State;

command(#channel{type = Type} = Channel, State) 
	when (Type =:= ?RTMP_TYPE_AUDIO) or (Type =:= ?RTMP_TYPE_VIDEO) or (Type =:= ?RTMP_TYPE_META_DATA) ->
%	?D({"Recording",Type}),
	gen_fsm:send_event(self(), {record, Channel}),
	State;


command(#channel{type = ?RTMP_TYPE_INVOKE} = Channel, State) ->
	case ems_amf:decode(Channel#channel.msg) of
		AMF when is_record(AMF,amf) -> 
			Command = AMF#amf.command,
			{App,NextState} = case Command of
				connect -> 
					[{object,PlayerInfo}] = AMF#amf.args,
					{gen_rtmp,State#ems_fsm{player_info=PlayerInfo}};
				_ -> {check_app(State,Command),State}
			end,
			?D({"AppCmd: ", Channel, AMF}),
			App:Command(self(), AMF, Channel),
			NextState;
		_ -> 
			?D("AMF Error"),
			State
			
	end.



check_app(State,Command) when is_record(State,ems_fsm) -> check_app(State#ems_fsm.player_info,Command);
check_app(PlayerInfo,Command) ->
	case lists:keysearch(app,1,PlayerInfo) of
		{value,{app,[]}} -> 
			gen_rtmp;		 
		{value,{app,Value}} -> 		
			case code:get_object_code(list_to_atom(Value)) of
				{App,_Obj,BeamPath} -> 
					case beam_lib:chunks(BeamPath,[exports]) of
						{ok,{_Mod,[{exports,Exports}]}} ->
							case lists:member({Command,3},Exports) of
								true -> App;
								false -> {error,command_not_found}
							end;
						{error,_BeamLib,Reason} -> {error,Reason}
					end;
				error -> {error,no_module}
			end;
		_ -> 
			gen_rtmp
	end.



header(<<?RTMP_HDR_CONTINUE:2,?RTMP_HDR_MED_ID:6,Id:8,Rest/binary>>) ->
	{#channel{id=Id + 64},Rest};
header(<<?RTMP_HDR_CONTINUE:2,?RTMP_HDR_LRG_ID:6,Id:16,Rest/binary>>) ->
	{#channel{id=Id + 64},Rest};
header(<<?RTMP_HDR_CONTINUE:2,Id:6,Rest/binary>>) ->
	{#channel{id=Id},Rest};
header(<<?RTMP_HDR_TS_CHG:2,?RTMP_HDR_MED_ID:6,Id:8,TimeStamp:24,Rest/binary>>) ->
	{#channel{id=Id + 64,timestamp=TimeStamp},Rest};
header(<<?RTMP_HDR_TS_CHG:2,?RTMP_HDR_LRG_ID:6,Id:16,TimeStamp:24,Rest/binary>>) ->
	{#channel{id=Id + 64,timestamp=TimeStamp},Rest};
header(<<?RTMP_HDR_TS_CHG:2,Id:6,TimeStamp:24,Rest/binary>>) ->
	{#channel{id=Id,timestamp=TimeStamp},Rest};
header(<<?RTMP_HDR_SAME_SRC:2,?RTMP_HDR_MED_ID:6,Id:8,TimeStamp:24,Length:24,Type:8,Rest/binary>>) ->
	{#channel{id=Id + 64,timestamp=TimeStamp,length=Length,type=Type},Rest};
header(<<?RTMP_HDR_SAME_SRC:2,?RTMP_HDR_LRG_ID:6,Id:16,TimeStamp:24,Length:24,Type:8,Rest/binary>>) ->
	{#channel{id=Id + 64,timestamp=TimeStamp,length=Length,type=Type},Rest};
header(<<?RTMP_HDR_SAME_SRC:2,Id:6,TimeStamp:24,Length:24,Type:8,Rest/binary>>) ->
	{#channel{id=Id,timestamp=TimeStamp,length=Length,type=Type},Rest};
header(<<?RTMP_HDR_NEW:2,?RTMP_HDR_MED_ID:6,Id:8,TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>) ->
	{#channel{id=Id + 64,timestamp=TimeStamp,length=Length,type=Type,stream=StreamId,msg= <<>>},Rest};
header(<<?RTMP_HDR_NEW:2,?RTMP_HDR_LRG_ID:6,Id:16,TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>) ->
	{#channel{id=Id + 64,timestamp=TimeStamp,length=Length,type=Type,stream=StreamId,msg= <<>>},Rest};
header(<<?RTMP_HDR_NEW:2,Id:6,TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>) ->
	{#channel{id=Id,timestamp=TimeStamp,length=Length,type=Type,stream=StreamId,msg= <<>>},Rest}.








chunk_size(Channel,State,Size) ->
	Length = Channel#channel.length,
	ChunkSize = State#ems_fsm.chunk_size,
	if
		Length < ChunkSize -> Length;
		Size < Length, Size < ChunkSize -> Size;		
		true -> ChunkSize
	end.


	




channel_get(Channel,ChannelList) when is_record(Channel,channel) -> channel_get(Channel#channel.id,ChannelList);
channel_get(ChannelId,ChannelList) when is_integer(ChannelId) ->
	case lists:keysearch(ChannelId, 2, ChannelList) of
		{value,Channel} when is_record(Channel,channel) -> Channel;
		_ -> undefined
	end.

%% Make sure that the First channel is the most recent channel
channel_put(Channel,ChannelList) -> 
	case lists:keysearch(Channel#channel.id, 2, ChannelList) of
		{value,OrigChannel} when is_record(OrigChannel,channel) -> 
			List = lists:keydelete(Channel#channel.id,2,ChannelList),
			[Channel | List];
		_ -> [Channel | ChannelList]
	end.


channel_merge(Channel,State) when is_record(State,ems_fsm) -> channel_merge(Channel,State#ems_fsm.channels);
channel_merge(Channel,ChannelList) ->
	case channel_get(Channel,ChannelList) of
		undefined -> {Channel,ChannelList};
		OrigChannel ->
			TimeStamp = case Channel#channel.timestamp of undefined -> OrigChannel#channel.timestamp; _ -> Channel#channel.timestamp end,
			Length    = case Channel#channel.length    of undefined -> OrigChannel#channel.length;    _ -> Channel#channel.length    end,
			Type      = case Channel#channel.type      of undefined -> OrigChannel#channel.type;      _ -> Channel#channel.type      end,
			StreamId  = case Channel#channel.stream    of undefined -> OrigChannel#channel.stream;    _ -> Channel#channel.stream    end,
			Msg       = case Channel#channel.msg       of undefined -> OrigChannel#channel.msg;       _ -> Channel#channel.msg       end,
			NewChannel = OrigChannel#channel{timestamp=TimeStamp,length=Length,type=Type,stream=StreamId,msg=Msg},
			NewChannelList = channel_put(NewChannel,ChannelList),
			{NewChannel,NewChannelList}
	end.