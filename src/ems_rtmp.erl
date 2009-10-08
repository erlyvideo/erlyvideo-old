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
-author('max@maxidoors.ru').
-include("../include/ems.hrl").

-export([encode/1, encode/2, handshake/1, decode/2, chunk/1]).


handshake(C1) when is_binary(C1) -> 
  <<(rtmp_handshake:s1())/binary, (rtmp_handshake:s2(C1))/binary>>.

encode(#channel{msg = Msg} = Channel) ->
    encode(Channel,Msg,<<>>).

encode(#channel{} = Channel, #amf{} = AMF) -> 
	encode(Channel,ems_amf:encode(AMF));

encode(#channel{} = Channel, Data) when is_binary(Data) -> 
	encode(Channel,Data,<<>>).

encode(_Channel, <<>>, Packet) -> Packet;


encode(#channel{id = Id, timestamp = TimeStamp, type= Type, stream = StreamId, chunk_size = ChunkSize} = Channel, Data, <<>>) -> 
	{Chunk,Rest} = chunk(Data, ChunkSize),
	BinId = encode_id(?RTMP_HDR_NEW,Id),
	NextPacket = <<BinId/binary,TimeStamp:24/big-integer,(size(Data)):24/big-integer,Type:8,StreamId:32/little,Chunk/binary>>,
	encode(Channel, Rest, NextPacket);

encode(#channel{id = Id, chunk_size = ChunkSize} = Channel, Data, Packet) -> 
	{Chunk,Rest} = chunk(Data, ChunkSize),
	BinId = encode_id(?RTMP_HDR_CONTINUE, Id),
	NextPacket = <<Packet/binary,BinId/binary,Chunk/binary>>,
	encode(Channel, Rest, NextPacket).


encode_id(Type, Id) when Id > 319 -> 
	NewId = Id - 64,
	<<Type:2,?RTMP_HDR_LRG_ID:6,NewId:16/big-integer>>;
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
					NewState = command(NextChannel,State), % Perform Commands here
					NextChannelList = channel_put(NextChannel#channel{msg = <<>>},NewChannelList),
					NextState = NewState#ems_fsm{channels=NextChannelList,complete = true, prev_buff = <<>>},
					decode(Next,NextState); 
				next_packet     -> % add case to determine if the processing is done even though the binary is not empty.
					NextChannelList = channel_put(NewChannel,NewChannelList),
					State#ems_fsm{channels=NextChannelList, prev_buff = Chunk, complete = false};
				continue -> 
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

command(#channel{type = ?RTMP_TYPE_WINDOW_ACK_SIZE, msg = <<WindowSize:32/big-integer>>} = _Channel, State) ->
  ?D({"Window acknolegement size", WindowSize}),
  State;

command(#channel{type = ?RTMP_TYPE_CHUNK_SIZE, msg = <<ChunkSize:32/big-integer>>} = _Channel, State) ->
  % ?D({"Change Chunk Size",Channel,ChunkSize}),
	State#ems_fsm{client_chunk_size = ChunkSize};

command(#channel{type = ?RTMP_TYPE_BYTES_READ, msg = <<_Length:32/big-integer>>} = _Channel, State) ->
  ?D({"Stream bytes read: ", _Length}),
	State;
	
command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_PING:16/big-integer, Timestamp:32/big-integer>>} = Channel, State) ->
  gen_fsm:send_event(self(), {send, {Channel, <<?RTMP_CONTROL_STREAM_PONG:16/big-integer, Timestamp:32/big-integer>>}}),
	State;	

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_BUFFER:16/big-integer, _StreamId:32/big-integer, BufferSize:32/big-integer>>} = _Channel, 
        #ems_fsm{video_player = Player} = State) ->
  % ?D({"Buffer size on stream id", BufferSize, _StreamId}),
  case Player of
    undefined -> ok;
    _ -> gen_fsm:send_event(Player, {client_buffer, BufferSize})
  end,
	State#ems_fsm{client_buffer = BufferSize};	


command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<EventType:16/big-integer, _/binary>>} = _Channel, State) ->
	?D({"Ping - ignoring", EventType}),
	State;	


command(#channel{type = Type} = Channel, State) 
	when (Type =:= ?RTMP_TYPE_AUDIO) or (Type =:= ?RTMP_TYPE_VIDEO) or (Type =:= ?RTMP_TYPE_METADATA) ->
%	?D({"Recording",Type}),
	gen_fsm:send_event(self(), {publish, Channel}),
	State;

command(#channel{type = ?RTMP_TYPE_INVOKE} = Channel, State) ->
	case ems_amf:decode(Channel#channel.msg) of
		#amf{command = Command} = AMF ->
			{App,NextState} = case Command of
				connect -> 
  				gen_fsm:send_event(self(), {send, {Channel#channel{id = 2, type = ?RTMP_TYPE_WINDOW_ACK_SIZE, msg = <<>>}, <<0,16#26, 16#25,16#a0>>}}),
    			gen_fsm:send_event(self(), {send, {Channel#channel{id = 2, type = ?RTMP_TYPE_BW_PEER, msg = <<>>}, <<0,16#26, 16#25,16#a0, 16#02>>}}),
				  case AMF#amf.args of
    				[{object, PlayerInfo}, {string, _Session}, {string, SUserId}] ->
    				  {UserId, _} = string:to_integer(SUserId),
    	        ems_cluster:add_client(erlang:pid_to_list(self()), UserId, self()),
    					{'apps_rtmp',State#ems_fsm{player_info = PlayerInfo, user_id = UserId}};
    				[{object, PlayerInfo}, {string, _Session}, {number, UserId}] ->
    	        ems_cluster:add_client(erlang:pid_to_list(self()), UserId, self()),
    					{'apps_rtmp',State#ems_fsm{player_info = PlayerInfo, user_id = UserId}};
				    [{object, PlayerInfo} | _] ->
    					{'apps_rtmp',State#ems_fsm{player_info = PlayerInfo, user_id = undefined}};
    				_Msg -> 
    				  ?D({"Unknown player connect", _Msg})
    			end;
				_ -> {ems:check_app(State,Command, 2),State}
			end,
			App:Command(AMF, NextState);
		_ -> 
			?D("AMF Error"),
			State
			
	end;
	
command(#channel{type = Type}, State) ->
  ?D({"Unhandled message type", Type}),
  State.


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








chunk_size(#channel{length = Length}, #ems_fsm{client_chunk_size = ChunkSize}, Size) ->
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
