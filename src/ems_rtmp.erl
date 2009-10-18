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

-export([encode/1, encode/2, handshake/1, decode/1]).


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
  % {Chunk,Rest} = chunk(Data, ChunkSize),
  ChunkList = chunk(Data, ChunkSize, Id),
	BinId = encode_id(?RTMP_HDR_NEW,Id),
  %   NextPacket = <<BinId/binary,TimeStamp:24/big-integer,(size(Data)):24/big-integer,Type:8,StreamId:32/little,Chunk/binary>>,
  % encode(Channel, Rest, NextPacket);
  [<<BinId/binary,TimeStamp:24/big-integer,(size(Data)):24/big-integer,Type:8,StreamId:32/little>> | ChunkList].

% encode(#channel{id = Id, chunk_size = ChunkSize} = Channel, Data, Packet) -> 
%   {Chunk,Rest} = chunk(Data, ChunkSize, Id),
%   BinId = encode_id(?RTMP_HDR_CONTINUE, Id),
%   NextPacket = <<Packet/binary,BinId/binary,Chunk/binary>>,
%   encode(Channel, Rest, NextPacket).


encode_id(Type, Id) when Id > 319 -> 
	<<Type:2,?RTMP_HDR_LRG_ID:6, (Id - 64):16/big-integer>>;
encode_id(Type, Id) when Id > 63 -> 
	<<Type:2,?RTMP_HDR_MED_ID:6, (Id - 64):8>>;
encode_id(Type, Id) when Id >= 2 -> 
  <<Type:2, Id:6>>.


% chunk(Data) -> chunk(Data,?RTMP_DEF_CHUNK_SIZE).

chunk(Data, ChunkSize, Id) -> chunk(Data, ChunkSize, Id, []).

chunk(Data, ChunkSize, Id, List) when size(Data) =< ChunkSize ->
  lists:reverse([Data | List]);


chunk(Data, ChunkSize, Id, List) when is_binary(Data) ->
  <<Chunk:ChunkSize/binary,Rest/binary>> = Data,
  chunk(Rest, ChunkSize, Id, [encode_id(?RTMP_HDR_CONTINUE, Id), Chunk | List]).
		
decode(#ems_fsm{buff = <<>>} = State) -> State;
decode(#ems_fsm{} = State) -> decode_channel_id(State).

% First extracting channel id
decode_channel_id(#ems_fsm{buff = <<>>} = State) ->
  State;
decode_channel_id(#ems_fsm{buff = <<Format:2, ?RTMP_HDR_LRG_ID:6,Id:16,Rest/binary>>} = State) ->
  decode_channel_header(Format, Id + 64, Rest, State);
decode_channel_id(#ems_fsm{buff = <<Format:2, ?RTMP_HDR_MED_ID:6,Id:8,Rest/binary>>} = State) ->
  decode_channel_header(Format, Id + 64, Rest, State);
decode_channel_id(#ems_fsm{buff = <<Format:2, Id:6,Rest/binary>>} = State) ->
  decode_channel_header(Format, Id, Rest, State).

% Now extracting channel header
decode_channel_header(?RTMP_HDR_CONTINUE, Id, Rest, State) ->
  {value, Channel} = lists:keysearch(Id, #channel.id, State#ems_fsm.channels),
  decode_channel(Channel, Rest, State);

decode_channel_header(?RTMP_HDR_TS_CHG, Id, <<16#ffffff:24, TimeStamp:24, Rest/binary>>, State) ->
  {value, Channel} = lists:keysearch(Id, #channel.id, State#ems_fsm.channels),
  decode_channel(Channel#channel{timestamp = TimeStamp+16#ffffff}, Rest, State);
decode_channel_header(?RTMP_HDR_TS_CHG, Id, <<TimeStamp:24, Rest/binary>>, State) ->
  {value, Channel} = lists:keysearch(Id, #channel.id, State#ems_fsm.channels),
  decode_channel(Channel#channel{timestamp = TimeStamp}, Rest, State);
  
decode_channel_header(?RTMP_HDR_SAME_SRC, Id, <<16#ffffff:24,Length:24,Type:8,TimeStamp:24,Rest/binary>>, State) ->
  {value, Channel} = lists:keysearch(Id, #channel.id, State#ems_fsm.channels),
	decode_channel(Channel#channel{timestamp=TimeStamp+16#ffffff,length=Length,type=Type},Rest,State);
	
decode_channel_header(?RTMP_HDR_SAME_SRC, Id, <<TimeStamp:24,Length:24,Type:8,Rest/binary>>, State) ->
  {value, Channel} = lists:keysearch(Id, #channel.id, State#ems_fsm.channels),
	decode_channel(Channel#channel{timestamp=TimeStamp,length=Length,type=Type},Rest,State);

decode_channel_header(?RTMP_HDR_NEW,Id,<<16#ffffff:24,Length:24,Type:8,StreamId:32/little,TimeStamp:24,Rest/binary>>, State) ->
  case lists:keysearch(Id, #channel.id, State#ems_fsm.channels) of
    {value, Channel} -> ok;
    _ -> Channel = #channel{}
  end,
	decode_channel(Channel#channel{id=Id,timestamp=TimeStamp+16#ffffff,length=Length,type=Type,stream=StreamId},Rest,State);
	
decode_channel_header(?RTMP_HDR_NEW,Id,<<TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>, State) ->
  case lists:keysearch(Id, #channel.id, State#ems_fsm.channels) of
    {value, Channel} -> ok;
    _ -> Channel = #channel{}
  end,
	decode_channel(Channel#channel{id=Id,timestamp=TimeStamp,length=Length,type=Type,stream=StreamId},Rest,State);

decode_channel_header(_Type, _Id, _Rest, State) -> % Still small buffer
  State.

% Now trying to fill channel with required data
bytes_for_channel(#channel{length = Length, msg = Msg}, #ems_fsm{client_chunk_size = ChunkSize}) ->
  ?D({"Required:", Length, size(Msg), ChunkSize}),
  RemainingBytes = Length - size(Msg),
  if
    RemainingBytes < ChunkSize -> RemainingBytes;
    true -> ChunkSize
  end.
  

decode_channel(Channel, Data, State) ->
	BytesRequired = bytes_for_channel(Channel, State),
	push_channel_packet(Channel, Data, State, BytesRequired).
	
	
% Nothing to do when buffer is small

push_channel_packet(#channel{} = Channel, Data, State, BytesRequired) when size(Data) < BytesRequired -> 
  State;
  
% And decode channel when bytes required are in buffer
push_channel_packet(#channel{msg = Msg} = Channel, Data, State, BytesRequired) -> 
  <<Chunk:BytesRequired/binary, Rest/binary>> = Data,
  decode_channel_packet(Channel#channel{msg = <<Msg/binary, Chunk/binary>>}, State#ems_fsm{buff = Rest}).

% When chunked packet hasn't arived, just accumulate it
decode_channel_packet(#channel{msg = Msg, length = Length} = Channel, #ems_fsm{channels = Channels} = State) when size(Msg) < Length ->
  NextChannelList = channel_put(Channel, Channels),
  decode(State#ems_fsm{channels=NextChannelList});

% Work with packet when it has accumulated and flush buffers
decode_channel_packet(#channel{msg = Msg, length = Length} = Channel, #ems_fsm{channels = Channels} = State) when size(Msg) == Length ->
  NewState = command(Channel, State), % Perform Commands here
  NextChannelList = channel_put(Channel#channel{msg = <<>>}, Channels),
  decode(NewState#ems_fsm{channels=NextChannelList}).


command(#channel{type = ?RTMP_TYPE_WINDOW_ACK_SIZE, msg = <<WindowSize:32/big-integer>>} = _Channel, State) ->
  ?D({"Window acknolegement size", WindowSize}),
  State;

command(#channel{type = ?RTMP_TYPE_CHUNK_SIZE, msg = <<ChunkSize:32/big-integer>>} = _Channel, State) ->
  % ?D({"Change Chunk Size",Channel,ChunkSize}),
	State#ems_fsm{client_chunk_size = ChunkSize};

command(#channel{type = ?RTMP_TYPE_BYTES_READ, msg = <<_Length:32/big-integer>>} = _Channel, State) ->
  % ?D({"Stream bytes read: ", _Length}),
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

command(#channel{type = ?RTMP_INVOKE_AMF0} = Channel, State) ->
	AMF = ems_amf:decode(Channel#channel.msg),
	#amf{command = Command} = AMF,
	App = ems:check_app(State,Command, 2),
	App:Command(AMF, State);
	
command(#channel{type = Type}, State) ->
  ?D({"Unhandled message type", Type}),
  State.


channel_get(Channel,ChannelList) when is_record(Channel,channel) -> channel_get(Channel#channel.id,ChannelList);
channel_get(ChannelId,ChannelList) when is_integer(ChannelId) ->
	case lists:keysearch(ChannelId, 2, ChannelList) of
		{value,Channel} when is_record(Channel,channel) -> Channel;
		_ -> undefined
	end.

%% Make sure that the First channel is the most recent channel
channel_put(Channel,ChannelList) ->
	lists:keytake(Channel#channel.id, #channel.id, ChannelList),
	[Channel | ChannelList].

choose_value(Channel, OrigChannel, Position) ->
  case element(Position, Channel) of
    undefined -> element(Position, OrigChannel);
    Value -> Value
  end.

channel_merge(Channel, #ems_fsm{channels = Channels}) -> channel_merge(Channel,Channels);
channel_merge(Channel,ChannelList) ->
	case channel_get(Channel,ChannelList) of
		undefined -> {Channel,ChannelList};
		OrigChannel ->
			TimeStamp = choose_value(Channel, OrigChannel, #channel.timestamp),
			Length    = choose_value(Channel, OrigChannel, #channel.length),
			Type      = choose_value(Channel, OrigChannel, #channel.type),
			StreamId  = choose_value(Channel, OrigChannel, #channel.stream),
			NewChannel = OrigChannel#channel{timestamp=TimeStamp,length=Length,type=Type,stream=StreamId},
			?D({"Merge", Channel, NewChannel}),
			NewChannelList = channel_put(NewChannel,ChannelList),
			{NewChannel,NewChannelList}
	end.
