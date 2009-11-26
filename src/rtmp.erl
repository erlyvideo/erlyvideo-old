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
-module(rtmp).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('max@maxidoors.ru').
-include("../include/ems.hrl").

-export([encode/1, encode/2, handshake/1, decode/1]).


handshake(C1) when is_binary(C1) -> 
  [rtmp_handshake:s1(), rtmp_handshake:s2(C1)].

encode(#channel{msg = Msg} = Channel) ->
    encode(Channel,Msg,<<>>).

encode(#channel{type = ?RTMP_INVOKE_AMF0} = Channel, #amf{} = AMF) -> 
	encode(Channel,amf0:encode(AMF));

encode(#channel{type = ?RTMP_INVOKE_AMF3} = Channel, #amf{} = AMF) -> 
	encode(Channel,amf3:encode(AMF));

encode(#channel{} = Channel, Data) when is_binary(Data) -> 
	encode(Channel,Data,<<>>).

encode(_Channel, <<>>, Packet) -> Packet;

encode(#channel{timestamp = TimeStamp} = Channel, Data, Buffer) when is_float(TimeStamp) -> 
  encode(Channel#channel{timestamp = round(TimeStamp)}, Data, Buffer);

encode(#channel{id = Id, timestamp = TimeStamp, type= Type, stream = StreamId, chunk_size = ChunkSize} = _Channel, Data, <<>>) -> 
  % {Chunk,Rest} = chunk(Data, ChunkSize),
  ChunkList = chunk(Data, ChunkSize, Id),
	BinId = encode_id(?RTMP_HDR_NEW,Id),
  %   NextPacket = <<BinId/binary,TimeStamp:24/big-integer,(size(Data)):24/big-integer,Type:8,StreamId:32/little,Chunk/binary>>,
  % encode(Channel, Rest, NextPacket);
  [<<BinId/binary,TimeStamp:24,(size(Data)):24,Type:8,StreamId:32/little>> | ChunkList].

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

chunk(Data, ChunkSize, _Id, List) when size(Data) =< ChunkSize ->
  lists:reverse([Data | List]);


chunk(Data, ChunkSize, Id, List) when is_binary(Data) ->
  <<Chunk:ChunkSize/binary,Rest/binary>> = Data,
  chunk(Rest, ChunkSize, Id, [encode_id(?RTMP_HDR_CONTINUE, Id), Chunk | List]).
		
decode(#rtmp_client{buff = <<>>} = State) -> State;
decode(#rtmp_client{} = State) -> decode_channel_id(State).

% First extracting channel id
decode_channel_id(#rtmp_client{buff = <<>>} = State) ->
  State;
decode_channel_id(#rtmp_client{buff = <<Format:2, ?RTMP_HDR_LRG_ID:6,Id:16,Rest/binary>>} = State) ->
  decode_channel_header(Rest, Format, Id + 64, State);
decode_channel_id(#rtmp_client{buff = <<Format:2, ?RTMP_HDR_MED_ID:6,Id:8,Rest/binary>>} = State) ->
  decode_channel_header(Rest, Format, Id + 64, State);
decode_channel_id(#rtmp_client{buff = <<Format:2, Id:6,Rest/binary>>} = State) ->
  decode_channel_header(Rest, Format, Id, State).

% Now extracting channel header
decode_channel_header(Rest, ?RTMP_HDR_CONTINUE, Id, State) ->
  {value, #channel{msg = Msg, timestamp = Timestamp, delta = Delta} = Channel} = lists:keysearch(Id, #channel.id, State#rtmp_client.channels),
  Channel1 = case size(Msg) of
    0 -> Channel#channel{timestamp = Timestamp + Delta};
    _ -> Channel
  end,
  % case Channel#channel.type of
  %   8 -> ?D({"    Continue", Id, size(Msg), size(Rest), Channel1#channel.length, Delta, Channel1#channel.timestamp});
  %   _ -> ok
  % end,
  decode_channel(Channel1, Rest, State);

decode_channel_header(<<16#ffffff:24, TimeStamp:24, Rest/binary>>, ?RTMP_HDR_TS_CHG, Id, State) ->
  {value, Channel} = lists:keysearch(Id, #channel.id, State#rtmp_client.channels),
  decode_channel(Channel#channel{timestamp = TimeStamp+16#ffffff, delta = undefined}, Rest, State);
  
decode_channel_header(<<Delta:24, Rest/binary>>, ?RTMP_HDR_TS_CHG, Id, State) ->
  {value, #channel{timestamp = TimeStamp} = Channel} = lists:keysearch(Id, #channel.id, State#rtmp_client.channels),
  % case Channel#channel.type of
  %   8 -> ?D({"        TSDelta", Id, size(Channel#channel.msg), size(Rest), Channel#channel.length, Delta, TimeStamp+Delta});
  %   _ -> ok
  % end,
  decode_channel(Channel#channel{timestamp = TimeStamp + Delta, delta = Delta}, Rest, State);
  
decode_channel_header(<<16#ffffff:24,Length:24,Type:8,TimeStamp:24,Rest/binary>>, ?RTMP_HDR_SAME_SRC, Id, State) ->
  {value, Channel} = lists:keysearch(Id, #channel.id, State#rtmp_client.channels),
	decode_channel(Channel#channel{timestamp=TimeStamp+16#ffffff, delta = undefined, length=Length,type=Type},Rest,State);
	
decode_channel_header(<<Delta:24,Length:24,Type:8,Rest/binary>>, ?RTMP_HDR_SAME_SRC, Id, State) ->
  {value, #channel{timestamp = TimeStamp} = Channel} = lists:keysearch(Id, #channel.id, State#rtmp_client.channels),
  % case Channel#channel.type of
  %   8 -> ?D({"          NewTS", Id, size(Channel#channel.msg), Length, Channel#channel.length, Delta, TimeStamp+Delta});
  %   _ -> ok
  % end,
	decode_channel(Channel#channel{timestamp=TimeStamp + Delta, delta = Delta, length=Length,type=Type},Rest,State);

decode_channel_header(<<16#ffffff:24,Length:24,Type:8,StreamId:32/little,TimeStamp:24,Rest/binary>>,?RTMP_HDR_NEW,Id, State) ->
  case lists:keysearch(Id, #channel.id, State#rtmp_client.channels) of
    {value, Channel} -> ok;
    _ -> Channel = #channel{}
  end,
	decode_channel(Channel#channel{id=Id,timestamp=TimeStamp+16#ffffff,delta = undefined, length=Length,type=Type,stream=StreamId},Rest,State);
	
decode_channel_header(<<TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>,?RTMP_HDR_NEW,Id, State) ->
  case lists:keysearch(Id, #channel.id, State#rtmp_client.channels) of
    {value, Channel} -> ok;
    _ -> Channel = #channel{}
  end,
  % case Type of
  %   8 -> ?D({"      New", Id, Type, 0, TimeStamp});
  %   _ -> ok
  % end,
	decode_channel(Channel#channel{id=Id,timestamp=TimeStamp,delta = undefined, length=Length,type=Type,stream=StreamId},Rest,State);

decode_channel_header(_Rest,_Type, _Id,  State) -> % Still small buffer
  State.

% Now trying to fill channel with required data
bytes_for_channel(#channel{length = Length, msg = Msg}, _) when size(Msg) == Length ->
  0;

bytes_for_channel(#channel{length = Length, msg = Msg}, #rtmp_client{client_chunk_size = ChunkSize}) when Length - size(Msg) < ChunkSize ->
  Length - size(Msg);
  
bytes_for_channel(_, #rtmp_client{client_chunk_size = ChunkSize}) -> ChunkSize.
  

decode_channel(Channel, Data, State) ->
	BytesRequired = bytes_for_channel(Channel, State),
  % ?D({"Channels:",lists:map(fun(#channel{id = Id}) -> Id end, State#rtmp_client.channels)}),
	push_channel_packet(Channel, Data, State, BytesRequired).
	
% Nothing to do when buffer is small

push_channel_packet(#channel{} = _Channel, Data, State, BytesRequired) when size(Data) < BytesRequired ->
  State;
  
% And decode channel when bytes required are in buffer
push_channel_packet(#channel{msg = Msg} = Channel, Data, State, BytesRequired) -> 
  <<Chunk:BytesRequired/binary, Rest/binary>> = Data,
  decode_channel_packet(Channel#channel{msg = <<Msg/binary, Chunk/binary>>}, State#rtmp_client{buff = Rest}).



% When chunked packet hasn't arived, just accumulate it
decode_channel_packet(#channel{msg = Msg, length = Length} = Channel, #rtmp_client{channels = Channels} = State) when size(Msg) < Length ->
  NextChannelList = lists:keystore(Channel#channel.id, #channel.id, Channels, Channel),
  decode(State#rtmp_client{channels=NextChannelList});

% Work with packet when it has accumulated and flush buffers
decode_channel_packet(#channel{msg = Msg, length = Length} = Channel, #rtmp_client{channels = Channels} = State) when size(Msg) == Length ->
  NewState = command(Channel, State), % Perform Commands here
  NextChannelList = lists:keystore(Channel#channel.id, #channel.id, Channels, Channel#channel{msg = <<>>}),
  decode(NewState#rtmp_client{channels=NextChannelList}).

command(#channel{type = ?RTMP_TYPE_ACK_READ, msg = <<_Length:32/big-integer>>} = _Channel, #rtmp_client{previous_ack = Prev} = State) ->
  Time = timer:now_diff(erlang:now(), Prev)/1000,
  Speed = round(_Length*1000 / Time),
  % ?D({"Stream bytes read: ", _Length, round(Time/1000), round(Speed)}),
	State#rtmp_client{previous_ack = erlang:now(), current_speed = Speed};

command(#channel{type = ?RTMP_TYPE_WINDOW_ACK_SIZE, msg = <<WindowSize:32/big-integer>>} = _Channel, State) ->
  ?D({"Window acknolegement size", WindowSize}),
  State;

command(#channel{type = ?RTMP_TYPE_CHUNK_SIZE, msg = <<ChunkSize:32/big-integer>>} = _Channel, State) ->
  ?D({"Change Chunk Size",ChunkSize}),
	State#rtmp_client{client_chunk_size = ChunkSize};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_PONG:16/big-integer, _Timestamp:32/big-integer>>}, State) ->
	State#rtmp_client{pinged = false};	

	
command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_PING:16/big-integer, Timestamp:32/big-integer>>} = Channel, State) ->
  gen_fsm:send_event(self(), {send, {Channel, <<?RTMP_CONTROL_STREAM_PONG:16/big-integer, Timestamp:32/big-integer>>}}),
	State;	

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_BUFFER:16/big-integer, _StreamId:32/big-integer, BufferSize:32/big-integer>>} = _Channel, 
        #rtmp_client{video_player = Player} = State) ->
  ?D({"Buffer size on stream id", BufferSize, _StreamId}),
  case Player of
    undefined -> ok;
    _ -> Player ! {client_buffer, BufferSize}
  end,
	State#rtmp_client{client_buffer = BufferSize};	


command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<EventType:16/big-integer, _/binary>>} = _Channel, State) ->
	?D({"Ping - ignoring", EventType}),
	State;	


command(#channel{type = Type, delta = 0}, State) 
	when (Type =:= ?RTMP_TYPE_AUDIO) or (Type =:= ?RTMP_TYPE_VIDEO) or (Type =:= ?RTMP_TYPE_METADATA_AMF0) ->
  ?D({"Throw away garbage audio"}),
  State;

command(#channel{type = Type} = Channel, State) 
	when (Type =:= ?RTMP_TYPE_AUDIO) or (Type =:= ?RTMP_TYPE_VIDEO) or (Type =:= ?RTMP_TYPE_METADATA_AMF0) ->
%	?D({"Recording",Type}),
	gen_fsm:send_event(self(), {publish, Channel}),
	State;

command(#channel{type = ?RTMP_INVOKE_AMF0} = Channel, State) ->
	AMF = amf0:decode(Channel#channel.msg),
	#amf{command = Command} = AMF,
	call_function(ems:check_app(State,Command, 2), Command, State, AMF);

command(#channel{type = ?RTMP_INVOKE_AMF3} = Channel, State) ->
	AMF = amf3:decode(Channel#channel.msg),
	#amf{command = Command} = AMF,
	call_function(ems:check_app(State,Command, 2), Command, State, AMF);

command(#channel{type = ?RTMP_TYPE_SO_AMF0, msg = Message}, State) ->
  decode_shared_object_amf0(Message, State);
  

	
command(#channel{type = Type}, State) ->
  ?D({"Unhandled message type", Type}),
  State.

call_function(unhandled, Command, #rtmp_client{addr = IP, port = Port} = State, #amf{args = Args}) ->
  error_logger:error_msg("Client ~p:~p requested unknown function ~p/~p~n", [IP, Port, Command, length(Args)]),
  State;

call_function(App, Command, State, AMF) ->
	App:Command(AMF, State).


decode_shared_object_amf0(<<>>, State) -> State;
decode_shared_object_amf0(<<Length:16, SharedObject:Length/binary, _VersionFlags:12/binary, EventType, 
                            EventDataLength:32, EventData:EventDataLength/binary, Rest/binary>>, State) ->
  State1 = apps_shared_objects:command({SharedObject, EventType, EventData}, State),
  decode_shared_object_amf0(Rest, State1).
