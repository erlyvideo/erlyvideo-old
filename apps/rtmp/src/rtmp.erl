%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP encoding/decoding module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").
-include("shared_objects.hrl").
-version(1.1).

-export([encode/2, encode_id/2, decode/2]).
-export([encode_list/1, decode_list/1]).
-export([element/2, setelement/3, justify_ts/1]).

%%--------------------------------------------------------------------
%% @spec (Socket::rtmp_socket(), Message::rtmp_message()) -> {NewSocket::rtmp_socket(), Packet::binary()}
%% @doc Encodes outgoing message to chunked binary packet, prepared to be written
%% to TCP socket. As RTMP is a stateful protocol, state is modified on each encode.
%% Things, that change in state are chunk size and "last" stream_id, timestamp, packet length.
%% Thus, two sequential calls to encode can give you different results:
%% 
%% <code>{NewSocket, Packet} = encode(Socket, #rtmp_message{type = chunk_size, body = 10}),<br/>
%% {NewSocket1, Packet1} = encode(NewSocket, #rtmp_message{type = chunk_size, body = 10}).</code><br/>
%% First message will be chunked sent with default chunk size 128, but second will use chunk size 10
%% and will differ from previous one.
%%
%% You can ask, where can you take Socket to use this module? Currently there are no methods to
%% construct it from nowhere, because RTMP protocol is useless without other connected side,
%% so take a use of {@link rtmp_socket. rtmp_socket} module, that instantiates Socket for you.
%%
%% To read description about rtmp_message, look for {@link decode/2. decode/2} documentation.
%% @end 
%%--------------------------------------------------------------------

-spec(encode(State::rtmp_socket(), Message::rtmp_message()) -> {State::rtmp_socket(), Binary::iolist()}).

encode(State, #rtmp_message{timestamp = undefined} = Message) -> 
  encode(State, Message#rtmp_message{timestamp = 0});

encode(State, #rtmp_message{stream_id = undefined} = Message) -> 
  encode(State, Message#rtmp_message{stream_id = 0});

encode(State, #rtmp_message{channel_id = undefined, type = invoke} = Message)  -> 
  encode(State, Message#rtmp_message{channel_id = 3});

encode(State, #rtmp_message{channel_id = undefined, type = shared_object} = Message)  -> 
  encode(State, Message#rtmp_message{channel_id = 4});

encode(State, #rtmp_message{channel_id = undefined} = Message)  -> 
  encode(State, Message#rtmp_message{channel_id = 2});

encode(State, #rtmp_message{type = chunk_size, body = ChunkSize} = Message) ->
  encode(State#rtmp_socket{server_chunk_size = ChunkSize}, Message#rtmp_message{type = ?RTMP_TYPE_CHUNK_SIZE, body = <<ChunkSize:32>>});

encode(#rtmp_socket{bytes_unack = Bytes} = State, #rtmp_message{type = ack_read} = Message) ->
  encode(State#rtmp_socket{bytes_unack = 0}, Message#rtmp_message{type = ?RTMP_TYPE_ACK_READ, body = <<Bytes:32>>});

encode(State, #rtmp_message{type = stream_begin} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = ?RTMP_CONTROL_STREAM_BEGIN});

encode(State, #rtmp_message{type = stream_end} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = ?RTMP_CONTROL_STREAM_EOF});

encode(State, #rtmp_message{type = stream_recorded} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = ?RTMP_CONTROL_STREAM_RECORDED});

encode(State, #rtmp_message{type = burst_start} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = ?RTMP_CONTROL_STREAM_BURST_START});

encode(State, #rtmp_message{type = burst_stop} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = ?RTMP_CONTROL_STREAM_BURST_STOP});

encode(State, #rtmp_message{type = buffer_size, body = BufferSize, stream_id = StreamId} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = <<?RTMP_CONTROL_STREAM_BUFFER:16, StreamId:32, BufferSize:32>>});

encode(State, #rtmp_message{type = ping} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = ?RTMP_CONTROL_STREAM_PING});

encode(State, #rtmp_message{type = pong} = Message) ->
  encode(State, Message#rtmp_message{type = control, body = ?RTMP_CONTROL_STREAM_PONG});

encode(State, #rtmp_message{type = control, body = {EventType, Body}} = Message) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_CONTROL, body = <<EventType:16, Body/binary>>});

encode(State, #rtmp_message{type = control, body = EventType, stream_id = StreamId} = Message) when is_number(EventType) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_CONTROL, body = <<EventType:16, StreamId:32>>});

encode(State, #rtmp_message{type = control, body = EncodedControl} = Message) when is_binary(EncodedControl) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_CONTROL});

encode(State, #rtmp_message{type = window_size, body = WindowAckSize} = Message) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_WINDOW_ACK_SIZE, body = <<WindowAckSize:32>>});

encode(State, #rtmp_message{type = bw_peer, body = WindowAckSize} = Message) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_BW_PEER, body = <<WindowAckSize:32, 16#02>>});

encode(State, #rtmp_message{type = audio} = Message) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_AUDIO});

encode(State, #rtmp_message{type = video} = Message) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_VIDEO});

encode(State, #rtmp_message{type = abort, body = ChannelId} = Message) ->
  encode(State, Message#rtmp_message{type = ?RTMP_TYPE_ABORT, body = <<ChannelId:32>>});

encode(#rtmp_socket{amf_version = 0} = State, #rtmp_message{type = invoke, body = AMF} = Message) when is_record(AMF, rtmp_funcall)-> 
	encode(State, Message#rtmp_message{body = encode_funcall(AMF), type = ?RTMP_INVOKE_AMF0});

encode(#rtmp_socket{amf_version = 3} = State, #rtmp_message{type = invoke, body = AMF} = Message) when is_record(AMF, rtmp_funcall)-> 
	encode(State, Message#rtmp_message{body = <<0, (encode_funcall(AMF))/binary>>, type = ?RTMP_INVOKE_AMF3});

encode(#rtmp_socket{amf_version = 0} = State, #rtmp_message{type = metadata, body = Body} = Message) when is_binary(Body) -> 
  encode(State, Message#rtmp_message{body = Body, type = ?RTMP_TYPE_METADATA_AMF0});

encode(#rtmp_socket{amf_version = 3} = State, #rtmp_message{type = metadata, body = Body} = Message) when is_binary(Body)-> 
  encode(State, Message#rtmp_message{body = <<0, Body/binary>>, type = ?RTMP_TYPE_METADATA_AMF3});

encode(#rtmp_socket{amf_version = 0} = State, #rtmp_message{type = metadata, body = Metadata} = Message) -> 
  encode(State, Message#rtmp_message{body = encode_list(Metadata), type = ?RTMP_TYPE_METADATA_AMF0});

encode(#rtmp_socket{amf_version = 3} = State, #rtmp_message{type = metadata, body = Metadata} = Message) -> 
  encode(State, Message#rtmp_message{body = <<0, (encode_list(Metadata))/binary>>, type = ?RTMP_TYPE_METADATA_AMF3});

encode(#rtmp_socket{amf_version = 0} = State, #rtmp_message{type = shared_object, body = SOMessage} = Message) when is_record(SOMessage, so_message)->
  encode(State, Message#rtmp_message{body = encode_shared_object(SOMessage), type = ?RTMP_TYPE_SO_AMF0});

encode(#rtmp_socket{amf_version = 3} = State, #rtmp_message{type = shared_object, body = SOMessage} = Message) when is_record(SOMessage, so_message)->
  encode(State, Message#rtmp_message{body = <<0, (encode_shared_object(SOMessage))/binary>>, type = ?RTMP_TYPE_SO_AMF3});

encode(State, #rtmp_message{timestamp = TimeStamp} = Message) when is_float(TimeStamp) -> 
  encode(State, Message#rtmp_message{timestamp = round(TimeStamp)});

encode(State, #rtmp_message{stream_id = StreamId} = Message) when is_float(StreamId) -> 
  encode(State, Message#rtmp_message{stream_id = round(StreamId)});

encode(_State, #rtmp_message{stream_id = StreamId}) when not is_number(StreamId) ->
  erlang:error({invalid_rtmp_stream_id,StreamId});


encode(#rtmp_socket{} = State, #rtmp_message{type = Type, body = Data} = Message) when is_binary(Data) and is_integer(Type)-> 
  encode_bin(State, Message).


-spec element(Id::non_neg_integer(), Tuple::tuple()) -> any().
element(Id, Tuple) when Id > size(Tuple) -> undefined;
element(Id, Tuple) -> erlang:element(Id, Tuple).

-spec setelement(Id::non_neg_integer(), Tuple::tuple(), Value::any()) -> any().
setelement(Id, Tuple, Value) when Id > size(Tuple) -> rtmp:setelement(Id, erlang:append_element(Tuple, undefined), Value);
setelement(Id, Tuple, Value) -> erlang:setelement(Id, Tuple, Value).


% encode_bin(#rtmp_socket{server_chunk_size = ChunkSize, out_channels = Channels} = State, 
%        #rtmp_message{channel_id = Id, timestamp = Timestamp, type = Type, stream_id = StreamId, body = Data}) when is_binary(Data) and is_integer(Type) andalso element(Id, Channels) -> 
%   ChunkList = chunk(Data, ChunkSize, Id),
% 
%   case rtmp:element(Id, Channels) of
%     #channel{timestamp = PrevTS, stream_id = StreamId} = Channel when PrevTS =/= undefined andalso PrevTS =< Timestamp andalso Timestamp - PrevTS < 10000 ->
% 

encode_bin(#rtmp_socket{server_chunk_size = ChunkSize, out_channels = Channels, bytes_sent = BytesSent} = State, 
       #rtmp_message{channel_id = Id, timestamp = Timestamp, type = Type, stream_id = StreamId, body = Data} = Msg) when is_binary(Data) and is_integer(Type) -> 
  ChunkList = chunk(Data, ChunkSize, Id),
  Channel = rtmp:element(Id, Channels),

  case timestamp_type(State, Msg) of
    delta ->
      #channel{timestamp = PrevTS} = Channel,
    	BinId = encode_id(?RTMP_HDR_SAME_SRC,Id),
    	{Delta, NewTS} = case Timestamp of
    	  same -> {0, PrevTS};
    	  _ -> {justify_ts(Timestamp - PrevTS), Timestamp}
    	end,
      % if
      %   Type == 8 orelse Type == 9 -> ok;
      %   true -> io:format("d ~p ~p ~p~n",[Id, Type, Delta])
      % end,
    	Channel1 = Channel#channel{timestamp = NewTS, delta = undefined, type = Type},
    	Header = case Delta < 16#FFFFFF of
    	  true -> <<BinId/binary,Delta:24,(size(Data)):24,Type:8>>;
    	  false -> <<BinId/binary,16#FFFFFF:24,(size(Data)):24,Type:8,Delta:32>>
    	end,
    	Bin = [Header | ChunkList],
      {State#rtmp_socket{out_channels = rtmp:setelement(Id, Channels, Channel1), bytes_sent = BytesSent + iolist_size(Bin)}, Bin};
    new ->
      TS = justify_ts(case Timestamp of
        same -> 0;
        _ -> Timestamp
      end),
      % io:format("n ~p ~p ~p~n",[Id, Type, TS]),
      Channel1 = case Channel of
        undefined -> #channel{id = Id, timestamp = TS, delta = undefined, stream_id = StreamId, type = Type};
        _ -> Channel#channel{timestamp = TS, delta = undefined, stream_id = StreamId, type = Type}
      end,
    	BinId = encode_id(?RTMP_HDR_NEW,Id),
      Header = case TS < 16#FFFFFF of
        true -> <<BinId/binary,TS:24,(size(Data)):24,Type:8,StreamId:32/little>>;
        false -> <<BinId/binary,16#FFFFFF:24,(size(Data)):24,Type:8,StreamId:32/little,TS:32>>
      end,
      Bin = [Header | ChunkList],
      {State#rtmp_socket{out_channels = rtmp:setelement(Id, Channels, Channel1), bytes_sent = BytesSent + iolist_size(Bin)}, Bin}
  end.

-define(MAX_TS, 16#1000000).

justify_ts(TS) when is_float(TS) -> justify_ts(trunc(TS));
justify_ts(TS) when TS < 0 -> 0;
justify_ts(TS) when TS >= ?MAX_TS -> justify_ts(TS - ?MAX_TS);
justify_ts(TS) when TS >= 0 andalso TS < ?MAX_TS -> TS.

  

-spec timestamp_type(Socket::rtmp_socket(), Message::rtmp_message()) -> fixed_timestamp_type().
timestamp_type(_State, #rtmp_message{ts_type = new}) -> new;
timestamp_type(_State, #rtmp_message{ts_type = delta}) -> delta;
timestamp_type(_, #rtmp_message{channel_id = 3}) -> new;
timestamp_type(#rtmp_socket{out_channels = Channels}, #rtmp_message{channel_id = Id, type = Type, timestamp = Timestamp, stream_id = StreamId}) -> 
  case rtmp:element(Id, Channels) of
    #channel{timestamp = PrevTS, type = Type, stream_id = StreamId} when PrevTS =< Timestamp -> delta;
    _ -> new
  end.

binarize(Command) when is_atom(Command) -> atom_to_binary(Command, utf8);
binarize(Command) when is_list(Command) -> list_to_binary(Command);
binarize(Command) when is_binary(Command) -> Command.

encode_funcall(#rtmp_funcall{command = Command, id = Id, type = invoke} = AMF) ->
  <<(amf0:encode(binarize(Command)))/binary, (amf0:encode(Id))/binary, (encode_args(AMF))/binary>>;
 
encode_funcall(#rtmp_funcall{command = Command, type = notify} = AMF) -> 
  <<(amf0:encode(binarize(Command)))/binary, (encode_args(AMF))/binary>>.

encode_args(#rtmp_funcall{args = Args, version = Version, command = _Cmd}) ->
  encode_list(<<>>, fix_amf_version(Args, Version)).

fix_amf_version(Args, 0) -> Args;
fix_amf_version(Args, 3) -> [{avmplus, Object} || Object <- Args].

encode_list(List) -> encode_list(<<>>, List).

encode_list(Message, []) -> Message;
encode_list(Message, [Arg | Args]) ->
  AMF = amf0:encode(Arg),
  encode_list(<<Message/binary, AMF/binary>>, Args).


encode_id(new, Id) -> encode_id(?RTMP_HDR_NEW, Id);
encode_id(same, Id) -> encode_id(?RTMP_HDR_SAME_SRC, Id);
encode_id(new_ts, Id) -> encode_id(?RTMP_HDR_TS_CHG, Id);
encode_id(continue, Id) -> encode_id(?RTMP_HDR_CONTINUE, Id);

encode_id(Type, Id) when Id =< 63 -> 
  <<Type:2, Id:6>>;
encode_id(Type, Id) when Id =< 319 -> 
	<<Type:2,?RTMP_HDR_MED_ID:6, (Id - 64):8>>;
encode_id(Type, Id) when Id > 319 -> 
	<<Type:2,?RTMP_HDR_LRG_ID:6, (Id - 64):16/big-integer>>.


% chunk(Data) -> chunk(Data,?RTMP_DEF_CHUNK_SIZE).

chunk(Data, ChunkSize, _) when size(Data) =< ChunkSize -> Data;
chunk(Data, ChunkSize, Id) -> chunk(Data, ChunkSize, Id, []).

chunk(Data, ChunkSize, _Id, List) when size(Data) =< ChunkSize ->
  lists:reverse([Data | List]);


chunk(Data, ChunkSize, Id, List) when is_binary(Data) ->
  <<Chunk:ChunkSize/binary,Rest/binary>> = Data,
  chunk(Rest, ChunkSize, Id, [encode_id(?RTMP_HDR_CONTINUE, Id), Chunk | List]).
		
		
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DECODING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
		
		
%%--------------------------------------------------------------------
%% @spec (Socket::rtmp_socket(), Packet::binary()) -> {NewSocket::rtmp_socket(), Message::rtmp_message(), Rest::binary()} | {NewSocket::rtmp_socket, Rest::binary()}
%% @doc Encodes outgoing message to chunked binary packet, prepared to be written
%% to TCP socket.
%% 
%% RTMP is stateful protocol, so incoming messages modify state of protocol decoder. You need to keep
%% track of it. {@link rtmp_socket. rtmp_socket} module can do this for you.
%% 
%% If it is enough data received, decode will return rtmp_message() and unconsumed bytes, that you need to store somewhere
%% in buffer and pass them next time, when you get more bytes. Even in this case you need to keep new state, 
%% because rtmp_socket() has its own internal buffers, that are filled. It is required for chunks of packets.
%%
%% So, always keep new state and keep returned buffer somewhere.
%%
%% rtmp_message has following (interesting for you) structure:
%% <ul>
%% <li><code>timestamp</code> this field is interesting only for audio/video messages</li>
%% <li><code>type</code> you can read different types further</li>
%% <li><code>stream_id</code> this field is default 0 (message received not on NetStream object, but on NetConnection)</li>
%% <li><code>body</code> this field is correlated with type, read further.</li>
%% </ul>
%%
%% Here goes list of possible messages, you can receive from RTMP stream:
%% <ol>
%% <li><code>#rtmp_message{type=window_size, body=WindowSize}</code> other side wants you to send ack_read message once in <code>WindowSize</code> bytes</li>
%% <li><code>#rtmp_message{type=stream_begin, stream_id=StreamId}</code> audio/video is starting on stream StreamId</li>
%% <li><code>#rtmp_message{type=stream_end, stream_id=StreamId}</code> audio/video is finished on stream StreamId</li>
%% <li><code>#rtmp_message{type=buffer_size, body=BufferSize, stream_id=StreamId}</code> other side tells us, that it has BufferSize a/v buffer on stream StreamId</li>
%% <li><code>#rtmp_message{type=stream_recorded, stream_id=StreamId}</code> stream StreamId was recorded on disc before sending to us</li>
%% <li><code>#rtmp_message{type=ping, body=Timestamp}</code> at time Timestamp peer sent us ping, we should reply with pong</li>
%% <li><code>#rtmp_message{type=control, body={EventType, Data}, stream_id=StreamId}</code> some other undecoded control message received</li>
%% <li><code>#rtmp_message{type=broken_meta, stream_id=StreamId}</code> audio/video with null size received. It happens sometimes, never mind and ignore.</li>
%% <li><code>#rtmp_message{type=audio, body=Body, stream_id=StreamId}</code> audio packet received on StreamId</li>
%% <li><code>#rtmp_message{type=video, body=Body, stream_id=StreamId}</code> video packet received on StreamId</li>
%% <li><code>#rtmp_message{type=metadata, body=Body, stream_id=StreamId}</code> metadata packet received on StreamId. It is usually sent, when flash client starts recording video stream from camera and tells size of video.</li>
%% <li><code>#rtmp_message{type=invoke, body=Funcall::rtmp_funcall()}</code> function is invoked. Read further about Funcall object.</li>
%% <li><code>#rtmp_message{type=shared_object, body=SharedObjectMessage}</code> shared object event happened. Not implemented yet.</li>
%% </ol>
%% @end 
%%--------------------------------------------------------------------
decode(#rtmp_socket{} = State, <<>>) -> {State, <<>>};
  
decode(#rtmp_socket{} = State, Data) when is_binary(Data) -> 
  case decode_channel_id(Data, State) of
    {NewState, Message, Rest} -> {NewState, Message, Rest};
    {NewState, Rest} -> {NewState, Rest};
    NewState -> {NewState, Data}
  end.

% First extracting channel id
decode_channel_id(<<Format:2, ?RTMP_HDR_LRG_ID:6,Id:16, Rest/binary>>, State) ->
  decode_channel_header(Rest, Format, Id + 64, State);
decode_channel_id(<<Format:2, ?RTMP_HDR_MED_ID:6,Id:8, Rest/binary>>, State) ->
  decode_channel_header(Rest, Format, Id + 64, State);
decode_channel_id(<<Format:2, Id:6, Rest/binary>>, State) ->
  decode_channel_header(Rest, Format, Id, State).

% Now extracting channel header
decode_channel_header(Packet, ?RTMP_HDR_CONTINUE = Type, Id, #rtmp_socket{channels = Channels} = S) when size(Channels) < Id orelse erlang:element(Id, Channels) == undefined ->
  erlang:error({invalid_rtmp,Id,Type,S, Packet});
  
decode_channel_header(Packet, ?RTMP_HDR_SAME_SRC = Type, Id, #rtmp_socket{channels = Channels} = S) when size(Channels) < Id orelse erlang:element(Id, Channels) == undefined ->
  erlang:error({invalid_rtmp,Id,Type,S, Packet});

decode_channel_header(Packet, ?RTMP_HDR_TS_CHG = Type, Id, #rtmp_socket{channels = Channels} = S) when size(Channels) < Id orelse erlang:element(Id, Channels) == undefined ->
  erlang:error({invalid_rtmp,Id,Type,S, Packet});

decode_channel_header(Packet, ?RTMP_HDR_CONTINUE, Id, State) ->
  Channel = rtmp:element(Id, State#rtmp_socket.channels),
  #channel{msg = Msg, timestamp = Timestamp, delta = Delta, abs_ts = AbsTs} = Channel,
  
  {Channel1, Rest} = case {AbsTs, Packet} of
    {true, <<TS:32, Data/binary>>} -> {Channel#channel{timestamp = TS}, Data};
    {true, _} -> State;
    {false, _} ->
      Chan1 = case {Delta, size(Msg)} of
        {undefined, _} -> Channel;
        {_, 0} -> Channel#channel{timestamp = Timestamp + Delta};
        _ -> Channel
      end,
      {Chan1, Packet}
  end,
  decode_channel(Rest, Channel1, State);

decode_channel_header(<<16#ffffff:24, TimeStamp:32, Rest/binary>>, ?RTMP_HDR_TS_CHG, Id, State) ->
  Channel = rtmp:element(Id, State#rtmp_socket.channels),
  decode_channel(Rest,Channel#channel{timestamp = TimeStamp+16#ffffff, delta = undefined, abs_ts = true},State);
  
decode_channel_header(<<Delta:24, Rest/binary>>, ?RTMP_HDR_TS_CHG, Id, State) ->
  Channel = rtmp:element(Id, State#rtmp_socket.channels),
  #channel{timestamp = TimeStamp} = Channel,
  decode_channel(Rest, Channel#channel{timestamp = TimeStamp + Delta, delta = Delta, abs_ts = false},State);
  
decode_channel_header(<<16#ffffff:24,Length:24,Type:8,TimeStamp:32,Rest/binary>>, ?RTMP_HDR_SAME_SRC, Id, State) ->
  Channel = rtmp:element(Id, State#rtmp_socket.channels),
	decode_channel(Rest,Channel#channel{timestamp=TimeStamp+16#ffffff, delta = undefined, length=Length,type=Type, abs_ts = true},State);
	
decode_channel_header(<<Delta:24,Length:24,Type:8,Rest/binary>>, ?RTMP_HDR_SAME_SRC, Id, State) ->
  Channel = rtmp:element(Id, State#rtmp_socket.channels),
  #channel{timestamp = TimeStamp} = Channel,
	decode_channel(Rest,Channel#channel{timestamp=TimeStamp + Delta, delta = Delta, length=Length,type=Type, abs_ts = false},State);

decode_channel_header(<<16#ffffff:24,Length:24,Type:8,StreamId:32/little,TimeStamp:32,Rest/binary>>,?RTMP_HDR_NEW,Id, State) ->
  Channel = case rtmp:element(Id, State#rtmp_socket.channels) of
    undefined -> #channel{};
    Chan -> Chan
  end,
	decode_channel(Rest,Channel#channel{id=Id,timestamp=TimeStamp+16#ffffff,delta = undefined, length=Length,type=Type,stream_id=StreamId, abs_ts = true},State);
	
decode_channel_header(<<TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>,?RTMP_HDR_NEW,Id, State) ->
  case rtmp:element(Id, State#rtmp_socket.channels) of
    #channel{} = Channel -> ok;
    _ -> Channel = #channel{}
  end,
	decode_channel(Rest,Channel#channel{id=Id,timestamp=TimeStamp,delta = undefined, length=Length,type=Type,stream_id=StreamId, abs_ts = false},State);

decode_channel_header(_Rest,_Type, _Id,  State) -> % Still small buffer
  State.

% Now trying to fill channel with required data
bytes_for_channel(#channel{length = Length, msg = Msg}, _) when size(Msg) == Length ->
  0;

bytes_for_channel(#channel{length = Length, msg = Msg}, #rtmp_socket{client_chunk_size = ChunkSize}) when Length - size(Msg) < ChunkSize ->
  Length - size(Msg);
  
bytes_for_channel(_, #rtmp_socket{client_chunk_size = ChunkSize}) -> ChunkSize.
  

decode_channel(Data, Channel, State) ->
	BytesRequired = bytes_for_channel(Channel, State),
	push_channel_packet(Data, Channel, State, BytesRequired).
	
% Nothing to do when buffer is small

push_channel_packet(Data, _Channel, State, BytesRequired) when size(Data) < BytesRequired ->
  State;
  
% And decode channel when bytes required are in buffer
push_channel_packet(Data, #channel{msg = Msg} = Channel, State, BytesRequired) -> 
  <<Chunk:BytesRequired/binary, Rest/binary>> = Data,
  decode_channel_packet(Rest, Channel#channel{msg = <<Msg/binary, Chunk/binary>>}, State).



% When chunked packet hasn't arived, just accumulate it
decode_channel_packet(Rest, #channel{msg = Msg, length = Length} = Channel, #rtmp_socket{channels = Channels} = State) when size(Msg) < Length ->
  NextChannelList = rtmp:setelement(Channel#channel.id, Channels, Channel),
  rtmp:decode(State#rtmp_socket{channels=NextChannelList}, Rest);

% Work with packet when it has accumulated and flush buffers
decode_channel_packet(Rest, #channel{msg = Msg, length = Length} = Channel, #rtmp_socket{channels = Channels} = State) when size(Msg) == Length ->
  {NewState, Message} = command(Channel, State), % Perform Commands here
  TSType = case Channel#channel.delta of
    undefined -> new;
    _ -> delta
  end,
  NextChannelList = rtmp:setelement(Channel#channel.id, Channels, Channel#channel{msg = <<>>}),
  {NewState#rtmp_socket{channels=NextChannelList}, Message#rtmp_message{ts_type = TSType}, Rest}.
  
extract_message(#channel{id = Id, timestamp = Timestamp, stream_id = StreamId}) -> 
  #rtmp_message{channel_id = Id, timestamp = Timestamp, stream_id = StreamId}.




-spec(command(Channel::channel(), Socket::rtmp_socket()) -> {Socket::rtmp_socket(), Message::rtmp_message()}).

command(#channel{type = ?RTMP_TYPE_CHUNK_SIZE, msg = <<ChunkSize:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State#rtmp_socket{client_chunk_size = ChunkSize}, Message#rtmp_message{type = chunk_size, body = ChunkSize}};

command(#channel{type = ?RTMP_TYPE_ACK_READ, msg = <<BytesRead:32>>} = Channel, #rtmp_socket{previous_ack = undefined} = State) ->
  TimeNow = erlang:now(),
  Message = extract_message(Channel),
  AckMessage = #rtmp_message_ack{
    bytes_read = BytesRead,
    previous_ack = undefined,
    current_ack = TimeNow,
    speed = undefined
  },
  {State#rtmp_socket{previous_ack = TimeNow}, Message#rtmp_message{type = ack_read, body = AckMessage}};

command(#channel{type = ?RTMP_TYPE_ACK_READ, msg = <<BytesRead:32>>} = Channel, #rtmp_socket{previous_ack = Prev} = State) ->
  TimeNow = erlang:now(),
  Time = timer:now_diff(TimeNow, Prev)/1000,
  Speed = round(BytesRead*1000 / Time),
  Message = extract_message(Channel),
  AckMessage = #rtmp_message_ack{
    bytes_read = BytesRead,
    previous_ack = Prev,
    current_ack = TimeNow,
    speed = Speed
  },
  {State#rtmp_socket{previous_ack = TimeNow, current_speed = Speed}, Message#rtmp_message{type = ack_read, body = AckMessage}};

command(#channel{type = ?RTMP_TYPE_WINDOW_ACK_SIZE, msg = <<WindowSize:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = window_size, body = WindowSize}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_BEGIN:16, StreamId:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = stream_begin, stream_id = StreamId}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_EOF:16, StreamId:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = stream_end, stream_id = StreamId}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_BUFFER:16, StreamId:32, BufferSize:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State#rtmp_socket{client_buffer = BufferSize}, Message#rtmp_message{type = buffer_size, body = BufferSize, stream_id = StreamId}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_RECORDED:16, StreamId:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State#rtmp_socket{pinged = false}, Message#rtmp_message{type = stream_recorded, stream_id = StreamId}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_BURST_STOP:16, StreamId:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = burst_stop, stream_id = StreamId}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_BURST_START:16, StreamId:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = burst_start, stream_id = StreamId}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_PING:16, Timestamp:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = ping, body = Timestamp}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<?RTMP_CONTROL_STREAM_PONG:16, Timestamp:32>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State#rtmp_socket{pinged = false}, Message#rtmp_message{type = pong, body = Timestamp}};

command(#channel{type = ?RTMP_TYPE_CONTROL, msg = <<EventType:16, Body/binary>>} = Channel, State) ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = control, body = {EventType, Body}}};

% command(#channel{type = Type, delta = 0} = Channel, State) when (Type =:= ?RTMP_TYPE_AUDIO) or (Type =:= ?RTMP_TYPE_VIDEO) or (Type =:= ?RTMP_TYPE_METADATA_AMF0) ->
%   Message = extract_message(Channel),
%   {State, Message#rtmp_message{type = broken_meta}};
% 
% command(#channel{type = Type, length = 0} = Channel, State) when (Type =:= ?RTMP_TYPE_AUDIO) or (Type =:= ?RTMP_TYPE_VIDEO) or (Type =:= ?RTMP_TYPE_METADATA_AMF0) ->
%   Message = extract_message(Channel),
%   {State, Message#rtmp_message{type = broken_meta}};

command(#channel{type = ?RTMP_TYPE_AUDIO, msg = Body} = Channel, State)	 ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = audio, body = Body}};

command(#channel{type = ?RTMP_TYPE_VIDEO, msg = Body} = Channel, State)	 ->
  Message = extract_message(Channel),
	{State, Message#rtmp_message{type = video, body = Body}};

command(#channel{type = ?RTMP_TYPE_METADATA_AMF0, msg = Body} = Channel, State)	 ->
  Message = extract_message(Channel),
  Metadata = decode_list(Body),
	{State, Message#rtmp_message{type = metadata, body = Metadata}};

command(#channel{type = ?RTMP_TYPE_METADATA_AMF3, msg = <<_, Body/binary>>} = Channel, State)	 ->
  Message = extract_message(Channel),
  Metadata = decode_list(Body),
	{State, Message#rtmp_message{type = metadata, body = Metadata}};

% Red5: net.rtmp.codec.RTMPProtocolDecoder
% // TODO: Unknown byte, probably encoding as with Flex SOs?
% in.skip(1);
command(#channel{type = ?RTMP_INVOKE_AMF3, msg = <<_, Body/binary>>, stream_id = StreamId} = Channel, State) ->
  Message = extract_message(Channel),
  AMF = decode_funcall(Body, StreamId),
  {State, Message#rtmp_message{type = invoke, body = AMF#rtmp_funcall{version = 3}}};

command(#channel{type = ?RTMP_INVOKE_AMF0, msg = Body, stream_id = StreamId} = Channel, State) ->
  Message = extract_message(Channel),
  Funcall = decode_funcall(Body, StreamId),
  State1 = case Funcall of
    #rtmp_funcall{command = <<"connect">>, args = [{object, ConnectObj}|_]} ->
      case proplists:get_value(flashVer, ConnectObj) of
        <<"FMLE/",_/binary>> -> State#rtmp_socket{fmle_3 = true};
        _ -> State
      end;
    _ -> State
  end,    
  {State1, Message#rtmp_message{type = invoke, body = Funcall}};


command(#channel{type = ?RTMP_TYPE_SO_AMF0, msg = Body} = Channel, State) ->
  Message = extract_message(Channel),
  {State, Message#rtmp_message{type = shared_object, body = decode_shared_object(Body)}};

command(#channel{type = ?RTMP_TYPE_SO_AMF3, msg = <<_, Body/binary>>} = Channel, State) ->
  Message = extract_message(Channel),
  {State, Message#rtmp_message{type = shared_object, body = decode_shared_object(Body)}};
  
command(#channel{type = Type, msg = Body} = Channel, State) ->
  Message = extract_message(Channel),
  {State, Message#rtmp_message{type = Type, body = Body}}.

decode_funcall(Message, StreamId) ->
	{Command, Rest1} = amf0:decode(Message),
	{InvokeId, Rest2} = amf0:decode(Rest1),
	Arguments = decode_list(Rest2),
	#rtmp_funcall{command = Command, args = Arguments, type = invoke, id = InvokeId, stream_id = StreamId}.
  
  
decode_list(Data) -> decode_list(Data, []).

decode_list(<<>>, Acc) -> lists:reverse(Acc);

decode_list(Body, Acc) ->
  {Object, Rest} = case amf0:decode(Body) of
    {{avmplus, Element}, Data} -> {Element, Data};
    {Element, Data} -> {Element, Data}
  end,
  decode_list(Rest, [Object | Acc]).


encode_shared_object(#so_message{name = Name, version = Version, persistent = true, events = Events}) ->
  encode_shared_object_events(<<(size(Name)):16, Name/binary, Version:32, 34:32, 0:32>>, Events);

encode_shared_object(#so_message{name = Name, version = Version, persistent = false, events = Events}) ->
  encode_shared_object_events(<<(size(Name)):16, Name/binary, Version:32, 0:32, 0:32>>, Events).

encode_shared_object_events(Body, []) ->
  Body;

encode_shared_object_events(Body, [{set_attribute, {Key, Value}} | Events]) when is_list(Key) ->
  encode_shared_object_events(Body, [{set_attribute, {list_to_binary(Key), Value}} | Events]);

encode_shared_object_events(Body, [{set_attribute, {Key, Value}} | Events]) ->
  EventData = <<(size(Key)):16, Key/binary, (amf0:encode(Value))/binary>>,
  encode_shared_object_events(<<Body/binary, ?SO_SET_ATTRIBUTE, (size(EventData)):32, EventData/binary>>, Events);

encode_shared_object_events(Body, [{update_attribute, Name} | Events]) ->
  EventData = <<(size(Name)):16, Name/binary>>,
  encode_shared_object_events(<<Body/binary, ?SO_UPDATE_ATTRIBUTE, (size(EventData)):32, EventData/binary>>, Events);

encode_shared_object_events(Body, [{delete_attribute, Name} | Events]) ->
  EventData = <<(size(Name)):16, Name/binary>>,
  encode_shared_object_events(<<Body/binary, ?SO_DELETE_ATTRIBUTE, (size(EventData)):32, EventData/binary>>, Events);

encode_shared_object_events(Body, [{update_data, Data} | Events]) ->
  EventData = encode_shared_object_data(<<>>, Data),
  encode_shared_object_events(<<Body/binary, ?SO_UPDATE_DATA, (size(EventData)):32, EventData/binary>>, Events);

encode_shared_object_events(Body, [{delete_data, Name} | Events]) ->
  EventData = <<(size(Name)):16, Name/binary>>,
  encode_shared_object_events(<<Body/binary, ?SO_DELETE_DATA, (size(EventData)):32, EventData/binary>>, Events);

encode_shared_object_events(Body, [{send_message, {Name, Args}} | Events]) ->
  EventData = encode_list([Name | Args]),
  encode_shared_object_events(<<Body/binary, ?SO_SEND_MESSAGE, (size(EventData)):32, EventData/binary>>, Events);

encode_shared_object_events(Body, [{EventType, Event} | Events]) ->
  EventData = amf0:encode(Event),
  encode_shared_object_events(<<Body/binary, (encode_so_type(EventType)), (size(EventData)):32, EventData/binary>>, Events);

encode_shared_object_events(Body, [EventType | Events]) ->
  encode_shared_object_events(<<Body/binary, (encode_so_type(EventType)), 0:32>>, Events).


encode_shared_object_data(Data, []) -> Data;
encode_shared_object_data(Data, [{Key, Value}|Properties]) -> 
  encode_shared_object_data(<<Data/binary, (size(Key)):16, Key/binary, (amf0:encode(Value))/binary>>, Properties).

decode_shared_object(<<Length:16, Name:Length/binary, Version:32, 0:32, _:32, Events/binary>>) ->
  decode_shared_object_events(Events, #so_message{name = Name, version = Version, persistent = false});

decode_shared_object(<<Length:16, Name:Length/binary, Version:32, _:32, _:32, Events/binary>>) ->
  decode_shared_object_events(Events, #so_message{name = Name, version = Version, persistent = true}).


decode_shared_object_events(<<>>, #so_message{events = Events} = Message) ->
  Message#so_message{events = lists:reverse(Events)};

decode_shared_object_events(<<EventType, 0:32, Rest/binary>>, #so_message{events = Events} = Message) ->
  decode_shared_object_events(Rest, Message#so_message{events = [decode_so_type(EventType)|Events]});

decode_shared_object_events(<<?SO_SET_ATTRIBUTE, EventDataLength:32, EventData:EventDataLength/binary, Rest/binary>>,
  #so_message{events = Events} = Message) ->
  <<Length:16, Name:Length/binary, Data/binary>> = EventData,
  {Value, _} = amf0:decode(Data),
  decode_shared_object_events(Rest, Message#so_message{events = [{set_attribute, {Name, Value}}|Events]});

decode_shared_object_events(<<?SO_SEND_MESSAGE, EventDataLength:32, EventData:EventDataLength/binary, Rest/binary>>,
  #so_message{events = Events} = Message) ->
  {Name, Data} = amf0:decode(EventData),
  Args = decode_list(Data),
  decode_shared_object_events(Rest, Message#so_message{events = [{send_message, {Name, Args}}|Events]});

decode_shared_object_events(<<?SO_DELETE_ATTRIBUTE, EventDataLength:32, EventData:EventDataLength/binary, Rest/binary>>,
  #so_message{events = Events} = Message) ->
  <<Length:16, Name:Length/binary, _/binary>> = EventData,
  decode_shared_object_events(Rest, Message#so_message{events = [{delete_attribute, Name}|Events]});

decode_shared_object_events(<<EventType, EventDataLength:32, EventData:EventDataLength/binary, Rest/binary>>,
  #so_message{events = Events} = Message) ->
  decode_shared_object_events(Rest, Message#so_message{events = [{decode_so_type(EventType), EventData}|Events]}).

decode_so_type(?SO_CONNECT) -> connect;
decode_so_type(?SO_DISCONNECT) -> disconnect;
decode_so_type(?SO_SET_ATTRIBUTE) -> set_attribute;
decode_so_type(?SO_UPDATE_DATA) -> update_data;
decode_so_type(?SO_UPDATE_ATTRIBUTE) -> update_attribute;
decode_so_type(?SO_SEND_MESSAGE) -> send_message;
decode_so_type(?SO_STATUS) -> status;
decode_so_type(?SO_CLEAR_DATA) -> clear_data;
decode_so_type(?SO_DELETE_DATA) -> delete_data;
decode_so_type(?SO_DELETE_ATTRIBUTE) -> delete_attribute;
decode_so_type(?SO_INITIAL_DATA) -> initial_data.

encode_so_type(Type) when is_integer(Type) -> Type;
encode_so_type(connect) -> ?SO_CONNECT;
encode_so_type(disconnect) -> ?SO_DISCONNECT;
encode_so_type(set_attribute) -> ?SO_SET_ATTRIBUTE;
encode_so_type(update_data) -> ?SO_UPDATE_DATA;
encode_so_type(update_attribute) -> ?SO_UPDATE_ATTRIBUTE;
encode_so_type(send_message) -> ?SO_SEND_MESSAGE;
encode_so_type(status) -> ?SO_STATUS;
encode_so_type(clear_data) -> ?SO_CLEAR_DATA;
encode_so_type(delete_data) -> ?SO_DELETE_DATA;
encode_so_type(delete_attribute) -> ?SO_DELETE_ATTRIBUTE;
encode_so_type(initial_data) -> ?SO_INITIAL_DATA.



%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").


element1_test() ->
  ?assertEqual(undefined, rtmp:element(5, {})).

element2_test() ->
  ?assertEqual(a, rtmp:element(1, {a,b})).

element3_test() ->
  ?assertEqual(b, rtmp:element(2, {a,b})).


setelement1_test() ->
  ?assertEqual({a,b}, rtmp:setelement(2, {a}, b)).

setelement2_test() ->
  ?assertEqual({undefined,b}, rtmp:setelement(2, {}, b)).

setelement3_test() ->
  ?assertEqual({undefined, undefined,b}, rtmp:setelement(3, {}, b)).




