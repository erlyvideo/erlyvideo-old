%%-------------------------------------------------------------------
%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev.
%% @doc AMF serialization/deserialization.
%% @end
%%-------------------------------------------------------------------
-module(amf).
-export([encode_packet/1, decode_packet/1]).

-include("amf.hrl").

%% @doc Decode AMF Packet.
%% @spec (Data::binary()) -> #amf_packet{}
decode_packet(<<Version:16, HeaderCount:16, Data/binary>>) ->
    {Headers, <<MessageCount:16, Rest/binary>>} =
	decode_header(Data, HeaderCount, [], Version),
    {Messages, _Rest} = decode_message(Rest, MessageCount, [], Version),
    #amf_packet{version = Version, headers = Headers, messages = Messages}.

decode_header(Rest, 0, Acc, _V) ->
    {lists:reverse(Acc), Rest};
decode_header(<<NL:16, Name:NL/binary, M, _DL:32, Data/binary>>, N, Acc, V) ->
    {Body, Rest} = amf0:decode(Data),
    Header = #amf_header{name = Name, must_understand = (M /= 0), body = Body},
    decode_header(Rest, N - 1, [Header | Acc], V).

decode_message(Rest, 0, Acc, _V) ->
    {lists:reverse(Acc), Rest};
decode_message(<<TL:16, Target:TL/binary, RL:16, Response:RL/binary,
		_DL:32, Data/binary>>, N, Acc, V) ->
    {Body, Rest} = amf0:decode(Data),
    Message = #amf_message{target = Target, response = Response, body = Body},
    decode_message(Rest, N - 1, [Message | Acc], V).

%% @doc Encode AMF Packet.
%% @spec (Packet::#amf_packet{}) -> binary()
encode_packet(#amf_packet{version = Version, headers = Headers,
			  messages = Messages}) ->
    HeadersBin = encode_headers(Headers, [], Version),
    MessagesBin = encode_messages(Messages, [], Version),
    <<Version:16, (length(Headers)):16, HeadersBin/binary,
     (length(Messages)):16, MessagesBin/binary>>.

encode_headers([], Acc, _Version) ->
    list_to_binary(lists:reverse(Acc));
encode_headers([Header | Rest], Acc, Version) ->
    Name = Header#amf_header.name,
    M = case Header#amf_header.must_understand of
	    true ->  1;
	    false -> 0
	end,
    Body = encode(Header#amf_header.body, Version),
    Bin = <<(size(Name)):16, Name/binary, M, (size(Body)):32, Body/binary>>,
    encode_headers(Rest, [Bin | Acc], Version).

encode_messages([], Acc, _Version) ->
    list_to_binary(lists:reverse(Acc));
encode_messages([Message | Rest], Acc, Version) ->
    #amf_message{target = Target, response = Response, body = Body} = Message,
    Bin0 = encode(Body, Version),
    Bin1 = <<(size(Target)):16, Target/binary,
	    (size(Response)):16, Response/binary,
	    (size(Bin0)):32, Bin0/binary>>,
    encode_messages(Rest, [Bin1 | Acc], Version).

encode(Body, 0) -> amf0:encode(Body);
encode(Body, 3) -> amf0:encode({avmplus, Body}).
