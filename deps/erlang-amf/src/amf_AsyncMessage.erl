%%-------------------------------------------------------------------
%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev.
%% @doc AsyncMessage Deserialization.
%% @end
%%-------------------------------------------------------------------
-module(amf_AsyncMessage).
-export([decode_members/4]).

-define(CORRELATION_ID,         1).
-define(CORRELATION_ID_BYTES,   2).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).
-define(CLEAR(Byte, Flag), ((Byte) band bnot Flag)).

decode_members(Data, Strings, Objects, Traits) ->
    {AbstractMessageMembers, Rest, Strings1, Objects1, Traits1} =
	amf_AbstractMessage:decode_members(Data, Strings, Objects, Traits),
    {Bytes, Rest1} = amf_AbstractMessage:decode_flag_bytes(Rest),
    Flags = decode_flags(Bytes),
    {AsyncMessageMembers, Rest2, Strings2, Objects2, Traits2} =
	amf_AbstractMessage:decode_members(Flags, Rest1, Strings1,
					   Objects1, Traits1, []),
    Members = AbstractMessageMembers ++ AsyncMessageMembers,
    {Members, Rest2, Strings2, Objects2, Traits2}.

decode_flags([B]) ->
    decode_flags1(B, []).

decode_flags1(0, Acc) ->
    lists:reverse(Acc);
decode_flags1(B, Acc) when ?IS_SET(B, ?CORRELATION_ID) ->
    decode_flags1(?CLEAR(B, ?CORRELATION_ID), [correlationId | Acc]);
decode_flags1(B, Acc) when ?IS_SET(B, ?CORRELATION_ID_BYTES) ->
    decode_flags1(?CLEAR(B, ?CORRELATION_ID_BYTES), [correlationId | Acc]);
decode_flags1(B, Acc) ->
    amf_AbstractMessage:decode_ignored_flags(B bsr 2, Acc).
