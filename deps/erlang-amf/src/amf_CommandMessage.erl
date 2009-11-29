%%-------------------------------------------------------------------
%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev.
%% @doc CommandMessage Deserialization.
%% @end
%%-------------------------------------------------------------------
-module(amf_CommandMessage).
-export([decode_members/4]).

-define(OPERATION,   1).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).
-define(CLEAR(Byte, Flag), ((Byte) band bnot Flag)).

decode_members(Data, Strings, Objects, Traits) ->
    {AsyncMessageMembers, Rest, Strings1, Objects1, Traits1} =
	amf_AsyncMessage:decode_members(Data, Strings, Objects, Traits),
    {Bytes, Rest1} = amf_AbstractMessage:decode_flag_bytes(Rest),
    Flags = decode_flags(Bytes),
    {CommandMessageMembers, Rest2, Strings2, Objects2, Traits2} =
	amf_AbstractMessage:decode_members(Flags, Rest1, Strings1,
					   Objects1, Traits1, []),
    Members = AsyncMessageMembers ++ CommandMessageMembers,
    {Members, Rest2, Strings2, Objects2, Traits2}.

decode_flags([B]) ->
    decode_flags1(B, []).

decode_flags1(0, Acc) ->
    lists:reverse(Acc);
decode_flags1(B, Acc) when ?IS_SET(B, ?OPERATION) ->
    decode_flags1(?CLEAR(B, ?OPERATION), [operation | Acc]);
decode_flags1(B, Acc) ->
    amf_AbstractMessage:decode_ignored_flags(B bsr 1, Acc).
