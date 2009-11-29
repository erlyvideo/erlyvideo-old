%%-------------------------------------------------------------------
%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev.
%% @doc AcknowledgeMessage Deserialization.
%% @end
%%-------------------------------------------------------------------
-module(amf_AcknowledgeMessage).
-export([decode_members/4]).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).
-define(CLEAR(Byte, Flag), ((Byte) band bnot Flag)).

decode_members(Data, Strings, Objects, Traits) ->
    {AsyncMessageMembers, Rest, Strings1, Objects1, Traits1} =
	amf_AsyncMessage:decode_members(Data, Strings, Objects, Traits),
    {Bytes, Rest1} = amf_AbstractMessage:decode_flag_bytes(Rest),
    Flags = decode_flags(Bytes),
    {_IgnoredMembers, Rest2, Strings2, Objects2, Traits2} =
	amf_AbstractMessage:decode_members(Flags, Rest1, Strings1,
					   Objects1, Traits1, []),
    {AsyncMessageMembers, Rest2, Strings2, Objects2, Traits2}.

decode_flags([B]) ->
    amf_AbstractMessage:decode_ignored_flags(B, []).
