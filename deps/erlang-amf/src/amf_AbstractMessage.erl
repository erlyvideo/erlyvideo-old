%%-------------------------------------------------------------------
%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev.
%% @doc AbstractMessage Deserialization.
%% @end
%%-------------------------------------------------------------------
-module(amf_AbstractMessage).
-export([decode_members/4,
	 decode_members/6,
	 decode_flag_bytes/1,
	 decode_ignored_flags/2]).

-define(BODY,                 1).
-define(CLIENT_ID,            2).
-define(DESTINATION,          4).
-define(HEADERS,              8).
-define(MESSAGE_ID,          16).
-define(TIMESTAMP,           32).
-define(TIME_TO_LIVE,        64).
-define(HAS_NEXT,           128).

-define(CLIENT_ID_BYTES,      1).
-define(MESSAGE_ID_BYTES,     2).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).
-define(CLEAR(Byte, Flag), ((Byte) band bnot Flag)).

decode_members(Data, Strings, Objects, Traits) ->
    {Bytes, Rest} = decode_flag_bytes(Data),
    Flags = decode_flags(Bytes),
    decode_members(Flags, Rest, Strings, Objects, Traits, []).

decode_flag_bytes(Data) ->
    decode_flag_bytes(Data, []).

decode_flag_bytes(<<B, Rest/binary>>, Acc) when ?IS_SET(B, ?HAS_NEXT) ->
    decode_flag_bytes(Rest, [?CLEAR(B, ?HAS_NEXT) | Acc]);
decode_flag_bytes(<<B, Rest/binary>>, Acc) ->
    {lists:reverse([B | Acc]), Rest}.

decode_flags([B1, B2]) ->
    decode_flags1(B1, []) ++ decode_flags2(B2, []);
decode_flags([B]) ->
    decode_flags1(B, []).

decode_flags1(0, Acc) ->
    lists:reverse(Acc);
decode_flags1(B, Acc) when ?IS_SET(B, ?BODY) ->
    decode_flags1(?CLEAR(B, ?BODY), [body | Acc]);
decode_flags1(B, Acc) when ?IS_SET(B, ?CLIENT_ID) ->
    decode_flags1(?CLEAR(B, ?CLIENT_ID), [clientId | Acc]);
decode_flags1(B, Acc) when ?IS_SET(B, ?DESTINATION) ->
    decode_flags1(?CLEAR(B, ?DESTINATION), [destination | Acc]);
decode_flags1(B, Acc) when ?IS_SET(B, ?HEADERS) ->
    decode_flags1(?CLEAR(B, ?HEADERS), [headers | Acc]);
decode_flags1(B, Acc) when ?IS_SET(B, ?MESSAGE_ID) ->
    decode_flags1(?CLEAR(B, ?MESSAGE_ID), [messageId | Acc]);
decode_flags1(B, Acc) when ?IS_SET(B, ?TIMESTAMP) ->
    decode_flags1(?CLEAR(B, ?TIMESTAMP), [timestamp | Acc]);
decode_flags1(B, Acc) when ?IS_SET(B, ?TIME_TO_LIVE) ->
    decode_flags1(?CLEAR(B, ?TIME_TO_LIVE), [timeToLive | Acc]).

decode_flags2(0, Acc) ->
    lists:reverse(Acc);
decode_flags2(B, Acc) when ?IS_SET(B, ?CLIENT_ID_BYTES) ->
    decode_flags2(?CLEAR(B, ?CLIENT_ID_BYTES), [clientId | Acc]);
decode_flags2(B, Acc) when ?IS_SET(B, ?MESSAGE_ID_BYTES) ->
    decode_flags2(?CLEAR(B, ?MESSAGE_ID_BYTES), [messageId | Acc]);
decode_flags2(B, Acc) ->
    decode_ignored_flags(B bsr 2, Acc).

decode_ignored_flags(0, Acc) ->
    Acc;
decode_ignored_flags(B, Acc) when ?IS_SET(B, 1) ->
    decode_ignored_flags(B bsr 1, [ignore | Acc]);
decode_ignored_flags(B, Acc) ->
    decode_ignored_flags(B bsr 1, Acc).

decode_members([], Rest, Strings, Objects, Traits, Acc) ->
    {lists:reverse(Acc), Rest, Strings, Objects, Traits};
decode_members([M | Flags], Data, Strings, Objects, Traits, Acc) ->
    {V, Rest, Strings1, Objects1, Traits1} =
	amf3:decode(Data, Strings, Objects, Traits),
    Acc1 = case M of
	       ignore -> Acc;
	       _ -> [{M, V} | Acc]
	   end,
    decode_members(Flags, Rest, Strings1, Objects1, Traits1, Acc1).
