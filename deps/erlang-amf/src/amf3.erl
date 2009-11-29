%%-------------------------------------------------------------------
%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev.
%% @doc AMF3 serialization/deserialization.
%% @end
%%-------------------------------------------------------------------
-module(amf3).
-export([encode/1, decode/1]).

-define(UNDEFINED, 16#00).
-define(NULL,      16#01).
-define(FALSE,     16#02).
-define(TRUE,      16#03).
-define(INTEGER,   16#04).
-define(DOUBLE,    16#05).
-define(STRING,    16#06).
-define(XMLDOC,    16#07).
-define(DATE,      16#08).
-define(ARRAY,     16#09).
-define(OBJECT,    16#0A).
-define(XML,       16#0B).
-define(BYTEARRAY, 16#0C).

%% @type proplist() = [{atom() | binary(), amf3()}].
%% @type object() = {object, Class::binary(), Members::proplist()}.
%% @type date() = {date, MilliSecs::float(), TimeZone::integer()}.
%% @type xmldoc() = {xmldoc, Contents::binary()}.
%% @type xml() = {xml, Contents::binary()}.
%% @type array() = [{Key::binary(), Value::amf3()} | amf3()].
%% @type bytearray() = {bytearray, Bytes::binary()}.
%% @type amf3() = undefined | null | false | true | integer() |
%%       double() | binary() | xmldoc() | date() | array() |
%%       object() | xml() | bytearray().

-record(trait, {class, is_dynamic, is_externalizable, property_names}).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).

%% @doc Deserialize Erlang terms from AMF3.
%% @spec (binary()) -> {amf3(), Rest::binary()}
decode(Data) ->
    Empty = gb_trees:empty(),
    {AMF, Rest, _, _, _} = decode(Data, Empty, Empty, Empty),
    {AMF, Rest}.

decode(<<?UNDEFINED, Rest/binary>>, Strings, Objects, Traits) ->
    {undefined, Rest, Strings, Objects, Traits};
decode(<<?NULL, Rest/binary>>, Strings, Objects, Traits) ->
    {null, Rest, Strings, Objects, Traits};
decode(<<?FALSE, Rest/binary>>, Strings, Objects, Traits) ->
    {false, Rest, Strings, Objects, Traits};
decode(<<?TRUE, Rest/binary>>, Strings, Objects, Traits) ->
    {true, Rest, Strings, Objects, Traits};
decode(<<?INTEGER, Data/binary>>, Strings, Objects, Traits) ->
    {UInt29, Rest} = decode_uint29(Data),
    {uint29_to_int29(UInt29), Rest, Strings, Objects, Traits};
decode(<<?DOUBLE, Double:64/float, Rest/binary>>, Strings, Objects, Traits) ->
    {Double, Rest, Strings, Objects, Traits};
decode(<<?STRING, Data/binary>>, Strings, Objects, Traits) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    {String, Rest, Strings1, Objects, Traits};
decode(<<?XMLDOC, Data/binary>>, Strings, Objects, Traits) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    {{xmldoc, String}, Rest, Strings1, Objects, Traits};
decode(<<?DATE, Data/binary>>, Strings, Objects, Traits) ->
    try decode_as_reference(Data, Objects) of
	{Date, Rest} ->
	    {Date, Rest, Strings, Objects, Traits}
    catch
	{inline, _, <<TS:64/float, Rest/binary>>} ->
	    Date = {date, TS, 0},
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Date, Objects),
	    {Date, Rest, Strings, Objects1, Traits}
    end;
decode(<<?ARRAY, Data/binary>>, Strings, Objects, Traits) ->
    try decode_as_reference(Data, Objects) of
	{Array, Rest} ->
	    {Array, Rest, Strings, Objects, Traits}
    catch
	{inline, Len, Rest} ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, [], Objects),
	    {Associative, Rest2, Strings2, Objects2, Traits2} =
		decode_assoc(Rest, Strings, Objects1, Traits, []),
	    {Dense, Rest3, Strings3, Objects3, Traits3} =
		decode_dense(Len, Rest2, Strings2, Objects2, Traits2, []),
	    Array = Associative ++ Dense,
	    Objects4 = gb_trees:update(Key, Array, Objects3),
	    {Array, Rest3, Strings3, Objects4, Traits3}
    end;
decode(<<?OBJECT, Data/binary>>, Strings, Objects, Traits) ->
    try decode_as_reference(Data, Objects) of
	{Object, Rest} ->
	    {Object, Rest, Strings, Objects, Traits}
    catch
	{inline, Len, Rest} ->
	    {Trait, Rest1, Strings1, Traits1} =
		decode_trait(Len, Rest, Strings, Traits),
	    Object0 = {object, Trait#trait.class, []},
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Object0, Objects),
	    {Object, Rest2, Strings2, Objects2, Traits2} =
		decode_object(Trait, Rest1, Strings1, Objects1, Traits1),
	    Objects3 = gb_trees:update(Key, Object, Objects2),
	    {Object, Rest2, Strings2, Objects3, Traits2}
    end;
decode(<<?XML, Data/binary>>, Strings, Objects, Traits) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    {{xml, String}, Rest, Strings1, Objects, Traits};
decode(<<?BYTEARRAY, Data/binary>>, Strings, Objects, Traits) ->
    {ByteArray, Rest, Objects1} =  decode_bytearray(Data, Objects),
    {ByteArray, Rest, Strings, Objects1, Traits}.

decode_as_reference(Bin, Tree) ->
    case decode_uint29(Bin) of
	{Int, Rest} when ?IS_SET(Int, 1) ->
	    throw({inline, Int bsr 1, Rest});
	{Int, Rest} ->
	    {gb_trees:get(Int bsr 1, Tree), Rest}
    end.

decode_uint29(Data) ->
    decode_uint29(Data, 0, 0).

decode_uint29(<<1:1, Num:7, Data/binary>>, Result, N) when N < 3 ->
    decode_uint29(Data, (Result bsl 7) bor Num, N + 1);
decode_uint29(<<0:1, Num:7, Data/binary>>, Result, N) when N < 3 ->
    {(Result bsl 7) bor Num, Data};
decode_uint29(<<Byte, Data/binary>>, Result, _N) ->
    {(Result bsl 8) bor Byte, Data}.

uint29_to_int29(UInt29) ->
    case UInt29 >= (1 bsl 28) of
	true ->
	    UInt29 - (1 bsl 29);
	false ->
	    UInt29
    end.

decode_string(Data, Strings) ->
    try decode_as_reference(Data, Strings) of
	{String, Rest} ->
	    {String, Rest, Strings}
    catch
	{inline, Len, Rest} ->
	    <<String:Len/binary, Rest1/binary>> = Rest,
	    {String, Rest1, insert_string(String, Strings)}
    end.

decode_bytearray(Data, Objects) ->
    try decode_as_reference(Data, Objects) of
	{ByteArray, Rest} ->
	    {ByteArray, Rest, Objects}
    catch
	{inline, Len, Rest} ->
	    <<Bytes:Len/binary, Rest1/binary>> = Rest,
	    Key = gb_trees:size(Objects),
	    ByteArray = {bytearray, Bytes},
	    {ByteArray, Rest1, gb_trees:insert(Key, ByteArray, Objects)}
    end.

decode_assoc(Data, Strings, Objects, Traits, Acc) ->
    case decode_string(Data, Strings) of
	{<<>>, Rest, Strings1} ->
	    {lists:reverse(Acc), Rest, Strings1, Objects, Traits};
	{Key, Rest, Strings1} ->
	    {Value, Rest1, S2, O2, T2} =
		decode(Rest, Strings1, Objects, Traits),
	    decode_assoc(Rest1, S2, O2, T2, [{Key, Value} | Acc])
    end.

decode_dense(0, Rest, Strings, Objects, Traits, Acc) ->
    {lists:reverse(Acc), Rest, Strings, Objects, Traits};
decode_dense(N, Data, Strings, Objects, Traits, Acc) ->
    {Element, Rest, S1, O1, T1} = decode(Data, Strings, Objects, Traits),
    decode_dense(N - 1, Rest, S1, O1, T1, [Element | Acc]).

decode_trait(Ref, Data, Strings, Traits) ->
    case Ref band 1 of
	1 ->
	    {ClassName, Rest, Strings1} = decode_string(Data, Strings),
	    {PropertyNames, Rest1, Strings2} =
		decode_strings_as_atoms(Ref bsr 3, Rest, Strings1, []),
	    Trait = #trait{class = ClassName,
			   is_externalizable = ?IS_SET(Ref, 2),
			   is_dynamic = ?IS_SET(Ref, 4),
			   property_names = PropertyNames},
	    Key = gb_trees:size(Traits),
	    Traits1 = gb_trees:insert(Key, Trait, Traits),
	    {Trait, Rest1, Strings2, Traits1};
	0 ->
	    {gb_trees:get(Ref bsr 1, Traits), Data, Strings, Traits}
    end.

decode_strings_as_atoms(0, Rest, Strings, Acc) ->
    {lists:reverse(Acc), Rest, Strings};
decode_strings_as_atoms(N, Data, Strings, Acc) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    Atom = binary_to_atom(String, utf8),
    decode_strings_as_atoms(N - 1, Rest, Strings1, [Atom | Acc]).

decode_object(Trait, Data, Strings, Objects, Traits)
  when Trait#trait.is_externalizable ->
    case Trait#trait.class of
	<<"flex.messaging.io.ArrayCollection">> ->
	    decode(Data, Strings, Objects, Traits);
	<<"flex.messaging.io.ObjectProxy">> ->
	    decode(Data, Strings, Objects, Traits);
	<<"flex.messaging.io.SerializationProxy">> ->
	    decode(Data, Strings, Objects, Traits);
	Class ->
	    Module = external_module(Class),
	    {Members, Rest, Strings1, Objects1, Traits1} =
		Module:decode_members(Data, Strings, Objects, Traits),
	    {{object, Class, Members}, Rest, Strings1, Objects1, Traits1}
    end;
decode_object(Trait, Data, Strings, Objects, Traits) ->
    Len = length(Trait#trait.property_names),
    {PropertyValues, Rest1, Strings1, Objects1, Traits1} =
	decode_dense(Len, Data, Strings, Objects, Traits, []),
    Sealed = lists:zip(Trait#trait.property_names, PropertyValues),
    {Dynamic, Rest2, Strings2, Objects2, Traits2} =
	case Trait#trait.is_dynamic of
	    true ->
		decode_assoc(Rest1, Strings1, Objects1, Traits1, []);
	    false ->
		{[], Rest1, Strings1, Objects1, Traits1}
	end,
    Object = {object, Trait#trait.class, Sealed ++ Dynamic},
    {Object, Rest2, Strings2, Objects2, Traits2}.

external_module(<<"DSA">>) -> amf_AsyncMessage;
external_module(<<"DSC">>) -> amf_CommandMessage;
external_module(<<"DSK">>) -> amf_AcknowledgeMessage;
external_module(Class) ->
    throw({?MODULE, ?LINE, unknown_class, Class}).

%% @doc Serialize Erlang terms into AMF3.
%% @spec (amf3()) -> binary()
encode(AMF3) ->
    Empty = gb_trees:empty(),
    {Bin, _Strings, _Objects, _Traits} = encode(AMF3, Empty, Empty, Empty),
    Bin.

encode(undefined, Strings, Objects, Traits) ->
    {<<?UNDEFINED>>, Strings, Objects, Traits};
encode(null, Strings, Objects, Traits) ->
    {<<?NULL>>, Strings, Objects, Traits};
encode(false, Strings, Objects, Traits) ->
    {<<?FALSE>>, Strings, Objects, Traits};
encode(true, Strings, Objects, Traits) ->
    {<<?TRUE>>, Strings, Objects, Traits};
encode(Integer, Strings, Objects, Traits) when is_integer(Integer) ->
    Bin = encode_int29(Integer),
    {<<?INTEGER, Bin/binary>>, Strings, Objects, Traits};
encode(Double, Strings, Objects, Traits) when is_float(Double) ->
    {<<?DOUBLE, Double/float>>, Strings, Objects, Traits};
encode(String, Strings, Objects, Traits) when is_binary(String) ->
    {Bin, Strings1} = encode_string(String, Strings),
    {<<?STRING, Bin/binary>>, Strings1, Objects, Traits};
encode({xmldoc, String}, Strings, Objects, Traits) ->
    {Bin, Strings1} = encode_string(String, Strings),
    {<<?XMLDOC, Bin/binary>>, Strings1, Objects, Traits};
encode({date, TS, TZ}, Strings, Objects, Traits) ->
    try encode_as_reference({date, TS, TZ}, gb_trees:iterator(Objects)) of
	Bin ->
	    {<<?DATE, Bin/binary>>, Strings, Objects, Traits}
    catch
	noref ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, {date, TS, TZ}, Objects),
	    {<<?DATE, 1, TS:64/float>>, Strings, Objects1, Traits}
    end;
encode(Array, Strings, Objects, Traits) when is_list(Array) ->
    try encode_as_reference(Array, gb_trees:iterator(Objects)) of
	Bin ->
	    {<<?ARRAY, Bin/binary>>, Strings, Objects, Traits}
    catch
	noref ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Array, Objects),
	    F = fun({K, _V}) when is_binary(K) -> true;
		   (_V) -> false
		end,
	    {AssocList, DenseList} = lists:partition(F, Array),
	    {AssocBin, Strings2, Objects2, Traits2} =
		encode_assoc(AssocList, [], Strings, Objects1, Traits),
	    DenseLen = encode_uint29(length(DenseList) bsl 1 bor 1),
	    {DenseBin, Strings3, Objects3, Traits3} =
		encode_dense(DenseList, [], Strings2, Objects2, Traits2),
	    Bin = <<?ARRAY, DenseLen/binary, AssocBin/binary,
		   DenseBin/binary>>,
	    {Bin, Strings3, Objects3, Traits3}
    end;
encode({object, Class, Members} = Object, Strings, Objects, Traits) ->
    try encode_as_reference(Object, gb_trees:iterator(Objects)) of
	Bin ->
	    {<<?OBJECT, Bin/binary>>, Strings, Objects, Traits}
    catch
	noref ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Object, Objects),
	    F = fun({K, _}) when is_atom(K)   -> true;
		   ({K, _}) when is_binary(K) -> false
		end,
	    {SealedMembers, DynamicMembers} =
		try lists:partition(F, Members)
		catch
		    error:function_clause ->
			throw({?MODULE, ?LINE, badmember})
		end,
	    {SealedKeys, SealedVals} = lists:unzip(SealedMembers),
	    Trait = #trait{class = Class,
			   is_dynamic = (length(DynamicMembers) > 0),
			   is_externalizable = false, % TODO: handle ext
			   property_names = SealedKeys
			  },
	    {TraitBin, Strings1, Traits1} =
		encode_trait(Trait, Strings, Traits),
	    {Sealed, Strings2, Objects2, Traits2} =
		encode_dense(SealedVals, [], Strings1, Objects1, Traits1),
	    {Dynamic, Strings3, Objects3, Traits3} =	    
		case Trait#trait.is_dynamic of
		    true ->
			encode_assoc(DynamicMembers, [],
				     Strings2, Objects2, Traits2);
		    false ->
			{<<>>, Strings2, Objects2, Traits2}
		end,
	    Bin = <<?OBJECT, TraitBin/binary, Sealed/binary, Dynamic/binary>>,
	    {Bin, Strings3, Objects3, Traits3}
    end;
encode({xml, String}, Strings, Objects, Traits) ->
    {Bin, Strings1} = encode_string(String, Strings),
    {<<?XML, Bin/binary>>, Strings1, Objects, Traits};
encode({bytearray, _Bytes} = ByteArray, Strings, Objects, Traits) ->
    {Bin, Objects1} = encode_bytearray(ByteArray, Objects),
    {<<?BYTEARRAY, Bin/binary>>, Strings, Objects1, Traits};
encode(Value, _Strings, _Objects, _Traits) ->
    throw({?MODULE, ?LINE, badval, Value}).

encode_int29(I) when I >= -16#10000000, I < 0 ->
    encode_uint29(16#20000000 + I);
encode_int29(I) when I =< 16#0FFFFFFF ->
    encode_uint29(I).

encode_uint29(I) when I >= 16#00000000, I =< 16#0000007F ->
    <<I>>;
encode_uint29(I) when I >= 16#00000080, I =< 16#00003FFF ->
    X1 = 16#80 bor (I bsr 7),
    X2 = I band 16#7F,
    <<X1, X2>>;
encode_uint29(I) when I >= 16#00004000, I =< 16#001FFFFF ->
    X1 = 16#80 bor (I bsr 14),
    X2 = 16#80 bor (I bsr 7),
    X3 = I band 16#7F,
    <<X1, X2, X3>>;
encode_uint29(I) when I >= 16#00200000, I =< 16#1FFFFFFF ->
    X1 = 16#80 bor (I bsr 22),
    X2 = 16#80 bor (I bsr 15),
    X3 = 16#80 bor (I bsr 8),
    X4 = I band 16#FF,
    <<X1, X2, X3, X4>>;
encode_uint29(_) ->
    throw({?MODULE, ?LINE, badrange}).

encode_string(String, Strings) ->
    try encode_as_reference(String, gb_trees:iterator(Strings)) of
	Bin ->
	    {Bin, Strings}
    catch
	noref ->
	    Strings1 = insert_string(String, Strings),
	    Ref = encode_uint29(size(String) bsl 1 bor 1),
	    {<<Ref/binary, String/binary>>, Strings1}
    end.

encode_bytearray({bytearray, Bytes} = ByteArray, Objects) ->
    try encode_as_reference(ByteArray, gb_trees:iterator(Objects)) of
	Bin ->
	    {Bin, Objects}
    catch
	noref ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, ByteArray, Objects),
	    Ref = encode_uint29(size(Bytes) bsl 1 bor 1),
	    {<<Ref/binary, Bytes/binary>>, Objects1}
    end.

encode_as_reference(Value, Iterator0) ->
    case gb_trees:next(Iterator0) of
	{Key, Value, _} when is_record(Value, trait) ->
	    %% Obj is inline, Trait is a 27bit reference.
	    encode_uint29(Key bsl 2 bor 1);
	{Key, Value, _} ->
	    encode_uint29(Key bsl 1);
	{_, _, Iterator1} ->
	    encode_as_reference(Value, Iterator1);
	none ->
	    throw(noref)
    end.

encode_assoc([{Key, Value} | Rest], Acc, Strings, Objects, Traits)
  when is_binary(Key) ->
    {KeyBin, Strings1} = encode_string(Key, Strings),
    {ValBin, Strings2, Objects2, Traits2} =
	encode(Value, Strings1, Objects, Traits),
    Bin = <<KeyBin/binary, ValBin/binary>>,
    encode_assoc(Rest, [Bin | Acc], Strings2, Objects2, Traits2);
encode_assoc([], Acc, Strings, Objects, Traits) ->
    {EmptyString, _} = encode_string(<<>>, Strings),
    Bin = list_to_binary(lists:reverse([EmptyString | Acc])),
    {Bin, Strings, Objects, Traits};
encode_assoc([Property | _Rest], _Acc, _Strings, _Objects, _Traits) ->
    throw({?MODULE, ?LINE, badprop, Property}).

encode_dense([], Acc, Strings, Objects, Traits) ->
    {list_to_binary(lists:reverse(Acc)), Strings, Objects, Traits};
encode_dense([Element | Rest], Acc, Strings, Objects, Traits) ->
    {Bin, Strings1, Objects1, Traits1} =
	encode(Element, Strings, Objects, Traits),
    encode_dense(Rest, [Bin | Acc], Strings1, Objects1, Traits1).

encode_trait(Trait, Strings, Traits) ->
    try encode_as_reference(Trait, gb_trees:iterator(Traits)) of
	Bin ->
	    {Bin, Strings, Traits}
    catch
	noref ->
	    Key = gb_trees:size(Traits),
	    Traits1 = gb_trees:insert(Key, Trait, Traits),
	    {Class, Strings1} = encode_string(Trait#trait.class, Strings),
	    Ref0 = length(Trait#trait.property_names) bsl 4,
	    Ref1 = Ref0 bor 2#011, % non-ext, trait-inline, obj-inline
	    Ref2 = case Trait#trait.is_dynamic of
		       true ->
			   Ref1 bor 2#1000;
		       false ->
			   Ref1
		   end,
	    RefBin = encode_uint29(Ref2),
	    {PropNames, Strings2} =
		encode_atoms_as_strings(Trait#trait.property_names, Strings1),
	    Bin = <<RefBin/binary, Class/binary, PropNames/binary>>,
	    {Bin, Strings2, Traits1}
    end.

encode_atoms_as_strings(Atoms, Strings) ->
    encode_atoms_as_strings(Atoms, [], Strings).

encode_atoms_as_strings([], Acc, Strings) ->
    {list_to_binary(lists:reverse(Acc)), Strings};
encode_atoms_as_strings([Atom | Rest], Acc, Strings) ->
    {Bin, Strings1} = encode_string(atom_to_binary(Atom, utf8), Strings),
    encode_atoms_as_strings(Rest, [Bin | Acc], Strings1).

insert_string(<<>>, Strings) ->
    Strings;
insert_string(String, Strings) ->
    gb_trees:insert(gb_trees:size(Strings), String, Strings).
