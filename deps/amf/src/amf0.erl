%%%%===================================================================================================================
%% 
%%  amf0
%%
%%  @author Mrinal Wadhwa < email@mrinalwadhwa.com > [ http://www.mrinalwadhwa.com ]
%%
%%  @doc This module can be used to serialize and deserialize data to/from Action Message Format (AMF) 0, which is a
%%  compact binary format used by Adobe Flash Player to serialize ActionScript object graphs 
%%
%%  @reference <a href="http://bit.ly/amf-spec">AMF 3 Specification</a>
%% 
%%  @end 
%%
%%%%===================================================================================================================

-module(amf0).
-export([decode/1, encode/1]).
-define(D(X), io:format("~p~n", [X])).

%%---------------------------------------
%% @doc Decode AMF 0 encoded binary data
%% @end 
%%---------------------------------------

decode(Data) -> 
  {Read, Remaining, _} = read(Data, dict:new()),
  {Read, Remaining}.
  
%%---------------------------------------
%% @doc Encode data to an AMF 0 encoded 
%%      binary form
%% @end 
%%---------------------------------------

encode(Data) ->  
  {Written, _} = write(Data, dict:new()),
  Written.
  
%%%%===================================================================================================================
%%
%%    Internal       
%%
%%%%===================================================================================================================

%%---------------------------------------
%%  Markers
%%---------------------------------------  

-define(NUMBER,         16#00).
-define(BOOLEAN,        16#01).
-define(STRING,         16#02).
-define(OBJECT,         16#03).
-define(MOVIECLIP,      16#04).
-define(NULL,           16#05).
-define(UNDEFINED,      16#06).
-define(REFERENCE,      16#07).
-define(ECMA_ARRAY,     16#08).
-define(OBJECT_END,     16#09).
-define(STRICT_ARRAY,   16#0A).
-define(DATE,           16#0B).
-define(LONG_STRING,    16#0C).
-define(UNSUPPORTED,    16#0D).
-define(RECORDSET,      16#0E).
-define(XML_DOCUMENT,   16#0F).
-define(TYPED_OBJECT,   16#10).
-define(AVMPLUS_OBJECT, 16#11).

%%---------------------------------------
%%  Read 
%%---------------------------------------

read(<<?NUMBER, Number:64/float, Remaining/binary>>, Objects) -> {Number, Remaining, Objects};

read(<<?BOOLEAN, 16#01, Remaining/binary>>, Objects) -> {true, Remaining, Objects};
read(<<?BOOLEAN, 16#00, Remaining/binary>>, Objects) -> {false, Remaining, Objects};

read(<<?NULL, Remaining/binary>>, Objects) -> {null, Remaining, Objects};
read(<<?UNDEFINED, Remaining/binary>>, Objects) -> {undefined, Remaining, Objects};
read(<<?UNSUPPORTED, Remaining/binary>>, Objects) -> {unsupported, Remaining, Objects};

read(<<?DATE, MilliSeconds:64/float, _:16, Remaining/binary>>, Objects) -> {{date, MilliSeconds}, Remaining, Objects};

read(<<?STRING, Length:16, String:Length/binary, Remaining/binary>>, Objects) -> {String, Remaining, Objects};
read(<<?LONG_STRING, Length:32, String:Length/binary, Remaining/binary>>, Objects) -> {String, Remaining, Objects};

read(<<?XML_DOCUMENT, Length:32, String:Length/binary, Remaining/binary>>, Objects) -> 
    {{xmldoc, String}, Remaining, Objects};

read(<<?AVMPLUS_OBJECT, AMF3/binary>>, Objects) ->
    {Object, Remaining} = amf3:decode(AMF3),
    {{avmplus, Object}, Remaining, Objects};
    
read(<<?STRICT_ARRAY, Size:32, Remaining/binary>>, Objects) ->
    store_in_refs(fun(Objects1) ->
      read_array(Remaining, Size, [], Objects1)
    end, Objects);

read(<<?ECMA_ARRAY, _Size:32, Remaining/binary>>, Objects) ->
    {{object,Val}, Rest, Objects2} = store_in_refs(fun(Objects1) ->
      read_object(Remaining, [], Objects1, ecma_array)
    end, Objects),
    {Val, Rest, Objects2};

read(<<?OBJECT, Remaining/binary>>, Objects) ->
    store_in_refs(fun(Objects1) ->
      read_object(Remaining, [], Objects1, undefined)
    end, Objects);

read(<<?TYPED_OBJECT, Len:16, Class:Len/binary, Remaining/binary>>, Objects) ->
    store_in_refs(fun(Objects1) ->
      read_object(Remaining, [], Objects1, binary_to_atom(Class, utf8))
    end, Objects);

read(<<?REFERENCE, Index:16, Remaining/binary>>, Objects) ->
    ?D({"Read",Index}),
    {dict:fetch(Index, Objects), Remaining, Objects}.

read_array(Remaining, Size, Array, Objects) when length(Array) == Size ->
    {lists:reverse(Array), Remaining, Objects};
  
read_array(Bin, Size, Array, Objects) ->
    {Val, Remaining, Objects1} = read(Bin, Objects),
    read_array(Remaining, Size, [Val|Array], Objects1).


read_object(<<0:16, ?OBJECT_END, Remaining/binary>>, Object, Objects, Class) ->
    Val = case Class of
      undefined -> {object, lists:reverse(Object)};
      ecma_array -> {object, lists:reverse(Object)};
      _ -> {object, Class, lists:reverse(Object)}
    end,
    {Val, Remaining, Objects};

read_object(<<Len:16, Key:Len/binary, Bin/binary>>, Object, Objects, Class) ->
    {Val, Remaining, Objects1} = read(Bin, Objects),
    K = case Class of
      ecma_array -> Key;
      _ -> binary_to_atom(Key, utf8)
    end,
    read_object(Remaining, [{K, Val}|Object], Objects1, Class).

store_in_refs(Fun, Objects) ->
  Index = dict:size(Objects),
  Objects1 = dict:store(Index, place_holder, Objects),
  {Val, Remaining, Objects2} = Fun(Objects1),
  {Val, Remaining, dict:store(Index, Val, Objects2)}.


%%---------------------------------------
%%  Write 
%%---------------------------------------

write(Number, Objects) when is_number(Number) -> {<<?NUMBER, Number:64/float>>, Objects};

write(true, Objects) -> {<<?BOOLEAN, 16#01>>, Objects};
write(false, Objects) -> {<<?BOOLEAN, 16#00>>, Objects};

write(null, Objects) -> {<<?NULL>>, Objects};
write(undefined, Objects) -> {<<?UNDEFINED>>, Objects};
write(unsupported, Objects) -> {<<?UNSUPPORTED>>, Objects};

write({date, MilliSeconds}, Objects) -> {<<?DATE, MilliSeconds:64/float, 16#00, 16#00>>, Objects};

write(Atom,Objects) when is_atom(Atom) -> write(list_to_binary(atom_to_list(Atom)), Objects);
write(String, Objects) when is_binary(String) ->
    Length = size(String),
    case Length =< 65535 of
      true  -> {<<?STRING, Length:16, String/binary>>, Objects};
      false -> {<<?LONG_STRING, Length:32, String/binary>>, Objects}
    end;
    
write({xmldoc, XML}, Objects) ->
    Length = size(XML),
    {<<?XML_DOCUMENT, Length:32, XML/binary>>, Objects};

write({avmplus, Object}, Objects) ->
    Binary = amf3:encode(Object),
    {<<?AVMPLUS_OBJECT, Binary/binary>>, Objects};

write({object, Object}, Objects) ->
    write_object(Object, <<?OBJECT>>, Objects);

write({object, Name, Object}, Objects) ->
    NameS = binarize(Name),
    write_object(Object, <<?TYPED_OBJECT, (size(NameS)):16, NameS/binary>>, Objects);

write([{Key,_Value}|_] = Object, Objects) when is_binary(Key) ->
    write_object(Object, <<?ECMA_ARRAY, (length(Object)):32>>, Objects);


write([{Key,_Value}|_] = Object, Objects) when Key =/= object ->
    write({object, Object}, Objects);

write(Array, Objects) when is_list(Array) ->
    write_array(Array, <<?STRICT_ARRAY, (length(Array)):32>>, Objects).


write_array([], Acc, Objects) ->
    {Acc, Objects};

write_array([Value|Array], Acc, Objects) ->
    {Bin, Objects1} = write(Value, Objects),
    write_array(Array, <<Acc/binary, Bin/binary>>, Objects1).


write_object([], Acc, Objects) ->
    {<<Acc/binary, 0:16, ?OBJECT_END>>, Objects};

write_object([{Key,Value}|Array], Acc, Objects) ->
    {Bin, Objects1} = write(Value, Objects),
    KeyS = binarize(Key),
    write_object(Array, <<Acc/binary, (size(KeyS)):16, KeyS/binary, Bin/binary>>, Objects1).


binarize(S) when is_atom(S) -> atom_to_binary(S, latin1);
binarize(S) when is_list(S) -> list_to_binary(S);
binarize(S) when is_binary(S) -> S;
binarize(S) when is_integer(S) -> binarize(integer_to_list(S)).





    