%%% @author     Max Lapshin <max@maxidoors.ru> [http://maxidoors.ru]
%%% @copyright  2009 Max Lapshin
%%% @doc        AMF3 encoding/decoding module
%%% @reference  See <a href="http://github.com/maxlapshin/erlyvideo" target="_top">http://github.com/maxlapshin/erlyvideo</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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
-module(amf3).
-author('max@maxidoors.ru').
-include("../include/ems.hrl").
-include("../include/amf.hrl").

-export([encode/1, decode/1]).


encode(#amf{command = Command, args = Args, id = Id, type = invoke}) -> 
  <<(encode(Command))/binary, (encode(Id))/binary, (encode(Args, <<>>))/binary>>;

encode(#amf{command = Command, args = Args, type = notify}) -> 
  <<(encode(Command))/binary, (encode(Args, <<>>))/binary>>;

encode(null) -> <<?AMF3_NULL>>;
encode(false) -> <<?AMF3_FALSE>>;
encode(true) -> <<?AMF3_TRUE>>;
encode(Atom) when is_atom(Atom) -> encode(atom_to_list(Atom));
encode(Number) when is_number(Number) -> <<?AMF_NUMBER, Number:64/big-float>>;
encode({mixed_array,List} = Array) when is_list(List) -> encode_mixed_array(Array);
encode({object,List} = Object)     when is_list(List) -> encode_object(Object);
encode({string,List} = _String)    when is_list(List) -> encode(List);
encode([H|_] = List) when is_list(List), is_tuple(H) -> 
	encode_object(List, <<>>);
encode(List) when is_list(List) -> 
  String = unicode:characters_to_binary(List),
	<<?AMF_STRING, (size(String)):16/big-integer, String/binary>>;
encode(_Value) -> 
	?D(_Value),
	<<>>.
%%--------------------------------------------------------------------
%% @spec (List::list(),Bin::binary()) -> any()
%% @hidden
%% @doc Encode AMF call into a binary
%% @end 
%%--------------------------------------------------------------------
encode([],Bin) -> Bin;

encode([{Key, Value} | T], Bin) ->
  encode(T, <<Bin/binary, (encode(Key))/binary, (encode(Value))/binary>>);

encode([Value | T], Bin) ->
  encode(T, <<Bin/binary, (encode(Value))/binary>>).
  

encode_object({object,List} = _Object) -> encode_object(List, <<>>).
encode_object([], Bin) -> <<?AMF_OBJECT,Bin/binary,0,0,9>>;
encode_object([{Key,Value}|T], Bin) ->
	KeyBinary = atom_to_binary(Key, latin1),
	Part = <<0, (size(KeyBinary)), KeyBinary/binary, (encode(Value))/binary>>,
	encode_object(T, <<Bin/binary,Part/binary>>).


encode_mixed_array(List) when is_list(List) -> encode_mixed_array(List, 0, <<>>);
encode_mixed_array({mixed_array,List} = _Array) when is_list(List) -> encode_mixed_array(List, 0, <<>>).

encode_mixed_array([], Length, Bin) -> <<?AMF_MIXED_ARRAY:8/integer, Length:32/big-integer, Bin/binary, 0:16/big-integer, ?AMF_END_OF_OBJECT:8/integer>>;
encode_mixed_array([{Key0,Value}|T], Max, Bin) when is_number(Key0) ->
	Key = number_to_string(Key0),
	NewMax = if
		Key0 > Max -> Key0;
		true -> Max
	end,
	encode_mixed_array([{Key,Value}|T], NewMax, Bin);

encode_mixed_array([{Key,Value}|T], Max, Bin) when is_list(Key) ->
	KeyValue = <<(length(Key)):16/big-integer, (list_to_binary(Key))/binary, (encode(Value))/binary>>,
	encode_mixed_array(T, Max, <<KeyValue/binary, Bin/binary>>);

encode_mixed_array([{KeyAtom,Value}|T], Max, Bin) when is_atom(KeyAtom) ->
  Key = atom_to_binary(KeyAtom, utf8),
	KeyValue = <<(size(Key)):16/big-integer, Key/binary, (encode(Value))/binary>>,
	encode_mixed_array(T, Max, <<KeyValue/binary, Bin/binary>>).

decode_list(List, Data) when size(Data) == 0 ->
  lists:reverse(List);

decode_list(List, Data) ->
  {Type, Value, Rest} = parse(Data),
  Arg = {Type, Value},
  decode_list([Arg | List], Rest).



decode(Bin) ->	
  {string, Command, Rest} = parse(Bin),
  FullArguments = decode_list([], Rest),
  [_Num | Arguments] = FullArguments,
  #amf{command = list_to_atom(Command), args = Arguments, type = invoke}.
  % case parse(Rest) of
  %   {{mixed_array, Rest2}, _Rest3} -> %without id,  set type to notify
  %     Args = decode_args(Rest2, []),
  %     #amf{command = list_to_atom(Command), args = Args, type=notify};
  %   {number, Id, Rest2}  -> 
  %     Arguments = decode_list([], Rest2).
  % 
  %     %            case parse(Rest2) of
  %     %   {{mixed_array, Rest3}, _Rest4} ->
  %     %       Args = decode_args(Rest3, []),
  %     %       _AMF = #amf{command = list_to_atom(Command), id=Id, args = Args, type=invoke};
  %     %   {{object, Rest3}, _Rest4}  -> 
  %     %       %Args = decode_args(Rest3, []),
  %     %       _AMF = #amf{command = list_to_atom(Command), args = {object, Rest3}, type=invoke}
  %     % end;
  %     _AMF = #amf{command = list_to_atom(Command), args = {object, Rest3}, type=invoke};
  %   _ -> ?D("AMF Decode Error")
% end.

parse(<<?AMF3_UNDEF,Rest/binary>>) ->
	{undefined,undefined,Rest};

parse(<<?AMF3_NULL,Rest/binary>>) ->
	{null,null,Rest};

parse(<<?AMF3_FALSE,Rest/binary>>) ->
	{boolean,false,Rest};

parse(<<?AMF3_TRUE,Rest/binary>>) ->
	{boolean,true,Rest};

parse(<<?AMF3_INTEGER, Rest/binary>>) ->
  parse_int(Rest);

parse(<<?AMF3_DOUBLE, Double:64/big-float, Rest/binary>>) ->
	{number, Double, Rest};
	
parse(<<?AMF3_STRING, StringWithLen/binary>>) ->
  parse_string(StringWithLen);

parse(<<?AMF3_ARRAY:8/integer, Rest/binary>>) ->
  {number, Int0, Rest1} = parse_int(Rest),
  DenseCount = Int0 bsr 1,
  case Int0 band 1 of
    0 -> 
      ?D({"Unknown array ref", DenseCount}),
      {array, ["Array ref " ++ integer_to_list(DenseCount)], Rest1};
    1 ->
      {mixed_array, Array1, Rest2} = parse_mixed_array(Rest1, []),
      {array, Array2, Rest3} = parse_array(Rest2, DenseCount, []),
      {array, {Array1, Array2}, Rest3}
	end;

	
parse(<<?AMF_XML:8/integer, Length:32/big-integer, String:Length/binary, Rest/binary>>) ->
	{string, binary_to_list(String), Rest};
parse(<<?AMF_OBJECT:8/integer, Rest/binary>>) ->
	parse_object(Rest,[]);
parse(<<?AMF_MIXED_ARRAY:8/integer,_Index:32,Rest/binary>>) ->
	parse_mixed_array(Rest,[]).



parse_int(<<0:1, Int0:7, Rest/binary>>) ->
	{number,Int0,Rest};

parse_int(<<1:1, Int1:7, 0:1, Int0:7, Rest/binary>>) ->
	{number,Int1 bsl 7 + Int0,Rest};

parse_int(<<1:1, Int2:7, 1:1, Int1:7, 0:1, Int0:7, Rest/binary>>) ->
	{number,Int2 bsl 14 + Int1 bsl 7 + Int0,Rest};

parse_int(<<1:1, Int3:7, 1:1, Int2:7, 1:1, Int1:7, 0:1, Int0:7, Rest/binary>>) ->
	{number,Int3 bsl 21 + Int2 bsl 14 + Int1 bsl 7 + Int0,Rest}.


parse_array(<<>>,_,Array) -> {array,lists:reverse(Array), <<>>};
parse_array(Rest,0,Array) -> {array,lists:reverse(Array), Rest};
parse_array(Data,Length,Array) -> 
	{Type,Value,Rest} = parse(Data),
	parse_array(Rest,Length - 1,[{Type,Value}|Array]).




parse_mixed_array(KeyValue, Array) ->
  {string, Key, Rest1} = parse_string(KeyValue),
  case Key of
    "" -> {mixed_array, Array, Rest1};
    _ ->
      {Type,Value,Rest2} = parse(Rest1),
      parse_mixed_array(Rest2, [{Type, Value} | Array])
  end.


parse_object(<<0:16/big-integer, ?AMF_END_OF_OBJECT:8/integer, Rest/binary>>, KeyValueList) -> {object, lists:reverse(KeyValueList), Rest};
parse_object(Data, KeyValueList) ->
  {Key, Rest} = parse_string(Data),
  case parse(Rest) of
	  {Value, Rest2} -> 
			parse_object(Rest2, [{list_to_atom(Key), Value} | KeyValueList]);
    {_Type, Value, Rest2} ->
			parse_object(Rest2, [{list_to_atom(Key), Value} | KeyValueList])
  end. 

parse_string(StringWithLen) ->
  {number, StringLength, Data} = parse_int(StringWithLen),
  Length = StringLength bsr 1,
  case StringLength band 1 of
    0 ->
      ?D({"String ref", Length}),
    	{string,"String ref "++integer_to_list(Length),Data};
    1 ->
      <<Binary:Length/binary, Rest/binary>> = Data,
    	{string,binary_to_list(Binary),Rest}
  end.



to_number(String) ->
	case string:to_float(String) of
		{F,_} when is_float(F) -> F;
		{error,no_float} ->
			case string:to_integer(String) of
				{I,_} when is_integer(I) -> I;
				{error,no_integer} -> String
			end
	end.


%%--------------------------------------------------------------------
%% @spec (Number::number()) -> string()
%% @hidden
%% @doc Converts a numer into a string/list()
%% @end 
%%--------------------------------------------------------------------
number_to_string(Float) when is_float(Float) -> float_to_list(Float);
number_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer).
