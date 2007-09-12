%%% Copyright (c) 2007 Roberto Saccon <rsaccon@gmail.com>
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

%%% -------------------------------------------------------------------
%%% Author  : rsaccon@gmail.com
%%% Description : AMF encoder / decoder
%%%
%%% Created : 18 Nov,06
%%% -------------------------------------------------------------------
-module(erlyvideo_amf).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("erlyvideo.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([parse/1, parse_signature/1, to_binary/1]).


%% --------------------------------------------------------------------
%% definitions
%% --------------------------------------------------------------------

%% AMF DataTypes
%%--------------
-define(AMF_NUMBER, 0).
-define(AMF_BOOL, 1).
-define(AMF_STRING, 2).
-define(AMF_OBJECT, 3).
-define(AMF_NULL, 5).
-define(AMF_END_OF_OBJECT, 9).


%% ====================================================================
%% External functions
%% ====================================================================

parse(<<?AMF_NUMBER:8, Double:64/float, Rest/binary>>) ->
    {number, round(Double), Rest};

parse(<<?AMF_BOOL:8, Bool:1/binary, Rest/binary>>) ->
    case Bool of
        0 ->
            {bool, false, Rest};
        1 ->
            {bool, true, Rest};
        _ ->
            {undefined, true, Rest}
    end;

parse(<<?AMF_STRING, Rest/binary>>) ->
    {String, NewRest} = parse_string(Rest),
    {string, String, NewRest};

parse(<<?AMF_OBJECT, Rest/binary>>) ->
    parse_object(Rest, []);

parse(<<?AMF_NULL, Rest/binary>>) ->
    {null, undefined, Rest}.


parse_signature(Data) ->
    {string, Name, Rest} = parse(Data),
    {number, Iid, Rest2} = parse(Rest),
    {list_to_atom(Name), Iid, to_list(Rest2, [])}. 


to_binary(Value) when Value =:= null ->
    <<?AMF_NULL>>;

to_binary(Value) when (Value =:= true) or (Value =:= false) ->
    case Value of
        true ->
            <<?AMF_NUMBER, 1:8>>;
        false ->
            <<?AMF_NUMBER, 0:8>>
                end;

to_binary(Value) when is_number(Value) ->
    <<?AMF_NUMBER, Value:64/float>>;

to_binary([First | _] = Value) when is_list(Value) and is_tuple(First) ->
    object_to_binary(Value, []);

to_binary(Value) when is_list(Value) ->
    Len = length(Value),
    Bin = list_to_binary(Value),
    <<?AMF_STRING, Len:16/unsigned, Bin/binary>>;

to_binary({Cmd, Id, Args}) ->
  Cmd2 = to_binary(Cmd),
  Id2 = to_binary(Id),   
  Data = to_binary(Args, []),         
  <<Cmd2/binary, Id2/binary, Data/binary>>.     
 

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_object(<<0, 0, 9, Rest/binary>>, KeyValueList) ->
    {object, KeyValueList, Rest};

parse_object(Data, KeyValueList) ->    
    case parse_string(Data) of
        {Key, Rest} ->
            case parse(Rest) of
                {_,  Value, After} ->
                    NewKeyValueList =  [{list_to_atom(Key), Value} 
                                        | KeyValueList],
                    parse_object(After, NewKeyValueList);     
                _ ->
                    {error, Key, Rest}
            end
    end. 

parse_string(<<Len:16/unsigned, Rest/binary>>) ->
    <<String:Len/binary, NewRest/binary>> = Rest,
    {binary_to_list(String), NewRest}.


to_list(Data, Args) ->       
    case parse(Data) of
        {Type, Value, <<>>} ->
            lists:reverse([{Type, Value} | Args]);
        {Type, Value, After} ->            
            to_list(After, [{Type, Value} | Args])
    end.

to_binary([], List) ->
  list_to_binary(lists:reverse(List));
to_binary([First | Rest], List) ->
  to_binary(Rest, [to_binary(First) | List]).

object_to_binary([], List) ->
    list_to_binary([<<?AMF_OBJECT>> 
                    | lists:reverse([?AMF_END_OF_OBJECT | [0 | [0 | List]]])]);

object_to_binary([{Key, Value} | Rest], List) ->   
    KeyString = atom_to_list(Key),
    KeyStringLen = length(KeyString),
    NewList1 = [binary_to_list(<<KeyStringLen:16>>) | List],
    NewList2 = [KeyString | NewList1],   
    NewList3 = [to_binary(Value) | NewList2], 
    object_to_binary(Rest, NewList3).                                                             
