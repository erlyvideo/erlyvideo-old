%%%-------------------------------------------------------------------
%%% File    : hmac256.erl
%%% Author  :  Sriram Krishnan<mail@sriramkrishnan.com>
%%% Description : HMAC-SHA256 implementation. Implementation based on Wikipedia's
%% pseudocode description of HMAC.  Relies on Steve Vinoski's SHA256 implementation
%%% from http://steve.vinoski.net/code/sha2.erl
%%%
%%% Created : 30 Dec 2008
%%%-------------------------------------------------------------------
%% @private
-module(hmac256).
-export([hexdigest/2,digest/2,digest_bin/2,test/0]).
-export([unhex/1]).
-version(1.0).

-spec hexdigest(string()|binary(),string()|binary()) -> string().
hexdigest(Key, Data)->
    digest(Key, Data, true).

-spec digest(string()|binary(),string()|binary()) -> string().
digest(Key, Data) ->
    digest(Key, Data, false).

digest_bin(Key, Data) ->
  list_to_binary(digest(Key,Data)).


digest(Key, Data, Hex) when is_binary(Key) -> 
  digest(binary_to_list(Key), Data, Hex);

digest(Key, Data, Hex) when is_binary(Data) -> 
  digest(Key, binary_to_list(Data), Hex);


digest(Key, Data, Hex) when is_list(Key) and is_list(Data) -> 
    BlockSize = 64,
    
    %% Initialize OPad and IPad arrays filled with magic 0x5c and 0x36 values
    %% respectively. The arrays need to be of the same size as the block length 
    OPad = array:new( [{size,BlockSize},{fixed,true},{default,92}]),
    IPad = array:new ( [{size,BlockSize},{fixed,true},{default,54}]),

    
    
    %% If key is longer than block size, hash it to bring it below block size
    if
	length(Key)>BlockSize -> ShortHashKey = array:from_list(unhex(sha2:digest256(Key),[]));
	true-> ShortHashKey = array:from_list(Key) 
    end,
    

    HashKey = array:resize(BlockSize, ShortHashKey), %% Zero-pad array


    PadUpdateFunc =  fun (Index, Term) ->
		      
		       KeyTerm =  array:get(Index, HashKey),
		       if 
			   KeyTerm=:= undefined -> Term bxor 0;
			   true -> Term bxor KeyTerm
		       end
	       end, 

    OPadUpdated = array:map(PadUpdateFunc, OPad),
    IPadUpdated = array:map(PadUpdateFunc, IPad),
    
    FinalTransform = OPadUpdated:to_list() ++ unhex(sha2:digest256( IPadUpdated:to_list() ++ Data),[]),
    
    if
	Hex =:= true -> sha2:digest256(FinalTransform);
	Hex=:= false -> unhex(sha2:digest256( FinalTransform),[])
    end.

%%
%% Unhex functions adapted from ssl_debug and covered by the Erlang public license
%%
is_hex_digit(C) when C >= $0, C =< $9 -> true;
is_hex_digit(C) when C >= $A, C =< $F -> true;
is_hex_digit(C) when C >= $a, C =< $f -> true;
is_hex_digit(_) -> false.

-spec unhex(string()) -> string().
unhex(S) -> unhex(S, []).

unhex([], Acc) ->
    lists:reverse(Acc);
unhex([_], Acc) ->
    unhex([], Acc);
unhex([$  | Tl], Acc) ->
    unhex(Tl, Acc);
unhex([D1, D2 | Tl], Acc) ->
    case {is_hex_digit(D1), is_hex_digit(D2)} of
        {true, true} ->
            unhex(Tl, [erlang:list_to_integer([D1, D2], 16) | Acc]);
        _ ->
            unhex([], Acc)
    end.



test() ->  
    %% Test cases taken from Python's HMAC 256 test cases

    test(lists:duplicate(20, 11), "Hi There", "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"),
    
    test("Jefe","what do ya want for nothing?","5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"),

    test(lists:duplicate(20,170), lists:duplicate(50, 221), "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"),

    test(lists:duplicate(131, 170), "Test Using Larger Than Block-Size Key - Hash Key First", "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"),

    test(lists:duplicate(131, 170), "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.", "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"),

    ok.
    
test(Key, Data, Expect) ->
    Result = hexdigest(Key, Data),
    try
	Result = Expect,
	io:format("Passed!\n")
    catch _:_ ->
	    io:format("error: expected ~s , got ~s~n", [Expect, Result]),
	    error
    end.
