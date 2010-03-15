-module(amf0_tests).
-include_lib("eunit/include/eunit.hrl").


-define(assertEncode(Term, AMF),  (AMF == amf0:encode(Term))).
-define(assertDecode(Term, AMF),  ({Term,<<>>} == amf0:decode(AMF))).
-define(assertEncodeDecode(Term), ({Term,<<>>} == amf0:decode(amf0:encode(Term)))).
-define(assertDecodeEncode(AMF),  (AMF == amf0:encode(element(1,amf0:decode(AMF))))).

-define(a(Term, AMF),   ?assert(?assertEncode(Term, AMF) and 
                                ?assertDecode(Term, AMF) and 
                                ?assertEncodeDecode(Term) and 
                                ?assertDecodeEncode(AMF))).
                                
-define(_a(Term, AMF), ?_assert(?assertEncode(Term, AMF) and 
                                ?assertDecode(Term, AMF) and 
                                ?assertEncodeDecode(Term) and 
                                ?assertDecodeEncode(AMF))).
                                
number_test_() ->
  [
    ?_a(10,<<16#00,16#40,16#24,16#00,16#00,16#00,16#00,16#00,16#00>>),
    ?_a(-500,<<16#00,16#C0,16#7F,16#40,16#00,16#00,16#00,16#00,16#00>>)
  ].
  
true_test() -> ?a(true,<<16#01,16#01>>).
false_test() -> ?a(false,<<16#01,16#00>>). 

null_test() -> ?a(null,<<16#05>>).
undefined_test() -> ?a(undefined,<<16#06>>).    
unsupported_test() -> ?a(unsupported,<<16#0D>>). 

date_test() -> ?a({date,1261385577404.0},<<16#0B,16#42,16#72,16#5B,16#07,16#07,16#3B,16#C0,16#00,16#00,16#00>>).

string_test_() ->
  [
    ?_a(<<"hello">>,<<16#02,16#00,16#05,"hello">>),
    ?_a(<<"hello world">>,<<16#02,16#00,16#0B,"hello world">>)
  ].  

long_string_test_() ->
  [
    ?_a(list_to_binary(lists:duplicate(200000,"a")),list_to_binary([12,0,3,13,64,"a",lists:duplicate(199999,"a")]))
  ].

%% atoms are encoded as strings
string_atom_test_() ->
  [
    ?_assertEqual(amf0:encode(<<"hello">>), amf0:encode(hello)),
    ?_assertEqual(amf0:encode(<<"hello world">>), amf0:encode('hello world'))  
  ].
  
xmldoc_test_() ->
  [
    ?_a({xmldoc,<<"<test>hello</test>">>},<<16#0F,16#00,16#00,16#00,16#12,"<test>hello</test>">>)
  ].  
  
avmplus_test_() ->
  [
    ?_assertEqual(list_to_binary([16#11,amf3:encode(<<"hello">>)]), amf0:encode({avmplus,<<"hello">>})),
    ?_assertEqual(element(1,amf3:decode(<<16#00>>)), element(2,element(1,amf0:decode(<<16#11,16#00>>))))
  ].  