-module(amf3_tests).
-include_lib("eunit/include/eunit.hrl").

decode_undefined_test() -> ?assert(undefined =:= amf3:decode(<<16#00>>)).
decode_null_test() -> ?assert(null =:= amf3:decode(<<16#01>>)).
decode_false_test() -> ?assert(false =:= amf3:decode(<<16#02>>)).
decode_true_test() -> ?assert(true =:= amf3:decode(<<16#03>>)).

%% integers -268435456 to 268435455 are encoded as integers
decode_integer_test_() ->
  [     
    ?_assert(-268435456 =:= amf3:decode(<<16#04,16#C0,16#80,16#80,16#00>>)),
    ?_assert(-10 =:= amf3:decode(<<16#04,16#FF,16#FF,16#FF,16#F6>>)), 
    ?_assert(-1 =:= amf3:decode(<<16#04,16#FF,16#FF,16#FF,16#FF>>)),
    ?_assert(0 =:= amf3:decode(<<16#04,00>>)),  
    ?_assert(127 =:= amf3:decode(<<16#04,16#7F>>)),
    ?_assert(128 =:= amf3:decode(<<16#04,16#81,16#00>>)),
    ?_assert(16383 =:= amf3:decode(<<16#04,16#FF,16#7F>>)),
    ?_assert(16384 =:= amf3:decode(<<16#04,16#81,16#80,16#00>>)),
    ?_assert(2097151 =:= amf3:decode(<<16#04,16#FF,16#FF,16#7F>>)),
    ?_assert(2097152 =:= amf3:decode(<<16#04,16#80,16#C0,16#80,16#00>>)),
    ?_assert(268435455 =:= amf3:decode(<<16#04,16#BF,16#FF,16#FF,16#FF>>))
  ].    

%% All floating point Numbers are enoded as doubles
%% 
%% These Integer values are encoded as doubles   
%%  -1.79e308 to -268435457,    
%%  268435455 to 1.79e308, 
%%
%% These constants of the AS3 Number class are also encoded as doubles 
%%  -infinity 
%%  infinity
%%  nan
decode_double_test_() ->
  [ 
    ?_assert(268435456.0 =:= amf3:decode(<<16#05,16#41,16#B0,16#00,
                                           16#00,16#00,16#00,16#00,16#00>>)),
                                           
    ?_assert(268435456.5 =:= amf3:decode(<<16#05,16#41,16#B0,16#00,
                                           16#00,16#00,16#80,16#00,16#00>>)),
                                           
    ?_assert(10.1 =:= amf3:decode(<<16#05,16#40,16#24,16#33,16#33,
                                    16#33,16#33,16#33,16#33>>)),
                                    
    ?_assert(-268435457.0 =:= amf3:decode(<<16#05,16#C1,16#B0,16#00,
                                            16#00,16#01,16#00,16#00,16#00>>)),
                                            
    ?_assert(-72057594037927940.0 =:= amf3:decode(<<16#05,16#C3,16#70,
                                                    16#00,16#00,16#00,
                                                    16#00,16#00,16#00>>)),
                                                    
    ?_assert(72057594037927940.0 =:= amf3:decode(<<16#05,16#43,16#70,16#00,
                                                   16#00,16#00,16#00,16#00,
                                                   16#00>>)),
                                                   
    ?_assert(1.79e308 =:= amf3:decode(<<16#05,16#7F,16#EF,16#DC,16#F1,
                                        16#58,16#AD,16#BB,16#99>>)),
                                        
    ?_assert(-1.79e308 =:= amf3:decode(<<16#05,16#FF,16#EF,16#DC,16#F1,
                                         16#58,16#AD,16#BB,16#99>>)),
                                         
    ?_assert(infinity =:= amf3:decode(<<16#05,16#7F,16#F0,16#00,16#00,
                                        16#00,16#00,16#00,16#00>>)),
                                        
    ?_assert('-infinity' =:= amf3:decode(<<16#05,16#FF,16#F0,16#00,16#00,
                                           16#00,16#00,16#00,16#00>>)),
                                           
    ?_assert(nan =:= amf3:decode(<<16#05,16#FF,16#F8,16#00,16#00,16#00,
                                   16#00,16#00,16#00>>))
  ].  

decode_string_test_() ->
  [
    ?_assert(<<"">> =:= amf3:decode(<<16#06,16#01>>)),
   
    ?_assert(<<"hello">> =:= 
               amf3:decode(<<16#06,16#0B,16#68,16#65,16#6C,16#6C,16#6F>>)),
    
    ?_assert([null,<<"hello">>,null] =:= 
               amf3:decode(<<16#01,16#06,16#0B,16#68,16#65,16#6C,
                             16#6C,16#6F,16#01>>)),
    
    ?_assert(<<"hello world">> =:= 
               amf3:decode(<<16#06,16#17,16#68,16#65,16#6C,16#6C,
                             16#6F,16#20,16#77,16#6F,16#72,16#6C,
                             16#64>>)),
   
    ?_assert([<<"hello">>,<<"world">>] =:= 
               amf3:decode(<<16#06,16#0B,16#68,16#65,16#6C,16#6C,
                             16#6F,16#06,16#0B,16#77,16#6F,16#72,
                             16#6C,16#64>>)),
    
    ?_assert(<<"œ∑´®†¥¨ˆøπ“‘«åß∂©˙∆˚¬…æΩç√˜µ≤≥÷">> =:= 
               amf3:decode(<<16#06,16#81,16#11,16#C5,16#93,16#E2,
                             16#88,16#91,16#C2,16#B4,16#C2,16#AE,
                             16#E2,16#80,16#A0,16#C2,16#A5,16#C2,
                             16#A8,16#CB,16#86,16#C3,16#B8,16#CF,
                             16#80,16#E2,16#80,16#9C,16#E2,16#80,
                             16#98,16#C2,16#AB,16#C3,16#A5,16#C3,
                             16#9F,16#E2,16#88,16#82,16#C2,16#A9,
                             16#CB,16#99,16#E2,16#88,16#86,16#CB,
                             16#9A,16#C2,16#AC,16#E2,16#80,16#A6,
                             16#C3,16#A6,16#CE,16#A9,16#C3,16#A7,
                             16#E2,16#88,16#9A,16#CB,16#9C,16#C2,
                             16#B5,16#E2,16#89,16#A4,16#E2,16#89,
                             16#A5,16#C3,16#B7>>))
  ].

decode_xmldoc_test_() ->
  [
    ?_assert({xmldoc,<<"<text>hello</text>">>} 
                        =:= amf3:decode(<<16#07,16#25,16#3C,16#74,16#65,
                                          16#78,16#74,16#3E,16#68,16#65,16#6C,
                                          16#6C,16#6F,16#3C,16#2F,16#74,16#65,
                                          16#78,16#74,16#3E>>))
  ].

decode_date_test_() -> 
  [
    ?_assert({date,1260103478896.0} =:= amf3:decode(<<16#08,16#01,16#42,
                                                      16#72,16#56,16#40,
                                                      16#52,16#E7,16#00,
                                                      16#00>>))
  ].

decode_array_test_() ->
  [
    %% dense only
    ?_assert([100] =:= amf3:decode(<<16#09,16#03,16#01,16#04,16#64>>)),
    
    ?_assert([100,200] =:= amf3:decode(<<16#09,16#05,16#01,16#04,16#64,
                                         16#04,16#81,16#48>>)),
    
    ?_assert([null,[100,200],null] =:= amf3:decode(<<16#01,16#09,16#05,
                                                     16#01,16#04,16#64,
                                                     16#04,16#81,16#48,
                                                     16#01>>)),
                                                     
    %% associative only
    ?_assert(dict:store(b, 200, dict:store(a,100,dict:new())) 
                  =:= amf3:decode(<<16#09,16#01,16#03,16#61,
                                    16#04,16#64,16#03,16#62,
                                    16#04,16#81,16#48,16#01>>)),                                                 

    %% mixed  
    ?_assert({array,dict:store(b, 200,
                      dict:store(a, 100, 
                      dict:new())),
                    [500,600]} =:= amf3:decode(<<16#09,16#05,16#03,
                                                 16#61,16#04,16#64,
                                                 16#03,16#62,16#04,
                                                 16#81,16#48,16#01,
                                                 16#04,16#83,16#74,
                                                 16#04,16#84,16#58>>))

    
  ].

%% decode_object_test

decode_xml_test_() ->
  [
    ?_assert({xml,<<"<text>hello</text>">>} 
                    =:= amf3:decode(<<16#0B,16#25,16#3C,16#74,16#65,
                                      16#78,16#74,16#3E,16#68,16#65,16#6C,
                                      16#6C,16#6F,16#3C,16#2F,16#74,16#65,
                                      16#78,16#74,16#3E>>))
  ].

decode_bytearray_test_() -> 
  [
    ?_assert({bytearray,<<100,200>>} =:= amf3:decode(<<16#0C,16#05,100,200>>)) 
  ].

encode_list_test() -> 
  ?assert(<<16#00,16#01>> =:= amf3:encode([undefined,null])).

encode_undefined_test() -> ?assert(<<16#00>> =:= amf3:encode(undefined)).
encode_null_test() -> ?assert(<<16#01>> =:= amf3:encode(null)).
encode_false_test() -> ?assert(<<16#02>> =:= amf3:encode(false)).
encode_true_test() -> ?assert(<<16#03>> =:= amf3:encode(true)).

%% integers -268435456 to 268435455 are encoded as integers
encode_integer_test_() ->
  [     
    ?_assert(<<16#04,16#C0,16#80,16#80,16#00>> =:= amf3:encode(-268435456)),
    ?_assert(<<16#04,16#FF,16#FF,16#FF,16#F6>> =:= amf3:encode(-10)), 
    ?_assert(<<16#04,16#FF,16#FF,16#FF,16#FF>> =:= amf3:encode(-1)),
    ?_assert(<<16#04,16#00>> =:= amf3:encode(0)), 
    ?_assert(<<16#04,16#7F>> =:= amf3:encode(127)),
    ?_assert(<<16#04,16#81,16#00>> =:= amf3:encode(128)),
    ?_assert(<<16#04,16#FF,16#7F>> =:= amf3:encode(16383)),
    ?_assert(<<16#04,16#81,16#80,16#00>> =:= amf3:encode(16384)),
    ?_assert(<<16#04,16#FF,16#FF,16#7F>> =:= amf3:encode(2097151)),
    ?_assert(<<16#04,16#80,16#C0,16#80,16#00>> =:= amf3:encode(2097152)),
    ?_assert(<<16#04,16#BF,16#FF,16#FF,16#FF>> =:= amf3:encode(268435455))
  ].

encode_double_test_() ->
  [ 
    ?_assert(<<16#05,16#41,16#B0,16#00,
               16#00,16#00,16#00,16#00,
               16#00>> =:= amf3:encode(268435456.0)),
               
    ?_assert(<<16#05,16#41,16#B0,16#00,
               16#00,16#00,16#80,16#00,
               16#00>> =:= amf3:encode(268435456.5)),
               
    ?_assert(<<16#05,16#40,16#24,16#33,16#33,
               16#33,16#33,16#33,16#33>> =:= amf3:encode(10.1)),
               
    ?_assert(<<16#05,16#C1,16#B0,16#00,
               16#00,16#01,16#00,16#00,
               16#00>> =:= amf3:encode(-268435457.0)),
               
    ?_assert(<<16#05,16#C3,16#70,16#00,
               16#00,16#00,16#00,16#00,
               16#00>> =:= amf3:encode(-72057594037927940.0)),
               
    ?_assert(<<16#05,16#43,16#70,16#00,
               16#00,16#00,16#00,16#00,
               16#00>> =:= amf3:encode(72057594037927940.0)),
               
    ?_assert(<<16#05,16#7F,16#EF,16#DC,
               16#F1,16#58,16#AD,16#BB,
               16#99>> =:= amf3:encode(1.79e308)),
               
    ?_assert(<<16#05,16#FF,16#EF,16#DC,
               16#F1,16#58,16#AD,16#BB,
               16#99>> =:= amf3:encode(-1.79e308)),
               
    ?_assert(<<16#05,16#7F,16#F0,16#00,
               16#00,16#00,16#00,16#00,
               16#00>> =:= amf3:encode(infinity)),
               
    ?_assert(<<16#05,16#FF,16#F0,16#00,
               16#00,16#00,16#00,16#00,
               16#00>> =:= amf3:encode('-infinity')),
               
    ?_assert(<<16#05,16#FF,16#F8,16#00,
               16#00,16#00,16#00,16#00,
               16#00>> =:= amf3:encode(nan))
  ].


encode_string_test_() ->
  [     
    ?_assert(<<16#06,16#01>> =:= amf3:encode(<<"">>)),
    
    ?_assert(<<16#06,16#0B,16#68,16#65,
               16#6C,16#6C,16#6F>> =:= amf3:encode(<<"hello">>)),
    
    ?_assert(<<16#01,16#06,16#0B,16#68,16#65,16#6C,
               16#6C,16#6F,16#01>> =:= amf3:encode([null,<<"hello">>,null])),
    
    ?_assert(<<16#06,16#17,16#68,16#65,16#6C,16#6C,
               16#6F,16#20,16#77,16#6F,16#72,16#6C,
               16#64>> =:= amf3:encode(<<"hello world">>)),
               
    ?_assert(<<16#06,16#0B,16#68,16#65,16#6C,16#6C,
               16#6F,16#06,16#0B,16#77,16#6F,16#72,
               16#6C,16#64>> =:= amf3:encode([<<"hello">>,<<"world">>])),
    
    ?_assert(<<16#06,16#81,16#11,16#C5,16#93,16#E2,
               16#88,16#91,16#C2,16#B4,16#C2,16#AE,
               16#E2,16#80,16#A0,16#C2,16#A5,16#C2,
               16#A8,16#CB,16#86,16#C3,16#B8,16#CF,
               16#80,16#E2,16#80,16#9C,16#E2,16#80,
               16#98,16#C2,16#AB,16#C3,16#A5,16#C3,
               16#9F,16#E2,16#88,16#82,16#C2,16#A9,
               16#CB,16#99,16#E2,16#88,16#86,16#CB,
               16#9A,16#C2,16#AC,16#E2,16#80,16#A6,
               16#C3,16#A6,16#CE,16#A9,16#C3,16#A7,
               16#E2,16#88,16#9A,16#CB,16#9C,16#C2,
               16#B5,16#E2,16#89,16#A4,16#E2,16#89,
               16#A5,16#C3,16#B7>> 
               =:= amf3:encode(<<"œ∑´®†¥¨ˆøπ“‘«åß∂©˙∆˚¬…æΩç√˜µ≤≥÷">>)),
               
               
    %% atoms are treated as strings         
    ?_assert(<<16#06,16#0B,16#68,16#65,16#6C,
               16#6C,16#6F>> =:= amf3:encode(hello)),
    ?_assert(<<16#06,16#17,16#68,16#65,16#6C,16#6C,
               16#6F,16#20,16#77,16#6F,16#72,
               16#6C,16#64>> =:= amf3:encode('hello world'))      
  ].

encode_xmldoc_test_() ->
  [
    ?_assert(<<16#07,16#25,16#3C,16#74,
               16#65,16#78,16#74,16#3E,
               16#68,16#65,16#6C,16#6C,
               16#6F,16#3C,16#2F,16#74,
               16#65,16#78,16#74,16#3E>>
                =:= amf3:encode({xmldoc,<<"<text>hello</text>">>}))
  ].

encode_date_test_() -> 
  [
    ?_assert(<<16#08,16#01,16#42,16#72,
               16#56,16#40,16#52,16#E7,
               16#00,16#00>> =:= amf3:encode({date,1260103478896.0}))
  ].

encode_array_test_() ->
  [
    %% dense only
    ?_assert(<<16#09,16#03,16#01,16#04,16#64>> =:= amf3:encode({array,[100]})),
    
    ?_assert(<<16#09,16#05,16#01,16#04,
               16#64,16#04,16#81,16#48>> =:= amf3:encode({array,[100,200]})),
    
    %% first containing list is the stream of amf, so its ignored
    ?_assert(<<16#09,16#05,16#01,16#04,
               16#64,16#04,16#81,16#48>> =:= amf3:encode([[100,200]])),
               
    ?_assert(<<16#01,16#09,16#05,16#01,
               16#04,16#64,16#04,16#81,
               16#48,16#02>> =:= amf3:encode([null,[100,200],false])),  
               
    
    %% associative only           
    ?_assert(<<16#09,16#01,16#03,16#61,16#04,16#64,
               16#03,16#62,16#04,16#81,16#48,16#01>> 
               =:= amf3:encode(dict:store(b, 200,
                                          dict:store(a, 100, 
                                                     dict:new())))),
                                                     
    %% mixed  
    ?_assert(<<16#09,16#05,16#03,16#61,16#04,16#64,
               16#03,16#62,16#04,16#81,16#48,16#01,
               16#04,16#83,16#74,16#04,16#84,16#58>>
               =:= amf3:encode({array,dict:store(b, 200,
                                        dict:store(a, 100, 
                                            dict:new())),
                                      [500,600]}))                                               
               
               
  ].

encode_xml_test_() ->
  [
    ?_assert(<<16#0B,16#25,16#3C,16#74,16#65,
              16#78,16#74,16#3E,16#68,16#65,16#6C,
              16#6C,16#6F,16#3C,16#2F,16#74,16#65,
              16#78,16#74,16#3E>>
               =:= amf3:encode({xml,<<"<text>hello</text>">>}))
  ].

encode_bytearray_test_() -> 
  [
    ?_assert(<<12,3,1>> =:= amf3:encode({bytearray,<<2#00000001>>})), 
    ?_assert(<<16#0C,16#05,100,200>> =:= amf3:encode({bytearray,<<100,200>>})) 
  ].