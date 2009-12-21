%%%%=========================================================================
%% 
%%  amf3
%%
%%  @author Mrinal Wadhwa < email@mrinalwadhwa.com >
%%       [ http://www.mrinalwadhwa.com ]
%%
%%  @doc This module can be used to serialize and deserialize data to/from
%%  Action Message Format (AMF) 3, which is a compact binary format used by
%%  Adobe Flash Player to serialize ActionScript object graphs 
%%
%%  @reference <a href="http://bit.ly/amf-spec">AMF 3 Specification</a>
%% 
%%  @end 
%%
%%%%==========================================================================

-module(amf3).
-export([decode/1, encode/1]).



%% @type amf3()      =   undefined | null | bool() |
%%                       integer() | float() | binary() | 
%%                       xmldoc() | date() | array() | 
%%                       list() | dictionary()| object() | 
%%                       xml() | bytearray().
%% @type xmldoc()    =   {xmldoc, Document::binary()}.
%% @type date()      =   {date, MilliSecs::float()}.
%% @type array()     =   {array, Associative::dictionary(), Dense::list()}.
%% @type object()    =   {object, Class::binary(), Members::dictionary()}.
%% @type xml()       =   {xml, Document::binary()}.
%% @type bytearray() =   {bytearray, Bytes::binary()}.



%%---------------------------------------
%% @doc Decode AMF 3 encoded binary data
%% @end 
%%---------------------------------------

decode(Data) ->
  parse(read, Data, queue:new(), dict:new(), dict:new(), dict:new()).
  
%%---------------------------------------
%% @doc Encode data to an AMF 3 encoded 
%%      binary form
%% @end 
%%---------------------------------------

encode(Data) -> 
  parse(write, Data, [], [], dict:new(), dict:new()).




%%%%==========================================================================
%%
%%    Internal       
%%
%%%%==========================================================================


%%---------------------------------------
%%  Markers
%%---------------------------------------

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


%%---------------------------------------
%%  Parse
%%---------------------------------------

parse(read, <<>>, AlreadyRead, _, _, _) -> 
  L = [H|T] = queue:to_list(AlreadyRead),
  case length(T) of
    0 -> H;
    _ -> L
  end;                


parse(read, ToRead, AlreadyRead, Strings, Objects, Traits) ->
  {JustRead, Remaining, S, O, T} = read(ToRead, Strings, Objects, Traits),  
  EverythingRead = queue:in(JustRead, AlreadyRead),
  parse(read, Remaining, EverythingRead, S, O, T);


parse(write, [], AlreadyWritten, _, _, _) -> list_to_binary(AlreadyWritten);


parse(write, [ToWrite|Remaining], AlreadyWritten, Strings, Objects, Traits) ->
  {JustWrote, S, O, T} = write(ToWrite, Strings, Objects, Traits),
  EverythingWritten = [AlreadyWritten,JustWrote],
  parse(write, Remaining, EverythingWritten, S, O, T);
parse(write, ToWrite, _, Strings, Objects, Traits) ->
  {JustWrote, _, _, _} = write(ToWrite, Strings, Objects, Traits),
  JustWrote.


%%---------------------------------------
%%  Read 
%%---------------------------------------

read(<<?UNDEFINED, Remaining/binary>>, Strings, Objects, Traits) ->
  {undefined, Remaining, Strings, Objects, Traits};


read(<<?NULL, Remaining/binary>>, Strings, Objects, Traits) ->
  {null, Remaining, Strings, Objects, Traits};


read(<<?FALSE, Remaining/binary>>, Strings, Objects, Traits) ->
  {false, Remaining, Strings, Objects, Traits};


read(<<?TRUE, Remaining/binary>>, Strings, Objects, Traits) ->
  {true, Remaining, Strings, Objects, Traits};


read(<<?INTEGER, Data/binary>>, Strings, Objects, Traits) ->
  {Unsigned, Remaining} = read_uint29(Data),
  case Unsigned > 268435455 of
    true  -> Signed = Unsigned - 536870912,
             {Signed, Remaining, Strings, Objects, Traits};
    false -> Signed = Unsigned,
             {Signed, Remaining,Strings, Objects, Traits}
  end;


read(<<?DOUBLE, Data:64/float, Remaining/binary>>, Strings, Objects, Traits) -> 
  {Data, Remaining, Strings, Objects, Traits};


read(<<?DOUBLE, 16#7F,16#F0,16#00,16#00,16#00,16#00,16#00,16#00, 
        Remaining/binary>>, Strings, Objects, Traits) -> 
          {infinity, Remaining, Strings, Objects, Traits};


read(<<?DOUBLE, 16#FF,16#F0,16#00,16#00,16#00,16#00,16#00,16#00,
        Remaining/binary>>, Strings, Objects, Traits) -> 
          {'-infinity', Remaining, Strings, Objects, Traits};

read(<< ?DOUBLE, 16#FF,16#F8,16#00,16#00,16#00,16#00,16#00,16#00, 
        Remaining/binary>>, Strings, Objects, Traits) -> 
          {nan, Remaining, Strings, Objects, Traits};


read(<<?STRING, Data/binary>>, Strings, Objects, Traits) -> 
  {String,Remaining} = read_string(Data,Strings),
  S1 = remember(String, Strings),
  {String, Remaining, S1, Objects, Traits}; 


read(<<?XMLDOC, Data/binary>>, Strings, Objects, Traits) -> 
  {XMLDocString,Remaining} = read_string(Data,Strings),
  S1 = remember(XMLDocString, Strings),
  {{xmldoc, XMLDocString}, Remaining, S1, Objects, Traits};


read(<<?DATE, Data/binary>>, Strings, Objects, Traits) -> 
  {Header, R} =  read_uint29(Data),
  case Header band 1 =:= 0 of
    true  ->  Date = find(Header bsr 1, Objects),
              {Date, R, Strings, Objects, Traits};
    false ->  {MilliSeconds, R1, S1, O1, T1} = read(<<?DOUBLE, R/binary>>, 
                                                      Strings, Objects, Traits),
              Date = {date, MilliSeconds},
              O2 = remember(Date, O1),
              {Date, R1, S1, O2, T1}
  end;


read(<< ?ARRAY, Data/binary >>, Strings, Objects, Traits) -> 
  {Header, R} = read_uint29(Data),
  case Header band 1 =:= 0 of
    true  ->  A = find(Header bsr 1, Objects),
              {A, R, Strings, Objects, Traits};
    false ->  Length = Header bsr 1,
              {Associative, R1, S1, O1, T1} = 
                      read_associative_array(R, dict:new(),
                                             Strings, Objects, Traits),
              {Dense, R2, S2, O2, T2} = 
                      read_dense_array(Length, [], R1, S1, O1, T1),
              AssocSize = dict:size(Associative),
              DenseSize = length(Dense),
              if
                AssocSize > 0, DenseSize == 0  ->
                  Arr = Associative;
                AssocSize == 0, DenseSize > 0  ->
                  Arr = Dense;
                AssocSize > 0, DenseSize > 0 ->
                  Arr = {array, Associative, Dense};
                AssocSize == 0, DenseSize == 0 ->
                  Arr = {array, Associative, Dense}
              end,
              O3 = remember(Arr, O2),
              {Arr, R2, S2, O3, T2}
  end;


read(<< ?OBJECT, Data/binary >>, Strings, Objects, Traits) ->
  {Header, R} = read_uint29(Data),
  case Header band 1 =:= 0 of
    true  ->  Object = find(Header bsr 1, Objects),
              {Object, R, Strings, Objects, Traits};
    false ->  {{ClassName,IsDynamic,IsExternalizable,_,Properties},
                  R1, S1, O1, T1} = read_traits(Header, R,
                                       Strings, Objects, Traits),
              case IsExternalizable of
                true  -> {externalizable, R1, S1, O1, T1}; %% @todo
                false -> {SealedPD, R2, S2, O2, T2} = 
                              read_property_values(Properties, 
                                                        R1,dict:new(), 
                                                        S1, O1, T1),
                        case  IsDynamic of
                          false -> {Object, R4, S4, O4, T4 } = 
                                        {{object, ClassName, SealedPD},
                                          R2, S2, O2, T2};
                          true  -> {PD, R3, S3, O3, T3} = 
                                      read_dynamic_properties(
                                              R2, SealedPD, S2, O2, T2), 
                                   {Object, R4, S4, O4, T4 } = 
                                                {{object, ClassName, PD},
                                                 R3, S3, O3, T3}
                         end, 
                         O5 = remember(Object, O4),
                         {Object, R4, S4, O5, T4}
              end 
    end;


read(<< ?XML, Data/binary >>, Strings, Objects, Traits) -> 
  {XMLString,Remaining} = read_string(Data,Strings),
  S1 = remember(XMLString, Strings),
  {{xml, XMLString}, Remaining, S1, Objects, Traits};


read(<< ?BYTEARRAY, Data/binary >>, 
     Strings, Objects, Traits) -> 
        {Header, R} = read_uint29(Data),
        case Header band 1 =:= 0 of
          true  ->  ByteArray = find(Header bsr 1, Objects),
                    {ByteArray, R, Strings, Objects, Traits};
          false ->  Length = Header bsr 1,
                    << ByteArray:Length/binary, Remaining/binary >>  = R,
                    BA = {bytearray,ByteArray},
                    Objects1 = remember(BA, Objects),  
                    {BA, Remaining, Strings, Objects1, Traits}
        end.


read_uint29(<<0:1, A:7, Remaining/binary>>) -> {A, Remaining};
read_uint29(<<1:1, A:7, 0:1, B:7, Remaining/binary>>) -> 
  {((A bsl 7) bor B),Remaining};
read_uint29(<<1:1, A:7, 1:1, B:7, 0:1, C:7, Remaining/binary>>) -> 
  {((A bsl 14) bor (B bsl 7) bor C), Remaining};  
read_uint29(<<1:1, A:7, 1:1, B:7, 1:1, C:7, D:8, Remaining/binary>>) -> 
  {((A bsl 22) bor (B bsl 15) bor (C bsl 8) bor D), Remaining}.


read_associative_array(Data, Dictionary, Strings, Objects, Traits) ->
  {Name, R, S, O, T} = read(<<?STRING,Data/binary>>, Strings, Objects, Traits),
  case Name of
     <<>> -> {Dictionary, R, S, O, T}; 
        N -> {Value, R2, S2, O2, T2} = read(R, S, O, T),
             D = dict:store(list_to_atom(binary_to_list(N)), Value, Dictionary),
             read_associative_array( R2, D, S2, O2, T2)
  end.


read_dense_array(0, List, Data, Strings, Objects, Traits) -> 
  {lists:reverse(List), Data, Strings, Objects, Traits};
read_dense_array(Length, List, Data, Strings, Objects, Traits) ->
  {Item, R, S, O, T} = read(Data, Strings, Objects, Traits),
  read_dense_array(Length-1, [Item|List], R, S, O, T).  


read_traits(Header, Data, Strings, Objects, Traits) ->
  case Header band 3 =:= 0 of
    true  -> Trait = find(Header bsr 1, Objects),
             {Trait, Data, Strings, Objects, Traits}; 
    false -> IsExternalizable = Header band 4 =:= 4,
             IsDynamic = Header band 8 =:= 8,
             Count = Header bsr 4,
             {ClassName, R} = read_string(Data, Strings),
             {Properties, R1, S1} = read_property_names(Count, [], R, Strings),
             Trait = {ClassName,IsDynamic,IsExternalizable,Count,Properties},
             T1 = remember(Trait, Traits), 
             {Trait, R1, S1, Objects, T1}
  end.          


read_dynamic_properties(<<16#01>>,Dictionary,Strings,Objects,Traits) ->
  {Dictionary,<<>>,Strings,Objects,Traits};
read_dynamic_properties(Data, Dictionary, Strings, Objects, Traits) ->
  {Property, R} = read_string(Data, Strings),
  {Value, R1, S1, O1, T1} = read(R, Strings,Objects,Traits),
  PropertyDictionary = dict:store(list_to_atom(binary_to_list(Property)),
                                  Value, Dictionary),
  read_dynamic_properties(R1, PropertyDictionary, S1, O1, T1).


read_property_names(0, Properties,Data,Strings) -> 
  {lists:reverse(Properties), Data, Strings};
read_property_names(Count,Properties,Data,Strings) ->
  {Property, R} = read_string(Data, Strings),
  PropertyAtom = list_to_atom(binary_to_list(Property)),
  read_property_names(Count-1, [PropertyAtom|Properties], R, Strings).


read_property_values([],Data, PropertyDictionary, Strings, Objects, Traits) -> 
        {PropertyDictionary, Data, Strings, Objects, Traits};
read_property_values([Property|Tail], 
    Data, Dictionary, Strings, Objects, Traits) ->
        {Value, R, S, O, T} = read(Data, Strings, Objects, Traits),
        PropertyDictionary = dict:store(Property, Value, Dictionary),
        read_property_values( Tail, R, PropertyDictionary, S, O, T).


read_string(Data,Strings) -> 
    {Header, R} = read_uint29(Data),
    N = Header bsr 1,
    case Header band 1 =:= 0 of
      true  ->  {find(N, Strings), R};
      false ->  <<String:N/binary, Remaining/binary>>  = R,
                {String, Remaining}
    end.


%%---------------------------------------
%%  Write 
%%---------------------------------------

write(undefined, Strings, Objects, Traits) -> 
  {<<?UNDEFINED>>, Strings, Objects, Traits};
  
  
write(null, Strings, Objects, Traits) -> 
  {<<?NULL>>, Strings, Objects, Traits};
  
  
write(false, Strings, Objects, Traits) -> 
  {<<?FALSE>>, Strings, Objects, Traits}; 
  
  
write(true, Strings, Objects, Traits) -> 
  {<<?TRUE>>, Strings, Objects, Traits};


write(Integer, Strings, Objects, Traits)
  when is_integer(Integer), Integer >= -268435456, Integer < 0 -> 
    I = write_uint29(536870912 + Integer),
    {<<?INTEGER, I/binary>>, Strings, Objects, Traits};


write(Integer, Strings, Objects, Traits)
  when is_integer(Integer), Integer =< 268435455, Integer >= 0 -> 
    I = write_uint29(Integer),
    {<<?INTEGER, I/binary>>, Strings, Objects, Traits};


write(Integer, Strings, Objects, Traits) when is_integer(Integer) ->
  Float = math:pow(Integer,1),
  write(Float, Strings, Objects, Traits);    


write(Double, Strings, Objects, Traits) when is_float(Double) -> 
  {<<?DOUBLE, Double/float>>, Strings, Objects, Traits};


write(infinity, Strings, Objects, Traits) -> 
  {<<?DOUBLE,16#7F,16#F0,16#00,16#00,16#00,16#00,16#00,16#00>>,
   Strings, Objects, Traits};


write('-infinity', Strings, Objects, Traits) -> 
  {<<?DOUBLE,16#FF,16#F0,16#00,16#00,16#00,16#00,16#00,16#00>>,
   Strings, Objects, Traits};


write(nan, Strings, Objects, Traits) -> 
  {<<?DOUBLE,16#FF,16#F8,16#00,16#00,16#00,16#00,16#00,16#00>>,
   Strings, Objects, Traits};


write(Atom, Strings, Objects, Traits) when is_atom(Atom) -> 
  write(list_to_binary(atom_to_list(Atom)), Strings, Objects, Traits);


write(String, Strings, Objects, Traits) when is_binary(String) -> 
  {Binary, Strings1} = write_string(?STRING,String,Strings),
  {Binary, Strings1, Objects, Traits};


write({xmldoc, XML}, Strings, Objects, Traits) when is_binary(XML) -> 
  {Binary, Strings1} = write_string(?XMLDOC,XML,Strings),
  {Binary, Strings1, Objects, Traits};


write({date, MilliSeconds}, Strings, Objects, Traits) ->
  {<<?DATE, 16#01, MilliSeconds/float>>, Strings, Objects, Traits};


write({object, ClassName, Members}, Strings, Objects, Traits) -> a;


write({xml, XML}, Strings, Objects, Traits) when is_binary(XML) -> 
  {Binary, Strings1} = write_string(?XML,XML,Strings),
  {Binary, Strings1, Objects, Traits};


write({bytearray, ByteArray}, Strings, Objects, Traits)
  when is_binary(ByteArray) -> 
    Length = write_uint29(size(ByteArray) bsl 1 bor 1),
    Binary = list_to_binary([?BYTEARRAY,Length,ByteArray]),
    {Binary, Strings, Objects, Traits};


write({array, Dictionary, List}, Strings, Objects, Traits)
  when is_tuple(Dictionary), is_list(List)-> 
    Length = write_uint29(length(List) bsl 1 bor 1),
    {Written,S,O,T} = write_associative_array(dict:to_list(Dictionary), 
                                              [?ARRAY,Length],
                                              Strings, Objects, Traits),
    {Output,S1,O1,T1} = write_dense_array(List, Written, S, O, T),
    {list_to_binary(Output),S1,O1,T1};


write({array, List}, Strings, Objects, Traits)
  when is_list(List) -> 
    write({array, dict:new(), List}, Strings, Objects, Traits);


write({array, Dictionary}, Strings, Objects, Traits)
  when is_tuple(Dictionary) -> 
    write({array, Dictionary, []}, Strings, Objects, Traits);


write(List, Strings, Objects, Traits) 
  when is_list(List) ->
    write({array, dict:new(), List}, Strings, Objects, Traits);


write(Dictionary, Strings, Objects, Traits) 
  when is_tuple(Dictionary) ->
    write({array, Dictionary, []}, Strings, Objects, Traits).


write_uint29(Unsigned) 
  when Unsigned >= 16#00000000, Unsigned =< 16#0000007F -> <<Unsigned>>;
write_uint29(Unsigned)
  when Unsigned >= 16#00000080, Unsigned =< 16#00003FFF ->
    <<((Unsigned bsr 7) bor 16#80), (Unsigned band 16#7F)>>;
write_uint29(Unsigned)
  when Unsigned >= 16#00004000, Unsigned =< 16#001FFFFF ->
    <<((Unsigned bsr 14) bor 16#80),((Unsigned bsr 7) bor 16#80),
       (Unsigned band 16#7F)>>;
write_uint29(Unsigned)
  when Unsigned >= 16#00200000, Unsigned =< 16#1FFFFFFF ->
    <<((Unsigned bsr 22) bor 16#80),((Unsigned bsr 15) bor 16#80),
      ((Unsigned bsr 8) bor 16#80),(Unsigned band 16#FF)>>.


write_dense_array([], Output, Strings, Objects, Traits) ->  
  {Output, Strings, Objects, Traits};
write_dense_array([H|Remaining], Output, Strings, Objects, Traits) -> 
  {JustWritten,S,O,T} = write(H, Strings, Objects, Traits),
  EverythingWritten = [Output,JustWritten],
  write_dense_array(Remaining, EverythingWritten, S, O, T). 


write_associative_array([], Output, Strings, Objects, Traits) ->  
  {lists:flatten([Output,16#01]), Strings, Objects, Traits}; 
write_associative_array([H|Remaining], Output, Strings, Objects, Traits) -> 
  {Key, Value} = H,
  {<<16#06,K/binary>>,S,O,T} = write(Key, Strings, Objects, Traits),    
  {V,S1,O1,T1} = write(Value, S, O, T),
  EverythingWritten = [Output, K, V],
  write_associative_array(Remaining, EverythingWritten, S1, O1, T1).


write_string(Marker, String, Strings) ->
  Length = write_uint29(size(String) bsl 1 bor 1),
  Binary = list_to_binary([Marker,Length,String]),
  {Binary, Strings}.


remember(ToRemember, Dictionary) ->
  Key = dict:size(Dictionary) + 1,
  dict:store(Key, ToRemember, Dictionary).

find(Reference, Dictionary) ->
  case dict:find(Reference, Dictionary) of
    {ok, Value} -> Value;
              _ -> throw(referenceNotFound)
  end.