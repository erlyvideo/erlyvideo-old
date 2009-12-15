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
%%                       object() | xml() | bytearray().
%% @type xmldoc()    =   {xmldoc, Document::binary()}.
%% @type date()      =   {date, MilliSecs::float()}.
%% @type array()     =   [amf3()] | dictionary().
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
    parse(write, Data, [], dict:new(), dict:new(), dict:new()).



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


parse(write, [], AlreadyWritten, Strings, Objects, Traits) -> 
  list_to_binary(AlreadyWritten);
parse(write, [ToWrite|Remaining], AlreadyWritten, Strings, Objects, Traits) ->
  {JustWrote, S, O, T} = write(ToWrite, Strings, Objects, Traits),
  EverythingWritten = [AlreadyWritten,JustWrote],
  parse(write, Remaining, EverythingWritten, S, O, T);
parse(write, ToWrite, _, Strings, Objects, Traits) ->
  {JustWrote, S, O, T} = write(ToWrite, Strings, Objects, Traits),
  JustWrote.


%%---------------------------------------
%%  Read 
%%---------------------------------------

read(<< ?UNDEFINED, Remaining/binary >>,
     Strings, Objects, Traits) ->
        {undefined, Remaining, Strings, Objects, Traits};

read(<< ?NULL, Remaining/binary >>,
     Strings, Objects, Traits) ->
        {null, Remaining, Strings, Objects, Traits};

read(<< ?FALSE, Remaining/binary >>,
     Strings, Objects, Traits) ->
        {false, Remaining, Strings, Objects, Traits};

read(<< ?TRUE, Remaining/binary >>,
     Strings, Objects, Traits) ->
        {true, Remaining, Strings, Objects, Traits};

read(<< ?INTEGER, Data/binary >>,
     Strings, Objects, Traits) ->
        {Unsigned, R, S, O, T} = read_uint29(Data, Strings, Objects, Traits),
        case Unsigned > 268435455 of
          true  -> Signed = Unsigned - 536870912,
                   {Signed, R, S, O, T};
          false -> Signed = Unsigned,
                   {Signed, R, S, O, T}
        end;

read(<< ?DOUBLE, Data:64/float, Remaining/binary >>, 
     Strings, Objects, Traits) -> 
        {Data, Remaining, Strings, Objects, Traits};

read(<< ?DOUBLE, 16#7F,16#F0,16#00,16#00,16#00,16#00,16#00,16#00, 
        Remaining/binary>>, Strings, Objects, Traits) -> 
          {infinity, Remaining, Strings, Objects, Traits};

read(<< ?DOUBLE, 16#FF,16#F0,16#00,16#00,16#00,16#00,16#00,16#00,
        Remaining/binary>>, Strings, Objects, Traits) -> 
          {'-infinity', Remaining, Strings, Objects, Traits};

read(<< ?DOUBLE, 16#FF,16#F8,16#00,16#00,16#00,16#00,16#00,16#00, 
        Remaining/binary>>, Strings, Objects, Traits) -> 
          {nan, Remaining, Strings, Objects, Traits};

read(<< ?STRING, Data/binary >>, 
     Strings, Objects, Traits) -> 
        {Header, R, S, O, T} = read_uint29(Data, Strings, Objects, Traits),
        case Header band 1 =:= 0 of
          true  ->  String = find_string(Header bsr 1, S),
                    {String, R, S, O, T};
          false ->  Length = Header bsr 1,
                    << String:Length/binary, Remaining/binary >>  = R,
                    S1 = remember_string(String, S),
                    {String, Remaining, S1, O, T}
        end;

read(<< ?XMLDOC, Data/binary >>,
     Strings, Objects, Traits) -> 
        {Header, R, S, O, T} = read_uint29(Data, Strings, Objects, Traits),
        case Header band 1 =:= 0 of
          true  ->  XMLDocString = find_string(Header bsr 1, S),
                    {XMLDocString, R, S, O, T};
          false ->  Length = Header bsr 1,
                    << XMLDocString:Length/binary, Remaining/binary >>  = R,
                    S1 = remember_string(XMLDocString, S),            
                    {{xmldoc, XMLDocString}, Remaining, S1, O, T} 
          end;

read(<< ?DATE, Data/binary >>, 
     Strings, Objects, Traits) -> 
        {Header, R, S, O, T} =  read_uint29(Data, Strings, Objects, Traits),
        case Header band 1 =:= 0 of
          true  ->  Date = find_object(Header bsr 1, O),
                    {Date, R, S, O, T};
          false ->  {MilliSeconds, R1, S1, O1, T1} =
                                        read(<<?DOUBLE, R/binary>>, S, O, T),
                    Date = {date, MilliSeconds},
                    O2 = remember_object(Date, O1),
                    {Date, R1, S1, O2, T1}
        end;

read(<< ?ARRAY, Data/binary >>,
     Strings, Objects, Traits) -> 
        {Header, R, S, O, T} = read_uint29(Data, Strings, Objects, Traits),
        case Header band 1 =:= 0 of
          true  ->  A = find_object(Header bsr 1, O),
                    {A, R, S, O, T};
          false ->  Length = Header bsr 1,
                    {Associative, R1, S1, O1, T1} = 
                            read_associative_array(R, dict:new(), S, O, T),
                    {Dense, R2, S2, O2, T2} = 
                            read_dense_array(Length, [], R1, S1, O1, T1),
                    Arr = {array,Associative,Dense},
                    O3 = remember_object(Arr, O2),
                    {Arr, R2, S2, O3, T2}
        end;


read(<< ?OBJECT, Data/binary >>,
     Strings, Objects, Traits) ->
        {Header, R, S, O, T} = read_uint29(Data, Strings, Objects, Traits),
        case Header band 1 =:= 0 of
          true  ->  Object = find_object(Header bsr 1, O),
                    {Object, R, S, O, T};
          false ->  {{ClassName,IsDynamic,IsExternalizable,_,Properties},
                                R1, S1, O1, T1} =
                                    read_object_traits(Header, R, S, O, T),
                    case IsExternalizable of
                      true  -> externalizable; %% @todo
                      false -> {SealedPD, R2, S2, O2, T2} = 
                                    read_object_property_values(Properties, 
                                                              R1,dict:new(), 
                                                              S1, O1, T1),
                              case  IsDynamic of
                                false -> {Object, R4, S4, O4, T4 } = 
                                              {{object, ClassName, SealedPD},
                                                R2, S2, O2, T2};
                                true  -> {PD, R3, S3, O3, T3} = 
                                            read_object_dynamic_properties(
                                                    R2, SealedPD, S2, O2, T2), 
                                         {Object, R4, S4, O4, T4 } = 
                                                      {{object, ClassName, PD},
                                                       R3, S3, O3, T3}
                               end, 
                               O5 = remember_object(Object, O4),
                               {Object, R4, S4, O5, T4}
                    end 
          end;

read(<< ?XML, Data/binary >>,
     Strings, Objects, Traits) -> 
        {Header, R, S, O, T} = read_uint29(Data, Strings, Objects, Traits),
        case Header band 1 =:= 0 of
          true  ->  XMLString = find_string(Header bsr 1, S),
                    {XMLString, R, S, O, T};
          false ->  Length = Header bsr 1,
                    << XMLString:Length/binary, Remaining/binary >>  = R,
                    Strings1 = remember_string(XMLString, Strings),           
                    {{xml, XMLString}, Remaining, Strings1, O, T}
          end;

read(<< ?BYTEARRAY, Data/binary >>, 
     Strings, Objects, Traits) -> 
        {Header, R, S, O, T} = read_uint29(Data, Strings, Objects, Traits),
        case Header band 1 =:= 0 of
          true  ->  ByteArray = find_object(Header bsr 1, O),
                    {ByteArray, R, S, O, T};
          false ->  Length = Header bsr 1,
                    << ByteArray:Length/binary, Remaining/binary >>  = R,
                    BA = {bytearray,ByteArray},
                    O1 = remember_object(BA, O),  
                    {BA, Remaining, S, O1, T}
        end.

read_uint29(<<0:1, A:7 , Remaining/binary>>, 
            Strings, Objects, Traits) -> 
                {A, Remaining, Strings, Objects, Traits};

read_uint29(<<1:1, A:7, 0:1, B:7, Remaining/binary>>, 
            Strings, Objects, Traits) -> 
                {((A bsl 7) bor B), 
                 Remaining, Strings, Objects, Traits};

read_uint29(<<1:1, A:7, 1:1, B:7, 0:1, C:7, Remaining/binary>>, 
            Strings, Objects, Traits) -> 
                {((A bsl 14) bor (B bsl 7) bor C),
                 Remaining, Strings, Objects, Traits};
        
read_uint29(<<1:1, A:7, 1:1, B:7, 1:1, C:7, D:8, Remaining/binary>>, 
            Strings, Objects, Traits) -> 
                {((A bsl 22) bor (B bsl 15) bor (C bsl 8) bor D),
                 Remaining, Strings, Objects, Traits}.

read_associative_array(Data, Dictionary, Strings, Objects, Traits) ->
  {Name, R, S, O, T} = read(<<?STRING,Data/binary>>, Strings, Objects, Traits),
  case Name of
     <<>> -> {Dictionary, R, S, O, T}; 
     N -> {Value, R2, S2, O2, T2} = read(R, S, O, T),
          D = dict:store(N, Value, Dictionary),
          read_associative_array( R2, D, S2, O2, T2)
  end.

read_object_traits(Header, Data, Strings, Objects, Traits) ->
  case Header band 3 =:= 0 of
    true  -> Trait = find_trait(Header bsr 1, Objects),
             {Trait,  Data, Strings, Objects, Traits}; 
    false -> IsExternalizable = Header band 4 =:= 4,
             IsDynamic = Header band 8 =:= 8,
             Count = Header bsr 4,
             {ClassName, R, S, O, T} = read(<<?STRING, Data/binary>>, 
                                            Strings, Objects, Traits),
             {Properties, R2, S2, O2, T2} = 
                    read_object_property_names(Count, [], R, S, O, T),
             Trait = {ClassName,IsDynamic,IsExternalizable,Count,Properties},
             T3 = remember_trait(T2, Traits), 
             {Trait, R2, S2, O2, T3}
  end.          

read_object_dynamic_properties(Data, Dictionary, Strings, Objects, Traits) ->
  {Property, R, S, O, T} =
          read(<<?STRING,Data/binary>>, Strings, Objects, Traits),
  case Property of
    [] -> {Dictionary, R, S, O, T};
    P  -> {Value, R2, S2, O2, T2} = read(R, S, O, T),
          PropertyDictionary = dict:store(P, Value, Dictionary),
          read_object_dynamic_properties(R2, PropertyDictionary, S2, O2, T2)
  end.  
  
read_dense_array(0, List, Data, Strings, Objects, Traits) -> 
  {lists:reverse(List), Data, Strings, Objects, Traits};
read_dense_array(Length, List, Data, Strings, Objects, Traits) ->
  {Item, R, S, O, T} = read(Data, Strings, Objects, Traits),
  read_dense_array(Length-1, [Item|List], R, S, O, T).  

read_object_property_names(0, Properties,Data,Strings,Objects,Traits) -> 
  {Properties, Data, Strings, Objects, Traits};
read_object_property_names(Count,Properties,Data,Strings,Objects,Traits) ->
  {Property, R, S, O, T} =
            read(<<?STRING, Data/binary>>, Strings, Objects, Traits),
  PropertyAtom = list_to_atom(Property),
  read_object_property_names(Count-1, [PropertyAtom|Properties], R, S, O, T).

read_object_property_values([],
        Data, PropertyDictionary, Strings, Objects, Traits) -> 
            {PropertyDictionary, Data, Strings, Objects, Traits};
read_object_property_values([Property|Tail], 
    Data, Dictionary, Strings, Objects, Traits) ->
        {Value, R, S, O, T} = read(Data, Strings, Objects, Traits),
        PropertyDictionary = dict:store(Property, Value, Dictionary),
        read_object_property_values( Tail, R, PropertyDictionary, S, O, T).
  
remember_string(ToRemember, Strings) ->
  Key = dict:size(Strings),
  dict:store(Key, ToRemember, Strings).

remember_object(ToRemember, Objects) ->
  Key = dict:size(Objects),
  dict:store(Key, ToRemember, Objects).

remember_trait(ToRemember, Traits) ->
  Key = dict:size(Traits),
  dict:store(Key, ToRemember, Traits).

find_string(Reference, Strings) ->
  case dict:find(Reference, Strings) of
    {ok, Value} -> Value;
          error -> throw(referenceNotFound)
  end.
  
find_object(Reference, Objects) ->
  case dict:find(Reference, Objects) of
    {ok, Value} -> Value;
          error -> throw(referenceNotFound)
  end.

find_trait(Reference, Traits) ->
  case dict:find(Reference, Traits) of
    {ok, Value} -> Value;
          error -> throw(referenceNotFound)
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
  when is_integer(Integer), Integer >= -16#10000000, Integer < 0 -> 
    I = write_uint29(16#20000000 + Integer),
    {<<?INTEGER, I/binary>>, Strings, Objects, Traits};

write(Integer, Strings, Objects, Traits)
  when is_integer(Integer), Integer =< 16#0FFFFFFF -> 
    I = write_uint29(Integer),
    {<<?INTEGER, I/binary>>, Strings, Objects, Traits};

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
  Length = write_uint29(size(String) bsl 1 bor 1),
  Binary = list_to_binary([?STRING,Length,String]),
  {Binary, Strings, Objects, Traits};

write({xmldoc, XML}, Strings, Objects, Traits) when is_binary(XML) -> 
  Length = write_uint29(size(XML) bsl 1 bor 1),
  Binary = list_to_binary([?XMLDOC,Length,XML]),
  {Binary, Strings, Objects, Traits};

write({date, MilliSeconds}, Strings, Objects, Traits) ->
  {<<?DATE, 16#01, MilliSeconds/float>>, Strings, Objects, Traits};

write({array, Array}, Strings, Objects, Traits) when is_list(Array) -> 
  Length = write_uint29(length(Array) bsl 1 bor 1),
  write_dense_array(Array, [?ARRAY,Length,16#01], Strings, Objects, Traits);

write({array, Dictionary}, Strings, Objects, Traits)
  when is_tuple(Dictionary)-> 
    ggg;

write({xml, XML}, Strings, Objects, Traits) when is_binary(XML) -> 
  Length = write_uint29(size(XML) bsl 1 bor 1),
  Binary = list_to_binary([?XML,Length,XML]),
  {Binary, Strings, Objects, Traits};

write({bytearray, ByteArray}, Strings, Objects, Traits)
  when is_binary(ByteArray) -> 
    Length = write_uint29(size(ByteArray) bsl 1 bor 1),
    Binary = list_to_binary([?BYTEARRAY,Length,ByteArray]),
    {Binary, Strings, Objects, Traits}.

write_uint29(Unsigned) 
  when Unsigned >= 16#00000000, Unsigned =< 16#0000007F ->
    << Unsigned >>;
  
write_uint29(Unsigned)
  when Unsigned >= 16#00000080, Unsigned =< 16#00003FFF ->
    << ((Unsigned bsr 7) bor 16#80), (Unsigned band 16#7F) >>;

write_uint29(Unsigned)
  when Unsigned >= 16#00004000, Unsigned =< 16#001FFFFF ->
    << ((Unsigned bsr 14) bor 16#80), 
       ((Unsigned bsr 7) bor 16#80),
       (Unsigned band 16#7F) >>;
  
write_uint29(Unsigned)
  when Unsigned >= 16#00200000, Unsigned =< 16#1FFFFFFF ->
    << ((Unsigned bsr 22) bor 16#80),
       ((Unsigned bsr 15) bor 16#80),
       ((Unsigned bsr 8) bor 16#80),
          (Unsigned band 16#FF) >>.

write_dense_array([], Output, Strings, Objects, Traits) ->  
  {list_to_binary(Output), Strings, Objects, Traits};
write_dense_array([H|Remaining], Output, Strings, Objects, Traits) -> 
  {JustWritten,S,O,T} = write(H, Strings, Objects, Traits),
  EverythingWritten = [Output,JustWritten],
  write_dense_array(Remaining, EverythingWritten, S, O, T). 
 
