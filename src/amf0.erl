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

read(<<?STRING, Length:16, String:Length/binary, Remaining/binary>>, Objects) -> {String, Remaining, Objects};
read(<<?LONG_STRING, Length:32, String:Length/binary, Remaining/binary>>, Objects) -> {String, Remaining, Objects};

read(<<?XML_DOCUMENT, Length:32, String:Length/binary, Remaining/binary>>, Objects) -> 
    {{xmldoc, String}, Remaining, Objects}.


%%---------------------------------------
%%  Write 
%%---------------------------------------

write(Number, Objects) when is_number(Number) -> {<<?NUMBER, Number:64/float>>, Objects};

write(true, Objects) -> {<<?BOOLEAN, 16#01>>, Objects};
write(false, Objects) -> {<<?BOOLEAN, 16#00>>, Objects};

write(null, Objects) -> {<<?NULL>>, Objects};
write(undefined, Objects) -> {<<?UNDEFINED>>, Objects};
write(unsupported, Objects) -> {<<?UNSUPPORTED>>, Objects};

write(Atom,Objects) when is_atom(Atom) -> write(list_to_binary(atom_to_list(Atom)), Objects);
write(String, Objects) when is_binary(String) ->
    Length = size(String),
    case Length =< 65535 of
      true  -> {<<?STRING, Length:16, String/binary>>, Objects};
      false -> {<<?LONG_STRING, Length:32, String/binary>>, Objects}
    end;
    
write({xmldoc, XML}, Objects) ->
    Length = size(XML),
    {<<?XML_DOCUMENT, Length:32, XML/binary>>, Objects}.