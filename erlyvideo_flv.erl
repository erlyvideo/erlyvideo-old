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
%%% Description : FLV reader / writer
%%%
%%% Created : 18 Nov,06
%%% -------------------------------------------------------------------
-module(erlyvideo_flv).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("erlyvideo.hrl").

%% --------------------------------------------------------------------
%% definitions
%% --------------------------------------------------------------------
-define(FLV_HEADER_LENGTH, 9).
-define(PREVIOUS_TAG_SIZE_LENGTH, 4).
-define(TAG_HEADER_LENGTH, 11).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([read_header/1, read_tag/2, write_header/1, write_tag/5]).


%% ====================================================================
%% External functions
%% ====================================================================


read_header(IoDev) ->
    case file:read(IoDev, ?FLV_HEADER_LENGTH) of
        {ok, Data} ->
            <<_Signature:3/binary, 
             _Version:1/binary, 
             _TypeFlagsReserved1:5, 
             _TypeFlagsAudio:1,
             _TypeFlagsReserved2:1,
             _TypeFlagsVideo:1,
             _DataOffset:4/binary>> = iolist_to_binary(Data), 
            {ok, ?FLV_HEADER_LENGTH};  %% TODO: send back results
        eof -> 
            {error, unexpected_eof};
        {error, Reason} ->
            {error, Reason}           
    end. 


read_tag(IoDev, Pos) ->  
    case file:pread(IoDev, Pos, ?PREVIOUS_TAG_SIZE_LENGTH) of
        {ok, Data} -> 
            <<_PreviousTagSize:32/integer>> = iolist_to_binary(Data), 
            Pos2 = Pos + ?PREVIOUS_TAG_SIZE_LENGTH,
            case file:pread(IoDev, Pos2, ?TAG_HEADER_LENGTH) of
                {ok, DataHeader} -> 
                    Header = iolist_to_binary(DataHeader),
                    <<TagType:8/integer, 
                     DataSize:24/integer, 
                     Ts:24/integer, 
                     TsExtended:8/integer, 
                     _FlvStId:24/integer>> = Header,       
                    <<AbsTs:32/integer>> = <<TsExtended:8, Ts:24>>, 
                    Pos3 = Pos2 + ?TAG_HEADER_LENGTH,
                    case file:pread(IoDev, Pos3, DataSize) of
                        {ok, Data2} ->
                            {ok, 
                             TagType, 
                             AbsTs, 
                             iolist_to_binary(Data2), 
                             Pos3 + DataSize};
                        eof ->  
                            {error, unexpected_eof};
                        {error, Reason} ->   
                            {error, Reason}             
                    end;                                           
                eof ->
                    {ok, done};
                {error, Reason} -> 
                    {error, Reason}       
            end;
        eof ->  
            {error, unexpected_eof};
        {error, Reason} ->   
            {error, Reason}
    end.


write_header(IoDev) ->    %% not yet cleaned up ..
    Version = 1, 
    TypeFlagsReserved1 = 0, 
    TypeFlagsAudio = 0, 
    TypeFlagsReserved2 = 0, 
    TypeFlagsVideo = 1,
    DataOffset = 9,   
    Header = << "FLV", 
              Version:8, 
              TypeFlagsReserved1:5, 
              TypeFlagsAudio:1,
              TypeFlagsReserved2:1,
              TypeFlagsVideo:1,
              DataOffset:32>>,
    case file:write(IoDev, Header) of
        ok ->
            PreviousTagsize = 0,
            case file:write(IoDev, <<PreviousTagsize:32>>) of
                ok ->
                    io:format("ok writing tag ~n");
                {errror, Reason} ->
                    ?TRACE({"Err", Reason}) 
            end;
        {errror, Reason} ->
           ?TRACE({"Err", Reason}) 
    end. 


write_tag(IoDev, TagType, Timestamp, StreamId, Data) ->        %% not yet cleaned up ..
    TimestampExtended = 0,
    DataSize = size(Data),
    PreviousTagSize = size(Data)+11, 
    TagAndPreviousTagsize = <<TagType:8/integer, 
                             DataSize:24/integer, 
                             Timestamp:24/integer, 
                             TimestampExtended:8/integer, 
                             StreamId:24/integer,
                             Data/binary,
                             PreviousTagSize:32/integer>>,      
    case file:write(IoDev, TagAndPreviousTagsize) of
        ok ->
            io:format("ok writing tag ~n");
        {errror, _Reason} ->
            io:format("Error writing tag ~n")    
    end.  
