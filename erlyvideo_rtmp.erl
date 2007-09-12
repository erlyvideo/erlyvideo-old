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
%%% Description : Protocol abstraction
%%%
%%% Created : 18 Nov,06
%%% -------------------------------------------------------------------
-module(erlyvideo_rtmp).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("erlyvideo.hrl").

%% --------------------------------------------------------------------
%% definitions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% External exports - RTMP calls
%% --------------------------------------------------------------------
-export([connect/4, 
         createStream/4, 
         deleteStream/4, 
         play/4, 
         publish/4]).


%% --------------------------------------------------------------------
%% External exports 
%% --------------------------------------------------------------------
-export([header/1,
         handshake/1,
         do_channels_packet/4]).


%% ====================================================================
%% External functions - RTMP calls
%% ====================================================================


connect(From, Iid, [{object, _PlayerInfo} | _], Ch) ->  
    ?TRACE({"invoke - connect - Channel:", Ch}),   
    Msg = [{level, "status"}, 
           {code, "NetConnection.Connect.Success"}, 
           {description, "Connected."}],  
    Data = {"_result", Iid, [null, Msg]}, 
    gen_fsm:send_event(From, {send, Ch, erlyvideo_amf:to_binary(Data)}).    


createStream(From, Iid, _Args, Ch) ->
    ?TRACE({"invoke - createStream - Channel:", Ch}),   
    Data = {"_result", Iid, [null, generate_stream_id()]}, 
    gen_fsm:send_event(From, {send, Ch, erlyvideo_amf:to_binary(Data)}).     
  

deleteStream(_From, _Iid, _Args, Ch) ->  
    ?TRACE({"invoke - deleteStream (not implemented yet) - Channel:", Ch}).   

play(From, Iid, [WhatIsThis, {string, Name}], {_, _, _, StId} = Ch) -> 
    ?TRACE({"invoke - play - Channel:", Ch, WhatIsThis}),                                                                                    
    MsgPlayReset = [{level, "status"}, 
                    {code, "NetStream.Play.Reset"}, 
                    {description, "Resetting."},
                    {details, Name},
                    {clientid, StId}], 
    DataPlayReset = {"onStatus", Iid, [null, MsgPlayReset]},
    gen_fsm:send_event(From, {send, Ch, erlyvideo_amf:to_binary(DataPlayReset)}),
    MsgPlayStart = [{level, "status"}, 
                    {code, "NetStream.Play.Start"}, 
                    {description, "Start playing."},
                    {details, Name},
                    {clientid, StId}],
    DataPlayStart = {"onStatus", Iid, [null, MsgPlayStart]}, 
    gen_fsm:send_event(From, {send, Ch, erlyvideo_amf:to_binary(DataPlayStart)}),
    gen_fsm:send_event(From, {play, Name, StId}).

            
publish(From, Iid, 
        [WhatIsThis, {string, Name}, {string, "record"}], 
        {_, _, _, StId} = Ch) ->
    ?TRACE({"invoke - publish - Channel:", Ch, WhatIsThis}),             
    Msg = [{level, "status"}, 
           {code, "NetStream.Publish.Start"}, 
           {description, "Start recording."},
           {clientid, StId}], 
    Data = {"onStatus", Iid, [null, Msg]},
    gen_fsm:send_event(From, {send,  Ch, erlyvideo_amf:to_binary(Data)}),
    gen_fsm:send_event(From, {record, Name}).

 
closeStream(From, _Iid, [WhatIsThis], Ch) ->
    ?TRACE({"invoke - closeStream - Channel:", Ch, WhatIsThis}),   
    gen_fsm:send_event(From, {stop}).        



%% ====================================================================
%% External functions - API
%% ====================================================================

handshake(Data) ->
    %% a better way is documented somewhere on Red5 mailing list
    <<Data/binary, Data/binary>>.


generate_stream_id() ->  %% provisory !!!!!!!
   1.  %% TODO: replace by (distrubuted) gen_server to get a unique Id


header(<<?RTMP_HEADER_CONTINUE:2, ChId:6, Rest/binary>>) ->
    {{ChId, undefined, undefined, undefined, undefined, undefined}, Rest};
header(<<?RTMP_HEADER_TIMER_CHANGE:2, ChId:6, Ts:24/integer, Rest/binary>>) ->
    {{ChId, Ts, undefined, undefined, undefined, undefined}, Rest};
header(<<?RTMP_HEADER_SAME_SOURCE:2, ChId:6, Ts:24/integer, Length:24/integer, 
        Type:8/integer, Rest/binary>>) ->
    {{ChId, Ts, Length, Type, undefined, undefined}, Rest};
header(<<?RTMP_HEADER_NEW:2, ChId:6, Ts:24/integer, Length:24/integer, 
        Type:8/integer, StId:32/little, Rest/binary>>) ->
    {{ChId, Ts, Length, Type, StId, <<>>}, Rest}.



do_channels_packet({ChId, Ts, Type, StId}, Data, Chs, ChunkSize) -> 
    Length = size(Data),   
    Data2 = chunk(binary_to_list(Data), ChId, [], ChunkSize),
    case lists:keysearch(ChId, 1, Chs) of 
        {value, {_, Ts2, Length2, Type2, StId2, _}} ->  
            {Ch, Pck} = if
                            (Ts == Ts2) and (Type == Type2) 
                            and (Length == Length2) and (StId == StId2) ->
                                {{ChId, Ts2, Length2, Type2, StId2, <<>>}, 
                                 packet(ChId, Data2)};
                            (Type == Type2) and (Length == Length2) 
                            and (StId == StId2) ->  
                                {{ChId, Ts, Length2, Type2, StId2, <<>>},
                                 packet(ChId, Ts, Data2)};
                            (StId == StId2) ->   
                                {{ChId, Ts, Length, Type, StId2, <<>>}, 
                                 packet(ChId, Ts, Length, Type, Data2)};
                            true ->  
                                {{chId, Ts, Length, Type, StId, <<>>}, 
                                 packet(ChId, Ts, Length, Type, StId, Data2)}                          
                        end,                                    
            {lists:keyreplace(ChId, 1, Chs, Ch), Pck};                                            
        false ->            
            Ch = {ChId, Ts, Length, Type, StId, <<>>},       
            {[Ch | Chs],  packet(ChId, Ts, Length, Type, StId, Data2)}         
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

chunk(DataList, ChId, Buffer, ChunkSize) -> 
    Header = <<?RTMP_HEADER_CONTINUE:2, ChId:6>>,
    DataSize = length(DataList),  
    if 
        (DataSize > ChunkSize) ->   
            {First, Rest} = lists:split(ChunkSize, DataList),
            chunk(Rest, ChId, [Header | [First | Buffer]], ChunkSize);
        true ->
            list_to_binary(lists:reverse([DataList | Buffer]))     
    end.


packet(ChId, Data) -> 
    <<?RTMP_HEADER_CONTINUE:2, ChId:6, Data/binary>>.  

packet(ChId, Ts, Data) ->   
    <<?RTMP_HEADER_TIMER_CHANGE:2, ChId:6, Ts:24/integer, Data/binary>>.

packet(ChId, Ts, Len, Type, Data) -> 
    <<?RTMP_HEADER_SAME_SOURCE:2, ChId:6, Ts:24/integer, Len:24/integer, 
     Type:8/integer, Data/binary>>.

packet(ChId, Ts, Len, Type, StId, Data) ->  
    <<?RTMP_HEADER_NEW:2, ChId:6, Ts:24/integer, Len:24/integer, Type:8/integer, 
     StId:32/little, Data/binary>>.
