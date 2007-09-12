-ifdef(debug). 
-compile(export_all).
-define(TRACE(X), io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, X])). 
-else. 
-define(TRACE(X), void). 
-endif.


%% RTMP protocol description
%% =========================
%%
%% Header: <<HeaderType:2, 
%%           ChannelId:6, 
%%           Timestamp:24/integer, 
%%           BodyLength:24/integer, 
%%           CommandType:8/integer, 
%%           StreamId:32/little>>
%%
%% Body:   <<Body:BodyLength/binary>>


%% --------------------------------------------------------------------
%% global definitions
%% --------------------------------------------------------------------

%% Each RTMP Header is followed by a Body with a specified Size, which
%% has an initial value
%%
-define(RTMP_DEFAULT_CHUNK_SIZE, 128).


%% Handshake
%%
-define(RTMP_HANDSHAKE_FIRST_BYTE, 3).
-define(RTMP_HANDSHAKE_BLOCK_LENGTH, 1536).

%% Header Type (bit 7,8 of first Header Byte)
%%   defines the size of the header 
%%
%% If the Headersize is less then 12 Bytes, then "missing" fields carry
%% the information form the previous Header (from same Channel) which has
%% those fields set.
%%
%%                                    Headersize:   Value: 
%%                                    -----------   ------
-define(RTMP_HEADER_NEW, 0).          %% 12 Bytes   00
-define(RTMP_HEADER_SAME_SOURCE, 1).  %%  8 Bytes   01
-define(RTMP_HEADER_TIMER_CHANGE, 2). %%  4 Bytes   10
-define(RTMP_HEADER_CONTINUE, 3).     %%  1 Byte    11


%% RTMP DataTypes
%%
-define(TYPE_CHUNK_SIZE, 1).
%% Unknown: 2 
-define(TYPE_STREAM_BYTES_READ, 3).
-define(TYPE_PING, 4).
-define(TYPE_SERVER_BANDWIDTH, 5).
-define(TYPE_CLIENT_BANDWIDTH, 6).
%% Unknown: 7
-define(TYPE_AUDIO_DATA, 8).
-define(TYPE_VIDEO_DATA, 9).
%% Unknown: 10-17
-define(TYPE_META_DATA, 18).     %% Two AMF packets (name of event and data)
%% Unknown: 19
-define(TYPE_INVOKE, 20).

%% RTMP PING Codes
%%
-define(PING_STREAM_CLEAR, 0).
-define(PING_STREAM_PLAY, 1).
%% Unknown: 2
-define(PING_CLIENT_BUFFER, 3).
-define(PING_STREAM_RESET, 4).
%% Unknown: 5
-define(PING_PING_CLIENT, 6).
-define(PING_PONG_SERVER, 7).
%% Unknown: 8
