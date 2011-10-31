%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP private constants
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------

-define(RTMP_TIMEOUT, 120000).
-define(RTMP_DEF_CHUNK_SIZE, 128).
-define(MIN_CLIENT_BUFFER, 100).
-define(HS_UNCRYPTED,        3).
-define(HS_CRYPTED,        6).
-define(HS_HEADER,        3).
-define(HS_BODY_LEN,   1536).


-include("log.hrl").



%% RTMP header
%%                                 Headersize:   Value: 
%%                                 -----------   ------
-define(RTMP_HDR_NEW,          0). %% 12 Bytes   00
-define(RTMP_HDR_SAME_SRC,     1). %%  8 Bytes   01
-define(RTMP_HDR_TS_CHG,       2). %%  4 Bytes   10
-define(RTMP_HDR_CONTINUE,     3). %%  1 Byte    11

-define(RTMP_HDR_MED_ID,       0).
-define(RTMP_HDR_LRG_ID,       1).





%% RTMP data 
-define(RTMP_TYPE_CHUNK_SIZE,     1).
-define(RTMP_TYPE_ABORT,          2).
-define(RTMP_TYPE_ACK_READ,       3).
-define(RTMP_TYPE_CONTROL,        4).
-define(RTMP_TYPE_WINDOW_ACK_SIZE,5).
-define(RTMP_TYPE_BW_PEER,        6).
%-define(RTMP_TYPE_UNKNOWN,       7).
-define(RTMP_TYPE_AUDIO,          8).
-define(RTMP_TYPE_VIDEO,          9).
%-define(RTMP_TYPE_UNKNOWN,      10).
%-define(RTMP_TYPE_UNKNOWN,      11).
%-define(RTMP_TYPE_UNKNOWN,      12).
%-define(RTMP_TYPE_UNKNOWN,      13).
% -define(RTMP_TYPE_UNKNOWN,      14).
-define(RTMP_TYPE_METADATA_AMF3,  15).
-define(RTMP_TYPE_SO_AMF3,        16).
-define(RTMP_INVOKE_AMF3,         17).
-define(RTMP_TYPE_METADATA_AMF0,  18).
-define(RTMP_TYPE_SO_AMF0,        19).
-define(RTMP_INVOKE_AMF0,         20).


-define(RTMP_CONTROL_STREAM_BEGIN,    0).
-define(RTMP_CONTROL_STREAM_EOF,      1).
-define(RTMP_CONTROL_STREAM_DRY,      2).
-define(RTMP_CONTROL_STREAM_BUFFER,   3).
-define(RTMP_CONTROL_STREAM_RECORDED, 4).
-define(RTMP_CONTROL_STREAM_PING,     6).
-define(RTMP_CONTROL_STREAM_PONG,     7).
-define(RTMP_CONTROL_STREAM_BURST_STOP, 31).
-define(RTMP_CONTROL_STREAM_BURST_START, 32).


-define(ENCODE, 1).
-define(DECODE, 2).
-define(OK, 3).
-define(MORE, 4).
-define(CONTINUE, 5).
-define(SET_CHUNK_SIZE_IN, 6).
-define(SET_CHUNK_SIZE_OUT, 7).
-define(GET_CHUNK_SIZE_IN, 8).
-define(GET_CHUNK_SIZE_OUT, 9).
-define(CHUNK_ABORT, 10).
-define(ALLOC_CSID, 11).
-define(SET_KEY_IN, 12).
-define(SET_KEY_OUT, 13).


-record(channel,{
	id            ::non_neg_integer(),
	timestamp     ::non_neg_integer(),
	delta         ::non_neg_integer(),
	length        ::non_neg_integer(),
	type          ::non_neg_integer(),
	stream_id = 0 ::non_neg_integer(),
	msg       = <<>> ::binary(),
	chunk_size    ::non_neg_integer(),
  abs_ts = false::boolean()
	}).
	
-type(channel() ::#channel{}).


-type handshake_version() ::version1|version2.



-record(rtmp_socket, {
  active = false    ::true|false|once,
  consumer          ::pid(),
  socket            ::port()|pid(),
  amf_version = 0   ::integer(),
  fmle_3 = false    ::boolean(),
	channels          ::tuple(),
	out_channels      ::tuple(),
	sent_audio_notify = false ::boolean(),
	sent_video_notify = false ::boolean(),
	debug = false     ::boolean(),
	url               ::string(),
	address           ::tuple(),
	port              ::integer(),
	key_in            ::binary()|undefined,
	key_out           ::binary()|undefined,
	buffer = <<>>     ::binary(),
	bytes_read = 0    ::integer(),
	bytes_sent = 0    ::integer(),
	client_buffer = ?MIN_CLIENT_BUFFER       ::integer(),
	client_chunk_size = ?RTMP_DEF_CHUNK_SIZE ::integer(),
	server_chunk_size = ?RTMP_DEF_CHUNK_SIZE ::integer(),
	window_size = 2500000     ::non_neg_integer(),
	previous_ack = undefined  ::time()|undefined,
	bytes_unack = 0           ::non_neg_integer(),
	current_speed = 0         ::non_neg_integer(),
	pinged = false            ::boolean()
}).

-type(rtmp_socket() ::#rtmp_socket{}).


