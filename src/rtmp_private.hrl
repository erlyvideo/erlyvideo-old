
-define(RTMP_TIMEOUT, 10000).
-define(RTMP_DEF_CHUNK_SIZE, 128).
-define(MIN_CLIENT_BUFFER, 100).
-define(HS_HEADER,        3).
-define(HS_BODY_LEN,   1536).



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
-define(RTMP_CONTROL_STREAM_MAYBE_SEEK, 32).


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


-record(channel,{
	id            ::non_neg_integer(),
	timestamp     ::non_neg_integer(),
	delta         ::non_neg_integer(),
	length        ::non_neg_integer(),
	type          ::non_neg_integer(),
	stream_id = 0 ::non_neg_integer(),
	msg       = <<>> ::binary(),
	chunk_size    ::non_neg_integer()
	}).
	
-type(channel() ::#channel{}).




-record(rtmp_socket, {
  active = false    ::true|false|once,
  consumer          ::pid(),
  socket            ::port()|pid(),
  codec = undefined ::port()|undefined,
  amf_version = 0   ::integer(),
	channels          ::array(),
	out_channels      ::array(),
	address           ::tuple(),
	port              ::integer(),
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


