
%% RTMP header
%%                                 Headersize:   Value: 
%%                                 -----------   ------
-define(RTMP_HDR_NEW,          0). %% 12 Bytes   00
-define(RTMP_HDR_SAME_SRC,     1). %%  8 Bytes   01
-define(RTMP_HDR_TS_CHG,       2). %%  4 Bytes   10
-define(RTMP_HDR_CONTINUE,     3). %%  1 Byte    11

-define(RTMP_HDR_MED_ID,       0).
-define(RTMP_HDR_LRG_ID,       1).


-define(RTMP_TIMEOUT, 10000).
-define(RTMP_DEF_CHUNK_SIZE, 128).
-define(RTMP_PREF_CHUNK_SIZE, (4*1024)).
-define(MIN_CLIENT_BUFFER, 100).
-define(RTMP_WINDOW_SIZE, 2500000).
-define(HS_HEADER,        3).
-define(HS_BODY_LEN,   1536).


-record(channel,{
	id        = undefined,
	timestamp = undefined,
	delta     = undefined,
	length    = undefined,
	type      = undefined,
	stream_id = 0,
	msg       = <<>>,
	chunk_size
	}).



-record(rtmp_socket, {
  consumer          ::pid(),
  socket            ::port(),
  amf_version       ::integer(),
	channels          ::array(),
	address           ::tuple(),
	port              ::integer(),
	buffer = <<>>     ::binary(),
	client_buffer = ?MIN_CLIENT_BUFFER       ::integer(),
	client_chunk_size = ?RTMP_DEF_CHUNK_SIZE ::integer(),
	server_chunk_size = ?RTMP_DEF_CHUNK_SIZE ::integer(),
	window_size = ?RTMP_WINDOW_SIZE          ::integer(),
	previous_ack = undefined  ::time(),
	current_speed = 0         ::integer(),
	pinged = false            ::boolean()
}).



