-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(MARK, io:format("MARK: ~p:~p ~p~n",[?MODULE, ?LINE])).


-define(MAX_RESTART,      5).
-define(MAX_TIME,        60).
-define(RTMP_PORT,     1935).
-define(TIMEOUT,     120000).
-define(HS_HEADER,        3).
-define(HS_BODY_LEN,   1536).
-define(MIN_CLIENT_BUFFER, 100).

-define(DEFAULT_FLV_DIR, "/sfe/sites/castini/htdocs/castinidemo/flv/").


%%                                    Headersize:   Value: 
%%                                   -----------   ------
-define(RTMP_HDR_NEW,          0). %% 12 Bytes   00
-define(RTMP_HDR_SAME_SRC,     1). %%  8 Bytes   01
-define(RTMP_HDR_TS_CHG,       2). %%  4 Bytes   10
-define(RTMP_HDR_CONTINUE,     3). %%  1 Byte    11

-define(RTMP_DEF_CHUNK_SIZE, 128).

-define(RTMP_TYPE_CHUNK_SIZE,     1).
%-define(RTMP_TYPE_UNKNOWN,       2).
-define(RTMP_TYPE_BYTES_READ,     3).
-define(RTMP_TYPE_PING,           4).
-define(RTMP_TYPE_BW_SERVER,      5).
-define(RTMP_TYPE_BW_CLIENT,      6).
%-define(RTMP_TYPE_UNKNOWN,       7).
-define(RTMP_TYPE_AUDIO,          8).
-define(RTMP_TYPE_VIDEO,          9).
%-define(RTMP_TYPE_UNKNOWN,      10).
%-define(RTMP_TYPE_UNKNOWN,      11).
%-define(RTMP_TYPE_UNKNOWN,      12).
%-define(RTMP_TYPE_UNKNOWN,      13).
%-define(RTMP_TYPE_UNKNOWN,      14).
%-define(RTMP_TYPE_UNKNOWN,      15).
%-define(RTMP_TYPE_UNKNOWN,      16).
%-define(RTMP_TYPE_UNKNOWN,      17).
-define(RTMP_TYPE_NOTIFY,        18).
-define(RTMP_TYPE_META_DATA,     18).
-define(RTMP_TYPE_SHARED_OBJECT, 19).
-define(RTMP_TYPE_INVOKE,        20).

-define(AMF_NUMBER,        0).
-define(AMF_BOOLEAN,       1).
-define(AMF_STRING,        2).
-define(AMF_OBJECT,        3).
-define(AMF_MOVIECLIP,     4).
-define(AMF_NULL,          5).
-define(AMF_UNDEFINED,     6).
-define(AMF_REFERENCE,     7).
-define(AMF_MIXED_ARRAY,   8).
-define(AMF_END_OF_OBJECT, 9).
-define(AMF_ARRAY,        10).
-define(AMF_DATE,         11).
-define(AMF_LONG_STRING,  12).
-define(AMF_UNSUPPORTED,  13).
-define(AMF_RECORD_SET,   14).
-define(AMF_XML,          15).
-define(AMF_TYPED_OBJECT, 16).
-define(AMF_AMF3,         17).


-define(FLV_HEADER_LENGTH,          9).
-define(FLV_HEAD_SIG,    <<70,76,86>>).
-define(FLV_HEAD_OFFSET,  <<0,0,0,9>>).
-define(FLV_PREV_TAG_SIZE_LENGTH,   4).
-define(FLV_TAG_HEADER_LENGTH,     11).

-define(FLV_TAG_TYPE_AUDIO, 8).
-define(FLV_TAG_TYPE_VIDEO, 9).
-define(FLV_TAG_TYPE_META,  18).

-define(FLV_AUDIO_TYPE_MONO,   0).
-define(FLV_AUDIO_TYPE_STEREO, 1).
-define(FLV_AUDIO_SIZE_8BIT,   0).
-define(FLV_AUDIO_SIZE_16BIT,  1).
-define(FLV_AUDIO_RATE_5_5, 0).
-define(FLV_AUDIO_RATE_11, 1).
-define(FLV_AUDIO_RATE_22, 2).
-define(FLV_AUDIO_RATE_44, 3).
-define(FLV_AUDIO_FORMAT_UNCOMPRESSED, 0).
-define(FLV_AUDIO_FORMAT_ADPCM,        1).
-define(FLV_AUDIO_FORMAT_MP3,          2).
-define(FLV_AUDIO_FORMAT_NELLYMOSER8,  5).
-define(FLV_AUDIO_FORMAT_NELLYMOSER ,  6).

-define(FLV_VIDEO_CODEC_SORENSEN,             2).
-define(FLV_VIDEO_CODEC_SCREENVIDEO,          3).
-define(FLV_VIDEO_CODEC_ON2VP6,               4).
-define(FLV_VIDEO_CODEC_ON2VP6_ALPHA,         5).
-define(FLV_VIDEO_CODEC_SCREENVIDEO2,         6).
-define(FLV_VIDEO_FRAME_TYPE_KEYFRAME,        1).
-define(FLV_VIDEO_FRAME_TYPEINTER_FRAME,      2).
-define(FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME, 3).


-record(flv_header,{
	version = 1,
	audio = 0,
	video = 0
	}).



-record(ems_server, {
	listener, % Listening socket
	acceptor, % Asynchronous acceptor's internal reference
	module    % FSM handling module
	}).

-record(ems_fsm, {
	socket,    % client socket
	addr,      % client address
	channels    = [],
	player_info = [],
	complete    = true,
	buff        = <<>>,
	prev_buff   = <<>>,
	client_buffer = ?MIN_CLIENT_BUFFER,
	chunk_size = ?RTMP_DEF_CHUNK_SIZE,
	flv_read_file,
	flv_write_file,
	flv_file_name,
	flv_file,
	flv_stream_id,
	flv_timer_start,
	flv_timer_ref,
	flv_ts_prev,
	flv_pos = 0
	}).

-record(flv_tag,{
	prev_tag_size = undefined,
	type          = undefined,
	body_length   = undefined,
	timestamp     = undefined,
	timestamp_ext = undefined,
	timestamp_abs = undefined,
	streamid      = undefined,
	pos           = undefined,
	nextpos       = undefined,
	body          = <<>>
	}).

-record(channel,{
	id        = undefined,
	timestamp = undefined,
	length    = undefined,
	type      = undefined,
	stream    = undefined,
	msg       = undefined
	}).

-record(amf,{
	command = [],
	id      = [],
	args    = []
	}).