% luke: "is it possilbe to use lowercase names? 
% uppercase breaks the syntax highlighting in textmate"
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

%% RTMP header
%%                                 Headersize:   Value: 
%%                                 -----------   ------
-define(RTMP_HDR_NEW,          0). %% 12 Bytes   00
-define(RTMP_HDR_SAME_SRC,     1). %%  8 Bytes   01
-define(RTMP_HDR_TS_CHG,       2). %%  4 Bytes   10
-define(RTMP_HDR_CONTINUE,     3). %%  1 Byte    11

-define(RTMP_HDR_MED_ID,       0).
-define(RTMP_HDR_LRG_ID,       1).


-define(RTMP_DEF_CHUNK_SIZE, 128).

%% RTMP data 
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
-define(RTMP_FLEX_STREAM_SEND,   15).
-define(RTMP_FLEX_SHARED_OBJECT, 16).
-define(RTMP_FLEX_MESSAGE,       17).
-define(RTMP_TYPE_NOTIFY,        18).
-define(RTMP_TYPE_META_DATA,     18).
-define(RTMP_TYPE_SHARED_OBJECT, 19).
-define(RTMP_TYPE_INVOKE,        20).

%% RTMP shared object
-define(SO_CONNECT,              1).
-define(SO_DISCONNECT,           2).
-define(SO_SET_ATTRIBUTE,        3).
-define(SO_UPDATE_DATA,          4).
-define(SO_UPDATE_ATTRIBUTE,     5).
-define(SO_SEND_MESSAGE,         6).
-define(SO_STATUS,               7).
-define(SO_CLEAR_DATA,           8).
-define(SO_DELETE_DATA,          9).
-define(SO_DELETE_ATTRIBUTE,    10).
-define(SO_INITIAL_DATA,        11).

%% PING Codes
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

%% AMF0 data 
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

%% AMF3 data 
-define(AMF3_NULL,         1).
-define(AMF3_BOOLEAN_FALSE,2).
-define(AMF3_BOOLEAN_TRUE, 3).
-define(AMF3_INTEGER,      4).
-define(AMF3_NUMBER,       5).
-define(AMF3_STRING,       6).
-define(AMF3_XML_UNKNOWN,  7). 
-define(AMF3_DATE,         8).
-define(AMF3_ARRAY,        9).
-define(AMF3_OBJECT,      10).
-define(AMF3_XML,         11).
-define(AMF3_BYTE_ARRAY,  12).

%% AMF object 
-define(AMF3_OBJECT_PROPERTY, 0).
-define(AMF3_OBJECT_EXTERNALIZABLE, 1).
-define(AMF3_OBJECT_VALUE, 2).
-define(AMF3_OBJECT_PROXY, 3).

%% FLV header
-define(FLV_HEADER_LENGTH,          9).
-define(FLV_HEAD_SIG,    <<70,76,86>>).
-define(FLV_HEAD_OFFSET,  <<0,0,0,9>>).
-define(FLV_PREV_TAG_SIZE_LENGTH,   4).
-define(FLV_TAG_HEADER_LENGTH,     11).

%% FLV tag
-define(FLV_TAG_TYPE_AUDIO, 8).
-define(FLV_TAG_TYPE_VIDEO, 9).
-define(FLV_TAG_TYPE_META,  18).

%% FLV audio
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

%% FLV video
-define(FLV_VIDEO_CODEC_SORENSEN,             2).
-define(FLV_VIDEO_CODEC_SCREENVIDEO,          3).
-define(FLV_VIDEO_CODEC_ON2VP6,               4).
-define(FLV_VIDEO_CODEC_ON2VP6_ALPHA,         5).
-define(FLV_VIDEO_CODEC_SCREENVIDEO2,         6).
-define(FLV_VIDEO_FRAME_TYPE_KEYFRAME,        1).
-define(FLV_VIDEO_FRAME_TYPEINTER_FRAME,      2).
-define(FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME, 3).

%% NetConnection Status
-define(NC_CALL_FAILED, "NetConnection.Call.Failed").
-define(NC_CALL_BAD_VERSION, "NetConnection.Call.BadVersion").
-define(NC_CONNECT_APP_SHUTDOWN, "NetConnection.Connect.AppShutdown").
-define(NC_CONNECT_CLOSED, "NetConnection.Connect.Closed").
-define(NC_CONNECT_FAILED, "NetConnection.Connect.Failed").
-define(NC_CONNECT_REJECTED, "NetConnection.Connect.Rejected").
-define(NC_CONNECT_SUCCESS, "NetConnection.Connect.Success").
-define(NC_CONNECT_INVALID_APP, "NetConnection.Connect.InvalidApp").

%% NetStream Status
-define(NS_INVALID_ARG, "NetStream.InvalidArg").
-define(NS_CLEAR_SUCCESS, "NetStream.Clear.Success").
-define(NS_CLEAR_FAILED, "NetStream.Clear.Failed").
-define(NS_PUBLISH_START, "NetStream.Publish.Start").
-define(NS_PUBLISH_BAD_NAME, "NetStream.Publish.BadName").
-define(NS_FAILED, "NetStream.Failed").
-define(NS_UNPUBLISHED_SUCCESS, "NetStream.Unpublish.Success").
-define(NS_RECORD_START, "NetStream.Record.Start").
-define(NS_RECORD_NO_ACCESS, "NetStream.Record.NoAccess").
-define(NS_RECORD_STOP, "NetStream.Record.Stop").
-define(NS_RECORD_FAILED, "NetStream.Record.Failed").
-define(NS_PLAY_INSUFFICIENT_BW, "NetStream.Play.InsufficientBW").
-define(NS_PLAY_START, "NetStream.Play.Start").
-define(NS_PLAY_STREAM_NOT_FOUND, "NetStream.Play.StreamNotFound").
-define(NS_PLAY_STOP, "NetStream.Play.Stop").
-define(NS_PLAY_FAILED, "NetStream.Play.Failed").
-define(NS_PLAY_RESET, "NetStream.Play.Reset").
-define(NS_PLAY_PUBLISH_NOTIFY, "NetStream.Play.PublishNotify").
-define(NS_PLAY_UNPUBLISH_NOTIFY, "NetStream.Play.UnpublishNotify").
-define(NS_PLAY_SWITCH, "NetStream.Play.Switch").
-define(NS_PLAY_COMPLETE, "NetStream.Play.Complete").
-define(NS_SEEK_NOTIFY, "NetStream.Seek.Notify").
-define(NS_SEEK_FAILED, "NetStream.Seek.Failed").
-define(NS_PAUSE_NOTIFY, "NetStream.Pause.Notify").
-define(NS_UNPAUSE_NOTIFY, "NetStream.Unpause.Notify").
-define(NS_DATA_START, "NetStream.Data.Start").

%% Application Status
-define(APP_SCRIPT_ERROR, "Application.Script.Error").
-define(APP_SCRIPT_WARNING, "Application.Script.Warning").
-define(APP_RESOURCE_LOW_MEMORY, "Application.Resource.LowMemory").
-define(APP_SHUTDOWN, "Application.Shutdown").
-define(APP_GC, "Application.GC").

%% Shared Object Status
-define(SO_NO_READ_ACCESS, "SharedObject.NoReadAccess").
-define(SO_NO_WRITE_ACCESS, "SharedObject.NoWriteAccess").
-define(SO_OBJECT_CREATION_FAILED, "SharedObject.ObjectCreationFailed").
-define(SO_BAD_PERSISTENCE, "SharedObject.BadPersistence").


-record(ems_cluster, {
	}).
	
-record(ems_connection, {
    client_id,
    pid
    }).

-record(ems_stream, {
    stream, 
    client_ids
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
	next_stream_id = 1,
	publish,
	flv_device,
	flv_buffer,
	flv_file_name,
	flv_stream_id,
	flv_timer_start,
	flv_timer_ref,
	flv_ts_prev = 0,
	flv_pos = 0
	}).

-record(flv_header,{
	version = 1,
	audio = 0,
	video = 0
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