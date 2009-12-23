%% RTMP header
%%                                 Headersize:   Value: 
%%                                 -----------   ------
-define(RTMP_HDR_NEW,          0). %% 12 Bytes   00
-define(RTMP_HDR_SAME_SRC,     1). %%  8 Bytes   01
-define(RTMP_HDR_TS_CHG,       2). %%  4 Bytes   10
-define(RTMP_HDR_CONTINUE,     3). %%  1 Byte    11

-define(RTMP_HDR_MED_ID,       0).
-define(RTMP_HDR_LRG_ID,       1).



-type(rtmp_message_type() ::chunk_size|ack_read|ping|pong|buffer_size|window_size|
                            bw_peer|audio|video|broken_meta|
                            metadata3|shared_object3|invoke3|metadata|shared_object|invoke).
-type(time() ::{MegaSecs::integer(), Secs::integer(), MicroSecs::integer()}).

-record(rtmp_message_ack, {
  bytes_read    ::integer(),
  previous_ack  ::time(),
  current_ack   ::time(),
  speed         ::integer()
}).

-record(rtmp_message_buffer_size, {
  stream_id     ::integer(),
  buffer_size   ::integer()
}).

-record(rtmp_message, {
  channel_id ::integer(),
  timestamp  ::integer(),
  type       ::rtmp_message_type(),
  stream_id  ::integer(),
  body       ::binary()|#rtmp_message_ack{}
}).

