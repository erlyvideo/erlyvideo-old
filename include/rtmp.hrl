-type(rtmp_message_type() ::chunk_size|ack_read|ping|pong|buffer_size|window_size|
                            bw_peer|audio|video|broken_meta|
                            metadata3|shared_object3|invoke3|metadata|shared_object|invoke).
-type(time() ::{MegaSecs::integer(), Secs::integer(), MicroSecs::integer()}).
-type(rtmp_socket_pid() ::pid()).

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


