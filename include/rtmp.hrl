-type(rtmp_message_type() ::chunk_size|ack_read|ping|pong|buffer_size|window_size|
                            bw_peer|audio|video|broken_meta|control|
                            metadata3|shared_object3|invoke3|metadata|shared_object|invoke).

-type(shared_object_event() ::connect|disconnect|set_attribute|update_data|update_attribute|send_message|
                              status|clear_data|delete_data|delete_attribute|initial_data).
                            
-type(rtmp_control_type() ::stream_begin).
-type(time() ::{MegaSecs::integer(), Secs::integer(), MicroSecs::integer()}).
-type(rtmp_socket_pid() ::pid()).

-record(rtmp_message_ack, {
  bytes_read    ::integer(),
  previous_ack  ::time(),
  current_ack   ::time(),
  speed         ::integer()
}).

-record(rtmp_message, {
  channel_id ::integer(),
  timestamp  ::integer(),
  type       ::rtmp_message_type(),
  stream_id  ::integer(),
  body       ::binary()|integer()|#rtmp_message_ack{}
}).

-type(rtmp_message() ::#rtmp_message{}).



-record(rtmp_funcall,{
  version = 0,
	command = [],
	id      = [],
	args    = [],
	stream_id = 0,
	type 	= invoke %if invoke then id, otherwise notify
	}).

-record(so_message, {
  name,
  version,
  persistent,
  events = []
}).



