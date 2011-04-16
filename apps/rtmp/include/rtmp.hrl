-type(rtmp_message_type() ::chunk_size|ack_read|ping|pong|buffer_size|window_size|
                            bw_peer|audio|video|control|
                            metadata3|shared_object3|invoke3|metadata|shared_object|invoke).

-type(shared_object_event() ::connect|disconnect|set_attribute|update_data|update_attribute|send_message|
                              status|clear_data|delete_data|delete_attribute|initial_data).
                            
-type(rtmp_control_type() ::stream_begin).
-type(time() ::{MegaSecs::integer(), Secs::integer(), MicroSecs::integer()}).
-type(rtmp_socket_pid() ::pid()).
-type(proplist() ::[{Key::atom(), Value::binary()}]).
-type(rtmp_control() ::{char(), binary()}).
-type(any_string() ::string()|binary()).

-record(rtmp_message_ack, {
  bytes_read    ::non_neg_integer(),
  previous_ack  ::time(),
  current_ack   ::time(),
  speed         ::non_neg_integer()
}).

-type(rtmp_message_ack() ::#rtmp_message_ack{}).

-record(rtmp_funcall,{
  version = 0,
	command = [],
	id      = [],
	args    = [],
	stream_id = 0,
	type 	= invoke %if invoke then id, otherwise notify
	}).

-type(rtmp_funcall() ::#rtmp_funcall{}).

-record(so_message, {
  name,
  version = 0,
  persistent = false,
  events = []
}).

-type(so_message() ::#so_message{}).
-type(content_type() ::audio|video|metadata).

-type(timestamp_type() ::new|delta|undefined).
-type(fixed_timestamp_type() ::new|delta).
-type(rtmp_timestamp() ::non_neg_integer()|float()|same).

-record(rtmp_message, {
  channel_id ::non_neg_integer(),
  ts_type    ::timestamp_type(),
  timestamp  ::rtmp_timestamp(),
  type       ::rtmp_message_type()|integer(),
  stream_id  ::non_neg_integer()|float(),
  body       ::binary()|non_neg_integer()|rtmp_message_ack()|rtmp_funcall()|so_message()|proplist()|rtmp_control()|list()
}).

-type(rtmp_message() ::#rtmp_message{}).





