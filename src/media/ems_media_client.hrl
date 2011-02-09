-record(client, {
  consumer,
  ref,
  stream_id,
  connected_at,
  ticker,
  ticker_ref,
  state = paused,
  bytes = 0
}).

-define(WAIT_FOR_CONFIG, 20).
