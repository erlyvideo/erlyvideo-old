-record(client, {
  consumer,
  ref,
  stream_id,
  ticker,
  ticker_ref,
  state = paused,
  bytes = 0
}).

-define(WAIT_FOR_CONFIG, 20).
