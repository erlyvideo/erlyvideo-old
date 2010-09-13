-record(client, {
  consumer,
  ref,
  stream_id,
  ticker,
  ticker_ref,
  state = paused
}).

-define(WAIT_FOR_CONFIG, 20).
