-record(ems_media, {
  url,
  name,
  module,
  state,
  type,
  options,
  video_config,
  audio_config,
  metadata,
  clients,
  source,
  source_ref,
  storage,
  format,
  
  last_dts = 0,
  ts_delta,
  
  source_timeout,
  source_timeout_ref,

  clients_timeout,
  clients_timeout_ref,
  
  retry_count
}).

