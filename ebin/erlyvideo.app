{application, erlyvideo,
 [
  {description, "Erlang Video Server"},
  {vsn, "1.0"},
  {id, "erlyvideo"},
  {modules,      [
  apps_push
  apps_recording
  apps_stat_collector
  apps_streaming
  array_timeshift
  auth_users_limit
  check_play_web
  directory_playlist
  ems
  ems_encoding
  ems_event
  ems_flv_streams
  ems_http
  ems_http_file
  ems_http_flv
  ems_http_push
  ems_http_rtmpt
  ems_http_stats
  ems_http_templates
  ems_license_client
  ems_media
  ems_media_clients
  ems_media_frame
  ems_media_tests
  ems_script
  ems_sup
  ems_users
  ems_vhosts
  erlyvideo
  erlyvideo_ctl
  file_media
  fitc_demo
  gen_cache
  http_media
  json_session
  limited_connections_login
  live_media
  media_detector
  media_provider
  media_ticker
  misultin_req
  misultin_socket
  mochijson2
  password_publish
  protected_play
  referer_check
  reverse_mpegts
  rtmp_media
  rtmp_publish
  rtmp_session
  shared_object
  shared_objects
  test_media
  trusted_login
  uuid
  ]},
  {registered,   []},
  {applications, [kernel,stdlib,crypto]},
  {mod, {erlyvideo, []}},
  {env, []}
 ]
}.








