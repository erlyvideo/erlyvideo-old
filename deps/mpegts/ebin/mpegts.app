%% This is the application resource file (.app file) for the 'base'
%% application.
{application, mpegts,
[{description, "MPEG-TS handling library"},
 {vsn, "0.1"},
 {modules, [ems_http_mpegts,
 iphone_streams,
 mpeg2_crc32,
 mpegts,
 mpegts_file_media,
 mpegts_file_reader,
 mpegts_media,
 mpegts_play,
 mpegts_reader,
 mpegts_sup]},
 {registered,[rtsp]},
 {applications, [kernel,stdlib]},
 {mod, {mpegts_sup,[]}}
]}.
