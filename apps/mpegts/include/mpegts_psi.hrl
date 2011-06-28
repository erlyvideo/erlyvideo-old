-record(dvb_ca_desc, {
  system_id,
  pid,
  private
}).

-record(psi_table, {
  table_id,
  ts_stream_id,
  version,
  current_next,
  section_number,
  last_section_number
}).

-record(pmt_entry, {
  pid,
  codec,
  program
}).

-record(eit_event, {
  pid,
  id,
  start,
  duration,
  status,
  language,
  name,
  about,
  encrypted
}).

-define(DESCRIPTOR_IOD, 29).
-define(DESCRIPTOR_CA, 9).
-define(DESCRIPTOR_SL, 31).




-define(TYPE_VIDEO_MPEG1, 1).
-define(TYPE_VIDEO_MPEG2, 2).
-define(TYPE_VIDEO_MPEG4, 16).
-define(TYPE_VIDEO_H264,  27).
-define(TYPE_VIDEO_VC1,   234).
-define(TYPE_VIDEO_DIRAC, 209).
-define(TYPE_AUDIO_MPEG1, 3).
-define(TYPE_AUDIO_MPEG2, 4).
-define(TYPE_AUDIO_AAC,   15).
-define(TYPE_AUDIO_AAC2,  17). % LATM
-define(TYPE_AUDIO_AC3,   129).
-define(TYPE_AUDIO_DTS,   138).


