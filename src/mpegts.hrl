-define(TYPE_VIDEO_MPEG1, 1).
-define(TYPE_VIDEO_MPEG2, 2).
-define(TYPE_VIDEO_MPEG4, 16).
-define(TYPE_VIDEO_H264,  27).
-define(TYPE_VIDEO_VC1,   234).
-define(TYPE_VIDEO_DIRAC, 209).
-define(TYPE_AUDIO_MPEG1, 3).
-define(TYPE_AUDIO_MPEG2, 4).
-define(TYPE_AUDIO_AAC,   15).
-define(TYPE_AUDIO_AAC2,  17).
-define(TYPE_AUDIO_AC3,   129).
-define(TYPE_AUDIO_DTS,   138).

-define(MPEGTS_STREAMID_AAC, 192).
-define(MPEGTS_STREAMID_H264, 224).

-define(DESCRIPTOR_IOD, 29).
-define(DESCRIPTOR_CA, 9).
-define(DESCRIPTOR_SL, 31).


-define(D(X), ems_log:debug(3, mpegts, "~p:~p ~p~n",[?MODULE, ?LINE, X])).


-record(mpegts_pat, {
  descriptors
}).

