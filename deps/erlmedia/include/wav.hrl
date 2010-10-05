%% Read more here: http://tools.ietf.org/html/rfc2361

-type(wav_audio_format() ::pcm_le|pcma|pcmu).

-define(WAV_PCM_LE, 1).
-define(WAV_PCMA, 6).
-define(WAV_PCMU, 7).

-record(wav_header, {
  audio,
  channels,
  rate,
  bps
}).

