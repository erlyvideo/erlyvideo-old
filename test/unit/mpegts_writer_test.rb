require 'test_helper'

class MpegtsWriterTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def test_read_video_mp4
    duration = media_duration("http://localhost:8082/stream/video.mp4", "-fs 100000")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert(duration > 0, "Duration should be positive: #{duration.inspect}")
  end

  def test_read_video_ts
    # video.mp4 is repacked into MPEG-TS
    # MPEG-TS demuxer reads it
    # and packs back to MPEG-TS
    duration = media_duration("http://localhost:8082/stream/video.ts", "-fs 100000")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert(duration > 0, "Duration should be positive: #{duration.inspect}")
  end
end
