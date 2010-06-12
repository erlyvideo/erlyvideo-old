require 'test_helper'
require 'thread'

class MpegtsWriterTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def test_read_video_mp4
    duration = media_duration("http://localhost:8082/stream/video.mp4", "-fs 100000")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert(duration > 0, "Duration should be positive: #{duration.inspect}")
    info = media_info("/tmp/test")
    assert(info =~ /h264/, "should have video")
    assert(info =~ /aac/, "should have audio")
  end

  def test_read_video_ts
    # video.mp4 is repacked into MPEG-TS
    # MPEG-TS demuxer reads it
    # and packs back to MPEG-TS
    duration = media_duration("http://localhost:8082/stream/video.ts", "-fs 100000")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert(duration > 0, "Duration should be positive: #{duration.inspect}")
    info = media_info("/tmp/test")
    assert(info =~ /h264/, "should have video")
    assert(info =~ /aac/, "should have audio")
  end
  
  def test_reverse_mpegts
    Thread.abort_on_exception = true
    lock = true
    Thread.new do
      lock = false
      limited_run("sh -c 'ERL_LIBS=deps ./contrib/reverse_mpegts http://localhost:8082/stream/video.ts http://localhost:8082/stream/a'", 15)
    end
    sleep 5
    limited_run("rtmpdump -r rtmp://localhost/vod/a --stop 15 -o /tmp/test.flv", 10)
    duration = flvtool2_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
    info = media_info("/tmp/test.flv")
    assert(info =~ /h264/, "should have video")
    assert(info =~ /aac/, "should have audio")
  end
end
