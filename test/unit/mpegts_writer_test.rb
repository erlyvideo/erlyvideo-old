require 'test_helper'

class MpegtsWriterTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def test_read_video_mp4
    info = media_info("http://localhost:8082/stream/video.mp4", "-fs 100000")
    assert info =~ /Video: h264/, "Video stream should be found"
    assert info =~ /Audio: aac/, "Audio stream should be found"
  end
end
