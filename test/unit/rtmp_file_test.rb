require 'test_helper'

class RtmpFileTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def teardown
    File.unlink("/tmp/test.flv")
  end
  
  def test_read_file
    `rtmpdump -r rtmp://localhost/vod/mp4:video --stop 5 -o /tmp/test.flv`
    duration = media_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
  end
end
