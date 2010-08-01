require 'test_helper'

class RtmpFileTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def teardown
    # File.unlink("/tmp/test.flv") if File.exists?("/tmp/test.flv")
  end
  
  def test_read_file
    limited_run("rtmpdump -r rtmp://localhost/vod/mp4:video --stop 5 -o /tmp/test.flv", 5)
    duration = flvtool2_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
  end

  def test_read_file_flv
    limited_run("rtmpdump -r rtmp://localhost/vod/flv:video --stop 5 -o /tmp/test.flv", 5)
    duration = flvtool2_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
  end

  def test_read_stream
    File.unlink("/tmp/test.flv") if File.exists?("/tmp/test.flv")
    
    result, output = limited_run("rtmpdump -r rtmp://localhost/vod/video.ts --stop 5 -o /tmp/test.flv", 7)
    assert( (File.size("/tmp/test.flv") > 0), "Should download file: #{result}")
    duration = flvtool2_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"

    # start = flvtool2_start("/tmp/test.flv")
    # assert start.is_a?(Numeric), "Start should be number: #{start.inspect}"
    # assert_equal 0, start , "Start should be strictly zero: #{start}"
    File.unlink("/tmp/test.flv")
  end
end
