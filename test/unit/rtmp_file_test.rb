require 'test_helper'

class RtmpFileTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def teardown
    # File.unlink("/tmp/test.flv") if File.exists?("/tmp/test.flv")
  end
  
  def test_read_file
    Timeout::timeout(6) {`rtmpdump -r rtmp://localhost/vod/mp4:video --stop 5 -o /tmp/test.flv 2>&1`} rescue true
    duration = media_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
  end

  def test_read_stream
    File.unlink("/tmp/test.flv") if File.exists?("/tmp/test.flv")
    result = begin
      Timeout::timeout(7) {`rtmpdump -r rtmp://localhost/vod/video.ts --stop 5 -o /tmp/test.flv 2>&1`} 
    rescue Timeout::Error
      :timeout
    end
    assert(File.size("/tmp/test.flv") > 0, "Should download file: #{result}")
    flvtool = `flvtool2 -D /tmp/test.flv 2>&1`
    assert(flvtool.split(/\n/).last =~ /timestamp (\d+),/, "FLVTool should dump file")
    duration = $1.to_i / 1000
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
    File.unlink("/tmp/test.flv")
  end
end
