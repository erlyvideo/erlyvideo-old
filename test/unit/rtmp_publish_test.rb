require 'test_helper'

class RtmpFileTest < Test::Unit::TestCase
  def setup
    # restart_erlyvideo
    # sleep(1)
    @pid = start_command("./contrib/rtmp_publish test/fixtures/livestream.flv rtmp://localhost/livestream", "/tmp/livestream.txt")
    sleep(3)
  end
  
  def teardown
    Process.kill("KILL", @pid)
    File.unlink("/tmp/livestream.txt") if File.exists?("/tmp/livestream.txt")
  end
  
  def test_published_stream_is_shifted
    File.unlink("/tmp/test.flv") if File.exists?("/tmp/test.flv")
    result, output = limited_run("rtmpdump -r rtmp://localhost/live/livestream --stop 5 -o /tmp/test.flv", 5)

    assert(File.size("/tmp/test.flv") > 0, "Should download file: #{result} #{File.read("/tmp/livestream.txt")}")
    
    duration = flvtool2_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
    
    start = media_start("/tmp/test.flv")
    assert start.is_a?(Numeric), "Start be number: #{start.inspect}"
    # assert_equal 0, start, "Start of rtmp stream should be strictly zero: #{start}"
    
    File.unlink("/tmp/test.flv")
  end
  
  def test_iphone_of_live_stream
    result, output = limited_run("curl -s http://localhost:8082/iphone/playlists/livestream.m3u8", 5)
    assert_equal <<-EOF, result
#EXTM3U
#EXT-X-MEDIA-SEQUENCE:0
#EXT-X-TARGETDURATION:10
#EXT-X-ALLOW-CACHE:YES
#EXTINF:10,
/iphone/segments/livestream/0.ts
    EOF

    result, output = limited_run("curl -s http://localhost:8082/iphone/segments/livestream/0.ts", 5)
    assert result.size > 10000, "Should download large content: size is #{result.size}"
  end
end
