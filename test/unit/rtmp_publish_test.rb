require 'test_helper'

class RtmpFileTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
    if !(@publisher = Process.fork)
      puts "Starting publisher"
      exec("contrib/rtmp_publish #{File.dirname(__FILE__)+"/../fixtures/livestream.flv"} rtmp://localhost/livestream")
    end
    sleep(3)
  end
  
  def teardown
    Process.kill("KILL", @publisher)
    # File.unlink("/tmp/test.flv") if File.exists?("/tmp/test.flv")
  end
  
  def test_published_stream_is_shifted
    File.unlink("/tmp/test.flv") if File.exists?("/tmp/test.flv")
    result = begin
      Timeout::timeout(7) {`rtmpdump -r rtmp://localhost/live/livestream --stop 5 -o /tmp/test.flv 2>&1`} 
    rescue Timeout::Error
      :timeout
    end
    assert(File.size("/tmp/test.flv") > 0, "Should download file: #{result}")
    
    duration = flvtool2_duration("/tmp/test.flv")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert duration > 4, "Duration should be positive: #{duration}"
    
    start = media_start("/tmp/test.flv")
    assert start.is_a?(Numeric), "Start be number: #{start.inspect}"
    assert_equal 0, start, "Start of rtmp stream should be strictly zero: #{start}"
    
    File.unlink("/tmp/test.flv")
  end
  
  def test_iphone_of_live_stream
    result = Timeout::timeout(7) {`curl -s http://localhost:8082/iphone/playlists/livestream.m3u8`}
    assert_equal <<-EOF, result
#EXTM3U
#EXT-X-MEDIA-SEQUENCE:0
#EXT-X-TARGETDURATION:10
#EXT-X-ALLOW-CACHE:YES
#EXTINF:10,
/iphone/segments/livestream/0.ts
    EOF

    result = Timeout::timeout(7) {`curl -s http://localhost:8082/iphone/segments/livestream/0.ts`}
    assert result.size > 10000, "Should download large content: size is #{result.size}"
  end
end
