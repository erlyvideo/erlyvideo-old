require 'test_helper'

class IphoneSegmentTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def test_segment_start
    start = media_start("http://localhost:8082/iphone/segments/video.mp4/1.ts")
    assert start.is_a?(Numeric), "Start should be number: #{start.inspect}"
    assert start > 9, "Start should be above 9 seconds: #{start}"
    assert start < 12, "Start should be below 12 seconds: #{start}"
  end
  
  def test_no_segment_after_end
    assert_raise(NotFound404) { media_start("http://localhost:8082/iphone/segments/video.mp4/3.ts") }
  end
  
  def test_segment_duration
    duration = media_duration("http://localhost:8082/iphone/segments/video.mp4/1.ts")
    assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    assert(duration > 9, "Duration should be above 9 seconds: #{duration}")
    assert(duration < 15, "Duration should be below 15 seconds: #{duration}")
  end
  
  def test_validator
    validation = limited_run("mediastreamvalidator validate http://localhost:8082/iphone/playlists/apple.mp4.m3u8", 20)
    assert validation =~ /Video codec: avc1/
    assert validation =~ /Audio codec: aac/
    assert validation =~ /Average segment duration: /
    assert !(validation =~ /ERROR: /), "Stream should be valid: #{validation}"
    
  end
end
