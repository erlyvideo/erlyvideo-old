require 'test_helper'

class IphoneSegmentTest < Test::Unit::TestCase
  def setup
    restart_erlyvideo
  end
  
  def test_segment
    start = media_start("http://localhost:8082/iphone/segments/video.mp4/1.ts")
    assert start.is_a?(Numeric), "Start should be number: #{start.inspect}"
    assert start > 9, "Start should be above 9 seconds: #{start}"
    assert start < 12, "Start should be below 12 seconds: #{start}"

    # duration = media_duration("http://localhost:8082/iphone/segments/video.mp4/1.ts")
    # assert duration.is_a?(Numeric), "Duration should be number: #{duration.inspect}"
    # assert duration > 9, "Duration should be positive: #{duration}"
    # assert duration > 15, "Duration should be positive: #{duration}"
  end
end
