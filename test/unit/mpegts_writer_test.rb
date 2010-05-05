require 'test_helper'

class MpegtsWriterTest < Test::Unit::TestCase
  def test_read_video_mp4
    lines = `ffmpeg -fs 100000 -i http://localhost:8082/stream/video.mp4 2>&1`
    md = /Duration: ([^\ ,]+),/.match(lines)
    assert md, "Should parse duration from ffmpeg output"
    assert md.captures.first =~ /(\d+):(\d+)/, "Duration should be positive: #{md.captures.first.inspect}"
  end
end
