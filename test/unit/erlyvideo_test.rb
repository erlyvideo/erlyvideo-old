require 'test_helper'

class ErlmediaTest < Test::Unit::TestCase
  
  def run_test(name)
    output = `ERL_LIBS=#{File.dirname(__FILE__)+"/../../deps"} #{File.dirname(__FILE__)}/erlyvideo_test.erl #{name} 2>&1`
    assert output =~ /(tests|Test) passed/, "erlmedia test #{name} should pass: #{output}"
  end
  
  def test_aac
    run_test("aac")
  end

  def test_h264
    run_test("h264")
  end

  def test_flv_video_frame
    run_test("flv_video_frame")
  end

  def test_mp4
    run_test("mp4")
  end
  
  def test_sdp
    run_test("sdp")
  end
end

