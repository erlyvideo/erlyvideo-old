require 'test_helper'

class ErlmediaTest < Test::Unit::TestCase
  
  #  ack eunit  |grep .erl | sed -n -e 's/.*\/\([^\/\.]*\)\.erl.*/\1/p'
  
  def run_test(name)
    output = `ERL_LIBS=#{File.dirname(__FILE__)+"/../../deps"} #{File.dirname(__FILE__)}/erlyvideo_test #{name} 2>&1`
    assert output =~ /(tests|Test) passed/, "erlmedia test #{name} should pass: #{output}"
  end
  
  def self.build_test(mod)
    class_eval <<-EOF
    def test_#{mod}
      run_test("#{mod}")
    end
    EOF
  end
    
  %w(aac
  flv_video_frame
  h264
  http_uri2
  mp4
  mp4_writer
  packet_codec
  sdp
  mpeg2_crc32
  mpegts_reader
  rtmp
  rtmp_handshake
  rtsp_socket
  gen_cache
  ems
  media_ticker).each do |mod|
    build_test mod
  end
  
  # build_test :ems_media
  
end

