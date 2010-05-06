require 'test/unit'
# require 'test_help'
require 'timeout'

class Test::Unit::TestCase
  def restart_erlyvideo
    `#{File.dirname(__FILE__)}/../contrib/erlyctl restart`
  end
  
  def media_info(url, options = nil)
    if url =~ /http:\/\//
      begin
        File.unlink("/tmp/test") if File.exists?("/tmp/test")
        Timeout::timeout(12) do
          `curl --connect-timeout 1 -s -S -o /tmp/test "#{url}" 2>&1`
        end
      rescue ::Timeout::Error
        puts "timeout"
      end
      File.exists?("/tmp/test") ? `ffmpeg -timelimit 8 #{options} -i /tmp/test 2>&1` : raise("Couldn't download #{url}")
    else
      `ffmpeg -i #{url} 2>&1`
    end
  end
  
  def media_duration(url, options = nil)
    lines = media_info(url, options)
    puts lines
    md = /Duration: ([^\ ,]+),/.match(lines)
    if md && md.captures.first =~ /(\d+):(\d+):([\d\.]+)/
      $1.to_i*3600 + $2.to_i*60 + $3.to_f
    else
      md && md.captures.first
    end
  end

  def media_start(url, options = nil)
    lines = media_info(url, options)
    if lines =~ /start: ([\d\.]+)/
      $1.to_f
    else
      nil
    end
  end
end

