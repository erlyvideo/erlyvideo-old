require 'test/unit'
# require 'test_help'


class Test::Unit::TestCase
  def restart_erlyvideo
    `#{File.dirname(__FILE__)}/../contrib/erlyctl restart`
  end
  
  def media_duration(url, options = nil)
    lines = `ffmpeg #{options} -i #{url} 2>&1`
    md = /Duration: ([^\ ,]+),/.match(lines)
    if md.captures.first =~ /(\d+):(\d+):([\d\.]+)/
      $1.to_i*3600 + $2.to_i*60 + $3.to_f
    else
      md.captures.first
    end
  end
end

