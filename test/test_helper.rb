require 'test/unit'
# require 'test_help'
require 'timeout'

class Test::Unit::TestCase
  def restart_erlyvideo
    `#{File.dirname(__FILE__)}/../contrib/erlyctl restart`
  end
  
  def start_command(command, output)
    if !(pid = Process.fork)
      writeme = File.open(output, "w+")
      STDERR.reopen(writeme)
      STDOUT.reopen(writeme)
      exec(command)
    end
    pid
  end  
  
  def limited_run(command, timeout)
    pid = start_command(command, "/tmp/output.txt")
    
    sleep(timeout)
    Process.kill("KILL", pid)
    File.read("/tmp/output.txt")
  end
  
  def media_info(url, options = nil)
    if url =~ /http:\/\//
      File.unlink("/tmp/test") if File.exists?("/tmp/test")
      limited_run("curl --connect-timeout 1 -s -S -o /tmp/test \"#{url}\"", 5)
      File.exists?("/tmp/test") ? `ffmpeg -timelimit 8 #{options} -i /tmp/test 2>&1` : raise("Couldn't download #{url}")
    else
      `ffmpeg -i #{url} 2>&1`
    end
  end
  
  def media_duration(url, options = nil)
    lines = media_info(url, options)
    md = /Duration: ([^\ ,]+),/.match(lines)
    if md && md.captures.first =~ /(\d+):(\d+):([\d\.]+)/
      $1.to_i*3600 + $2.to_i*60 + $3.to_f
    else
      md && md.captures.first
    end
  end
  
  def flvtool2_duration(file)
    flvtool = `flvtool2 -D #{file} 2>&1`
    assert(flvtool.split(/\n/).last =~ /timestamp (\d+),/, "FLVTool should dump file")
    $1.to_i / 1000
  end

  def flvtool2_start(file)
    flvtool = `flvtool2 -D #{file} 2>&1`
    assert(flvtool.split(/\n/).grep(/#1 /).first =~ /timestamp (\d+),/, "FLVTool should dump file")
    $1.to_i / 1000
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

