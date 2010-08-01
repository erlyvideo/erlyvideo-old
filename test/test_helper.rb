require 'test/unit'
# require 'test_help'
require 'timeout'


module Alarm
  require 'dl/import'
  extend DL::Importable
  path = File.exists?('/lib/libc.so') ? '/lib/libc.so' :
         File.exists?('/usr/lib/libSystem.B.dylib') ? '/usr/lib/libSystem.B.dylib' : nil
  dlload path
  extern "unsigned int alarm(unsigned int)"
end

class Test::Unit::TestCase
  class NotFound404 < StandardError
  end
  
  def restart_erlyvideo
    `#{File.dirname(__FILE__)}/../contrib/erlyctl restart`
    sleep 3
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

    trap("ALRM") do
      begin
        Process.kill("KILL", pid) 
      rescue Errno::ESRCH
      end
    end
    
    Alarm.alarm(timeout)
    begin
      Process.waitpid(pid, Process::WUNTRACED)
      status = $?.exitstatus
    rescue Errno::ECHILD
      status = nil
    end
    
    [status, File.read("/tmp/output.txt")]
  end
  
  def media_info(url, options = nil)
    if url =~ /http:\/\//
      File.unlink("/tmp/test") if File.exists?("/tmp/test")
      status, output = limited_run("curl --connect-timeout 1 --fail -s -S -o /tmp/test \"#{url}\"", 5)
      raise NotFound404, "no resource: #{url.inspect}" if status == 22
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

