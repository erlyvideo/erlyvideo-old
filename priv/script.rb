#!/usr/bin/env ruby
require 'rubygems'
require 'eventmachine'
require 'bertrem'

module Test
  # Add two numbers together
  def add(a, b)
    a + b
  end

  # Return the given number of bytes
  def bytes(n)
    'x' * n
  end

  # Sleep for +sec+ and then return :ok
  def slow(sec)
    sleep(sec)
    :ok
  end

  # Throw an error
  def error
    raise "abandon hope!"
  end
end

module Application
  def connect(*args)
    puts "Connect: #{args.inspect}"
    
    nil
  end
  
  def getStreamLength(movie)
    puts "Movie length: #{movie.inspect}"
    1000
  end
end

input = IO.new(3)
output = IO.new(4)
output.sync = true

BERTREM::Server.logfile(STDOUT)
BERTREM::Server.loglevel(Logger::DEBUG)

BERTREM::Server.send(:define_method, :write_berp) do |ruby|
  data = BERT.encode(ruby)
  output.write([data.length].pack("N"))
  output.write(data)
end

EM.run do
  BERTREM::Server.expose(:application, Application)
  BERTREM::Server.expose(:test, Test)
  
  puts "Attaching to #{input.inspect}"
  svc = EM.attach(input, BERTREM::Server)
end

